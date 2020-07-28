package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, ComponentValues, BaseMapping, Mapping, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.support.MappingProxy.ShallowProxy
import net.noresttherein.oldsql.schema.support.LazyMapping
import net.noresttherein.oldsql.schema.Mapping.ColumnFilter.AllColumns
import net.noresttherein.oldsql.schema.Mapping.{Component[_], Component}
import net.noresttherein.oldsql.sql.FromClause.{ParamSource, RowValues, SelectFrom, SubselectFrom, TableFormula}
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, CaseFormula, Formula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLMapper.SQLRewriter
import net.noresttherein.oldsql.sql.SQLTuple.CaseTuple
import net.noresttherein.oldsql.slang.SaferCasts._
import net.noresttherein.oldsql.sql.MappingFormula.ComponentFormula
import net.noresttherein.oldsql.sql.SQLCondition.ExistsFormula
import net.noresttherein.oldsql.sql.SelectFormula.{SelectAsRow, SelectAsRows}




/** Representation of an SQL select as an SQL formula used in the context of source `F`. If the source is
  * the abstract `FromClause`, this will be a `FreeSelectFormula` instance - a select independent of any external
  * tables or parameters, in which all formulas (''select'' clause, ''where'' clause, etc) can be evaluated
  * based on the values of the tables in its ''from'' clause. If `F` is not `FromClause`, but contains tables, this is
  * a subselect nested inside a select for source `F` - in its header, ''from'' or ''where'' clause. The source for
  * this formula, given by the member type `Source`, is always an extension of `F`, and in fact
  * `Source &lt;: AsSubselectOf[F]`, where `F` is the actual source type parameter given at this instance's creation -
  * we cannot declare it so because of contravariance.
  *
  * Apart from being an SQL formula, it is also a `BaseMapping[O, V]`, so it can be used as other mappings inside
  * a ''from'' clause.
  * @tparam F the source of data for the ''enclosing'' select - tables from the ''from'' clause and any unbound parameters.
  * @tparam V the mapped header type representing a single row.
  */
trait SelectFormula[-F <: FromClause, O, V] extends SQLFormula[F, Rows[V]] with BaseMapping[O, V] {
	type From <: FromClause

	trait SelectedColumn[X] {
		def name :String
		def formula :SQLFormula[From, X]
	}

	def header :SQLFormula[From, V]

	def headerColumns :Seq[SelectedColumn[_]]

	val from :From

	def tables :Seq[TableFormula[From, _, _, _]] =
		from.subselectTables.asInstanceOf[Seq[TableFormula[From, _, _, _]]]

	def filter :SQLFormula[From, Boolean] = from.filter.asInstanceOf[SQLFormula[From, Boolean]]



	def exists :SQLFormula[F, Boolean] = ExistsFormula[F, O, V](this)

	def notExists :SQLFormula[F, Boolean] = !ExistsFormula[F, O, V](this)

	def single :SelectAsRow[F, V] = new SelectAsRow(this)

	def rows :SelectAsRows[F, V] = new SelectAsRows(this)



	override def readForm :SQLReadForm[Rows[V]] = header.readForm.map(Rows(_))

	override def get(values: RowValues[F]) :Option[Rows[V]] =
		header.get(values.asInstanceOf[RowValues[From]]).map(Rows(_))

	override def isGroundedIn(tables: Iterable[TableFormula[_, _, _, _]]): Boolean =
		header.isGroundedIn(tables)

	override def freeValue :Option[Rows[V]] = header.freeValue.map(Rows(_))

	override def isFree :Boolean = header.isFree


	override protected def reverseCollect[X](fun: PartialFunction[SQLFormula[_<:FromClause, _], X], acc: List[X]): List[X] =
		filter.reverseCollect(fun, header.reverseCollect(fun, super.reverseCollect(fun, acc)))

	override def isomorphic(expression: Formula[_]): Boolean = expression match {
		case s :SelectFormula[_, _, _] =>
			(s eq this) || (s canEqual this) && (s.header isomorphic header) && (s.from == from)
		case _ => false
	}

	private[oldsql] override def equivalent(expression: Formula[_]): Boolean = expression match {
		case s :SelectFormula[_, _, _] =>
			(s eq this) || (s canEqual this) && (s.header equivalent header) && (s.from == from)
		case _ => false
	}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SelectFormula[_, _, _]]

	override def equals(that :Any) :Boolean = that match {
		case s :SelectFormula[_, _, _] => (s eq this) || (s canEqual this) && s.header == header && s.from == from
		case _ => false
	}

	override def hashCode :Int = header.hashCode * 31 + from.hashCode

	override def toString = s"select $header from $from"

}






object SelectFormula {

	def apply[F <: SelectFrom, T <: Component[O, E], M <: Component[O, V], O, E, V]
	         (from :F, header :ComponentFormula[F, T, M, O, E, V]) :SelectMapping[F, M, O, V] =
		new SelectComponentFormula[FromClause, F, T, M, O, E, V](from, header) with SelectMapping[F, M]

	def apply[F <: SelectFrom, V](from :F, header :SQLFormula[F, V]) :FreeSelectFormula[V] =
		header match {
			case comp :ComponentFormula[_, _, _, _, _, _] =>
				apply(from, comp.asInstanceOf[ComponentFormula[F, Component[Any, Any], Component[Any, V], Any, Any, V]])
			case _ =>
				new ArbitrarySelectFormula[FromClause, F, V](from, header) with FreeSelectFormula[V]
		}



	def subselect[F <: FromClause, S <: SubselectFrom[F], V](parent :F, from :S, header :SQLFormula[S, V]) :SubselectFormula[F, S, V] =
		subselect[F, S, V](from, header)

	def subselect[F <: FromClause, S <: SubselectFrom[F], V](from :S, header :SQLFormula[S, V]) :SubselectFormula[F, S, V] =
		header.ifSubclass[ComponentFormula[S, Mapping, Mapping[V]]] {
			comp => subselect[F, S, Mapping[V]](from, comp)
		} getOrElse
			new ArbitrarySelectFormula[F, S, V](from, header)

	def subselect[F <: FromClause, S <: SubselectFrom[F], H <: Mapping]
	             (from :S, header :ComponentFormula[S, _ <: Mapping, H]) :SubselectMapping[F, S, H] =
		new SelectComponentFormula[F, S, H](from, header) with SubselectMapping[F, S, H]

	def subselect[F <: FromClause, S <: SubselectFrom[F], H <: Mapping]
	             (parent :F, from :S, header :ComponentFormula[S, _ <: Mapping, H]) :SubselectMapping[F, S, H] =
		subselect[F, S, H](from, header)



	def exists[F <: FromClause, O, H](select :SelectFormula[F, O, H]) :BooleanFormula[F] = select.exists






	trait SelectAs[-F <: FromClause, H <: Mapping]
		extends SelectFormula[F, H#Owner, H#Subject] with ShallowProxy[H#Owner, H#Subject]
	{
		val schema :H

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[SelectAs[_, _]]

		override def equals(that :Any) :Boolean = super[SelectFormula].equals(that)

		override def hashCode :Int = super[SelectFormula].hashCode

		override def toString :String = super[SelectFormula].toString
	}



	/** A base trait for all SQL select expressions nested under another SQL select.
	  * @tparam F the from clause of the outer select.
	  * @tparam S the inlined synthetic from clause of this subselect.
	  * @tparam O marker trait serving as a unique alias for different members of a FROM clause.
	  * @tparam V the type of the scala value selected by this subselect.
	  */
	trait SubselectFormula[-F <: FromClause, S <: SubselectFrom[F], O, V] extends SelectFormula[F, O, V] {
		type From = S

		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[Rows[V]] = matcher.subselect(this)
	}

	/** Base trait for SQL select expressions which header depends solely on the explicit FROM clause of the select,
	  * i.e. it is not dependent on any outside rows. Such an expression is a valid select statement in opposition to
	  * subselect expressions.
	  */
	trait FreeSelectFormula[O, V] extends SelectFormula[FromClause, O, V] {
		override def applyTo[Y[+X]](matcher: FormulaMatcher[FromClause, Y]): Y[Rows[V]] = matcher.select(this)
	}

	trait SubselectMapping[-F <: FromClause, S <: SubselectFrom[F], H <: Mapping]
		extends SelectAs[F, H] with SubselectFormula[F, S, H#Owner, H#Subject]

	trait SelectMapping[F <: SelectFrom, H <: Mapping]
		extends SubselectMapping[FromClause, F, H] with FreeSelectFormula[H#Owner, H#Subject]



	private class SelectComponentFormula[-F <: FromClause, S <: SubselectFrom[F], T <: Component[O, E], H <: Component[O, V], O, E, V]
	                                    (val from :S, val  header :ComponentFormula[S, T, H, O, E, V])
		extends SelectAs[F, H] with SubselectFormula[F, S, H#Owner, H#Subject]
	{
		protected override val egg :H = header.component
		override val schema: H = header.component

		override val headerColumns: Seq[SelectedColumn[_]] = schema.selectable.toSeq.map(include)

		private def include[X](column :Component[X]) :SelectedColumn[X] = new SelectedColumn[X] {
			override val name :String = column.sqlName getOrElse {
				throw new IllegalArgumentException(s"Can't create select $header from $from: column $column has no sqlName")
			}

			override val formula = header :\ column
		}

	}



	/** A select formula based on the given row source and selecting an arbitrary expression `header` in its ''select''
	  * clause. This header will be translated by recursively flat mapping the header expression to obtain a flat sequence
	  * of columns. In particular, any sequences/tuples are inlined, and any `ComponentFormula`s referring to components
	  * of tables or whole last rows themselves are replaced with their columns. Column list declared by this mapping
	  * is thus created by recursively applying the following rules to the header formula:
	  *
	  *     1. If the formula is a component mapping, create a column for every lifted column of the declared mapping;
	  *     1. If the formula is a composite formula such as a tuple, recursively flatMap over it by applying these rules;
	  *     1. In other cases the formula is taken to represent a single column in the resulting select and
	  *        an appropriate column is created.
	  *
	  * Note that the above column list should be considered in the context of this instance as a mapping and represents
	  * all columns that potentially might be a part of the select clause. Existence of non-selectable and optional
	  * columns means that resulting select query may not contain all of the above. This distinctivon is also present
	  * when using this instance to assemble results of the created select statement; as individual columns
	  * in the select header may be any formulas, the source of values for evaluating the header formula are not values
	  * of the tables of the underlying source, but values for the whole column formulas. For example, a header formula
	  * in the form of `(current_date - birth_date, address, (first_name, family_name)) from users` could translate
	  * into a select formula declaring columns: `('col1', street, zip, city, country, first_name, family_name)`.
	  * Such columns would be available for any formulas using this mapping in their FromClause and are considered
	  * 'available header columns'. However, when using this instance as a mapping for assembling the header value,
	  * we don't have values for individual columns of the users last in the above example, but values for the columns
	  * declared by this mapping. This means that we need a bit of creative term rewriting to assemble the scala value
	  * as it would be evaluated by the original header formula. In particular, in the above example, the address object
	  * would be reassembled based on the values of individual columns included in the final select.
	  *
	  * @param from
	  * @param header
	  * @tparam F
	  * @tparam S
	  * @tparam H
	  */
	private class ArbitrarySelectFormula[-F <: FromClause, S <: SubselectFrom[F], O, H]
	                                    (val from :S, val header :SQLFormula[S, H])
		extends SubselectFormula[F, S, O, H] with LazyMapping[O, H]
	{ select =>

		/** A column in the header of owning select.
		  * @param formula sql formula providing value for the column
		  * @param name column name (sqlName) and suggested alias for the column in the select clause
		  */
		class ColumnExpression[T](val formula :SQLFormula[S, T], val name :String)
			extends ColumnMapping[Owner, T] with SelectedColumn[T]
		{ component =>

			if (formula.readForm.readColumns != 1)
				throw new IllegalArgumentException(s"Can't select $header from $from: $formula is not a column (${formula.readForm})")

			override val form :ColumnForm[T] = ColumnForm.combine(formula.readForm, SQLWriteForm.empty)

		}

		/** A source which will be used to provide data for evaluating the header during this select mapping assembly process.
		  * When performing the mapping of rows returned by an arbitrary select, we don't have values for tables listed
		  * in its from clause, but values for column expressions in its header clause. Therefore when evaluating the header,
		  * we won't have values for tables in source, just ComponentValues instance for this mapping.
		  */
		private val sourceForAssembly = ParamSource[Pieces]


		private type Res[T] = SQLFormula[ParamSource[Pieces], T]

		private val headerRewriter = new SQLMapper[S, Res] with CaseFormula[S, Res] with CaseTuple[S, Res] {
			private[this] var columns :List[Component[_]] = Nil

			override def component[T <: Mapping.Component[B, E], C <: Mapping.Component[B, V], B, E, V]
			                      (f: ComponentFormula[S, T, C, O, E, V]): SQLFormula[ParamSource[Pieces], V] =
			{
				val substitution = ComponentSubstitution(f)
				columns = substitution.columns reverse_::: columns
				substitution.substitute
			}

			override def tuple[X](f: SQLTuple[S, X]): SQLFormula[ParamSource[Pieces], X] =
				f.map[ParamSource[Pieces]](this)

			override def formula[X](f: SQLFormula[S, X]): SQLFormula[ParamSource[Pieces], X] = {
				val column = new ColumnExpression(f, nameFor(f))
				columns = column +: columns
				//todo: defer this form, as we don't know at this point which columns will be actually included
				implicit val form = f.readForm && SQLWriteForm.empty: SQLForm[X]
				sourceForAssembly.?[Pieces].opt { values => values.get(column) }
			}

			private def nameFor(f :SQLFormula[S, _]) :String = s"col_${columns.size}"
		}

		/** Header formula with formulas for individual columns in the select clause substituted by source parameter
		  * of type ComponentValues. It allows to evaluate the header formula using not the values for tables in its
		  * original from clause, but values for column formulas returned by executing the select passed as this mapping's
		  * ComponentValues. Final terms of this formula are 'source parameters' which return the value for a selected column
		  * from passed ComponentValues, either directly (single column) or by assembling a mapped value using mapping
		  * for substituted formula. They are composed to form equivalent header formula by tuple formulas, preserved
		  * as they were in original header formula grounded in source.
		  */
		private val headerForAssembly = headerRewriter(header)


		/** Substitution of a ComponentFormula referring to a last or last component/column into exploded list of its columns.
		  * @param component component formula occurring in header formula (select clause)
		  * @tparam T
		  * @tparam M
		  */
		private case class ComponentSubstitution[T <: Mapping, M <: Mapping](component :ComponentFormula[S, T, M]) {

			val (substitutions, columns) = {
				val mappedColumns =
					for (column <- component.path.lift(AllColumns)) yield {
						column -> new ColumnExpression(component.table :\ column, column.sqlName getOrElse {
							throw new IllegalArgumentException(s"Can't create select $header from $from: column $column has no name")
						}) :(T#Component[_], Component[_])
					}
				(mappedColumns.toMap[T#Component[_], Component[_]], mappedColumns.map(_._2))
			}

			//todo: defer this form, as we don't know at this point which columns will be actually included
			private implicit val form = component.readForm && SQLWriteForm.empty(component.readForm.readColumns): SQLForm[M#Subject]

			/** A substitute formula replacing original component formula with a parameter mapping assembling its value
			  * from values of columns of the outer select formula/mapping.
			  */
			val substitute = sourceForAssembly.?[Values].opt { values =>
				val substitutedValues = ComponentValues.of(component.table.mapping)(column => substitutions.get(column).flatMap(values.get(_)))
				substitutedValues.get(component.path)
			} :SQLFormula[ParamSource[Values], M#Subject]

		}

		val columns = headerRewriter.columns.reverse.indexed//columnsFor(header, Seq()).reverse.indexed
		def components = columns
		def subcomponents = columns
		def headerColumns = columns



		override def assemble(values: Values): Option[H] =
			headerForAssembly.get(RowValues(sourceForAssembly.last, values))



	}






	case class SelectAsRow[-F <: FromClause, H](select :SelectFormula[F, H]) extends AutoConversionFormula[F, Rows[H], H] {
		def expr = select

		override def convert(s: Rows[H]): H = s.head

		override def name = "SingleRow"


		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[H] = matcher.row(this)

		override def map[S <: FromClause](mapper: SQLRewriter[F, S]): AutoConversionFormula[S, Rows[H], H] =
			mapper(select) match {
				case sel :SelectFormula[_, _] => SelectAsRow(sel.asInstanceOf[SelectFormula[S, H]])
				case f => AutoConversionFormula(f)(_.head)
			}
	}

	case class SelectAsRows[-F <: FromClause, H](select :SelectFormula[F, H]) extends AutoConversionFormula[F, Rows[H], Seq[H]] {
		def expr = select

		override def convert(s: Rows[H]): Seq[H] = s.seq

		override def name = "Rows"


		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[Seq[H]] = matcher.rows(this)


		override def map[S <: FromClause](mapper: SQLRewriter[F, S]): AutoConversionFormula[S, Rows[H], Seq[H]] =
			mapper(select) match {
				case sel :SelectFormula[_, _] => SelectAsRows(sel.asInstanceOf[SelectFormula[S, H]])
				case f => AutoConversionFormula(f)(_.seq)
			}

	}






	trait GroundedSelectMatcher[+F <: FromClause, +Y[X]] {
		def select[X](f :FreeSelectFormula[X]) :Y[Rows[X]]
	}

	trait SubselectMatcher[+F <: FromClause, +Y[X]] {
		def subselect[X](f :SelectFormula[F, X]) :Y[Rows[X]]
	}

	trait SelectMatcher[+F <: FromClause, +Y[X]] extends GroundedSelectMatcher[F, Y] with SubselectMatcher[F, Y]

	type MatchSelect[+F <: FromClause, +Y[X]] = SelectMatcher[F, Y]

	trait CaseSelect[+F <: FromClause, +Y[X]] extends MatchSelect[F, Y] {
		def select[X](f :SelectFormula[F, X]) :Y[Rows[X]]

		def subselect[X](f: SelectFormula[F, X]): Y[Rows[X]] = select(f)

		def select[X](f: FreeSelectFormula[X]): Y[Rows[X]] = select(f :SelectFormula[F, X])
	}


	trait SingleRowMatcher[+F <: FromClause, +Y[X]] {
		def row[X](f :SelectAsRow[F, X]) :Y[X]
	}

	trait MultipleRowsMatcher[+F <: FromClause, +Y[X]] {
		def rows[X](f :SelectAsRows[F, X]) :Y[Seq[X]]
	}

	type MatchRow[+F <: FromClause, +Y[X]] = SingleRowMatcher[F, Y]

	type CaseRow[+F <: FromClause, +Y[X]] = SingleRowMatcher[F, Y]

	type MatchRows[+F <: FromClause, +Y[X]] = MultipleRowsMatcher[F, Y]

	type CaseRows[+F <: FromClause, +Y[X]] = MultipleRowsMatcher[F, Y]



}

