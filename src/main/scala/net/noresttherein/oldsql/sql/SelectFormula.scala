package net.noresttherein.oldsql.sql

import java.sql.PreparedStatement

import net.noresttherein.oldsql.collection.{Chain, NaturalMap, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.generic.{=#>, Self}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, ColumnMappingExtract, ColumnReadForm, ColumnWriteForm, ComponentValues, Mapping, MappingExtract, SQLForm, SQLReadForm, SQLWriteForm, TypedMapping}
import net.noresttherein.oldsql.schema.support.LazyMapping
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.@:
import net.noresttherein.oldsql.schema.Buff.NoSelectByDefault
import net.noresttherein.oldsql.schema.support.MappingAdapter.ShallowAdapter
import net.noresttherein.oldsql.schema.ColumnForm.JDBCSQLType
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.sql.AutoConversionFormula.PromotionConversion
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, OuterFrom, SubselectOf}
import net.noresttherein.oldsql.sql.JoinParam.{FromParam, WithParam}
import net.noresttherein.oldsql.sql.MappingFormula.{ColumnComponentFormula, ComponentFormula, JoinedRelation, SQLRelation}
import net.noresttherein.oldsql.sql.SQLCondition.Exists
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, CaseFormula, ColumnFormula, CompositeFormula, Formula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula.{CaseColumnFormula, ColumnFormulaMatcher}
import net.noresttherein.oldsql.sql.SQLMapper.FixedResult
import net.noresttherein.oldsql.sql.SQLScribe.{ColumnResult, ComponentSubstitutions, FormulaResult}
import net.noresttherein.oldsql.sql.SQLTerm.BoundParameter
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple.MatchChain
import net.noresttherein.oldsql.sql.SQLTuple.{ChainTuple, SeqTuple}
import slang._


/** Representation of an SQL select as an SQL formula used in the context of source `F`. If the source is
  * the abstract `FromClause`, this will be a `FreeSelectFormula` instance - a select independent of any external
  * tables or parameters, in which all formulas (''select'' clause, ''where'' clause, etc) can be evaluated
  * based on the values of the tables in its ''from'' clause. If `F` is not `FromClause`, but contains tables, this is
  * a subselect nested inside a select for source `F` - in its header, ''from'' or ''where'' clause. The source for
  * this formula, given by the member type `From`, is always an extension of `F`, and in fact
  * `From &lt;: SubselectOf[F]`, where `F` is the actual source type parameter given at this instance's creation -
  * we cannot declare it so because of contravariance. Subclasses should extend the trait for one of the above
  * cases: [[net.noresttherein.oldsql.sql.SelectFormula.FreeSelectFormula FreeSelectFormula]] or
  * [[net.noresttherein.oldsql.sql.SelectFormula.SubselectFormula SubselectFormula]], instead of deriving directly
  * from this trait.
  *
  * Apart from being an SQL formula, it is also a `TypedMapping[V, O]`, so it can be used as other mappings inside
  * a ''from'' clause.
  * @tparam F the source of data for the ''enclosing'' select - tables from the ''from'' clause and any unbound parameters.
  * @tparam V the mapped header type representing a single row.
  */
sealed trait SelectFormula[-F <: FromClause, V, O] extends SQLFormula[F, Rows[V]] with TypedMapping[V, O] {
	/** The from clause of this select */
	type From <: FromClause //SubselectOf[F]

	trait SelectedColumn[X] {
		def name :String
		def formula :ColumnFormula[From, X]
	}

	def header :SQLFormula[From, V]

	def headerColumns :Seq[SelectedColumn[_]]

	val from :From

	def tables :Seq[JoinedRelation.AnyIn[from.Generalized]] = from.subselectTableStack

	def filter :BooleanFormula[from.Generalized] = from.filter



	def exists :ColumnFormula[F, Boolean] = Exists(this)

	def notExists :ColumnFormula[F, Boolean] = !Exists(this)

	def single :SQLFormula[F, V] = to[V] //new SelectAsRow(this)

	def rows :SQLFormula[F, Seq[V]] = to[Seq[V]] //new SelectAsRows(this)



	override def readForm :SQLReadForm[Rows[V]] = header.readForm.map(Rows(_))

//	override def get(values: RowValues[F]) :Option[Rows[V]] =
//		header.get(values.asInstanceOf[RowValues[From]]).map(Rows(_))

//	override def isGroundedIn(tables: Iterable[AnyJoinedRelation]): Boolean =
//		header.isGroundedIn(tables)

	override def freeValue :Option[Rows[V]] = header.freeValue.map(Rows(_))

	override def isFree :Boolean = header.isFree


	protected override def reverseCollect[X](fun: PartialFunction[Formula[_], X], acc: List[X]): List[X] =
		reverseCollect(filter)(fun, reverseCollect(header)(fun, super.reverseCollect(fun, acc)))



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

	override def toString = s"SELECT $header FROM $from"

}






object SelectFormula {

	def apply[F <: OuterFrom, T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, O]
	         (from :F, header :ComponentFormula[F, T, E, M, V, O]) :TypedSelectMapping[F, M, V, O] =
		new SelectComponent[F, T, E, M, V, O](from, header)
	
	def apply[F <: OuterFrom, V, O](from :F, header :SQLTuple[F, V]) :FreeSelectFormula[V, O] =
		new ArbitraryFreeSelect[F, V, O](from, header) 
	
	def apply[F <: OuterFrom, X, Y, O](from :F, header :AutoConversionFormula[F, X, Y]) :FreeSelectFormula[Y, O] =
		new ArbitraryFreeSelect[F, Y, O](from, header)

	def apply[F <: OuterFrom, V, O](from :F, header :ColumnFormula[F, V]) :FreeSelectColumn[V, O] =
		new ArbitraryFreeSelectColumn(from, header)



	def subselect[F <: FromClause, S <: SubselectOf[F], T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, O]
	             (from :S, header :ComponentFormula[S, T, E, M, V, O]) :TypedSubselectMapping[F, S, M, V, O] =
		new SubselectComponent[F, S, T, E, M, V, O](from, header)

	def subselect[F <: FromClause, S <: SubselectOf[F], V, O](from :S, header :SQLTuple[S, V])
			:SubselectFormula[F, S, V, O] =
		new ArbitrarySubselect[F, S, V, O](from, header)

	def subselect[F <: FromClause, S <: SubselectOf[F], X, Y, O](from :S, header :AutoConversionFormula[S, X, Y])
			:SubselectFormula[F, S, Y, O] =
		new ArbitrarySubselect[F, S, Y, O](from, header)

	def subselect[F <: FromClause, S <: SubselectOf[F], V, O](from :S, header :ColumnFormula[S, V])
			:SubselectColumn[F, S, V, O] =
		new ArbitrarySubselectColumn(from, header)






	type * = SelectFormula[_ <: FromClause, _, _]



	trait SelectColumn[-F <: FromClause, V, O] extends SelectFormula[F, V, O] with ColumnFormula[F, Rows[V]] {
		override val header :ColumnFormula[From, V]
		override def readForm :ColumnReadForm[Rows[V]] = header.readForm.map(Rows(_))

		override def single :ColumnFormula[F, V] = to[V]
	}
	
	/** A `SelectFormula` interface exposing the mapping type `H` used as the header.
	  * Extending classes work as adapters for that mapping.
	  */
	trait SelectAs[-F <: FromClause, H <: Mapping] extends SelectFormula[F, H#Subject, H#Origin] {
		val mapping :H

//		def header = mapping

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[SelectAs[_, _]]

		override def equals(that :Any) :Boolean = super[SelectFormula].equals(that)

		override def hashCode :Int = super[SelectFormula].hashCode

		override def toString :String = super[SelectFormula].toString
	}






	/** Base trait for SQL select expressions whose header depends solely on the explicit FROM clause of the select,
	  * i.e. it is not dependent on any outside rows. Such an expression is a valid select statement in opposition to
	  * subselect expressions.
	  */
	trait FreeSelectFormula[V, O] extends SelectFormula[FromClause, V, O] {
		override type From <: OuterFrom

		override def stretch[U <: FromClause, S <: FromClause]
		                    (target :S)(implicit ev :U ExtendedBy S) :FreeSelectFormula[V, O] =
			this

		override def applyTo[Y[_]](matcher: FormulaMatcher[FromClause, Y]): Y[Rows[V]] = matcher.select(this)
	}
	
	trait FreeSelectColumn[V, O] extends FreeSelectFormula[V, O] with SelectColumn[FromClause, V, O] {

		override def stretch[U <: FromClause, S <: FromClause]
		                    (target :S)(implicit ev :U ExtendedBy S) :FreeSelectColumn[V, O] =
			this

		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[FromClause, Y]) :Y[Rows[V]] = matcher.select(this)
	}


	//todo: fix inconsistency: subselects are parameterized with From, but selects not.
	/** A base trait for all SQL select expressions nested under another SQL select.
	  * @tparam F the ''from'' clause of the outer select, forming a prefix of `S` until the last occurrence
	  *           of a `Subselect` join kind.
	  * @tparam S the inlined synthetic ''from'' clause of this subselect.
	  * @tparam O marker origin type serving as a unique alias for different members of a FROM clause.
	  * @tparam V the type of the scala value selected by this subselect.
	  */
	trait SubselectFormula[-F <: FromClause, S <: SubselectOf[F], V, O] extends SelectFormula[F, V, O] {
		override type From = S

		override def applyTo[Y[_]](matcher: FormulaMatcher[F, Y]): Y[Rows[V]] = matcher.subselect(this)
	}
	
	trait SubselectColumn[-F <: FromClause, S <: SubselectOf[F], V, O] 
		extends SubselectFormula[F, S, V, O] with SelectColumn[F, V, O]
	{
		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[F, Y]) :Y[Rows[V]] = matcher.subselect(this)
	}



	trait SelectMapping[F <: OuterFrom, H <: Mapping]
		extends SelectAs[F, H] with FreeSelectFormula[H#Subject, H#Origin]
	{
		override type From = F
	}

	trait SubselectMapping[-F <: FromClause, S <: SubselectOf[F], H <: Mapping]
		extends SelectAs[F, H] with SubselectFormula[F, S, H#Subject, H#Origin]

	trait TypedSelectMapping[F <: OuterFrom, H[A] <: TypedMapping[V, A], V, O] extends SelectMapping[F, H[O]]

	trait TypedSubselectMapping[-F <: FromClause, S <: SubselectOf[F], H[A] <: TypedMapping[V, A], V, O]
		extends SubselectMapping[F, S, H[O]]






	private abstract class SelectComponentFormula[-F <: FromClause, S <: SubselectOf[F],
	                                              T[A] <: TypedMapping[E, A], E, H[A] <: TypedMapping[V, A], V, O]
	                       (override val from :S, override val  header :ComponentFormula[S, T, E, H, V, O])
		extends SelectAs[F, H[O]] with ShallowProxy[V, O]
	{
		override type From = S

		protected override val egg = header.mapping
		override val mapping: H[O] = header.mapping

		override val headerColumns: Seq[SelectedColumn[_]] = mapping.selectable.toSeq.map(include(_))

		private def include[X](column :Column[X]) :SelectedColumn[X] = new SelectedColumn[X] {
			override val name :String = column.name
			override val formula = header \ column
		}


		override def optionally(pieces :Pieces) = pieces.assemble(this)


		override val extracts = super.extracts
		override val columnExtracts = super.columnExtracts
	}



	private class SelectComponent[F <: OuterFrom, T[A] <: TypedMapping[E, A], E, H[A] <: TypedMapping[V, A], V, O]
	                             (from :F, header :ComponentFormula[F, T, E, H, V, O])
		extends SelectComponentFormula[FromClause, F, T, E, H, V, O](from, header) with TypedSelectMapping[F, H, V, O]



	private class SubselectComponent[-F <: FromClause, S <: SubselectOf[F],
	                                 T[A] <: TypedMapping[E, A], E, H[A] <: TypedMapping[V, A], V, O]
	                                (subselect :S, component :ComponentFormula[S, T, E, H, V, O])
		extends SelectComponentFormula[F, S, T, E, H, V, O](subselect, component)
		   with TypedSubselectMapping[F, S, H, V, O]
	{
		override def stretch[U <: F, G <: FromClause](clause :G)(implicit ev :U ExtendedBy G) = {
			type Ext = FromClause { type Outer = G }
			val upcast = from :FromClause //scalac bug workaround
			val stretched = upcast.asSubselectOf(clause)(ev.asInstanceOf[upcast.Outer ExtendedBy G])
    				.asInstanceOf[Ext]
			val subselectTables = stretched.size - clause.size
			val table = header.from
			val replacement =
				if (table.shift < subselectTables) table.asInstanceOf[SQLRelation[Ext, T, E, O]]
				else stretched.tableStack(table.shift + ev.length).asInstanceOf[SQLRelation[Ext, T, E, O]]
			val component = replacement \ header.mapping
			new SubselectComponent[G, Ext, T, E, H, V, O](stretched, component)
		}

	}





	/** A select formula based on the given row source and selecting an arbitrary expression `header` in its ''select''
	  * clause. This header will be translated by recursively flat mapping the header expression to obtain a flat sequence
	  * of columns. In particular, any sequences/tuples are inlined, and any `ComponentFormula`s referring to components
	  * of tables or whole table rows themselves are replaced with their columns. Column list declared by this mapping
	  * is thus created by recursively applying the following rules to the header formula:
	  *
	  *     1. If the formula is a component mapping, create a column for every export column of the declared mapping;
	  *     1. If the formula is a composite formula such as a tuple, recursively flatMap over it by applying these rules;
	  *     1. In other cases the formula is taken to represent a single column in the resulting select and
	  *        an appropriate column is created.
	  *
	  * Note that the above column list should be considered in the context of this instance as a mapping and represents
	  * all columns that potentially might be a part of the select clause. Existence of non-selectable and optional
	  * columns means that resulting select query may not contain all of the above. This distinction is also present
	  * when using this instance to assemble results of the created select statement; as individual columns
	  * in the select header may be any formulas, the source of values for evaluating the header formula are not values
	  * of the tables of the underlying source, but values for the whole column formulas. For example, a header formula
	  * in the form of `(current_date - birth_date, address, (first_name, family_name)) from users` could translate
	  * into a select formula declaring columns: `('col1', street, zip, city, country, first_name, family_name)`.
	  * Such columns would be available for any formulas using this mapping in their FromClause and are considered
	  * 'available header columns'. However, when using this instance as a mapping for assembling the header value,
	  * we don't have values for individual columns of the users table in the above example, but values for the columns
	  * declared by this mapping. This means that we need a bit of creative term rewriting to assemble the scala value
	  * as it would be evaluated by the original header formula. In particular, in the above example, the address object
	  * would be reassembled based on the values of individual columns included in the final select.
	  */
	private abstract class ArbitrarySelectFormula[-F <: FromClause, S <: SubselectOf[F], V, O] protected
	                                             (override val from :S, override val header :SQLFormula[S, V])
		extends SelectFormula[F, V, O] with LazyMapping[V, O]
	{ outer =>

		def this(from :S, header :ColumnFormula[S, V]) = this(from, header :SQLFormula[S, V])

		def this(from :S, header :CompositeFormula[S, V]) = this(from, header :SQLFormula[S, V])

		def this(from :S, header :ComponentFormula[S, T, E, M, V, O] forSome {
				type T[A] <: TypedMapping[E, A]; type E; type M[A] <: TypedMapping[V, A]
			}) =
			this(from, header :SQLFormula[S, V])



		override type From = S
		
		/** A column in the header of owning select.
		  * @param formula sql expression providing the value for the column.
		  * @param name column name (sqlName) and suggested alias for the column in the select clause.
		  */
		class ColumnExpression[T](override val formula :ColumnFormula[S, T], override val name :String)
		                         (implicit override val form :ColumnForm[T] = //implicit only so that the arg list can be omitted
		                             formula.readForm <> ColumnWriteForm.dummy[T](formula.readForm.sqlType))
			extends ColumnMapping[T, O] with SelectedColumn[T]


/*

		private type ColumnListing[X] = List[ColumnExpression[_]]]

		private class ColumnCollector extends FormulaMatcher[S, ColumnListing]
			with CaseFormula[S, ColumnListing] with CaseColumnFormula[S, ColumnListing]
		{
			private[this] var names :Set[String] = Set("") //used column names, disallowing ""

			override def column[X](e :ColumnFormula[S, X]) = {
				implicit val form :ColumnForm[X] = e.readForm match {
					case form :ColumnForm[X @unchecked] => form
					case form => form <> ColumnWriteForm.dummy(e.readForm.sqlType)
				}
				new ColumnExpression(e, nameFor(e))::Nil
			}

			override def component[T[B] <: TypedMapping[E, B], E, M[B] <: TypedMapping[X, B], X, A]
			                      (e :ComponentFormula[S, T, E, M, X, A]) =
			{
				val table = e.table

				def headerColumn[C](column :table.Column[C]) :ColumnExpression[C] = {
					val expr = e.from \ column
					new ColumnExpression[C](e.from \ column, nameFor(expr))
				}
				e.mapping.selectable.view.map(table.export(_)).filter(NoSelectByDefault.disabled)
					.map(headerColumn).toList
			}

			override def tuple[X](e :SQLTuple[S, X]) =
				e.inOrder.view.flatMap(this(_)).toList

			override def conversion[T, U](e :AutoConversionFormula[S, T, U]) =
				this(e.expr)

			override def formula[X](e :SQLFormula[S, X]) =
				throw new IllegalArgumentException(
					s"SQLFormula $e cannot be used in a SelectFormula header expression."
				)



			private def nameFor(f :ColumnFormula[S, _]) :String = {
				val name :String = f match {
					case FromParam(param, extract) =>
						if (extract.isIdentity && !names(param.name)) param.name
						else param.name + "_" + columns.size

					case ComponentFormula(_, MappingExtract(_, _, component)) =>
						val name :String = component match {
							case column :ColumnMapping[_, _] => column.name
							case label @: _ => label
							case _ => component.sqlName getOrElse ""
						}
						if (!names(name)) name else name + "_" + columns.size

					case BoundParameter(_, Some(name)) => //unlikely to appear in this position
						if (!names(name)) name else name + "_" + columns.size

					case _ => "_" + columns.size
				}
				names = names + name
				name
			}
		}
*/

//		/** A source which will be used to provide data for evaluating the header during this select mapping's assembly
//		  * process. When performing the mapping of rows returned by an arbitrary select, we don't have values
//		  * for tables listed in its from clause, but values for column expressions in its header clause.
//		  * Therefore when evaluating the header, we won't have values for tables in source, just `ComponentValues`
//		  * instance for this mapping. */
//		private val fromForAssembly = from param SQLForm.Unknown[Pieces]()
//
//		private type FromPieces = S WithParam Pieces
//		private type Res[T] = SQLFormula[FromPieces, T]
//
//		/** Rewrites the header formula, treating it as a sequence of top-level column formulas.
//		  * Each such column formula is replaced with a `ParamMapping` derived from the parameter `Pieces` added
//		  * to the source FROM clause `S`. */
//		private var headerRewriter = new HeaderRewriter
//
//		private class HeaderRewriter extends SQLScribe[S, FromPieces]
//			with CaseFormula[S, FormulaResult[FromPieces]#T] with CaseColumnFormula[S, ColumnResult[FromPieces]#T]
//		{
//			var columns :List[ColumnExpression[_]] = Nil
//			private[this] var names :Set[String] = Set("") //used column names, disallowing ""
//
//			override def column[X](e :ColumnFormula[S, X]) :ColumnFormula[FromPieces, X] = {
//				implicit val form :ColumnForm[X] = e.readForm match {
//					case form :ColumnForm[X @unchecked] => form
//					case form => form <> ColumnWriteForm.dummy(e.readForm.sqlType)
//				}
//				val col = new ColumnExpression(e, nameFor(e))
//				columns = col :: columns
//				val param = fromForAssembly.last.mapping.optcol { pieces => pieces.get(col) }
//				fromForAssembly.last \ param
//			}
//
//			override def component[T[B] <: TypedMapping[E, B], E, M[B] <: TypedMapping[X, B], X, A]
//			                      (e :ComponentFormula[S, T, E, M, X, A]) :Res[X] =
//			{
//				val substitution = new ComponentSubstitution(e)
//				columns = substitution.columns reverse_::: columns
//				substitution.substitute
//			}
//
//			//the only composites that undergo structural mapping, anything else throws up from this.formula
//			override def tuple[X](e: SQLTuple[S, X]): Res[X] = e.map(this)
//
//			override def conversion[Z, X](e :AutoConversionFormula[S, Z, X]) :Res[X] = e.map(this)
//
//
//			override def formula[X](e :SQLFormula[S, X]) =
//				throw new IllegalArgumentException(
//					s"SQLFormula $e cannot be used in a SelectFormula header expression."
//				)
//
//
//
//			private def nameFor(f :ColumnFormula[S, _]) :String = {
//				val name :String = f match {
//					case FromParam(param, extract) =>
//						if (extract.isIdentity && !names(param.name)) param.name
//						else param.name + "_" + columns.size
//
//					case ComponentFormula(_, MappingExtract(_, _, component)) =>
//						val name :String = component match {
//							case column :ColumnMapping[_, _] => column.name
//							case label @: _ => label
//							case _ => component.sqlName getOrElse ""
//						}
//						if (!names(name)) name else name + "_" + columns.size
//
//					case BoundParameter(_, Some(name)) => //unlikely to appear in this position
//						if (!names(name)) name else name + "_" + columns.size
//
//					case _ => "_" + columns.size
//				}
//				names = names + name
//				name
//			}
//		}
//
//
//
//		/** Substitution of a `ComponentFormula` referring to a table or a table component/column
//		  * for the exploded list of its columns.
//		  * @param component component formula occurring in header formula (select clause) */
//		private class ComponentSubstitution[T[B] <: TypedMapping[E, B], E, M[B] <: TypedMapping[X, B], X, A]
//		                                   (component :ComponentFormula[S, T, E, M, X, A])
//		{
//			private[this] val table = component.table.asInstanceOf[RefinedMapping[T[A]#Subject, A]]
//
//			val (substitutions, columns) = {
//				def headerColumn[X](column :table.Column[X]) :Assoc[T[A]#Component, ColumnExpression, X] = {
//					val export = table.export(column)
//					Assoc(export, new ColumnExpression[X](component.table \ export, export.name))
//				}
//				val entries = component.mapping.selectable.toList.map(headerColumn(_)).filter {
//					entry => NoSelectByDefault.disabled(entry._1)
//				}
//				NaturalMap(entries :_*) -> entries.map(_._2)
//			}
//
//			private implicit val form = component.readForm <> SQLWriteForm.empty
//
//			/** A substitute formula replacing original component formula with a `ParamMapping` assembling its value
//			  * from values of columns of the outer select formula/mapping. */
//			val substitute :SQLFormula[FromPieces, M[A]#Subject] = fromForAssembly.last.mapping.opt { pieces =>
//				val substitutedValues = ComponentValues(table).apply(
//					new (T[A]#Component =#> Option) {
//						override def apply[X](x :RefinedMapping[X, A]) = substitutions.get(x) match {
//							case Some(col) => pieces.get(col)
//							case _ => None
//						}
//					}
//				)
//				val param = substitutedValues.get(component.mapping.asInstanceOf[RefinedMapping[M[A]#Subject, A]])
//				headerForAssembly \ param
//			}
//
//		}
//
//
//
//		/** Header formula with formulas for individual columns in the select clause substituted by join parameter
//		  * of type `ComponentValues`. It allows to evaluate the header formula using not the values for tables in its
//		  * original from clause, but values for column formulas returned by executing the select, passed
//		  * as this mapping's `ComponentValues`. Final terms of this formula are 'join parameters' which return
//		  * the value for a selected column from passed `ComponentValues`, either directly (single column),
//		  * or by assembling a mapped value using mapping for substituted formula. They are composed to form
//		  * the equivalent header formula by tuple formulas, preserved as they were in the original header formula
//		  * based on `this.from`.
//		  */
//		private val headerForAssembly = headerRewriter(header)

		private type Assembler[T] = Pieces => Option[T]

		private class AssemblerAssembler extends FormulaMatcher[S, Assembler]
			with CaseFormula[S, Assembler] with CaseColumnFormula[S, Assembler] with MatchChain[S, Assembler]
		{
			var columns :List[ColumnExpression[_]] = Nil
			private[this] var names :Set[String] = Set("") //used column names, disallowing ""


			override def column[X](e :ColumnFormula[S, X]) :Pieces => Option[X] = {
				implicit val form :ColumnForm[X] = e.readForm match {
					case form :ColumnForm[X @unchecked] => form
					case form => form <> ColumnWriteForm.dummy(e.readForm.sqlType)
				}
				val column = new ColumnExpression(e, nameFor(e))
				columns = column::columns
				pieces => pieces.get(column)
			}


			override def component[T[B] <: TypedMapping[E, B], E, M[B] <: TypedMapping[X, B], X, A]
			                      (e :ComponentFormula[S, T, E, M, X, A]) =
			{
				val table = e.table

				def headerColumn[C](column :table.Column[C]) :Assoc[table.Component, ColumnExpression, C] = {
					val expr = e.from \ column
					val selected = new ColumnExpression[C](e.from \ column, nameFor(expr))
					Assoc[table.Component, ColumnExpression, C](column, selected)
				}
				val mapping = e.mapping
				val columnMappings = mapping.selectable.view.map(table.export(_)).filter(NoSelectByDefault.disabled)
					.map(headerColumn(_)).toList
				columns = columnMappings.map(_._2) reverse_::: columns
				val aliases = NaturalMap(columnMappings :_*)

				{ pieces :Pieces =>
					val values = ComponentValues[X, A](new (MappingFrom[A]#Component =#> Option) {
						override def apply[C](x :RefinedMapping[C, A]) = aliases.get(x) match {
							case Some(column) => pieces.get(column)
							case _ => None
						}
					})
					e.mapping.optionally(values)
				}
			}



			override def conversion[T, U](e :AutoConversionFormula[S, T, U]) = {
				val base = this(e.expr)
				pieces => base(pieces).map(e.convert)
			}



			override def emptyChain = _ => Some(@~)

			override def chainHead[T <: Chain, H](tail :ChainTuple[S, T], head :SQLFormula[S, H]) = {
				val tl = apply(tail)
				val hd = apply(head)
				pieces => for (t <- tl(pieces); h <- hd(pieces)) yield t ~ h
			}

			override def seq[X](e :SeqTuple[S, X]) = {
				val assemblers = e.inOrder.map(apply).reverse
				val acc = Option(List.empty[X])
				pieces => (acc /: assemblers) {
					(acc, assembler) => for (t <- acc; h <- assembler(pieces)) yield h::t
				}
			}



			override def formula[X](e :SQLFormula[S, X]) =
				throw new IllegalArgumentException(
					s"SQLFormula $e cannot be used in a SelectFormula header expression."
				)



			private def nameFor(f :ColumnFormula[S, _]) :String = {
				val name :String = f match {
					case FromParam(param, extract) =>
						if (extract.isIdentity && !names(param.name)) param.name
						else param.name + "_" + columns.size

					case ComponentFormula(_, MappingExtract(_, _, component)) =>
						val name :String = component match {
							case column :ColumnMapping[_, _] => column.name
							case label @: _ => label
							case _ => component.sqlName getOrElse ""
						}
						if (!names(name)) name else name + "_" + columns.size

					case BoundParameter(_, Some(name)) => //unlikely to appear in this position
						if (!names(name)) name else name + "_" + columns.size

					case _ => "_" + columns.size
				}
				names = names + name
				name
			}
		}




		private type Extractors[X] = Seq[Assoc[Column, ({ type E[T] = ColumnMappingExtract[X, T, O]})#E, _]]

		private class ExtractsCollector extends FormulaMatcher[S, Extractors]
			with CaseFormula[S, Extractors] with CaseColumnFormula[S, Extractors] with MatchChain[S, Extractors]
		{
			private[this] var columnStack = outer.headerColumns.toList

			override def column[X](e :ColumnFormula[S, X]) :Extractors[X] = {
				val column = columnStack.head.asInstanceOf[ColumnExpression[X]]
				columnStack = columnStack.tail
				type ColumnEx[T] = ColumnMappingExtract[X, T, O]
				Assoc[Column, ColumnEx, X](column, MappingExtract.ident(column))::Nil
			}



			override def component[T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[X, A], X, G]
			                      (e :ComponentFormula[S, T, E, M, X, G]) :Extractors[X] =
			{
				val table = e.table
				val component = e.mapping
				val componentColumns = component.selectable.view.map(table.export(_)).filter(NoSelectByDefault.disabled)
				val count = componentColumns.size
				val (selectColumns, tail) = columnStack.splitAt(count)
				columnStack = tail
				type ColumnEx[C] = ColumnMappingExtract[X, C, O]

				def extract[C](selectColumn :ColumnExpression[C], componentColumn :component.Column[_]) = {
					val compatible = componentColumn.asInstanceOf[component.Column[C]]
					val componentExtract = component(compatible)
					val selectExtract = MappingExtract(selectColumn)(componentExtract)
					Assoc[Column, ColumnEx, C](selectColumn, selectExtract)
				}

				selectColumns.zip(componentColumns).map { case (selCol, compCol) => extract(selCol, compCol) }
			}



			override def conversion[Z, X](e :AutoConversionFormula[S, Z, X]) = {
				val extracts = apply(e.expr)
				extracts.map(schema.composeColumnExtractAssoc(outer, Extractor.none :X =?> Z)(_))
			}

			override def promotion[T, U](e :PromotionConversion[S, T, U]) = {
				val extracts = apply(e.expr)
				extracts.map(schema.composeColumnExtractAssoc(outer, Extractor(e.lift.inverse))(_))
			}



			override def seq[X](e :SeqTuple[S, X]) = {
				e.inOrder.view.zipWithIndex.flatMap { entry :(SQLFormula[S, X], Int) =>
					apply(entry._1).map(schema.composeColumnExtractAssoc(outer, (seq :Seq[X]) => seq(entry._2))(_))
				}.toList
			}

			override def chainHead[T <: Chain, H](tail :ChainTuple[S, T], head :SQLFormula[S, H]) = {
				val tailExs = apply(tail).map(schema.composeColumnExtractAssoc(outer, (_ :(T ~ H)).init)(_))
				val headExs = apply(head).map(schema.composeColumnExtractAssoc(outer, (_:(T ~ H)).last)(_))
				headExs ++: tailExs
			}

			override def emptyChain = Nil



			override def formula[X](e :SQLFormula[S, X]) = unhandled(e)
		}






		override def assemble(pieces: Pieces): Option[V] = assembler(pieces)


		val (assembler, headerColumns) = {
			val aa = new AssemblerAssembler
			aa(header) -> aa.columns.reverse
		}
		override val columns :Unique[Column[_]] = Unique(headerColumns :_*)
		override def components = columns
		override val subcomponents = columns

		override val columnExtracts = NaturalMap.Lazy((new ExtractsCollector)[V](header) :Seq[Assoc[Column, ColumnExtract, _]])
		override val extracts = columnExtracts.asInstanceOf[ExtractMap]

	}

	
	
	private class ArbitraryFreeSelect[S <: OuterFrom, V, O](from :S, header :SQLFormula[S, V])
		extends ArbitrarySelectFormula[FromClause, S, V, O](from, header) with FreeSelectFormula[V, O]
	
	
	
	private class ArbitrarySubselect[-F <: FromClause, S <: SubselectOf[F], V, O](subclause :S, header :SQLFormula[S, V])
		extends ArbitrarySelectFormula[F, S, V, O](subclause, header) with SubselectFormula[F, S, V, O]
	{
		override def stretch[U <: F, G <: FromClause](clause :G)(implicit ev :U ExtendedBy G) :SQLFormula[G, Rows[V]] = {
			type Ext = FromClause { type Outer = G }
			val upcast = from :FromClause //scalac bug workaround
			val stretched = upcast.asSubselectOf(clause)(ev.asInstanceOf[upcast.Outer ExtendedBy G])
				.asInstanceOf[Ext]
			val substitute = With.shiftAside[S, Ext](stretched.size - clause.size, ev.length)
			new ArbitrarySubselect[G, Ext, V, O](stretched, substitute(header))
		}
	}


	
	//todo: these could use custom, much simplified implementations
	private class ArbitraryFreeSelectColumn[S <: OuterFrom, V, O](from :S, override val header :ColumnFormula[S, V])
		extends ArbitrarySelectFormula[FromClause, S, V, O](from, header) with FreeSelectColumn[V, O]

	
	
	private class ArbitrarySubselectColumn[-F <: FromClause, S <: SubselectOf[F], V, O]
	                                      (clause :S, override val header :ColumnFormula[S, V])
		extends ArbitrarySelectFormula[F, S, V, O](clause, header) with SubselectColumn[F, S, V, O]
	{
		override def stretch[U <: F, G <: FromClause](clause :G)(implicit ev :U ExtendedBy G) = {
			type Ext = FromClause { type Outer = G }
			val upcast :FromClause = from
			val stretched = upcast.asSubselectOf(clause)(ev.asInstanceOf[upcast.Outer ExtendedBy G])
				.asInstanceOf[Ext]
			val substitute = With.shiftAside[S, Ext](stretched.size - clause.size, ev.length)
			new ArbitrarySubselectColumn[G, Ext, V, O](stretched, substitute(header))
		}
	}

/*
	case class SelectAsRow[-F <: FromClause, H, O](select :SelectFormula[F, H, O])
		extends AutoConversionFormula[F, Rows[H], H]
	{
		override def expr = select

		override def convert(s: Rows[H]): H = s.head

		override def name = "SingleRow"


		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[H] = matcher.row(this)


		override def map[S <: FromClause](mapper :SQLScribe[F, S]) :AutoConversionFormula[S, Rows[H], H] =
			mapper(expr) match {
				case select :SelectFormula[S, H, _] => new SelectAsRow(select)
				case e => AutoConversionFormula(e)
			}

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
*/





	trait FreeSelectColumnMatcher[+F <: FromClause, +Y[X]] {
		def select[V, O](e :FreeSelectColumn[V, O]) :Y[Rows[V]]
	}

	type MatchFreeSelectColumn[+F <: FromClause, +Y[X]] = FreeSelectColumnMatcher[F, Y]

	type CaseFreeSelectColumn[+F <: FromClause, +Y[X]] = FreeSelectColumnMatcher[F, Y]



	trait SubselectColumnMatcher[+F <: FromClause, +Y[X]] {
		def subselect[S <: SubselectOf[F], V, O](e :SubselectColumn[F, S, V, O]) :Y[Rows[V]]
	}

	type MatchSubselectColumn[+F <: FromClause, +Y[X]] = SubselectColumnMatcher[F, Y]

	type CaseSubselectColumn[+F <: FromClause, +Y[X]] = SubselectColumnMatcher[F, Y]



	trait SelectColumnMatcher[+F <: FromClause, +Y[X]]
		extends FreeSelectColumnMatcher[F, Y] with SubselectColumnMatcher[F, Y]

	type MatchSelectColumn[+F <: FromClause, +Y[X]] = SelectColumnMatcher[F, Y]

	trait CaseSelectColumn[+F <: FromClause, +Y[X]] extends MatchSelectColumn[F, Y] {
		def select[V, O](e :SelectColumn[F, V, O]) :Y[Rows[V]]

		override def select[V, O](e :FreeSelectColumn[V, O]) :Y[Rows[V]] =
			select(e :SelectColumn[F, V, O])

		override def subselect[S <: SubselectOf[F], V, O](e :SubselectColumn[F, S, V, O]) :Y[Rows[V]] =
			select(e :SelectColumn[F, V, O])
	}



	trait FreeSelectMatcher[+F <: FromClause, +Y[X]] extends FreeSelectColumnMatcher[F, Y] {
		def select[V, O](e :FreeSelectFormula[V, O]) :Y[Rows[V]]
	}

	type MatchFreeSelect[+F <: FromClause, +Y[X]] = CaseFreeSelect[F, Y]

	trait CaseFreeSelect[+F <: FromClause, +Y[X]] extends FreeSelectMatcher[F, Y] with CaseFreeSelectColumn[F, Y] {
		override def select[V, O](e :FreeSelectColumn[V, O]) :Y[Rows[V]] = select(e :FreeSelectFormula[V, O])
	}



	trait SubselectMatcher[+F <: FromClause, +Y[X]] {
		def subselect[S <: SubselectOf[F], V, O](e :SubselectFormula[F, S, V, O]) :Y[Rows[V]]
	}

	type MatchSubselect[+F <: FromClause, +Y[X]] = CaseSubselect[F, Y]

	trait CaseSubselect[+F <: FromClause, +Y[X]] extends SubselectMatcher[F, Y] with CaseSubselectColumn[F, Y] {
		override def subselect[S <: SubselectOf[F], V, O](e :SubselectColumn[F, S, V, O]) :Y[Rows[V]] =
			subselect(e :SubselectFormula[F, S, V, O])
	}



	trait SelectMatcher[+F <: FromClause, +Y[X]] extends SelectColumnMatcher[F, Y]
		with FreeSelectMatcher[F, Y] with SubselectMatcher[F, Y]

	trait MatchSelect[+F <: FromClause, +Y[X]] extends SelectMatcher[F, Y]
		with CaseFreeSelect[F, Y] with CaseSubselect[F, Y]

	trait CaseSelect[+F <: FromClause, +Y[X]] extends MatchSelect[F, Y] {
		def select[V, O](e :SelectFormula[F, V, O]) :Y[Rows[V]]

		def subselect[S <: SubselectOf[F], V, O](e: SubselectFormula[F, S, V, O]): Y[Rows[V]] = select(e)

		def select[V, O](e: FreeSelectFormula[V, O]): Y[Rows[V]] = select(e :SelectFormula[F, V, O])
	}


}

