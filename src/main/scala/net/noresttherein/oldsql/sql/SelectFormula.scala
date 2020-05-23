package net.noresttherein.oldsql.sql

import java.sql.PreparedStatement

import net.noresttherein.oldsql.collection.{Chain, NaturalMap, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.generic.=#>
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnMapping, ColumnMappingExtract, ColumnReadForm, ColumnWriteForm, ComponentValues, Mapping, MappingExtract, SQLReadForm, TypedMapping}
import net.noresttherein.oldsql.schema.support.LazyMapping
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.@:
import net.noresttherein.oldsql.schema.Buff.NoSelectByDefault
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.sql.AutoConversionFormula.PromotionConversion
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, OuterFrom, SubselectOf}
import net.noresttherein.oldsql.sql.JoinParam.FromParam
import net.noresttherein.oldsql.sql.MappingFormula.{ComponentFormula, JoinedRelation, SQLRelation}
import net.noresttherein.oldsql.sql.SQLCondition.Exists
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, CaseFormula, ColumnFormula, Formula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula.{CaseColumnFormula, ColumnFormulaMatcher}
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

	def tables :Seq[SQLRelation.AnyIn[from.Generalized]] = from.subselectTableStack

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



	override def stretch[U <: F, S <: FromClause](target :S)(implicit ev :U ExtendedBy S) :SelectFormula[S, V, O]



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

	def apply[F <: OuterFrom, T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, I >: F <: FromClause, O]
	         (from :F, header :ComponentFormula[F, T, E, M, V, I]) :TypedSelectMapping[F, M, V, O] =
		new SelectComponent[F, T, E, M, V, I, O](from, header)
	
	def apply[F <: OuterFrom, V, O](from :F, header :SQLTuple[F, V]) :FreeSelectFormula[V, O] =
		new ArbitraryFreeSelect[F, V, O](from, header) 
	
	def apply[F <: OuterFrom, X, Y, O](from :F, header :AutoConversionFormula[F, X, Y]) :FreeSelectFormula[Y, O] =
		new ArbitraryFreeSelect[F, Y, O](from, header)

	def apply[F <: OuterFrom, V, O](from :F, header :ColumnFormula[F, V]) :FreeSelectColumn[V, O] =
		new ArbitraryFreeSelectColumn(from, header)



	def subselect[F <: FromClause, S <: SubselectOf[F],
		          T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, I >: S <: FromClause, O]
	             (from :S, header :ComponentFormula[S, T, E, M, V, I]) :TypedSubselectMapping[F, S, M, V, O] =
		new SubselectComponent[F, S, T, E, M, V, I, O](from, header)

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

		override def stretch[U <: F, S <: FromClause](target :S)(implicit ev :U ExtendedBy S) :SelectColumn[S, V, O]
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

		override def applyTo[Y[_]](matcher: FormulaMatcher[FromClause, Y]): Y[Rows[V]] = matcher.freeSelect(this)
	}

	trait FreeSelectColumn[V, O] extends FreeSelectFormula[V, O] with SelectColumn[FromClause, V, O] {

		override def stretch[U <: FromClause, S <: FromClause]
		                    (target :S)(implicit ev :U ExtendedBy S) :FreeSelectColumn[V, O] =
			this

		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[FromClause, Y]) :Y[Rows[V]] = matcher.freeSelect(this)
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

		override def stretch[U <: F, G <: FromClause](target :G)(implicit extension :U ExtendedBy G)
			:SubselectFormula[G, _ <: SubselectOf[G], V, O]

		override def applyTo[Y[_]](matcher: FormulaMatcher[F, Y]): Y[Rows[V]] = matcher.subselect(this)
	}

	trait SubselectColumn[-F <: FromClause, S <: SubselectOf[F], V, O]
		extends SubselectFormula[F, S, V, O] with SelectColumn[F, V, O]
	{
		override def stretch[U <: F, G <: FromClause](base :G)(implicit extension :U ExtendedBy G)
				:SubselectColumn[G, _ <: SubselectOf[G], V, O]

		override def applyTo[Y[_]](matcher :ColumnFormulaMatcher[F, Y]) :Y[Rows[V]] = matcher.subselect(this)
	}



	trait SelectMapping[F <: OuterFrom, H <: Mapping]
		extends SelectAs[F, H] with FreeSelectFormula[H#Subject, H#Origin]
	{
		override type From = F
	}

	trait SubselectMapping[-F <: FromClause, S <: SubselectOf[F], H <: Mapping]
		extends SelectAs[F, H] with SubselectFormula[F, S, H#Subject, H#Origin]

	trait TypedSelectMapping[F <: OuterFrom, H[A] <: TypedMapping[V, A], V, O] extends FreeSelectFormula[V, O] with SelectMapping[F, H[O]]

	trait TypedSubselectMapping[-F <: FromClause, S <: SubselectOf[F], H[A] <: TypedMapping[V, A], V, O]
		extends SubselectFormula[F, S, V, O] with SubselectMapping[F, S, H[O]]






	private abstract class SelectComponentFormula[-F <: FromClause, S <: SubselectOf[F], T[A] <: TypedMapping[E, A], E,
	                                              H[A] <: TypedMapping[V, A], V, I >: S <: FromClause, O]
	                       (override val from :S, override val  header :ComponentFormula[S, T, E, H, V, I])
		extends SelectFormula[F, V, O] with SelectAs[F, H[O]] with ShallowProxy[V, O]
	{
		override type From = S

		protected override val egg = header.mapping.withOrigin[O]
		override val mapping = egg

		override val headerColumns: Seq[SelectedColumn[_]] = header.mapping.selectable.toSeq.map(include(_))

		private def include[X](column :ColumnMapping[X, I]) :SelectedColumn[X] = new SelectedColumn[X] {
			override val name :String = column.name
			override val formula  = header \ column
		}


		override def optionally(pieces :Pieces) = pieces.assemble(this)


		override val extracts = super.extracts
		override val columnExtracts = super.columnExtracts
	}



	private class SelectComponent[F <: OuterFrom, T[A] <: TypedMapping[E, A], E,
		                          H[A] <: TypedMapping[V, A], V, I >: F <: FromClause, O]
	                             (from :F, header :ComponentFormula[F, T, E, H, V, I])
		extends SelectComponentFormula[FromClause, F, T, E, H, V, I, O](from, header) with TypedSelectMapping[F, H, V, O]



	private class SubselectComponent[-F <: FromClause, S <: SubselectOf[F], T[A] <: TypedMapping[E, A], E,
	                                 H[A] <: TypedMapping[V, A], V, I >: S <: FromClause, O]
	                                (subselect :S, component :ComponentFormula[S, T, E, H, V, I])
		extends SelectComponentFormula[F, S, T, E, H, V, I, O](subselect, component)
		   with TypedSubselectMapping[F, S, H, V, O]
	{

		override def stretch[U <: F, G <: FromClause](clause :G)(implicit ev :U ExtendedBy G) :SubselectFormula[G, _ <: SubselectOf[G], V, O] =
//				:SubselectComponent[G, D, T, E, H, V, _ >: D <: FromClause, O] forSome { type D <: SubselectOf[G] } =
		{
			type Ext = SubselectOf[G] //pretend this is the actual type S after rebasing to the extension clause G
			val upcast = from :FromClause //scalac bug workaround
			val stretched = upcast.asSubselectOf(clause)(ev.asInstanceOf[upcast.Outer ExtendedBy G]).asInstanceOf[Ext]
			val subselectTables = stretched.size - clause.size
			val table = header.from
			val replacement =
				if (table.shift < subselectTables) table.asInstanceOf[SQLRelation[Ext, T, E, Ext]]
				else stretched.tableStack(table.shift + ev.length).asInstanceOf[SQLRelation[Ext, T, E, Ext]]
			val component = replacement \ header.mapping.withOrigin[Ext]
			new SubselectComponent[G, Ext, T, E, H, V, Ext, O](stretched, component)
		}

	}





	/** A select formula based on the given row source and selecting an arbitrary expression `header` in its ''select''
	  * clause. This header will be translated by recursively flat mapping the header expression to obtain a flat sequence
	  * of columns. In particular, any sequences/tuples are inlined, and any `ComponentFormula`s referring to components
	  * of tables or whole table rows themselves are replaced with their columns. Column list declared by this mapping
	  * is thus created by recursively applying the following rules to the header formula:
	  *
	  *     1. If the formula is a `ColumnFormula`, it is taken as a basis for a `SelectedColumn`;
	  *     1. If the formula is a component mapping, create a column for every export column of the declared mapping;
	  *     1. If the formula is a tuple formula such as a tuple, recursively flatMap over it by applying these rules;
	  *     1. If the formula is a conversion, proceed with the base expression;
	  *     1. Other types of expressions encountered anyone inside the `header` result in throwing an exception.
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

		override type From = S

		/** A column in the header of owning select.
		  * @param formula sql expression providing the value for the column.
		  * @param name column name (sqlName) and suggested alias for the column in the select clause.
		  */
		class ColumnExpression[T](override val formula :ColumnFormula[S, T], override val name :String)
		                         (implicit override val form :ColumnForm[T] = //implicit only so that the arg list can be omitted
		                             formula.readForm <> ColumnWriteForm.dummy[T](formula.readForm.sqlType))
			extends ColumnMapping[T, O] with SelectedColumn[T]



		private type Assembler[T] = Pieces => Option[T]

		private class AssemblerAssembler extends FormulaMatcher[S, Assembler]
			with CaseFormula[S, Assembler] with CaseColumnFormula[S, Assembler] with MatchChain[S, Assembler]
		{
			var columns :List[ColumnExpression[_]] = Nil
			private[this] var names :Set[String] = Set("") //used column names, disallowing ""


			override def column[X](e :ColumnFormula[S, X]) :Pieces => Option[X] = {
				implicit val form :ColumnForm[X] = e.readForm match {
					case form :ColumnForm[X @unchecked] => form
					case form => form <> ColumnWriteForm.dummy(form.sqlType)
				}
				val column = new ColumnExpression(e, nameFor(e))
				columns = column::columns
				pieces => pieces.get(column)
			}


			override def component[T[B] <: TypedMapping[E, B], E, M[B] <: TypedMapping[X, B], X, A >: S <: FromClause]
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



			override def component[T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[X, A], X, G >: S <: FromClause]
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
		override def stretch[U <: F, G <: FromClause](clause :G)(implicit ev :U ExtendedBy G)
				:SubselectFormula[G, _  <: SubselectOf[G], V, O] =
		{
			type Ext = FromClause { type Outer = G }
			val upcast = from :FromClause //scalac bug workaround
			val stretched = upcast.asSubselectOf(clause)(ev.asInstanceOf[upcast.Outer ExtendedBy G])
				.asInstanceOf[Ext]
			val substitute = With.shiftAside[S, Ext](stretched.size - clause.size, ev.length)
			new ArbitrarySubselect[G, Ext, V, O](stretched, substitute(header))
		}

	}



	private abstract class ArbitrarySelectColumn[-F <: FromClause, S <: SubselectOf[F], V, O]
	                                            (override val from :S, override val header :ColumnFormula[S, V])
		extends SelectColumn[F, V, O]
	{
		override type From = S

		class HeaderColumn extends ColumnMapping[V, O] with SelectedColumn[V] {
			override val name :String = header match {
				case FromParam(param, extract) => param.name

				case ComponentFormula(_, MappingExtract(_, _, component)) =>
					component match {
						case column :ColumnMapping[_, _] => column.name
						case label @: _ => label
						case _ => component.sqlName getOrElse "result"
					}

				case BoundParameter(_, Some(name)) => name //unlikely to appear in this position

				case _ => "result"
			}

			override val form :ColumnForm[V] = header.readForm match {
				case form :ColumnForm[V @unchecked] => form
				case form => form <> ColumnWriteForm.dummy(form.sqlType)
			}

			override def formula = header
		}

		private[this] val column = new HeaderColumn

		override val headerColumns = column::Nil

		override val extracts = NaturalMap.single[Component, Extract, V](column, MappingExtract.ident(column))

		override def columnExtracts =
			extracts.asInstanceOf[NaturalMap[Column, ColumnExtract]]

		override val columns = Unique.single[Column[V]](column)
		override def components = columns
		override def subcomponents = columns



		override def assemble(pieces :Pieces) :Option[V] = pieces.get(column)

		override def optionally(pieces :Pieces) :Option[V] = pieces.get(column)
	}




	private class ArbitraryFreeSelectColumn[S <: OuterFrom, V, O](from :S, header :ColumnFormula[S, V])
		extends ArbitrarySelectColumn[FromClause, S, V, O](from, header) with FreeSelectColumn[V, O]



	private class ArbitrarySubselectColumn[-F <: FromClause, S <: SubselectOf[F], V, O]
	                                      (clause :S, override val header :ColumnFormula[S, V])
		extends ArbitrarySelectColumn[F, S, V, O](clause, header) with SubselectColumn[F, S, V, O]
	{
		override def stretch[U <: F, G <: FromClause](clause :G)(implicit ev :U ExtendedBy G)
				:SubselectColumn[G, _ <: SubselectOf[G], V, O] =
		{
			type Ext = FromClause { type Outer = G }
			val upcast :FromClause = from
			val stretched = upcast.asSubselectOf(clause)(ev.asInstanceOf[upcast.Outer ExtendedBy G])
				.asInstanceOf[Ext]
			val substitute = With.shiftAside[S, Ext](stretched.size - clause.size, ev.length)
			new ArbitrarySubselectColumn[G, Ext, V, O](stretched, substitute(header))
		}
	}






	trait FreeSelectColumnMatcher[+F <: FromClause, +Y[X]] {
		def freeSelect[V, O](e :FreeSelectColumn[V, O]) :Y[Rows[V]]
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

		override def freeSelect[V, O](e :FreeSelectColumn[V, O]) :Y[Rows[V]] =
			select(e :SelectColumn[F, V, O])

		override def subselect[S <: SubselectOf[F], V, O](e :SubselectColumn[F, S, V, O]) :Y[Rows[V]] =
			select(e :SelectColumn[F, V, O])
	}



	trait FreeSelectMatcher[+F <: FromClause, +Y[X]] extends FreeSelectColumnMatcher[F, Y] {
		def freeSelect[V, O](e :FreeSelectFormula[V, O]) :Y[Rows[V]]
	}

	type MatchFreeSelect[+F <: FromClause, +Y[X]] = CaseFreeSelect[F, Y]

	trait CaseFreeSelect[+F <: FromClause, +Y[X]] extends FreeSelectMatcher[F, Y] with CaseFreeSelectColumn[F, Y] {
		override def freeSelect[V, O](e :FreeSelectColumn[V, O]) :Y[Rows[V]] = freeSelect(e :FreeSelectFormula[V, O])
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

		def freeSelect[V, O](e: FreeSelectFormula[V, O]): Y[Rows[V]] = select(e :SelectFormula[F, V, O])
	}


}

