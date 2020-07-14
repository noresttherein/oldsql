package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.{Chain, NaturalMap, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainApplication}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.generic.=#>
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.{ColumnExtract, ColumnForm, ColumnMapping, ColumnMappingExtract, ColumnReadForm, ColumnWriteForm, ComponentValues, Mapping, MappingExtract, SchemaMapping, SQLReadForm, TypedMapping}
import net.noresttherein.oldsql.schema.support.LazyMapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.@:
import net.noresttherein.oldsql.schema.Buff.NoSelectByDefault
import net.noresttherein.oldsql.schema.support.MappingProxy.ShallowProxy
import net.noresttherein.oldsql.schema.MappingSchema.SchemaFlattening
import net.noresttherein.oldsql.schema.SchemaMapping.FlatSchemaMapping
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.ColumnSQL.{CaseColumn, ColumnMatcher}
import net.noresttherein.oldsql.sql.ConversionSQL.PromotionConversion
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, OuterFrom, SubselectOf}
import net.noresttherein.oldsql.sql.JoinParam.FromParam
import net.noresttherein.oldsql.sql.MappingSQL.{ColumnComponentSQL, ComponentSQL, SQLRelation}
import net.noresttherein.oldsql.sql.SelectSQL.SelectAs
import net.noresttherein.oldsql.sql.ConditionSQL.ExistsSQL
import net.noresttherein.oldsql.sql.SQLExpression.{CaseExpression, ExpressionMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.SQLParameter
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple.MatchChain
import net.noresttherein.oldsql.sql.TupleSQL.{ChainTuple, SeqTuple}


//here be implicits
import slang._


/** Representation of an SQL select as an SQL expression used in the context of source `F`. If the source is
  * the abstract `FromClause`, this will be a `FreeSelectSQL` instance - a select independent of any external
  * tables or parameters, in which all formulas (''select'' clause, ''where'' clause, etc) can be evaluated
  * based on the values of the tables in its ''from'' clause. If `F` is not `FromClause`, but contains tables, this is
  * a subselect nested inside a select for source `F` - in its header, ''from'' or ''where'' clause. The source for
  * this expression, given by the member type `From`, is always an extension of `F`. Subclasses should extend the trait
  * for one of the above cases: [[net.noresttherein.oldsql.sql.SelectSQL.FreeSelectSQL FreeSelectSQL]] or
  * [[net.noresttherein.oldsql.sql.SelectSQL.SubselectSQL SubselectSQL]], instead of deriving directly
  * from this trait.
  *
  * Apart from being an SQL expression, it is also a `TypedMapping[V, O]`, so it can be used as other mappings inside
  * a ''from'' clause.
  * @tparam F the source of data for the ''enclosing'' select - tables from the ''from'' clause and any unbound parameters.
  * @tparam V the mapped header type representing a single row.
  */
sealed trait SelectSQL[-F <: FromClause, V, O] extends SQLExpression[F, Rows[V]] with TypedMapping[V, O] {

	/** The from clause of this select. */
	type From <: SubselectOf[F]

	trait SelectedColumn[X] {
		def name :String
		def expression :ColumnSQL[From, X]
	}

	val header :SQLExpression[From, V]

	def headerColumns :Seq[SelectedColumn[_]]

	val from :From

	def tables :Seq[SQLRelation.AnyIn[from.Generalized]] = from.subselectTableStack

	def filter :SQLBoolean[from.Generalized] = from.filter

	def isSubselect :Boolean = from.isSubselect



	def as[X <: FlatSchemaMapping[_, _, _, _], M <: FlatSchemaMapping[S, R, C, A], S, R <: Chain, C <: Chain, A]
	      (mapping :X)(implicit typer :Conforms[X, M, FlatSchemaMapping[S, R, C, A]], tuple :V =:= R) :SelectAs[F, M] = ???

	def as[FC <: Chain, FR <: Chain, X <: SchemaMapping[_, _, _, _], M <: SchemaMapping[S, R, C, A], S, R <: Chain, C <: Chain, A]
	      (mapping :X)(implicit typer :Conforms[X, M, SchemaMapping[S, R, C, A]],
	                   flat :SchemaFlattening[R, C, FR, FC], tuple :V =:= R) :SelectAs[F, M] = ???

	def exists :ColumnSQL[F, Boolean] = ExistsSQL(this)

	def notExists :ColumnSQL[F, Boolean] = !ExistsSQL(this)

	def single :SQLExpression[F, V] = to[V]

	def rows :SQLExpression[F, Seq[V]] = to[Seq[V]]



	def map[X](f :V => X) :SelectSQL[F, X, O]

	def map[Fun, C <: Chain, X](f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
			:SelectSQL[F, X, O] =
		map(applyFun(f))

	protected def applyFun[Fun, C <: Chain, X]
	                      (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C) :V => X =
		{ v => application(f, isChain(v)) }



	override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :SelectSQL[S, V, O]



	override def readForm :SQLReadForm[Rows[V]] = header.readForm.map(Rows(_))

	override def freeValue :Option[Rows[V]] = header.freeValue.map(Rows(_))

	override def isFree :Boolean = header.isFree



	protected override def reverseCollect[X](fun: PartialFunction[SQLExpression.*, X], acc: List[X]): List[X] =
		reverseCollect(filter)(fun, reverseCollect(header)(fun, super.reverseCollect(fun, acc)))



	override def isomorphic(expression: SQLExpression.*): Boolean = expression match {
		case s :SelectSQL[_, _, _] =>
			(s eq this) || (s canEqual this) && (s.header isomorphic header) && (s.from == from)
		case _ => false
	}

	private[oldsql] override def equivalent(expression: SQLExpression.*): Boolean = expression match {
		case s :SelectSQL[_, _, _] =>
			(s eq this) || (s canEqual this) && (s.header equivalent header) && (s.from == from)
		case _ => false
	}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SelectSQL[_, _, _]]

	override def equals(that :Any) :Boolean = that match {
		case s :SelectSQL[_, _, _] => (s eq this) || (s canEqual this) && s.header == header && s.from == from
		case _ => false
	}

	override def hashCode :Int = header.hashCode * 31 + from.hashCode

	override def toString = s"SELECT $header FROM $from"

}






object SelectSQL {
//todo: union, minus, product, symdiff
	def apply[F <: OuterFrom, T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, I >: F <: FromClause, O]
	         (from :F, header :ComponentSQL[F, T, E, M, V, I]) :SelectMapping[F, M, V, O] =
		new SelectComponent[F, T, E, M, V, I, O](from, header)

	def apply[F <: OuterFrom, T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, I >: F <: FromClause, O]
	         (from :F, column :ColumnComponentSQL[F, T, E, M, V, I]) :SelectColumnMapping[F, M, V, O] =
		new ArbitraryFreeSelectColumn[F, V, O](from, column) with SelectColumnMapping[F, M, V, O] {
			override val mapping :M[O] = column.mapping.asInstanceOf[M[O]]
			override val header = column
		}

	def apply[F <: OuterFrom, V, O](from :F, header :TupleSQL[F, V]) :FreeSelectSQL[V, O] =
		new ArbitraryFreeSelect[F, V, O](from, header)

	def apply[F <: OuterFrom, X, Y, O](from :F, header :ConversionSQL[F, X, Y]) :FreeSelectSQL[Y, O] =
		new ArbitraryFreeSelect[F, Y, O](from, header)

	def apply[F <: OuterFrom, V, O](from :F, header :ColumnSQL[F, V]) :FreeSelectColumn[V, O] =
		new ArbitraryFreeSelectColumn(from, header)



	def subselect[F <: FromClause, S <: SubselectOf[F],
		          T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, I >: S <: FromClause, O]
	             (from :S, header :ComponentSQL[S, T, E, M, V, I]) :SubselectMapping[F, S, M, V, O] =
		new SubselectComponent[F, S, T, E, M, V, I, O](from, header)

	def subselect[F <: FromClause, S <: SubselectOf[F],
	              T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, I >: S <: FromClause, O]
	             (from :S, column :ColumnComponentSQL[S, T, E, M, V, I]) :SubselectColumnMapping[F, S, M, V, O] =
		new ArbitrarySubselectColumn[F, S, V, O](from, column) with SubselectColumnMapping[F, S, M, V, O] {
			override val mapping :M[O] = column.mapping.asInstanceOf[M[O]]
			override val header = column
		}

	def subselect[F <: FromClause, S <: SubselectOf[F], V, O](from :S, header :TupleSQL[S, V])
			:SubselectSQL[F, V, O] =
		new ArbitrarySubselect[F, S, V, O](from, header)

	def subselect[F <: FromClause, S <: SubselectOf[F], X, Y, O](from :S, header :ConversionSQL[S, X, Y])
			:SubselectSQL[F, Y, O] =
		new ArbitrarySubselect[F, S, Y, O](from, header)

	def subselect[F <: FromClause, S <: SubselectOf[F], V, O](from :S, header :ColumnSQL[S, V])
			:SubselectColumn[F, V, O] =
		new ArbitrarySubselectColumn(from, header)






	type * = SelectSQL[_ <: FromClause, _, _]



	trait SelectColumn[-F <: FromClause, V, O] extends SelectSQL[F, V, O] with ColumnSQL[F, Rows[V]] {
		override val header :ColumnSQL[From, V]

		override def readForm :ColumnReadForm[Rows[V]] = header.readForm.map(Rows(_))

		override def single :ColumnSQL[F, V] = to[V]

		override def map[X](f :V => X) :SelectColumn[F, X, O]

		override def map[Fun, C <: Chain, X]
		                     (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
				:SelectColumn[F, X, O] =
			map(applyFun(f))

		override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :SelectColumn[S, V, O]
	}






	/** Base trait for SQL select expressions whose header depends solely on the explicit FROM clause of the select,
	  * i.e. it is not dependent on any outside rows. Such an expression is a valid select statement in opposition to
	  * subselect expressions.
	  */
	trait FreeSelectSQL[V, O] extends SelectSQL[FromClause, V, O] {
		override type From <: OuterFrom

		override def map[X](f :V => X) :FreeSelectSQL[X, O] =
			new ArbitraryFreeSelect[From, X, O](from, header.map(f))

		override def map[Fun, C <: Chain, X]
		                     (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
				:FreeSelectSQL[X, O] =
			map(applyFun(f))

		override def stretch[U <: FromClause, S <: FromClause]
		                    (base :S)(implicit ev :U ExtendedBy S) :FreeSelectSQL[V, O] =
			this

		override def applyTo[Y[_]](matcher: ExpressionMatcher[FromClause, Y]): Y[Rows[V]] = matcher.freeSelect(this)
	}



	trait FreeSelectColumn[V, O] extends FreeSelectSQL[V, O] with SelectColumn[FromClause, V, O] {

		override def map[X](f :V => X) :FreeSelectColumn[X, O] =
			new ArbitraryFreeSelectColumn[From, X, O](from, header.map(f))

		override def map[Fun, C <: Chain, X]
		                     (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
				:FreeSelectColumn[X, O] =
			map(applyFun(f))

		override def stretch[U <: FromClause, S <: FromClause]
		                    (base :S)(implicit ev :U ExtendedBy S) :FreeSelectColumn[V, O] =
			this

		override def applyTo[Y[_]](matcher :ColumnMatcher[FromClause, Y]) :Y[Rows[V]] = matcher.freeSelect(this)
	}



	/** A base trait for all SQL select expressions nested under another SQL select.
	  * @tparam F the ''from'' clause of the outer select, forming a prefix of `S` until the last occurrence
	  *           of a `Subselect` join kind.
	  * @tparam O marker origin type serving as a unique alias for different members of a FROM clause.
	  * @tparam V the type of the scala value selected by this subselect.
	  */
	trait SubselectSQL[-F <: FromClause, V, O] extends SelectSQL[F, V, O] {

		override def map[X](f :V => X) :SubselectSQL[F, X, O] =
			new ArbitrarySubselect[F, From, X, O](from, header.map(f))

		override def map[Fun, C <: Chain, X]
		                     (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
				:SubselectSQL[F, X, O] =
			map(applyFun(f))

		override def stretch[U <: F, G <: FromClause](base :G)(implicit extension :U ExtendedBy G)
			:SubselectSQL[G, V, O]

		override def applyTo[Y[_]](matcher: ExpressionMatcher[F, Y]): Y[Rows[V]] = matcher.subselect(this)
	}

	trait SubselectColumn[-F <: FromClause, V, O] extends SubselectSQL[F, V, O] with SelectColumn[F, V, O] {

		override def map[X](f :V => X) :SubselectColumn[F, X, O] =
			new ArbitrarySubselectColumn[F, From, X, O](from, header.map(f))

		override def map[Fun, C <: Chain, X]
		                     (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
				:SubselectColumn[F, X, O] =
			map(applyFun(f))

		override def stretch[U <: F, G <: FromClause](base :G)(implicit extension :U ExtendedBy G)
				:SubselectColumn[G, V, O]

		override def applyTo[Y[_]](matcher :ColumnMatcher[F, Y]) :Y[Rows[V]] = matcher.subselect(this)
	}






	/** A `SelectSQL` interface exposing the mapping type `H` used as the header.
	  * Extending classes work as adapters for that mapping.
	  *///todo: the infix notation is confusing, as F is the Outer clause, not the explicit Inner clause
	trait SelectAs[-F <: FromClause, H <: Mapping] extends SelectSQL[F, H#Subject, H#Origin] {
		val mapping :H

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[SelectAs[_, _]]

		override def equals(that :Any) :Boolean = super[SelectSQL].equals(that)

		override def hashCode :Int = super[SelectSQL].hashCode

		override def toString :String = super[SelectSQL].toString
	}



	trait FreeSelectAs[H <: Mapping] extends SelectAs[FromClause, H] with FreeSelectSQL[H#Subject, H#Origin]

	trait SubselectAs[-F <: FromClause, H <: Mapping] extends SelectAs[F, H] with SubselectSQL[F, H#Subject, H#Origin]

	trait SelectMapping[F <: OuterFrom, H[A] <: TypedMapping[V, A], V, O] extends FreeSelectAs[H[O]] {
		override type From = F
	}

	trait SubselectMapping[-F <: FromClause, S <: SubselectOf[F], H[A] <: TypedMapping[V, A], V, O]
		extends SubselectSQL[F, V, O] with SubselectAs[F, H[O]]
	{
		override type From = S
	}

	trait SelectColumnMapping[F <: OuterFrom, H[A] <: ColumnMapping[V, A], V, O]
		extends SelectMapping[F, H, V, O] with FreeSelectColumn[V, O]

	trait SubselectColumnMapping[F <: FromClause, S <: SubselectOf[F], H[A] <: ColumnMapping[V, A], V, O]
		extends SubselectMapping[F, S, H, V, O] with SubselectColumn[F, V, O]






	private abstract class SelectComponentSQL[-F <: FromClause, S <: SubselectOf[F], T[A] <: TypedMapping[E, A], E,
	                                              H[A] <: TypedMapping[V, A], V, I >: S <: FromClause, O]
	                       (override val from :S, override val  header :ComponentSQL[S, T, E, H, V, I])
		extends SelectSQL[F, V, O] with SelectAs[F, H[O]] with ShallowProxy[V, O]
	{
		override type From = S

		protected override val backer = header.mapping.withOrigin[O]
		override val mapping = backer

		override val headerColumns: Seq[SelectedColumn[_]] = header.mapping.selectable.toSeq.map(include(_))

		private def include[X](column :ColumnMapping[X, I]) :SelectedColumn[X] = new SelectedColumn[X] {
			override val name :String = column.name
			override val expression  = header \ column
		}


		override def optionally(pieces :Pieces) = pieces.assemble(this)


		override val extracts = super.extracts
		override val columnExtracts = super.columnExtracts
	}



	private class SelectComponent[F <: OuterFrom, T[A] <: TypedMapping[E, A], E,
		                          H[A] <: TypedMapping[V, A], V, I >: F <: FromClause, O]
	                             (from :F, header :ComponentSQL[F, T, E, H, V, I])
		extends SelectComponentSQL[FromClause, F, T, E, H, V, I, O](from, header) with SelectMapping[F, H, V, O]



	private class SubselectComponent[-F <: FromClause, S <: SubselectOf[F], T[A] <: TypedMapping[E, A], E,
	                                 H[A] <: TypedMapping[V, A], V, I >: S <: FromClause, O]
	                                (subselect :S, component :ComponentSQL[S, T, E, H, V, I])
		extends SelectComponentSQL[F, S, T, E, H, V, I, O](subselect, component)
		   with SubselectMapping[F, S, H, V, O]
	{

		override def stretch[U <: F, G <: FromClause](base :G)(implicit ev :U ExtendedBy G) :SubselectSQL[G, V, O] = {
			type Ext = SubselectOf[G] //pretend this is the actual type S after rebasing to the extension clause G
			val upcast = from :FromClause //scalac bug workaround
			val stretched = upcast.asSubselectOf(base)(ev.asInstanceOf[upcast.Implicit ExtendedBy G]).asInstanceOf[Ext]
			val subselectTables = stretched.size - base.size
			val table = header.from
			val replacement =
				if (table.shift < subselectTables) table.asInstanceOf[SQLRelation[Ext, T, E, Ext]]
				else stretched.tableStack(table.shift + ev.length).asInstanceOf[SQLRelation[Ext, T, E, Ext]]
			val component = replacement \ header.mapping.withOrigin[Ext]
			new SubselectComponent[G, Ext, T, E, H, V, Ext, O](stretched, component)
		}

	}





	/** A select expression based on the given row source and selecting an arbitrary expression `header` in its ''select''
	  * clause. This header will be translated by recursively flat mapping the header expression to obtain a flat sequence
	  * of columns. In particular, any sequences/tuples are inlined, and any `ComponentSQL`s referring to components
	  * of tables or whole table rows themselves are replaced with their columns. Column list declared by this mapping
	  * is thus created by recursively applying the following rules to the header expression:
	  *
	  *     1. If the expression is a `ColumnSQL`, it is taken as a basis for a `SelectedColumn`;
	  *     1. If the expression is a component mapping, create a column for every export column of the declared mapping;
	  *     1. If the expression is a tuple expression such as a tuple, recursively flatMap over it by applying these rules;
	  *     1. If the expression is a conversion, proceed with the base expression;
	  *     1. Other types of expressions encountered anyone inside the `header` result in throwing an exception.
	  *
	  * Note that the above column list should be considered in the context of this instance as a mapping and represents
	  * all columns that potentially might be a part of the select clause. Existence of non-selectable and optional
	  * columns means that resulting select query may not contain all of the above. This distinction is also present
	  * when using this instance to map results of the created select statement; as individual columns
	  * in the select header may be any formulas, the source of values for evaluating the header expression are not values
	  * of the tables of the underlying source, but values for the whole column formulas. For example, a header expression
	  * in the form of `(current_date - birth_date, address, (first_name, family_name)) from users` could translate
	  * into a select expression declaring columns: `('col1', street, zip, city, country, first_name, family_name)`.
	  * Such columns would be available for any formulas using this mapping in their FromClause and are considered
	  * 'available header columns'. However, when using this instance as a mapping for assembling the header value,
	  * we don't have values for individual columns of the users table in the above example, but values for the columns
	  * declared by this mapping. This means that we need a bit of creative term rewriting to map the scala value
	  * as it would be evaluated by the original header expression. In particular, in the above example, the address object
	  * would be reassembled based on the values of individual columns included in the final select.
	  */
	private abstract class ArbitrarySelect[-F <: FromClause, S <: SubselectOf[F], V, O] protected
	                                      (override val from :S, override val header :SQLExpression[S, V])
		extends SelectSQL[F, V, O] with LazyMapping[V, O]
	{ outer =>

		override type From = S

		/** A column in the header of owning select.
		  * @param expression sql expression providing the value for the column.
		  * @param name column name (sqlName) and suggested alias for the column in the select clause.
		  */
		class HeaderColumn[T](override val expression :ColumnSQL[S, T], override val name :String)
		                     (implicit override val form :ColumnForm[T] = //implicit only so that the arg list can be omitted
		                      expression.readForm <> ColumnWriteForm.dummy[T](expression.readForm.sqlType))
			extends ColumnMapping[T, O] with SelectedColumn[T]



		private type Assembler[T] = Pieces => Option[T]

		private class AssemblerAssembler extends ExpressionMatcher[S, Assembler]
			with CaseExpression[S, Assembler] with CaseColumn[S, Assembler] with MatchChain[S, Assembler]
		{
			var columns :List[HeaderColumn[_]] = Nil
			private[this] var names :Set[String] = Set("") //used column names, disallowing ""


			override def column[X](e :ColumnSQL[S, X]) :Pieces => Option[X] = {
				implicit val form :ColumnForm[X] = e.readForm match {
					case form :ColumnForm[X @unchecked] => form
					case form => form <> ColumnWriteForm.dummy(form.sqlType)
				}
				val column = new HeaderColumn(e, nameFor(e))
				columns = column::columns
				pieces => pieces.get(column)
			}


			override def component[T[B] <: TypedMapping[E, B], E, M[B] <: TypedMapping[X, B], X, A >: S <: FromClause]
			                      (e :ComponentSQL[S, T, E, M, X, A]) =
			{
				val table = e.table

				def headerColumn[C](column :table.Column[C]) :Assoc[table.Component, HeaderColumn, C] = {
					val expr = e.from \ column
					val selected = new HeaderColumn[C](e.from \ column, nameFor(expr))
					Assoc[table.Component, HeaderColumn, C](column, selected)
				}
				val mapping = e.mapping
				val columnMappings = mapping.selectable.view.map(table.export(_)).filter(NoSelectByDefault.disabled)
					.map(headerColumn(_)).toList
				columns = columnMappings.map(_._2) reverse_::: columns
				val aliases = NaturalMap(columnMappings :_*)

				{ pieces :Pieces =>
					val values = ComponentValues[X, A](new (MappingAt[A]#Component =#> Option) {
						override def apply[C](x :RefinedMapping[C, A]) = aliases.get(x) match {
							case Some(column) => pieces.get(column)
							case _ => None
						}
					})
					e.mapping.optionally(values)
				}
			}



			override def conversion[T, U](e :ConversionSQL[S, T, U]) = {
				val base = this(e.expr)
				pieces => base(pieces).map(e.convert)
			}



			override def emptyChain = _ => Some(@~)

			override def chainHead[T <: Chain, H](tail :ChainTuple[S, T], head :SQLExpression[S, H]) = {
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



			override def expression[X](e :SQLExpression[S, X]) =
				throw new IllegalArgumentException(
					s"SQLExpression $e cannot be used in a SelectSQL header expression."
				)



			private def nameFor(f :ColumnSQL[S, _]) :String = {
				val name :String = f match {
					case FromParam(param, extract, _) =>
						if (extract.isIdentity && !names(param.name)) param.name
						else param.name + "_" + columns.size

					case ComponentSQL(_, MappingExtract(_, _, component)) =>
						val name :String = component match {
							case column :ColumnMapping[_, _] => column.name
							case label @: _ => label
							case _ => component.sqlName getOrElse ""
						}
						if (!names(name)) name else name + "_" + columns.size

					case SQLParameter(_, Some(name)) => //unlikely to appear in this position
						if (!names(name)) name else name + "_" + columns.size

					case _ => "_" + columns.size
				}
				names = names + name
				name
			}
		}




		private type Extractors[X] = Seq[Assoc[Column, ({ type E[T] = ColumnMappingExtract[X, T, O]})#E, _]]

		private class ExtractsCollector extends ExpressionMatcher[S, Extractors]
			with CaseExpression[S, Extractors] with CaseColumn[S, Extractors] with MatchChain[S, Extractors]
		{
			private[this] var columnStack = outer.headerColumns.toList

			override def column[X](e :ColumnSQL[S, X]) :Extractors[X] = {
				val column = columnStack.head.asInstanceOf[HeaderColumn[X]]
				columnStack = columnStack.tail
				type ColumnEx[T] = ColumnMappingExtract[X, T, O]
				Assoc[Column, ColumnEx, X](column, ColumnExtract.ident(column))::Nil
			}



			override def component[T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[X, A], X, G >: S <: FromClause]
			                      (e :ComponentSQL[S, T, E, M, X, G]) :Extractors[X] =
			{
				val table = e.table
				val component = e.mapping
				val componentColumns = component.selectable.view.map(table.export(_)).filter(NoSelectByDefault.disabled)
				val count = componentColumns.size
				val (selectColumns, tail) = columnStack.splitAt(count)
				columnStack = tail
				type ColumnEx[C] = ColumnMappingExtract[X, C, O]

				def extract[C](selectColumn :HeaderColumn[C], componentColumn :component.Column[_]) = {
					val compatible = componentColumn.asInstanceOf[component.Column[C]]
					val componentExtract = component(compatible)
					val selectExtract = ColumnExtract(selectColumn)(componentExtract)
					Assoc[Column, ColumnEx, C](selectColumn, selectExtract)
				}

				selectColumns.zip(componentColumns).map { case (selCol, compCol) => extract(selCol, compCol) }
			}



			override def conversion[Z, X](e :ConversionSQL[S, Z, X]) = {
				val extracts = apply(e.expr)
				extracts.map(schema.composeColumnExtractAssoc(outer, Extractor.none :X =?> Z)(_))
			}

			override def promotion[T, U](e :PromotionConversion[S, T, U]) = {
				val extracts = apply(e.expr)
				extracts.map(schema.composeColumnExtractAssoc(outer, Extractor(e.lift.inverse))(_))
			}



			override def seq[X](e :SeqTuple[S, X]) = {
				e.inOrder.view.zipWithIndex.flatMap { entry :(SQLExpression[S, X], Int) =>
					apply(entry._1).map(schema.composeColumnExtractAssoc(outer, (seq :Seq[X]) => seq(entry._2))(_))
				}.toList
			}

			override def chainHead[T <: Chain, H](tail :ChainTuple[S, T], head :SQLExpression[S, H]) = {
				val tailExs = apply(tail).map(schema.composeColumnExtractAssoc(outer, (_ :(T ~ H)).init)(_))
				val headExs = apply(head).map(schema.composeColumnExtractAssoc(outer, (_:(T ~ H)).last)(_))
				headExs ++: tailExs
			}

			override def emptyChain = Nil



			override def expression[X](e :SQLExpression[S, X]) = unhandled(e)
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



	private class ArbitraryFreeSelect[S <: OuterFrom, V, O](from :S, header :SQLExpression[S, V])
		extends ArbitrarySelect[FromClause, S, V, O](from, header) with FreeSelectSQL[V, O]



	private class ArbitrarySubselect[-F <: FromClause, S <: SubselectOf[F], V, O](subclause :S, header :SQLExpression[S, V])
		extends ArbitrarySelect[F, S, V, O](subclause, header) with SubselectSQL[F, V, O]
	{
		override def stretch[U <: F, G <: FromClause](base :G)(implicit ev :U ExtendedBy G) :SubselectSQL[G, V, O] = {
			type Ext = FromClause { type Implicit = G }
			val upcast = from :FromClause //scalac bug workaround
			val stretched = upcast.asSubselectOf(base)(ev.asInstanceOf[upcast.Implicit ExtendedBy G]).asInstanceOf[Ext]
			val substitute = With.shiftBack[S, Ext](from, stretched, ev.length, upcast.subselectSize)
			new ArbitrarySubselect[G, Ext, V, O](stretched, substitute(header))
		}

	}






	private abstract class ArbitrarySelectColumn[-F <: FromClause, S <: SubselectOf[F], V, O]
	                                            (override val from :S, override val header :ColumnSQL[S, V])
		extends SelectColumn[F, V, O]
	{
		override type From = S

		class HeaderColumn extends ColumnMapping[V, O] with SelectedColumn[V] {
			override val name :String = header match {
				case FromParam(param, _, _) => param.name

				case ComponentSQL(_, MappingExtract(_, _, component)) =>
					component match {
						case column :ColumnMapping[_, _] => column.name
						case label @: _ => label
						case _ => component.sqlName getOrElse "result"
					}

				case SQLParameter(_, Some(name)) => name //unlikely to appear in this position

				case _ => "result"
			}

			override val form :ColumnForm[V] = header.readForm match {
				case form :ColumnForm[V @unchecked] => form
				case form => form <> ColumnWriteForm.dummy(form.sqlType)
			}

			override def expression = header
		}

		private[this] val column = new HeaderColumn

		override val headerColumns = column::Nil

		override val extracts = NaturalMap.single[Component, Extract, V](column, ColumnExtract.ident(column))

		override def columnExtracts =
			extracts.asInstanceOf[NaturalMap[Column, ColumnExtract]]

		override val columns = Unique.single[Column[V]](column)
		override def selectable = columns
		override def queryable = columns
		override def updatable = columns
		override def insertable = columns

		override def components = columns
		override def subcomponents = columns


		override def selectForm = column.form
		override def queryForm = column.form
		override def updateForm = column.form
		override def insertForm = column.form

		override def nullValue = column.nullValue

		override def assemble(pieces :Pieces) :Option[V] = pieces.get(column)

		override def optionally(pieces :Pieces) :Option[V] = pieces.get(column)
	}



	private class ArbitraryFreeSelectColumn[S <: OuterFrom, V, O](from :S, header :ColumnSQL[S, V])
		extends ArbitrarySelectColumn[FromClause, S, V, O](from, header) with FreeSelectColumn[V, O]



	private class ArbitrarySubselectColumn[-F <: FromClause, S <: SubselectOf[F], V, O]
	                                      (clause :S, override val header :ColumnSQL[S, V])
		extends ArbitrarySelectColumn[F, S, V, O](clause, header) with SubselectColumn[F, V, O]
	{
		override def stretch[U <: F, G <: FromClause](base :G)(implicit ev :U ExtendedBy G) :SubselectColumn[G, V, O] = {
			type Ext = FromClause { type Implicit = G }
			val upcast :FromClause = from
			val stretched = upcast.asSubselectOf(base)(ev.asInstanceOf[upcast.Implicit ExtendedBy G]).asInstanceOf[Ext]
			val substitute = With.shiftBack[S, Ext](from, stretched, ev.length, upcast.subselectSize)
			new ArbitrarySubselectColumn[G, Ext, V, O](stretched, substitute(header))
		}
	}






	trait FreeSelectColumnMatcher[+F <: FromClause, +Y[X]] {
		def freeSelect[V, O](e :FreeSelectColumn[V, O]) :Y[Rows[V]]
	}

	type MatchFreeSelectColumn[+F <: FromClause, +Y[X]] = FreeSelectColumnMatcher[F, Y]

	type CaseFreeSelectColumn[+F <: FromClause, +Y[X]] = FreeSelectColumnMatcher[F, Y]



	trait SubselectColumnMatcher[+F <: FromClause, +Y[X]] {
		def subselect[V, O](e :SubselectColumn[F, V, O]) :Y[Rows[V]]
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

		override def subselect[V, O](e :SubselectColumn[F, V, O]) :Y[Rows[V]] =
			select(e :SelectColumn[F, V, O])
	}



	trait FreeSelectMatcher[+F <: FromClause, +Y[X]] extends FreeSelectColumnMatcher[F, Y] {
		def freeSelect[V, O](e :FreeSelectSQL[V, O]) :Y[Rows[V]]
	}

	type MatchFreeSelect[+F <: FromClause, +Y[X]] = CaseFreeSelect[F, Y]

	trait CaseFreeSelect[+F <: FromClause, +Y[X]] extends FreeSelectMatcher[F, Y] with CaseFreeSelectColumn[F, Y] {
		override def freeSelect[V, O](e :FreeSelectColumn[V, O]) :Y[Rows[V]] = freeSelect(e :FreeSelectSQL[V, O])
	}



	trait SubselectMatcher[+F <: FromClause, +Y[X]] {
		def subselect[V, O](e :SubselectSQL[F, V, O]) :Y[Rows[V]]
	}

	type MatchSubselect[+F <: FromClause, +Y[X]] = CaseSubselect[F, Y]

	trait CaseSubselect[+F <: FromClause, +Y[X]] extends SubselectMatcher[F, Y] with CaseSubselectColumn[F, Y] {
		override def subselect[V, O](e :SubselectColumn[F, V, O]) :Y[Rows[V]] =
			subselect(e :SubselectSQL[F, V, O])
	}



	trait SelectMatcher[+F <: FromClause, +Y[X]] extends SelectColumnMatcher[F, Y]
		with FreeSelectMatcher[F, Y] with SubselectMatcher[F, Y]

	trait MatchSelect[+F <: FromClause, +Y[X]] extends SelectMatcher[F, Y]
		with CaseFreeSelect[F, Y] with CaseSubselect[F, Y]

	trait CaseSelect[+F <: FromClause, +Y[X]] extends MatchSelect[F, Y] {
		def select[V, O](e :SelectSQL[F, V, O]) :Y[Rows[V]]

		def subselect[V, O](e: SubselectSQL[F, V, O]): Y[Rows[V]] = select(e)

		def freeSelect[V, O](e: FreeSelectSQL[V, O]): Y[Rows[V]] = select(e :SelectSQL[F, V, O])
	}


}

