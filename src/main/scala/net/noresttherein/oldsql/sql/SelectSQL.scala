package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.{Chain, IndexedChain, NaturalMap, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainApplication}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.generic.=#>
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.{BaseMapping, ColumnExtract, ColumnForm, ColumnMapping, ColumnMappingExtract, ColumnReadForm, ColumnWriteForm, ComponentValues, Mapping, MappingExtract, SchemaMapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.support.LazyMapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.Buff.NoSelectByDefault
import net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy
import net.noresttherein.oldsql.schema.MappingSchema.SchemaFlattening
import net.noresttherein.oldsql.schema.SchemaMapping.FlatSchemaMapping
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.ColumnSQL.{CaseColumn, ColumnMatcher}
import net.noresttherein.oldsql.sql.ConversionSQL.PromotionConversion
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FreeFrom, NonEmptyFrom, PartOf, SubselectOf}
import net.noresttherein.oldsql.sql.UnboundParam.{FromParam, UnboundParamSQL}
import net.noresttherein.oldsql.sql.MappingSQL.{BaseColumnComponentSQL, BaseComponentSQL, ColumnComponentSQL, ComponentSQL, RelationSQL}
import net.noresttherein.oldsql.sql.SelectSQL.SelectAs
import net.noresttherein.oldsql.sql.ConditionSQL.ExistsSQL
import net.noresttherein.oldsql.sql.SQLExpression.{CaseExpression, ExpressionMatcher, GlobalScope, LocalScope, LocalSQL}
import net.noresttherein.oldsql.sql.SQLTerm.SQLParameter
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple.MatchChain
import net.noresttherein.oldsql.sql.TupleSQL.{ChainTuple, IndexedChainTuple, SeqTuple}
import net.noresttherein.oldsql.OperationType.WriteOperationType
import net.noresttherein.oldsql.collection.IndexedChain.{:~, |~}
import net.noresttherein.oldsql.schema.ColumnMapping.StableColumn
import net.noresttherein.oldsql.schema.ComponentValues.{ColumnValues, ComponentValuesBuilder}
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.TupleSQL.IndexedChainTuple.{IndexedSQLExpression, MatchIndexedChain}


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
  * Apart from being an SQL expression, it is also a `BaseMapping[V, O]`, so it can be used as other mappings inside
  * a ''from'' clause.
  * @tparam F the source of data for the ''enclosing'' select - tables from the ''from'' clause and any unbound parameters.
  * @tparam V the mapped header type representing a single row.
  */ //todo: instead of mapping, let it be a Relation - much more useful. Make it a type alias and SelectAs the real root
sealed trait SelectSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, V, O]
	extends SQLExpression[F, S, Rows[V]] with BaseMapping[V, O]
{
	override def readForm :SQLReadForm[Rows[V]] = header.readForm.map(Rows(_))

	/** The from clause of this select. */
	type From <: SubselectOf[F]

	trait SelectedColumn[X] {
		def name :String
		def expression :ColumnSQL[From, LocalScope, X]
	}

	val header :SQLExpression[From, LocalScope, V]

	def headerColumns :Seq[SelectedColumn[_]]

	val from :From

	def tables :Seq[RelationSQL.AnyIn[from.Generalized]] = from.innerTableStack

	def isSubselect :Boolean = from.isSubselect


//	def as[X <: FlatSchemaMapping[_, _, _, _], M <: FlatSchemaMapping[T, R, C, A], T, R <: Chain, C <: Chain, A]
//	      (mapping :X)(implicit typer :Conforms[X, M, FlatSchemaMapping[T, R, C, A]], tuple :V =:= R) :SelectAs[F, S, M, O] = ???
//
//	def as[FC <: Chain, FR <: Chain, X <: SchemaMapping[_, _, _, _], M <: SchemaMapping[T, R, C, A], T, R <: Chain, C <: Chain, A]
//	      (mapping :X)(implicit typer :Conforms[X, M, SchemaMapping[T, R, C, A]],
//	                   flat :SchemaFlattening[R, C, FR, FC], tuple :V =:= R) :SelectAs[F, S, M, O] = ???
//
	def isDistinct :Boolean

	def distinct :SelectSQL[F, S, V, O]

	def exists :ColumnSQL[F, S, Boolean] = ExistsSQL(this)

	def notExists :ColumnSQL[F, S, Boolean] = !ExistsSQL(this)

	def single :SQLExpression[F, S, V] = to[V]

	def rows :SQLExpression[F, S, Seq[V]] = to[Seq[V]]



	def map[X](f :V => X) :SelectSQL[F, S, X, O]

	def map[Fun, C <: Chain, X](f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
			:SelectSQL[F, S, X, O] =
		map(applyFun(f))

	protected def applyFun[Fun, C <: Chain, X]
	                      (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C) :V => X =
		{ v => application(f, isChain(v)) }



	override def freeValue :Option[Rows[V]] = header.freeValue.map(Rows(_))

	override def isFree :Boolean = header.isFree

	override def asGlobal :Option[SelectSQL[F, GlobalScope, V, O]]


	override def basedOn[U <: F, E <: FromClause](base :E)(implicit ext :U PartOf E) :SelectSQL[E, S, V, O]

	override def extend[U <: F, E <: FromClause]
	                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< S) :SelectSQL[E, S, V, O]


	protected override def reverseCollect[X](fun: PartialFunction[SQLExpression.*, X], acc: List[X]): List[X] = {
		val headerItems = reverseCollect(header)(fun, super.reverseCollect(fun, acc))
		reverseCollect(from.filter)(fun, from match {
			case GroupByClause(ungrouped) => reverseCollect(ungrouped.filter)(fun, headerItems)
			case _ => headerItems
		})
	}


	override def isomorphic(expression: SQLExpression.*): Boolean = expression match {
		case s :SelectSQL[_, _, _, _] =>
			(s eq this) || (s canEqual this) && (s.header isomorphic header) && (s.from == from)
		case _ => false
	}

	private[oldsql] override def equivalent(expression: SQLExpression.*): Boolean = expression match {
		case s :SelectSQL[_, _, _, _] =>
			(s eq this) || (s canEqual this) && (s.header equivalent header) && (s.from == from)
		case _ => false
	}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SelectSQL[_, _, _, _]]

	override def equals(that :Any) :Boolean = that match {
		case s :SelectSQL[_, _, _, _] => (s eq this) || (s canEqual this) && s.header == header && s.from == from
		case _ => false
	}

	override def hashCode :Int = header.hashCode * 31 + from.hashCode

	override def toString :String =
		if (isDistinct) s"SELECT DISTINCT $header FROM $from"
		else  s"SELECT $header FROM $from"

}






object SelectSQL {
	//todo: parameterized selects
	//todo: union, minus, product, symdiff

	def apply[F <: FreeFrom, M[A] <: BaseMapping[V, A], V, I >: F <: FromClause, O]
	         (from :F, header :BaseComponentSQL[F, M, I])
			:SelectMapping[F, M, V, O] =
		new SelectComponent[F, M, V, I, O](from, header, false)

	def apply[F <: FreeFrom, M[A] <: ColumnMapping[V, A], V, I >: F <: FromClause, O]
	         (from :F, column :BaseColumnComponentSQL[F, M, V, I]) :SelectColumnMapping[F, M, V, O] =
		new SelectColumnMappingImpl[F, M, V, O](from, column, false)

	def apply[F <: FreeFrom, V, O](from :F, header :TupleSQL[F, LocalScope, V]) :FreeSelectSQL[V, O] =
		new ArbitraryFreeSelect[F, V, O](from, SQLScribe.anchorLooseComponents(from)(header), false)

	def apply[F <: FreeFrom, X, Y, O](from :F, header :ConversionSQL[F, LocalScope, X, Y]) :FreeSelectSQL[Y, O] =
		new ArbitraryFreeSelect[F, Y, O](from, SQLScribe.anchorLooseComponents(from)(header), false)

	def apply[F <: FreeFrom, V, O](from :F, header :ColumnSQL[F, LocalScope, V]) :FreeSelectColumn[V, O] =
		new ArbitraryFreeSelectColumn(from, SQLScribe.anchorLooseComponents(from)(header), false)


	//fixme: Outer clauses are valid SubselectOf for any other clause and they will likely break dedicated subselects
	def subselect[F <: FromClause, S <: SubselectOf[F], M[A] <: BaseMapping[V, A], V, I >: S <: FromClause, O]
	             (from :S, header :BaseComponentSQL[S, M, I]) :SubselectMapping[F, S, M, V, O] =
		 new SubselectComponent[F, S, M, V, I, O](from, header, false)

	def subselect[F <: FromClause, S <: SubselectOf[F], M[A] <: ColumnMapping[V, A], V, I >: S <: FromClause, O]
	             (from :S, column :BaseColumnComponentSQL[S, M, V, I])
			:SubselectColumnMapping[F, S, M, V, O] =
		new SubselectColumnMappingImpl[F, S, M, V, O](from, column, false)

	def subselect[F <: FromClause, S <: SubselectOf[F], V, O]
	             (from :S, header :TupleSQL[S, LocalScope, V]) :SubselectSQL[F, V, O] =
		new ArbitrarySubselect[F, S, V, O](from, SQLScribe.anchorLooseComponents(from)(header), false)

	def subselect[F <: FromClause, S <: SubselectOf[F], X, Y, O]
	             (from :S, header :ConversionSQL[S, LocalScope, X, Y]) :SubselectSQL[F, Y, O] =
		new ArbitrarySubselect[F, S, Y, O](from, SQLScribe.anchorLooseComponents(from)(header), false)

	def subselect[F <: FromClause, S <: SubselectOf[F], V, O]
	             (from :S, header :ColumnSQL[S, LocalScope, V]) :SubselectColumn[F, V, O] =
		new ArbitrarySubselectColumn[F, S, V, O](from, SQLScribe.anchorLooseComponents(from)(header), false)



	type * = SelectSQL[_ <: FromClause, LocalScope, _, _]



	//todo: this should likely be a ColumnMapping, no?
	trait SelectColumn[-F <: FromClause, -S >: LocalScope <: GlobalScope, V, O]
		extends SelectSQL[F, S, V, O] with ColumnSQL[F, S, Rows[V]]
	{
		override def readForm :ColumnReadForm[Rows[V]] = header.readForm.map(Rows(_))

		override val header :ColumnSQL[From, LocalScope, V]

		override def distinct :SelectColumn[F, S, V, O]

		override def single :ColumnSQL[F, S, V] = to[V]

		override def map[X](f :V => X) :SelectColumn[F, S, X, O]

		override def map[Fun, C <: Chain, X](f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
				:SelectColumn[F, S, X, O] =
			map(applyFun(f))


		override def asGlobal :Option[SelectColumn[F, GlobalScope, V, O]]


		override def basedOn[U <: F, E <: FromClause](base :E)(implicit ext :PartOf[U, E]) :SelectColumn[E, S, V, O]

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< S) :SelectColumn[E, S, V, O]
	}



	/** Base trait for SQL select expressions whose header depends solely on the explicit FROM clause of the select,
	  * i.e. it is not dependent on any outside rows. Such an expression is a valid select statement in opposition to
	  * subselect expressions.
	  */
	//todo: conversion to Relation - would be nice if there were sources for subselects; theoretically possible
	trait FreeSelectSQL[V, O] extends SelectSQL[FromClause, GlobalScope, V, O] {
		override type From <: FreeFrom

		override def distinct :FreeSelectSQL[V, O]

		override def map[X](f :V => X) :FreeSelectSQL[X, O] =
			new ArbitraryFreeSelect[From, X, O](from, header.map(f), isDistinct)

		override def map[Fun, C <: Chain, X]
		                (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
				:FreeSelectSQL[X, O] =
			map(applyFun(f))


		override def isGlobal = true
		override def asGlobal :Option[SelectSQL[FromClause, GlobalScope, V, O]] = Some(this)


		override def basedOn[U <: FromClause, E <: FromClause](base :E)(implicit ext :U PartOf E) :FreeSelectSQL[V, O] =
			this

		override def extend[U <: FromClause, S <: FromClause]
		             (base :S)(implicit ev :U ExtendedBy S, global :GlobalScope <:< GlobalScope) :FreeSelectSQL[V, O] =
			this

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[FromClause, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.freeSelect(this)
	}



	trait FreeSelectColumn[V, O] extends FreeSelectSQL[V, O] with SelectColumn[FromClause, GlobalScope, V, O] {

		override def distinct :FreeSelectColumn[V, O]

		override def map[X](f :V => X) :FreeSelectColumn[X, O] =
			new ArbitraryFreeSelectColumn[From, X, O](from, header.map(f), isDistinct)

		override def map[Fun, C <: Chain, X]
		                (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
			:FreeSelectColumn[X, O] =
			map(applyFun(f))


		override def asGlobal :Option[SelectColumn[FromClause, GlobalScope, V, O]] = Some(this)


		override def basedOn[U <: FromClause, E <: FromClause]
		                    (base :E)(implicit ext :U PartOf E) :FreeSelectColumn[V, O] = this

		override def extend[U <: FromClause, E <: FromClause]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:FreeSelectColumn[V, O] =
			this

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ColumnMatcher[FromClause, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.freeSelect(this)
	}



	/** A base trait for all SQL select expressions nested under another SQL select.
	  * @tparam F the ''from'' clause of the outer select, forming a prefix of `S` until the last occurrence
	  *           of a `Subselect` join kind.
	  * @tparam O marker origin type serving as a unique alias for different members of a FROM clause.
	  * @tparam V the type of the scala value selected by this subselect.
	  *///consider: scope type parameter
	trait SubselectSQL[-F <: FromClause, V, O] extends SelectSQL[F, GlobalScope, V, O] {

		override def distinct :SubselectSQL[F, V, O]

		override def map[X](f :V => X) :SubselectSQL[F, X, O] =
			new ArbitrarySubselect[F, From, X, O](from, header.map(f), isDistinct)

		override def map[Fun, C <: Chain, X]
		                (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
				:SubselectSQL[F, X, O] =
			map(applyFun(f))


		override def isGlobal = true
		override def asGlobal :Option[SelectSQL[F, GlobalScope, V, O]] = Some(this)


		override def basedOn[U <: F, E <: FromClause](base :E)(implicit ext :U PartOf E) :SubselectSQL[E, V, O] =
			extend(base)(ext.asExtendedBy, implicitly[GlobalScope <:< GlobalScope])

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit extension :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectSQL[E, V, O]

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.subselect(this)

	}



	trait SubselectColumn[-F <: FromClause, V, O] extends SubselectSQL[F, V, O] with SelectColumn[F, GlobalScope, V, O] {

		override def distinct :SubselectColumn[F, V, O]

		override def map[X](f :V => X) :SubselectColumn[F, X, O] =
			new ArbitrarySubselectColumn[F, From, X, O](from, header.map(f), isDistinct)

		override def map[Fun, C <: Chain, X](f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
				:SubselectColumn[F, X, O] =
			map(applyFun(f))


		override def asGlobal :Option[SubselectColumn[F, V, O]] = Some(this)


		override def basedOn[U <: F, E <: FromClause](base :E)(implicit ext :U PartOf E) :SubselectColumn[E, V, O] =
			extend(base)(ext.asExtendedBy, implicitly[GlobalScope <:< GlobalScope])

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit extension :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectColumn[E, V, O]

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.subselect(this)
	}



	/** A `SelectSQL` interface exposing the mapping type `H` used as the header.
	  * Extending classes work as adapters for that mapping.
	  */
	trait SelectAs[-F <: FromClause, -S >: LocalScope <: GlobalScope, H[A] <: MappingAt[A], O]
		extends SelectSQL[F, S, H[O]#Subject, O]
	{
		val mapping :H[O]

		override def distinct :SelectAs[F, S, H, O]

		override def asGlobal :Option[SelectAs[F, GlobalScope, H, O]]

		override def canEqual(that :Any) :Boolean =
			that.isInstanceOf[SelectAs[_, _, M, _] forSome { type M[A] <: MappingAt[A] }]

		override def equals(that :Any) :Boolean = super[SelectSQL].equals(that)

		override def hashCode :Int = super[SelectSQL].hashCode

		override def toString :String = super[SelectSQL].toString
	}

	//consider: renaming all FreeSelect classes to OuterSelect
	trait FreeSelectAs[H[A] <: MappingAt[A], O]
		extends SelectAs[FromClause, GlobalScope, H, O] with FreeSelectSQL[H[O]#Subject, O]
	{
		override def distinct :FreeSelectAs[H, O]
		override def asGlobal :Option[SelectAs[FromClause, GlobalScope, H, O]] = Some(this)
	}


	trait SubselectAs[-F <: FromClause, H[A] <: MappingAt[A], O]
		extends SelectAs[F, GlobalScope, H, O] with SubselectSQL[F, H[O]#Subject, O]
	{
		override def distinct :SubselectAs[F, H, O]
		override def asGlobal :Option[SubselectAs[F, H, O]] = Some(this)
	}


	trait SelectMapping[F <: FreeFrom, H[A] <: BaseMapping[V, A], V, O] extends FreeSelectAs[H, O] {
		override type From = F
		override def distinct :SelectMapping[F, H, V, O]
	}

	trait SubselectMapping[-F <: FromClause, S <: SubselectOf[F], H[A] <: BaseMapping[V, A], V, O]
		extends SubselectSQL[F, V, O] with SubselectAs[F, H, O]
	{
		override type From = S
		override def distinct :SubselectMapping[F, S, H, V, O]
		override def asGlobal :Option[SubselectMapping[F, S, H, V, O]] = Some(this)
	}


	trait SelectColumnMapping[F <: FreeFrom, H[A] <: ColumnMapping[V, A], V, O]
		extends SelectMapping[F, H, V, O] with FreeSelectColumn[V, O]
	{
		override def distinct :SelectColumnMapping[F, H, V, O]
		override def asGlobal :Option[SelectColumnMapping[F, H, V, O]] = Some(this)
	}


	trait SubselectColumnMapping[-F <: FromClause, S <: SubselectOf[F], H[A] <: ColumnMapping[V, A], V, O]
		extends SubselectMapping[F, S, H, V, O] with SubselectColumn[F, V, O]
	{
		override def distinct :SubselectColumnMapping[F, S, H, V, O]
		override def asGlobal :Option[SubselectColumnMapping[F, S, H, V, O]] = Some(this)
	}






	private abstract class SelectComponentSQL[-F <: FromClause, S <: SubselectOf[F],
	                                          H[A] <: BaseMapping[V, A], V, I >: S <: FromClause, O]
	                                         (override val from :S, override val header :BaseComponentSQL[S, H, I])
		extends SelectSQL[F, GlobalScope, V, O] with SelectAs[F, GlobalScope, H, O] with DirectProxy[V, O]
	{
		override type From = S

		protected override val backer = header.mapping.withOrigin[O]
		override val mapping = backer

		override val headerColumns: Seq[SelectedColumn[_]] = header.mapping.selectable.toSeq.map(include(_))

		private def include[X](column :ColumnMapping[X, I]) :SelectedColumn[X] = new SelectedColumn[X] {
			override val name :String = column.name
			override val expression  = header \ column
		}


		override def apply[X](component :Component[X]) = extracts(component)

		override def apply[X](column :Column[X]) = columnExtracts(column)

		override val extracts = super.extracts
		override val columnExtracts = super.columnExtracts
	}



	private class SelectComponent[F <: FreeFrom, H[A] <: BaseMapping[V, A], V, I >: F <: FromClause, O]
	                             (override val from :F, override val header :BaseComponentSQL[F, H, I],
								  override val isDistinct :Boolean)
		extends SelectComponentSQL[FromClause, F, H, V, I, O](from, header) with SelectMapping[F, H, V, O]
	{
		override def distinct :SelectMapping[F, H, V, O] =
			if (isDistinct) this else new SelectComponent(from, header, true)
	}



	private class SubselectComponent[-F <: FromClause, S <: SubselectOf[F],
	                                 H[A] <: BaseMapping[V, A], V, I >: S <: FromClause, O]
	                                (subselect :S, component :BaseComponentSQL[S, H, I], override val isDistinct :Boolean)
		extends SelectComponentSQL[F, S, H, V, I, O](subselect, component)
		   with SubselectMapping[F, S, H, V, O]
	{
		override def distinct :SubselectMapping[F, S, H, V, O] =
			if (isDistinct) this else new SubselectComponent(from, header, true)

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectSQL[E, V, O] =
			from match { //would be safer to refactor this out as a FromClause method
				case some :NonEmptyFrom =>
					type Ext = SubselectOf[E] //pretend this is the actual type S after rebasing to the extension clause G
					implicit val extension = ev.asInstanceOf[some.Implicit ExtendedBy base.Generalized]
//					implicit val projection = header.projection
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val subselectTables = stretched.fullSize - base.fullSize
					val table = header.origin
					val replacement =
						if (table.shift < subselectTables)
							table.asInstanceOf[RelationSQL[Ext, MappingOf[Any]#TypedProjection, Any, Ext]]
						else
                            stretched.fullTableStack(table.shift + ev.length)
                                     .asInstanceOf[RelationSQL[Ext, MappingOf[Any]#TypedProjection, Any, Ext]]
					val component = replacement \[H[Ext], V] header.mapping.withOrigin[Ext]
					new SubselectComponent[E, Ext, H, V, Ext, O](stretched, component, isDistinct)

				case empty :Dual =>
					val adaptedHeader = header.asInstanceOf[BaseComponentSQL[Dual, H, FromClause]]
					new SubselectComponent[E, Dual, H, V, FromClause, O](empty, adaptedHeader, isDistinct)

				case _ =>
					throw new UnsupportedOperationException(s"Cannot rebase clause $from :${from.unqualifiedClassName} onto $base.")
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
	  */ //todo: just use ExpressionMapping
	private abstract class ArbitrarySelect[-F <: FromClause, S <: SubselectOf[F], V, O] protected
	                                      (override val from :S, override val header :SQLExpression[S, LocalScope, V])
		extends SelectSQL[F, GlobalScope, V, O] with LazyMapping[V, O]
	{ outer =>

		override type From = S

		/** A column in the header of owning select.
		  * @param expression sql expression providing the value for the column.
		  * @param name column name (sqlName) and suggested alias for the column in the select clause.
		  */
		class HeaderColumn[T](override val expression :ColumnSQL[S, LocalScope, T], override val name :String)
		                     (implicit override val form :ColumnForm[T] = //implicit only so that the arg list can be omitted
		                      expression.readForm <>[T] ColumnWriteForm.unsupported(
			                      s"Select clause column <$expression as $name> does not support write.",
			                      expression.readForm.sqlType
		                      ))
			extends StableColumn[T, O] with SelectedColumn[T]



		private type Assembler[-_ >: LocalScope <: GlobalScope, T] = Pieces => Option[T]

		private class AssemblerAssembler extends ExpressionMatcher[S, Assembler]
			with CaseExpression[S, Assembler] with CaseColumn[S, Assembler] with MatchChain[S, Assembler]
			with MatchIndexedChain[S, Assembler]
		{
			var columns :List[HeaderColumn[_]] = Nil
			private[this] var names :Set[String] = Set("") //used column names, disallowing ""


			override def column[C >: LocalScope <: GlobalScope, X](e :ColumnSQL[S, C, X]) :Pieces => Option[X] = {
				implicit val form :ColumnForm[X] = e.readForm match {
					case form :ColumnForm[X @unchecked] => form
					case form => form <> ColumnWriteForm.unsupported(
						s"Select clause column $e does not support write.", form.sqlType
					)
				}
				val column = new HeaderColumn(e, nameFor(e))
				columns = column::columns
				pieces => pieces.get(column)
			}


			override def component[T[B] <: BaseMapping[E, B], E, M[B] <: BaseMapping[X, B], X, A >: S <: FromClause]
			                      (e :ComponentSQL[S, T, E, M, X, A]) =
			{
				val table = e.entity

				def headerColumn[C](column :ColumnMapping[C, A]) :Assoc[table.Component, HeaderColumn, C] = {
					val expr = e.origin \ column
					val selected = new HeaderColumn[C](expr, nameFor(expr))
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


			override def conversion[C >: LocalScope <: GlobalScope, X, Y](e :ConversionSQL[S, C, X, Y]) = {
				val base = this(e.expr)
				pieces => base(pieces).map(e.convert)
			}


			override def emptyChain = _ => Some(@~)

			override def chainHead[C >: LocalScope <: GlobalScope, T <: Chain, H]
			                      (tail :ChainTuple[S, C, T], head :SQLExpression[S, C, H]) =
			{
				val tl = apply(tail)
				val hd = apply(head)
				pieces => for (t <- tl(pieces); h <- hd(pieces)) yield t ~ h
			}

			override def indexedChainHead[C >: LocalScope <: GlobalScope, I <: IndexedChain, K <: Label :ValueOf, L]
			                             (init :IndexedChainTuple[S, C, I], last :IndexedSQLExpression[S, C, L]) =
			{
				val tl = apply(init)
				val hd = apply(last)
				pieces => for (t <- tl(pieces); h <- hd(pieces)) yield t |~ :~[K](h)
			}

			override def seq[C >: LocalScope <: GlobalScope, X](e :SeqTuple[S, C, X]) = {
				val assemblers = e.inOrder.map(apply).reverse
				val acc = Option(List.empty[X])
				pieces => (acc /: assemblers) {
					(acc, assembler) => for (t <- acc; h <- assembler(pieces)) yield h::t
				}
			}


			override def expression[C >: LocalScope <: GlobalScope, X](e :SQLExpression[S, C, X]) =
				throw new IllegalArgumentException(
					s"SQLExpression $e cannot be used in a SelectSQL header expression."
				)


			private def nameFor(f :ColumnSQL[S, LocalScope, _]) :String = {
				val name :String = f match {
					case UnboundParamSQL(param, extract, _) => //first as it would match the following pattern, too
						if (extract.isIdentity && !names(param.name)) param.name
						else param.name + "_" + columns.size

					case ComponentSQL(_, MappingExtract(_, _, component)) =>
						val name :String = component match {
							case column :ColumnMapping[_, _] => column.name //this is the almost sure case
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



		private type Extractors[-_ >: LocalScope <: GlobalScope, X] =
			Seq[Assoc[Column, ({ type E[T] = ColumnMappingExtract[X, T, O]})#E, _]]

		private class ExtractsCollector extends ExpressionMatcher[S, Extractors]
			with CaseExpression[S, Extractors] with CaseColumn[S, Extractors] with MatchChain[S, Extractors]
			with MatchIndexedChain[S, Extractors]
		{
			private[this] var columnStack = outer.headerColumns.toList

			override def column[C >: LocalScope <: GlobalScope, X](e :ColumnSQL[S, C, X]) :Extractors[C, X] = {
				val column = columnStack.head.asInstanceOf[HeaderColumn[X]]
				columnStack = columnStack.tail
				type ColumnEx[T] = ColumnMappingExtract[X, T, O]
				Assoc[Column, ColumnEx, X](column, ColumnExtract.ident(column))::Nil
			}


			override def component[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[X, A], X, G >: S <: FromClause]
			                      (e :ComponentSQL[S, T, E, M, X, G]) :Extractors[GlobalScope, X] =
			{
				val table = e.entity
				val component = e.mapping
				val componentColumns = component.selectable.toSeq.map(table.export(_)).filter(NoSelectByDefault.disabled)
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


			override def conversion[C >: LocalScope <: GlobalScope, X, Y](e :ConversionSQL[S, C, X, Y]) = {
				val extracts = apply(e.expr)
				extracts.map(schema.composeColumnExtractAssoc(outer, Extractor.none :Y =?> X)(_))
			}

			override def promotion[C >: LocalScope <: GlobalScope, X, Y](e :PromotionConversion[S, C, X, Y]) = {
				val extracts = apply(e.expr)
				extracts.map(schema.composeColumnExtractAssoc(outer, Extractor(e.lift.inverse))(_))
			}


			override def seq[C >: LocalScope <: GlobalScope, X](e :SeqTuple[S, C, X]) = {
				e.inOrder.view.zipWithIndex.flatMap { entry :(SQLExpression[S, C, X], Int) =>
					apply(entry._1).map(
						schema.composeColumnExtractAssoc(outer, (seq :Seq[X]) => seq(entry._2))(_)
					)
				}.toList
			}

			override def chainHead[C >: LocalScope <: GlobalScope, T <: Chain, H]
			                      (tail :ChainTuple[S, C, T], head :SQLExpression[S, C, H]) =
			{
				val tailExs = apply(tail).map(schema.composeColumnExtractAssoc(outer, Chain.init[T] _)(_))
				val headExs = apply(head).map(schema.composeColumnExtractAssoc(outer, Chain.last[H] _)(_))
				headExs ++: tailExs
			}

			override def indexedChainHead[C >: LocalScope <: GlobalScope, I <: IndexedChain, K <: Label :ValueOf, L]
			                             (init :IndexedChainTuple[S, C, I], last :IndexedSQLExpression[S, C, L]) = {
				val tailExs = apply(init).map(schema.composeColumnExtractAssoc(outer, Chain.init[I] _)(_))
				val headExs = apply(last).map(schema.composeColumnExtractAssoc(outer, (_:(I |~ (K :~ L))).last.value)(_))
				headExs ++: tailExs
			}

			override def emptyChain = Nil


			override def expression[C >: LocalScope <: GlobalScope, X](e :SQLExpression[S, C, X]) = unhandled(e)
		}



		override def assemble(pieces: Pieces): Option[V] = assembler(pieces)


		val (assembler, headerColumns) = {
			val aa = new AssemblerAssembler
			aa(header) -> aa.columns.reverse
		}
		override val columns :Unique[Column[_]] = Unique(headerColumns :_*)
		override def components :Unique[Component[_]] = columns
		override val subcomponents = columns

		override val columnExtracts =
			NaturalMap.Lazy((new ExtractsCollector)(header) :Seq[Assoc[Column, ColumnExtract, _]])
		override val extracts = columnExtracts.asInstanceOf[ExtractMap]

	}



	private class ArbitraryFreeSelect[S <: FreeFrom, V, O]
	              (override val from :S, override val header :LocalSQL[S, V], override val isDistinct :Boolean)
		extends ArbitrarySelect[FromClause, S, V, O](from, header) with FreeSelectSQL[V, O]
	{
		override def distinct :FreeSelectSQL[V, O] =
			if (isDistinct) this else new ArbitraryFreeSelect(from, header, true)
	}



	private class ArbitrarySubselect[-F <: FromClause, S <: SubselectOf[F], V, O]
	                                (subclause :S, select :LocalSQL[S, V], override val isDistinct :Boolean)
		extends ArbitrarySelect[F, S, V, O](subclause, select) with SubselectSQL[F, V, O]
	{
		override def distinct :SubselectSQL[F, V, O] =
			if (isDistinct) this else new ArbitrarySubselect(from, header, true)

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ext :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectSQL[E, V, O] =
			from match { //would be safer to refactor this out as a FromClause method
				case some :NonEmptyFrom =>
					type Ext = SubselectOf[E] //FromClause { type Implicit = G }
					implicit val extension = ext.asInstanceOf[some.Implicit ExtendedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val substitute = SQLScribe.shiftBack[S, Ext](from, stretched, ext.length, some.innerSize)
					new ArbitrarySubselect[E, Ext, V, O](stretched, substitute(header), isDistinct)

				case empty :Dual =>
					new ArbitrarySubselect[E, Dual, V, O](empty, header.asInstanceOf[LocalSQL[Dual, V]], isDistinct)

				case _ =>
					throw new UnsupportedOperationException(s"Cannot rebase clause $from :${from.unqualifiedClassName} onto $base.")
			}

	}






	private abstract class ArbitrarySelectColumn[-F <: FromClause, S <: SubselectOf[F], V, O]
	                                            (override val from :S, override val header :ColumnSQL[S, LocalScope, V])
		extends SelectColumn[F, GlobalScope, V, O]
	{ //todo: this should probably be a ColumnAdapter
		override type From = S

		class HeaderColumn extends ColumnMapping[V, O] with SelectedColumn[V] {
			override val name :String = header match {
				case UnboundParamSQL(param, _, _) => param.name //first as it would match the following pattern, too

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
				case form => form <> ColumnWriteForm.unsupported(
					s"Select column $this does not support write", form.sqlType
				)
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


		override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[V] = column.form
		override def writeForm(op :WriteOperationType) :SQLWriteForm[V] = column.form

		override def writtenValues[T](op :WriteOperationType, subject :V, collector :ComponentValuesBuilder[T, O]) :Unit =
			collector.add(column, subject)

		override def writtenValues[T](op :WriteOperationType, subject :V) :ComponentValues[V, O] =
			ColumnValues.preset(column, subject)

		override def nullValue = column.nullValue

		override def assemble(pieces :Pieces) :Option[V] = pieces.get(column)
		//consider: this may be an unjustifiable optimisation if a value is preset for this mapping (unlikely, but possible)
		override def optionally(pieces :Pieces) :Option[V] = pieces.get(column)

	}



	private class ArbitraryFreeSelectColumn[S <: FreeFrom, V, O]
	              (override val from :S, override val header :ColumnSQL[S, LocalScope, V], override val isDistinct :Boolean)
		extends ArbitrarySelectColumn[FromClause, S, V, O](from, header) with FreeSelectColumn[V, O]
	{
		override def distinct :FreeSelectColumn[V, O] =
			if (isDistinct) this else new ArbitraryFreeSelectColumn(from, header, true)
	}



	private class ArbitrarySubselectColumn[-F <: FromClause, S <: SubselectOf[F], V, O]
	              (clause :S, override val header :ColumnSQL[S, LocalScope, V], override val isDistinct :Boolean)
		extends ArbitrarySelectColumn[F, S, V, O](clause, header) with SubselectColumn[F, V, O]
	{
		override def distinct :SubselectColumn[F, V, O] =
			if (isDistinct) this else new ArbitrarySubselectColumn(from, header, true)

		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ext :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectColumn[E, V, O] =
			from match {
				case some :FromSome =>
					type Ext = SubselectOf[E] //FromClause { type Implicit = G }
					implicit val extension = ext.asInstanceOf[some.Implicit ExtendedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val substitute = SQLScribe.shiftBack[S, Ext](from, stretched, ext.length, some.innerSize)
					new ArbitrarySubselectColumn[E, Ext, V, O](stretched, substitute(header), isDistinct)

				case empty :Dual =>
					val castHeader = header.asInstanceOf[ColumnSQL[Dual, LocalScope, V]]
					new ArbitrarySubselectColumn[E, Dual, V, O](empty, castHeader, isDistinct)
			}
	}



	private class SelectColumnMappingImpl[F <: FreeFrom, M[A] <: ColumnMapping[V, A], V, O]
	              (clause :F, override val header :BaseColumnComponentSQL[F, M, V, _ >: F <: FromClause],
	               override val isDistinct :Boolean)
		extends ArbitraryFreeSelectColumn[F, V, O](clause, header, isDistinct) with SelectColumnMapping[F, M, V, O]
	{
		override val mapping :M[O] = header.mapping.asInstanceOf[M[O]]

		override def distinct :SelectColumnMapping[F, M, V, O] =
			if (isDistinct) this else new SelectColumnMappingImpl(from, header, true)
	}



	private class SubselectColumnMappingImpl[-F <: FromClause, S <: SubselectOf[F], M[A] <: ColumnMapping[V, A], V, O]
	                                        (clause :S, override val header :BaseColumnComponentSQL[S, M, V, _ >: S <: FromClause],
		                                     override val isDistinct :Boolean)
		extends ArbitrarySubselectColumn[F, S, V, O](clause, header, isDistinct) with SubselectColumnMapping[F, S, M, V, O]
	{
		override val mapping :M[O] = header.mapping.asInstanceOf[M[O]]

		override def distinct :SubselectColumnMapping[F, S, M, V, O] =
			if (isDistinct) this else new SubselectColumnMappingImpl(from, header, true)
	}






	trait FreeSelectColumnMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def freeSelect[V, O](e :FreeSelectColumn[V, O]) :Y[GlobalScope, Rows[V]]
	}

	type MatchFreeSelectColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = FreeSelectColumnMatcher[F, Y]

	type CaseFreeSelectColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = FreeSelectColumnMatcher[F, Y]



	trait SubselectColumnMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def subselect[V, O](e :SubselectColumn[F, V, O]) :Y[GlobalScope, Rows[V]]
	}

	type MatchSubselectColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = SubselectColumnMatcher[F, Y]

	type CaseSubselectColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = SubselectColumnMatcher[F, Y]



	trait SelectColumnMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends FreeSelectColumnMatcher[F, Y] with SubselectColumnMatcher[F, Y]
	{
		def select[S >: LocalScope <: GlobalScope, V, O](e :SelectColumn[F, S, V, O]) :Y[S, Rows[V]]
	}

	type MatchSelectColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = SelectColumnMatcher[F, Y]

	trait CaseSelectColumn[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchSelectColumn[F, Y] {

		override def freeSelect[V, O](e :FreeSelectColumn[V, O]) :Y[GlobalScope, Rows[V]] =
			select(e :SelectColumn[F, GlobalScope, V, O])

		override def subselect[V, O](e :SubselectColumn[F, V, O]) :Y[GlobalScope, Rows[V]] =
			select(e :SelectColumn[F, GlobalScope, V, O])
	}



	trait FreeSelectMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends FreeSelectColumnMatcher[F, Y]
	{
		def freeSelect[V, O](e :FreeSelectSQL[V, O]) :Y[GlobalScope, Rows[V]]
	}

	trait MatchFreeSelect[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends FreeSelectMatcher[F, Y] with CaseFreeSelectColumn[F, Y]
	{
		override def freeSelect[V, O](e :FreeSelectColumn[V, O]) :Y[GlobalScope, Rows[V]] =
			freeSelect(e :FreeSelectSQL[V, O])
	}

	type CaseFreeSelect[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = MatchFreeSelect[F, Y]



	trait SubselectMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def subselect[V, O](e :SubselectSQL[F, V, O]) :Y[GlobalScope, Rows[V]]
	}

	trait MatchSubselect[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends SubselectMatcher[F, Y] with CaseSubselectColumn[F, Y]
	{
		override def subselect[V, O](e :SubselectColumn[F, V, O]) :Y[GlobalScope, Rows[V]] =
			subselect(e :SubselectSQL[F, V, O])
	}

	type CaseSubselect[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = MatchSubselect[F, Y]



	trait SelectMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends SelectColumnMatcher[F, Y]
		with FreeSelectMatcher[F, Y] with SubselectMatcher[F, Y]
	{
		def select[S >: LocalScope <: GlobalScope, V, O](e :SelectSQL[F, S, V, O]) :Y[S, Rows[V]]
	}

	trait MatchSelect[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends SelectMatcher[F, Y]
		with CaseFreeSelect[F, Y] with CaseSubselect[F, Y]
	{
		override def select[S >: LocalScope <: GlobalScope, V, O](e :SelectColumn[F, S, V, O]) :Y[S, Rows[V]] =
			select(e :SelectSQL[F, S, V, O])
	}

	trait CaseSelect[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchSelect[F, Y] {
		def subselect[V, O](e: SubselectSQL[F, V, O]): Y[GlobalScope, Rows[V]] = select(e)

		def freeSelect[V, O](e: FreeSelectSQL[V, O]): Y[GlobalScope, Rows[V]] = select(e)
	}


}

