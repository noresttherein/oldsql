package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Chain.{@~, ChainApplication}
import net.noresttherein.oldsql.collection.{Chain, IndexedChain}
import net.noresttherein.oldsql.schema.{AbstractRelation, ColumnMapping, ColumnReadForm, Relation, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.{ColumnSQL, ColumnSQLMapping, Dual, FromSome, GroupByClause, IndexedColumnSQLMapping, IndexedMapping, IndexedSQLMapping, ParamSelect, RowProduct, SQLExpression, SQLMapping}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnMatcher
import net.noresttherein.oldsql.sql.RowProduct.{ExtendedBy, GroundFrom, NonEmptyFrom, PartOf, SubselectOf}
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionMatcher, GlobalScope, LocalScope, LocalSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{ColumnComponentSQL, ComponentSQL, RelationSQL}
import net.noresttherein.oldsql.sql.ast.QuerySQL.{ColumnMappingQuery, ColumnQuery, MappingQuery, Rows}
import net.noresttherein.oldsql.sql.ast.TupleSQL.IndexedChainTuple.{IndexedColumn, IndexedSQLExpression}
import net.noresttherein.oldsql.sql.ast.TupleSQL.IndexedChainTuple
import net.noresttherein.oldsql.sql.mechanics.{SQLScribe, TableOffset}
import net.noresttherein.oldsql.sql.SelectAPI.SelectTemplate

//here be implicits
import net.noresttherein.oldsql.slang._






/** Representation of an SQL select as an SQL expression used in the context of source `F`. If the first type argument
  * is the wildcard `RowProduct`, this will be a `TopSelectSQL` instance - a select independent of any external
  * tables or parameters, in which all formulas (''select'' clause, ''where'' clause, etc) can be evaluated
  * based on the values of the tables in its ''from'' clause. If `F` is not `RowProduct`, but contains tables, this is
  * a subselect nested inside a select for source `F` - in its ''select'', ''from'' or ''where'' clause. The source for
  * this expression, given by the member type `From`, is always an extension of `F`. Subclasses should extend the trait
  * for one of the above cases: [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]] or
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]], instead of being derived directly
  * from this trait.
  *
  * @tparam F the source of data for the ''enclosing'' select - tables from the ''from'' clause and any unbound parameters.
  * @tparam V the combined type of the whole ''select'' clause, to which every returned row maps.
  */
sealed trait SelectSQL[-F <: RowProduct, V]
	extends QuerySQL[F, V] with SelectTemplate[V, ({ type S[X] = SelectSQL[F, X] })#S]
{
	override def readForm :SQLReadForm[Rows[V]] = selectClause.readForm.nullMap(Rows(_))

	/** The from clause of this select. */
	override type From <: SubselectOf[F]

	def isSubselect :Boolean = from.isSubselect
	override def isGlobal = true
	override def asGlobal :Option[SelectSQL[F, V]] = Some(this)
	override def isAnchored = true
	override def anchor(from :F) :SelectSQL[F, V] = this

	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SelectSQL[E, V]

	override def extend[U <: F, E <: RowProduct]
	                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :SelectSQL[E, V]

//	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//	                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
//		matcher.select(this)


	protected override def reverseCollect[X](fun: PartialFunction[SQLExpression.*, X], acc: List[X]): List[X] = {
		//we ignore filters in the implicit portion as, if this is a subselect, they would be collected by the enclosing expression.
		val headerItems = reverseCollect(selectClause)(fun, super.reverseCollect(fun, acc))
		reverseCollect(from.filter)(fun, from match {
			case GroupByClause(ungrouped) => reverseCollect(ungrouped.filter)(fun, headerItems)
			case _ => headerItems
		})
	}


	override def isomorphic(expression: SQLExpression.*): Boolean = expression match {
		case s :SelectSQL[_, _] =>
			(s eq this) || (s canEqual this) && (s.selectClause isomorphic selectClause) && (s.from == from)
		case _ => false
	}

	private[oldsql] override def equivalent(expression: SQLExpression.*): Boolean = expression match {
		case s :SelectSQL[_, _] =>
			(s eq this) || (s canEqual this) && (s.selectClause equivalent selectClause) && (s.from == from)
		case _ => false
	}


	override def canEqual(that :Any) :Boolean = that.getClass == getClass
}






object SelectSQL {
	//todo: order by
	//todo: limit, offset
	//todo: parameterized selects
	//todo: mapping indexed headers

	def apply[F <: GroundFrom, M[A] <: BaseMapping[V, A], V]
	         (from :F, header :ComponentSQL[F, M]) :SelectMapping[F, M, V] =
		new SelectComponent[F, M, V](from, header, false)

	def apply[F <: GroundFrom, M[A] <: ColumnMapping[V, A], V]
	         (from :F, header :ColumnComponentSQL[F, M, V]) :SelectColumnMapping[F, M, V] =
		new SelectComponentColumn[F, M, V](from, header, false)

	def apply[F <: GroundFrom, V](from :F, header :TupleSQL[F, LocalScope, V]) :TopSelectSQL[V] =
		new ArbitraryTopSelect[F, V](from, header.anchor(from), false)

	def apply[F <: GroundFrom, V <: IndexedChain](from :F, header :IndexedChainTuple[F, LocalScope, V])
			:TopSelectAs[IndexedMapping.Of[V]#Projection] =
		new TopIndexedSelect(from, header.anchor(from), false)

	def apply[F <: GroundFrom, A <: Label, V](from :F, header :IndexedColumn[F, LocalScope, A, V])
			:TopSelectColumnAs[IndexedMapping.Of[V]#Column, V] =
		new TopSelectIndexedColumn(from, header.anchor(from), false)

	def apply[F <: GroundFrom, X, Y](from :F, header :ConversionSQL[F, LocalScope, X, Y]) :TopSelectSQL[Y] =
		new ArbitraryTopSelect[F, Y](from, header.anchor(from), false)

	def apply[F <: GroundFrom, V](from :F, header :ColumnSQL[F, LocalScope, V]) :TopSelectColumn[V] =
		new ArbitraryTopSelectColumn(from, header.anchor(from), false)



	def subselect[F <: NonEmptyFrom, S <: SubselectOf[F], M[A] <: BaseMapping[V, A], V]
	             (from :S, header :ComponentSQL[S, M]) :SubselectMapping[F, S, M, V] =
		 new SubselectComponent[F, S, M, V](from, header, false)

	def subselect[F <: NonEmptyFrom, S <: SubselectOf[F], M[A] <: ColumnMapping[V, A], V]
	             (from :S, column :ColumnComponentSQL[S, M, V])
			:SubselectColumnMapping[F, S, M, V] =
		new SubselectComponentColumn[F, S, M, V](from, column, false)

	def subselect[F <: NonEmptyFrom, S <: SubselectOf[F], V]
	             (from :S, header :TupleSQL[S, LocalScope, V]) :SubselectSQL[F, V] =
		new ArbitrarySubselect[F, S, V](from, header.anchor(from), false)

	def subselect[F <: NonEmptyFrom, S <: SubselectOf[F], V <: IndexedChain]
	             (from :S, header :IndexedChainTuple[S, LocalScope, V])
			:SubselectAs[F, IndexedMapping.Of[V]#Projection] =
		new IndexedSubselect(from, header.anchor(from), false)

	def subselect[F <: NonEmptyFrom, S <: SubselectOf[F], A <: Label, V]
	             (from :S, header :IndexedColumn[S, LocalScope, A, V])
			:SubselectColumnAs[F, IndexedMapping.Of[V]#Column, V] =
		new SubselectIndexedColumn(from, header.anchor(from), false)

	def subselect[F <: NonEmptyFrom, S <: SubselectOf[F], X, Y]
	             (from :S, header :ConversionSQL[S, LocalScope, X, Y]) :SubselectSQL[F, Y] =
		new ArbitrarySubselect[F, S, Y](from, header.anchor(from), false)

	def subselect[F <: NonEmptyFrom, S <: SubselectOf[F], V]
	             (from :S, header :ColumnSQL[S, LocalScope, V]) :SubselectColumn[F, V] =
		new ArbitrarySubselectColumn[F, S, V](from, header.anchor(from), false)



	type * = SelectSQL[_ <: RowProduct, _]



	type GroundSelect[V] = SelectSQL[RowProduct, V]
	type GroundSelectColumn[V] = SelectColumn[RowProduct, V]
	type GroundSelectAs[H[A] <: MappingAt[A]] = SelectAs[RowProduct, H]
	type GroundSelectColumnAs[H[A] <: ColumnMapping[V, A], V] = SelectColumnAs[RowProduct, H, V]



	trait SelectColumn[-F <: RowProduct, V]
		extends ColumnQuery[F, V] with SelectSQL[F, V] with SelectTemplate[V, ({ type S[X] = SelectColumn[F, X] })#S]
	{
		override def readForm :ColumnReadForm[Rows[V]] = selectClause.readForm.nullMap(Rows(_))
		override val selectClause :ColumnSQL[From, LocalScope, V]
		override def single :ColumnSQL[F, GlobalScope, V] = to[V]

		override def asGlobal :Option[SelectColumn[F, V]] = Some(this)
		override def anchor(from :F) :SelectColumn[F, V] = this


		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :PartOf[U, E]) :SelectColumn[E, V]

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :SelectColumn[E, V]

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.select(this)
	}



	/** Base trait for SQL select expressions where the ''select'' clause depends solely on the explicit ''from'' clause
	  * of the ''select'', i.e. it is not dependent on any outside rows. Such an expression is a valid independent
	  * select statement in opposition to subselect expressions.
	  */
	trait TopSelectSQL[V]
		extends SelectSQL[RowProduct, V] with ParamSelect[@~, V] with SelectTemplate[V, TopSelectSQL]
	{
		override type From <: GroundFrom

		override def map[X](f :V => X) :TopSelectSQL[X] =
			new ArbitraryTopSelect[From, X](from, selectClause.map(f), isDistinct)

		override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit ext :U PartOf E) :TopSelectSQL[V] =
			this

		override def extend[U <: RowProduct, S <: RowProduct]
		             (base :S)(implicit ev :U ExtendedBy S, global :GlobalScope <:< GlobalScope) :TopSelectSQL[V] =
			this

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[RowProduct, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.topSelect(this)

		protected override def reverseCollect[X](fun :PartialFunction[SQLExpression.*, X], acc :List[X]) :List[X] =
			super[SelectSQL].reverseCollect(fun, acc)

		override def bind(params: @~) :TopSelectSQL[V] = this
	}



	trait TopSelectColumn[V]
		extends TopSelectSQL[V] with SelectColumn[RowProduct, V] with SelectTemplate[V, TopSelectColumn]
	{
		override def map[X](f :V => X) :TopSelectColumn[X] =
			new ArbitraryTopSelectColumn[From, X](from, selectClause.map(f), isDistinct)


		override def basedOn[U <: RowProduct, E <: RowProduct]
		                    (base :E)(implicit ext :U PartOf E) :TopSelectColumn[V] = this

		override def extend[U <: RowProduct, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:TopSelectColumn[V] =
			this

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ColumnMatcher[RowProduct, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.topSelect(this)
	}



	/** A base trait for all SQL select expressions nested under another SQL select.
	  * @tparam F the ''from'' clause of the outer select, forming a prefix of `S` until the last occurrence
	  *           of a `Subselect` join kind.
	  * @tparam V the type of the scala value selected by this subselect.
	  */ //consider: relations for subselects
	trait SubselectSQL[-F <: RowProduct, V] extends SelectSQL[F, V] {

		override def distinct :SubselectSQL[F, V]

		override def map[X](f :V => X) :SubselectSQL[F, X] =
			new ArbitrarySubselect[F, From, X](from, selectClause.map(f), isDistinct)

		override def map[Fun, C <: Chain, X]
		                (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
				:SubselectSQL[F, X] =
			map(applyFun(f))


		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SubselectSQL[E, V] =
			extend(base)(ext.asExtendedBy, implicitly[GlobalScope <:< GlobalScope])

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit extension :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectSQL[E, V]

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.subselect(this)

	}



	trait SubselectColumn[-F <: RowProduct, V] extends SubselectSQL[F, V] with SelectColumn[F, V] {

		override def distinct :SubselectColumn[F, V]

		override def map[X](f :V => X) :SubselectColumn[F, X] =
			new ArbitrarySubselectColumn[F, From, X](from, selectClause.map(f), isDistinct)

		override def map[Fun, C <: Chain, X](f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
				:SubselectColumn[F, X] =
			map(applyFun(f))


		override def asGlobal :Option[SubselectColumn[F, V]] = Some(this)


		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SubselectColumn[E, V] =
			extend(base)(ext.asExtendedBy, implicitly[GlobalScope <:< GlobalScope])

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit extension :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectColumn[E, V]

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.subselect(this)
	}



	/** A `SelectSQL` interface exposing the mapping type `H` used for the ''select'' clause. */
	trait SelectAs[-F <: RowProduct, H[A] <: MappingAt[A]]
		extends MappingQuery[F, H] with SelectSQL[F, H[()]#Subject]
	{
		override def distinct :SelectAs[F, H]

		override def asGlobal :Option[SelectAs[F, H]] = Some(this)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SelectAs[E, H]

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :SelectAs[E, H]

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[H[Any]#Subject]] =
//			matcher.selectMapping(this)
	}


	trait SelectColumnAs[-F <: RowProduct, H[A] <: ColumnMapping[V, A], V]
		extends ColumnMappingQuery[F, H, V] with SelectAs[F, H] with SelectColumn[F, V]
	{
		override def distinct :SelectColumnAs[F, H, V]

		override def asGlobal :Option[SelectColumnAs[F, H, V]] = Some(this)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SelectColumnAs[E, H, V]

		override def extend[U <: F, E <: RowProduct]
		             (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :SelectColumnAs[E, H, V]

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.selectMapping(this)
	}



	trait TopSelectAs[H[A] <: MappingAt[A]] extends TopSelectSQL[H[()]#Subject] with SelectAs[RowProduct, H] {
		override def distinct :TopSelectAs[H]
		override def asGlobal :Option[TopSelectAs[H]] = Some(this)

		override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit ext :U PartOf E) :TopSelectAs[H] =
			this

		override def extend[U <: RowProduct, S <: RowProduct]
		             (base :S)(implicit ev :U ExtendedBy S, global :GlobalScope <:< GlobalScope) :TopSelectAs[H] =
			this
	}

	trait TopSelectColumnAs[H[A] <: ColumnMapping[V, A], V]
		extends TopSelectColumn[V] with TopSelectAs[H] with SelectColumnAs[RowProduct, H, V]
	{
		override def distinct :TopSelectColumnAs[H, V]
		override def asGlobal :Option[TopSelectColumnAs[H, V]] = Some(this)

		override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit ext :U PartOf E) :TopSelectColumnAs[H, V] =
			this

		override def extend[U <: RowProduct, S <: RowProduct]
		                   (base :S)(implicit ev :U ExtendedBy S, global :GlobalScope <:< GlobalScope)
				:TopSelectColumnAs[H, V] =
			this
	}


	trait SubselectAs[-F <: RowProduct, H[A] <: MappingAt[A]]
		extends SelectAs[F, H] with SubselectSQL[F, H[()]#Subject]
	{
		override def distinct :SubselectAs[F, H]
		override def asGlobal :Option[SubselectAs[F, H]] = Some(this)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SubselectAs[E, H] =
			extend(base)(ext.asExtendedBy, implicitly[GlobalScope <:< GlobalScope])

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit extension :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectAs[E, H]
	}

	trait SubselectColumnAs[-F <: RowProduct, H[A] <: ColumnMapping[V, A], V]
		extends SubselectColumn[F, V] with SubselectAs[F, H] with SelectColumnAs[F, H, V]
	{
		override def distinct :SubselectColumnAs[F, H, V]
		override def asGlobal :Option[SubselectColumnAs[F, H, V]] = Some(this)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SubselectColumnAs[E, H, V] =
			extend(base)(ext.asExtendedBy, implicitly[GlobalScope <:< GlobalScope])

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit extension :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectColumnAs[E, H, V]
	}




	trait SelectMapping[F <: GroundFrom, H[A] <: BaseMapping[V, A], V] extends TopSelectAs[H] {
		override type From = F
		override def distinct :SelectMapping[F, H, V]

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[RowProduct, Y]) :Y[GlobalScope, Rows[H[()]#Subject]] =
			matcher.topSelectMapping[H](this)
	}

	trait SelectColumnMapping[F <: GroundFrom, H[A] <: ColumnMapping[V, A], V]
		extends TopSelectColumnAs[H, V] with SelectMapping[F, H, V]
	{
		override def distinct :SelectColumnMapping[F, H, V]
		override def asGlobal :Option[SelectColumnMapping[F, H, V]] = Some(this)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ColumnMatcher[RowProduct, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.topSelectMapping(this)
	}


	trait SubselectMapping[-F <: RowProduct, S <: SubselectOf[F], H[A] <: BaseMapping[V, A], V]
		extends SubselectAs[F, H]
	{
		override type From = S
		override def distinct :SubselectMapping[F, S, H, V]
		override def asGlobal :Option[SubselectMapping[F, S, H, V]] = Some(this)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[H[()]#Subject]] =
			matcher.subselectMapping[H](this)
	}

	trait SubselectColumnMapping[-F <: RowProduct, S <: SubselectOf[F], H[A] <: ColumnMapping[V, A], V]
		extends SubselectColumnAs[F, H, V] with SubselectMapping[F, S, H, V]
	{
		override def distinct :SubselectColumnMapping[F, S, H, V]
		override def asGlobal :Option[SubselectColumnMapping[F, S, H, V]] = Some(this)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.subselectMapping(this)
	}






	private abstract class BaseSelectComponent[-F <: RowProduct, S <: SubselectOf[F],
	                                           H[A] <: BaseMapping[V, A], V]
	                                          (override val from :S, override val selectClause :ComponentSQL[S, H])
		extends SelectSQL[F, V] with SelectAs[F, H]
	{
		override type From = S

		protected override def component[O] :ResultMapping[O] = selectClause.mapping.withOrigin[O]
		protected override def export[O] :RefinedMapping[V, O] = selectClause.export.withOrigin[O]

		override val columns: Seq[SelectedColumn[_]] = //todo: is this the place where we finally decide on the column set?
			selectClause.export.selectedByDefault.toSeq.map(include(_))

		private def include[X](column :ColumnMapping[X, selectClause.Origin]) :SelectedColumn[X] =
			new SelectedColumn[X] {
				override val name :String = column.name
				override val expression  = selectClause \ column
			}
	}



	private class SelectComponent[F <: GroundFrom, H[A] <: BaseMapping[V, A], V]
	                             (override val from :F, override val selectClause :ComponentSQL[F, H],
	                              override val isDistinct :Boolean)
		extends BaseSelectComponent[RowProduct, F, H, V](from, selectClause) with SelectMapping[F, H, V]
	{
		override def distinct :SelectMapping[F, H, V] =
			if (isDistinct) this else new SelectComponent(from, selectClause, true)
	}



	private class SubselectComponent[-F <: RowProduct, S <: SubselectOf[F], H[A] <: BaseMapping[V, A], V]
	                                (subselect :S, component :ComponentSQL[S, H], override val isDistinct :Boolean)
		extends BaseSelectComponent[F, S, H, V](subselect, component)
		   with SubselectMapping[F, S, H, V]
	{
		override def distinct :SubselectMapping[F, S, H, V] =
			if (isDistinct) this else new SubselectComponent(from, selectClause, true)

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectAs[E, H] =
			from match { //would be safer to refactor this out as a RowProduct method
				case some :NonEmptyFrom =>
					type Ext = SubselectOf[E] //pretend this is the actual type S after rebasing to the extension clause G
					implicit val extension = ev.asInstanceOf[some.Implicit ExtendedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val subselectTables = stretched.fullSize - base.fullSize
					val offset = selectClause.origin.offset
					val replacement =
						if (offset < subselectTables)
							selectClause.asInstanceOf[ComponentSQL[Ext, H]]
						else
							selectClause.moveTo(new TableOffset[Ext, selectClause.Entity](offset + ev.length))
					new SubselectComponent[E, Ext, H, V](stretched, replacement, isDistinct)

				case empty :Dual =>
					val adaptedHeader = selectClause.asInstanceOf[ComponentSQL[Dual, H]]
					new SubselectComponent[E, Dual, H, V](empty, adaptedHeader, isDistinct)

				case _ =>
					throw new UnsupportedOperationException(s"Cannot rebase clause $from :${from.localClassName} onto $base.")
			}

	}



	private class SelectComponentColumn[F <: GroundFrom, H[A] <: ColumnMapping[V, A], V]
	              (clause :F, override val selectClause :ColumnComponentSQL[F, H, V], override val isDistinct :Boolean)
		extends SelectComponent[F, H, V](clause, selectClause, isDistinct) with SelectColumnMapping[F, H, V]
	{
		override def distinct :SelectColumnMapping[F, H, V] =
			if (isDistinct) this else new SelectComponentColumn(from, selectClause, true)
	}



	private class SubselectComponentColumn
	              [-F <: RowProduct, S <: SubselectOf[F], H[A] <: ColumnMapping[V, A], V]
	              (override val from :S, override val selectClause :ColumnComponentSQL[S, H, V],
	               override val isDistinct :Boolean)
		extends SubselectComponent[F, S, H, V](from, selectClause, isDistinct)
		   with SubselectColumnMapping[F, S, H, V]
	{
		override def distinct :SubselectColumnMapping[F, S, H, V] =
			if (isDistinct) this else new SubselectComponentColumn(from, selectClause, true)

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectColumnAs[E, H, V] =
			from match { //would be safer to refactor this out as a RowProduct method
				case some :NonEmptyFrom => //todo: refactor this together with SubselectComponent; if S <: NonEmptyFrom, than the casting could conceivably by omitted
					type Ext = SubselectOf[E] //pretend this is the actual type S after rebasing to the extension clause G
					implicit val extension = ev.asInstanceOf[some.Implicit ExtendedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val subselectTables = stretched.fullSize - base.fullSize
					val offset = selectClause.origin.offset
					val table = selectClause.origin
					val replacement =
						if (offset < subselectTables)
							selectClause.asInstanceOf[ColumnComponentSQL[Ext, H, V]]
						else
							selectClause.moveTo(new TableOffset[Ext, selectClause.Entity](offset + ev.length))
					new SubselectComponentColumn[E, Ext, H, V](stretched, replacement, isDistinct)

				case empty :Dual =>
					val adaptedHeader = selectClause.asInstanceOf[ColumnComponentSQL[Dual, H, V]]
					new SubselectComponentColumn[E, Dual, H, V](empty, adaptedHeader, isDistinct)

				case _ =>
					throw new UnsupportedOperationException(
						s"Cannot rebase clause $from :${from.localClassName} onto $base."
					)
			}
	}





	/** A select expression based on the given row source and selecting an arbitrary expression `header` in its ''select''
	  * clause. This header will be translated by recursively flat mapping the header expression to obtain a flat sequence
	  * of columns.
	  */
	private abstract class ArbitrarySelect[-F <: RowProduct, S <: SubselectOf[F], V] protected
	                       (override val from :S, protected val mapping :SQLMapping[S, LocalScope, V, ()])
		extends SelectSQL[F, V]
	{
		def this(from :S, header :SQLExpression[S, LocalScope, V]) =
			this(from, SQLMapping[S, LocalScope, V, ()](header))

		override type From = S

		override val selectClause = mapping.expr

		/** A column in the header of owning select.
		  * @param column the `ColumnMapping` implementation based on a `ColumnSQL` expression
		  *               providing the value for the column.
		  */
		protected class HeaderColumn[T](column :ColumnSQLMapping[S, LocalScope, T, _])
			extends SelectedColumn[T]
		{
			override def expression :ColumnSQL[S, LocalScope, T] = column.expr
			override def name :String = column.name
		}

		override val columns :Seq[SelectedColumn[_]] = mapping.columns.map { col => new HeaderColumn(col) }

	}



	private trait ArbitrarySelectTemplate[-F <: RowProduct, S <: SubselectOf[F], M[O] <: BaseMapping[V, O], V]
		extends SelectSQL[F, V]
	{ this :ArbitrarySelect[F, S, V] =>
		override type ResultMapping[O] = M[O]

		protected val mapping :M[()]

		protected override def component[O] :M[O] =
			(this :ArbitrarySelectTemplate[F, S, M, V]).mapping.withOrigin[O]

		protected override def export[O] :M[O] = component[O]
	}



	private class ArbitraryTopSelect[F <: GroundFrom, V]
	              (override val from :F, override val selectClause :LocalSQL[F, V], override val isDistinct :Boolean)
		extends ArbitrarySelect[RowProduct, F, V](from, selectClause)
		   with ArbitrarySelectTemplate[RowProduct, F, SQLMapping.Project[F, LocalScope, V]#Expression, V]
		   with TopSelectSQL[V]
	{
		override def distinct :TopSelectSQL[V] =
			if (isDistinct) this else new ArbitraryTopSelect(from, selectClause, true)
	}



	private class ArbitrarySubselect[-F <: RowProduct, S <: SubselectOf[F], V]
	              (subclause :S, select :LocalSQL[S, V], override val isDistinct :Boolean)
		extends ArbitrarySelect[F, S, V](subclause, select)
		   with ArbitrarySelectTemplate[F, S, SQLMapping.Project[S, LocalScope, V]#Expression, V]
		   with SubselectSQL[F, V]
	{
		override def distinct :SubselectSQL[F, V] =
			if (isDistinct) this else new ArbitrarySubselect(from, selectClause, true)

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ext :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectSQL[E, V] =
			from match { //would be safer to refactor this out as a RowProduct method
				case some :NonEmptyFrom =>
					type Ext = SubselectOf[E] //RowProduct { type Implicit = G }
					implicit val extension = ext.asInstanceOf[some.Implicit ExtendedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val substitute = SQLScribe.shiftBack[S, Ext](from, stretched, ext.length, some.size)
					new ArbitrarySubselect[E, Ext, V](stretched, substitute(selectClause), isDistinct)

				case empty :Dual =>
					new ArbitrarySubselect[E, Dual, V](empty, selectClause.asInstanceOf[LocalSQL[Dual, V]], isDistinct)

				case _ =>
					throw new UnsupportedOperationException(
						s"Cannot rebase clause $from :${from.localClassName} onto $base."
					)
			}
	}






	private abstract class ArbitrarySelectColumn[-F <: RowProduct, S <: SubselectOf[F], V]
	                       (override val from :S, override val mapping :ColumnSQLMapping[S, LocalScope, V, ()])
		extends ArbitrarySelect[F, S, V](from, mapping)
		   with ArbitrarySelectTemplate[F, S, SQLMapping.Project[S, LocalScope, V]#Column, V]
		   with SelectColumn[F, V]
	{
		def this(from :S, expression :ColumnSQL[S, LocalScope, V]) =
			this(from, ColumnSQLMapping[S, LocalScope, V, ()](expression))

		override val selectClause = mapping.expr
	}



	private class ArbitraryTopSelectColumn[F <: GroundFrom, V]
	              (override val from :F, override val selectClause :ColumnSQL[F, LocalScope, V], override val isDistinct :Boolean)
		extends ArbitrarySelectColumn[RowProduct, F, V](from, selectClause) with TopSelectColumn[V]
	{
		override def distinct :TopSelectColumn[V] =
			if (isDistinct) this else new ArbitraryTopSelectColumn(from, selectClause, true)
	}



	private class ArbitrarySubselectColumn[-F <: RowProduct, S <: SubselectOf[F], V]
	              (clause :S, override val selectClause :ColumnSQL[S, LocalScope, V], override val isDistinct :Boolean)
		extends ArbitrarySelectColumn[F, S, V](clause, selectClause) with SubselectColumn[F, V]
	{
		override def distinct :SubselectColumn[F, V] =
			if (isDistinct) this else new ArbitrarySubselectColumn(from, selectClause, true)

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ext :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectColumn[E, V] =
			from match {
				case some :FromSome =>
					type Ext = SubselectOf[E] //RowProduct { type Implicit = G }
					implicit val extension = ext.asInstanceOf[some.Implicit ExtendedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val substitute = SQLScribe.shiftBack[S, Ext](from, stretched, ext.length, some.size)
					new ArbitrarySubselectColumn[E, Ext, V](stretched, substitute(selectClause), isDistinct)

				case empty :Dual =>
					val castHeader = selectClause.asInstanceOf[ColumnSQL[Dual, LocalScope, V]]
					new ArbitrarySubselectColumn[E, Dual, V](empty, castHeader, isDistinct)
			}
	}






	private abstract class IndexedSelect[-F <: RowProduct, S <: SubselectOf[F], V]
	                       (from :S, override val mapping :IndexedSQLMapping[S, LocalScope, V, ()])
		extends ArbitrarySelect[F, S, V](from, mapping)
		   with ArbitrarySelectTemplate[F, S, IndexedMapping.Of[V]#Projection, V]
	{
		def this(from :S, expression :IndexedSQLExpression[S, LocalScope, V]) =
			this(from, expression.mapping[()])

		override val selectClause = mapping.expr
	}



	private class TopIndexedSelect[F <: GroundFrom, V]
	              (clause :F, select :IndexedSQLExpression[F, LocalScope, V], override val isDistinct :Boolean)
		extends IndexedSelect[RowProduct, F, V](clause, select)
		   with TopSelectAs[IndexedMapping.Of[V]#Projection]
	{
		override def distinct :TopSelectAs[IndexedMapping.Of[V]#Projection] =
			if (isDistinct) this else new TopIndexedSelect(from, selectClause, true)
	}



	private class IndexedSubselect[-F <: RowProduct, S <: SubselectOf[F], V]
	              (subclause :S, select :IndexedSQLExpression[S, LocalScope, V], override val isDistinct :Boolean)
		extends IndexedSelect[F, S, V](subclause, select)
		   with SubselectAs[F, IndexedMapping.Of[V]#Projection]
	{
		override def distinct :SubselectAs[F, IndexedMapping.Of[V]#Projection] =
			if (isDistinct) this else new IndexedSubselect(from, selectClause, true)

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ext :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectAs[E, IndexedMapping.Of[V]#Projection] =
			from match { //would be safer to refactor this out as a RowProduct method
				case some :NonEmptyFrom =>
					type Ext = SubselectOf[E] //RowProduct { type Implicit = G }
					implicit val extension = ext.asInstanceOf[some.Implicit ExtendedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val substitute = SQLScribe.shiftBack[S, Ext](from, stretched, ext.length, some.size)
					val indexed = substitute(selectClause).asInstanceOf[IndexedSQLExpression[Ext, LocalScope, V]]
					new IndexedSubselect[E, Ext, V](stretched, indexed, isDistinct)

				case empty :Dual =>
					val cast = selectClause.asInstanceOf[IndexedSQLExpression[Dual, LocalScope, V]]
					new IndexedSubselect[E, Dual, V](empty, cast, isDistinct)

				case _ =>
					throw new UnsupportedOperationException(
						s"Cannot rebase clause $from :${from.localClassName} onto $base."
					)
			}
	}



	private abstract class SelectIndexedColumn[-F <: RowProduct, S <: SubselectOf[F], A <: Label, V]
	                       (override val from :S, override val mapping :IndexedColumnSQLMapping[S, LocalScope, A, V, ()])
		extends ArbitrarySelect[F, S, V](from, mapping)
		   with ArbitrarySelectTemplate[F, S, IndexedMapping.Of[V]#Column, V]
		   with SelectColumn[F, V]
	{
		def this(from :S, expression :IndexedColumn[S, LocalScope, A, V]) =
			this(from, IndexedColumnSQLMapping[S, LocalScope, A, V, ()](expression))

		override val selectClause = mapping.expr
	}



	private class TopSelectIndexedColumn[F <: GroundFrom, A <: Label, V]
	              (override val from :F, override val selectClause :IndexedColumn[F, LocalScope, A, V],
	               override val isDistinct :Boolean)
		extends SelectIndexedColumn[RowProduct, F, A, V](from, selectClause)
		   with TopSelectColumnAs[IndexedMapping.Of[V]#Column, V]
	{
		override def distinct :TopSelectColumnAs[IndexedMapping.Of[V]#Column, V] =
			if (isDistinct) this else new TopSelectIndexedColumn(from, selectClause, true)
	}



	private class SubselectIndexedColumn[-F <: RowProduct, S <: SubselectOf[F], A <: Label, V]
	              (clause :S, override val selectClause :IndexedColumn[S, LocalScope, A, V], override val isDistinct :Boolean)
		extends SelectIndexedColumn[F, S, A, V](clause, selectClause) with SubselectColumn[F, V]
		   with SubselectColumnAs[F, IndexedMapping.Of[V]#Column, V]
	{
		override def distinct :SubselectColumnAs[F, IndexedMapping.Of[V]#Column, V] =
			if (isDistinct) this else new SubselectIndexedColumn(from, selectClause, true)

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ext :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectColumnAs[E, IndexedMapping.Of[V]#Column, V] =
			from match {
				case some :FromSome =>
					type Ext = SubselectOf[E] //RowProduct { type Implicit = G }
					implicit val extension = ext.asInstanceOf[some.Implicit ExtendedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val substitute = SQLScribe.shiftBack[S, Ext](from, stretched, ext.length, some.size)
					val shift = selectClause.alias @: substitute(selectClause.column) :IndexedColumn[Ext, LocalScope, A, V]
					new SubselectIndexedColumn[E, Ext, A, V](stretched, shift, isDistinct)

				case empty :Dual =>
					val castHeader = selectClause.asInstanceOf[IndexedColumn[Dual, LocalScope, A, V]]
					new SubselectIndexedColumn[E, Dual, A, V](empty, castHeader, isDistinct)
			}
	}






	trait TopSelectColumnMappingMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def topSelectMapping[H[O] <: ColumnMapping[V, O], V](e :TopSelectColumnAs[H, V]) :Y[GlobalScope, Rows[V]]
	}

	type MatchTopSelectColumnMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
		TopSelectColumnMappingMatcher[F, Y]

	type CaseTopSelectColumnMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
		TopSelectColumnMappingMatcher[F, Y]



	trait TopSelectMappingMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends TopSelectColumnMappingMatcher[F, Y]
	{
		def topSelectMapping[M[O] <: MappingAt[O]](e :TopSelectAs[M]) :Y[GlobalScope, Rows[M[()]#Subject]]
	}

	trait MatchTopSelectMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends TopSelectMappingMatcher[F, Y]
	{
		override def topSelectMapping[H[O] <: ColumnMapping[V, O], V]
		                             (e :TopSelectColumnAs[H, V]) :Y[GlobalScope, Rows[V]] =
			{ val res = topSelectMapping(e :TopSelectAs[H]); res }
	}

	type CaseTopSelectMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = MatchTopSelectMapping[F, Y]



	trait SubselectColumnMappingMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def subselectMapping[H[O] <: ColumnMapping[V, O], V](e :SubselectColumnAs[F, H, V]) :Y[GlobalScope, Rows[V]]
	}

	type MatchSubselectColumnMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
		SubselectColumnMappingMatcher[F, Y]

	type CaseSubselectColumnMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
		SubselectColumnMappingMatcher[F, Y]



	trait SubselectMappingMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends SubselectColumnMappingMatcher[F, Y]
	{
		def subselectMapping[M[O] <: MappingAt[O]](e :SubselectAs[F, M]) :Y[GlobalScope, Rows[M[()]#Subject]]
	}

	trait MatchSubselectMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends SubselectMappingMatcher[F, Y]
	{
		override def subselectMapping[H[O] <: ColumnMapping[V, O], V]
		                             (e :SubselectColumnAs[F, H, V]) :Y[GlobalScope, Rows[V]] =
			{ val res = subselectMapping(e :SubselectAs[F, H]); res }
	}

	type CaseSubselectMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = MatchSubselectMapping[F, Y]



	trait SelectColumnMappingMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends TopSelectColumnMappingMatcher[F, Y] with SubselectColumnMappingMatcher[F, Y]
	{
		def selectMapping[H[O] <: ColumnMapping[V, O], V](e :SelectColumnAs[F, H, V]) :Y[GlobalScope, Rows[V]]
	}

	type MatchSelectColumnMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
		SelectColumnMappingMatcher[F, Y]

	trait CaseSelectColumnMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends SelectColumnMappingMatcher[F, Y]
	{
		override def topSelectMapping[H[O] <: ColumnMapping[V, O], V]
		                              (e :TopSelectColumnAs[H, V]) :Y[GlobalScope, Rows[V]] =
			selectMapping(e)

		override def subselectMapping[H[O] <: ColumnMapping[V, O], V]
		                             (e :SubselectColumnAs[F, H, V]) :Y[GlobalScope, Rows[V]] =
			selectMapping(e)
	}



	trait SelectMappingMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends SelectColumnMappingMatcher[F, Y] with TopSelectMappingMatcher[F, Y] with SubselectMappingMatcher[F, Y]
	{
		def selectMapping[H[O] <: MappingAt[O]](e :SelectAs[F, H]) :Y[GlobalScope, Rows[H[()]#Subject]]
	}

	trait MatchSelectMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends SelectMappingMatcher[F, Y] with CaseTopSelectMapping[F, Y] with CaseSubselectMapping[F, Y]
	{
		override def selectMapping[H[O] <: ColumnMapping[V, O], V](e :SelectColumnAs[F, H, V]) :Y[GlobalScope, Rows[V]] =
			{ val res = selectMapping(e :SelectAs[F, H]); res }
	}

	trait CaseSelectMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MatchSelectMapping[F, Y]
	{
		override def subselectMapping[M[O] <: MappingAt[O]](e :SubselectAs[F, M]) :Y[GlobalScope, Rows[M[()]#Subject]] =
			selectMapping(e)

		override def topSelectMapping[M[O] <: MappingAt[O]](e :TopSelectAs[M]) :Y[GlobalScope, Rows[M[()]#Subject]] =
			selectMapping(e)
	}



	trait TopSelectColumnMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends TopSelectColumnMappingMatcher[F, Y]
	{
		def topSelect[V](e :TopSelectColumn[V]) :Y[GlobalScope, Rows[V]]
	}

	type MatchTopSelectColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = TopSelectColumnMatcher[F, Y]

	trait CaseTopSelectColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MatchTopSelectColumn[F, Y]
	{
		override def topSelectMapping[H[O] <: ColumnMapping[V, O], V]
		                             (e :TopSelectColumnAs[H, V]) :Y[GlobalScope, Rows[V]] =
			topSelect(e)
	}



	trait TopSelectMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends TopSelectMappingMatcher[F, Y] with TopSelectColumnMatcher[F, Y]
	{
		def topSelect[V](e :TopSelectSQL[V]) :Y[GlobalScope, Rows[V]]
	}

	trait MatchTopSelect[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends TopSelectMatcher[F, Y] with CaseTopSelectMapping[F, Y]
	{
		override def topSelect[V](e :TopSelectColumn[V]) :Y[GlobalScope, Rows[V]] =
			topSelect(e :TopSelectSQL[V])
	}

	trait CaseTopSelect[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchTopSelect[F, Y] {
		override def topSelectMapping[M[O] <: MappingAt[O]](e :TopSelectAs[M]) :Y[GlobalScope, Rows[M[()]#Subject]] =
			topSelect(e)
	}



	trait SubselectColumnMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends SubselectColumnMappingMatcher[F, Y]
	{
		def subselect[V](e :SubselectColumn[F, V]) :Y[GlobalScope, Rows[V]]
	}

	type MatchSubselectColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = SubselectColumnMatcher[F, Y]

	trait CaseSubselectColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MatchSubselectColumn[F, Y]
	{
		override def subselectMapping[H[O] <: ColumnMapping[V, O], V]
		                             (e :SubselectColumnAs[F, H, V]) :Y[GlobalScope, Rows[V]] =
			subselect(e)
	}


	trait SelectColumnMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends TopSelectColumnMatcher[F, Y] with SubselectColumnMatcher[F, Y] with SelectColumnMappingMatcher[F, Y]
	{
		def select[V](e :SelectColumn[F, V]) :Y[GlobalScope, Rows[V]]
	}

	trait MatchSelectColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends SelectColumnMatcher[F, Y] with CaseTopSelectColumn[F, Y] with CaseSubselectColumn[F, Y]
	{
		override def selectMapping[H[O] <: ColumnMapping[V, O], V](e :SelectColumnAs[F, H, V]) :Y[GlobalScope, Rows[V]] =
			select(e)
	}

	trait CaseSelectColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchSelectColumn[F, Y] {

		override def topSelect[V](e :TopSelectColumn[V]) :Y[GlobalScope, Rows[V]] =
			select(e :SelectColumn[F, V])

		override def subselect[V](e :SubselectColumn[F, V]) :Y[GlobalScope, Rows[V]] =
			select(e :SelectColumn[F, V])
	}



	trait SubselectMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends SubselectMappingMatcher[F, Y] with SubselectColumnMatcher[F, Y]
	{
		def subselect[V](e :SubselectSQL[F, V]) :Y[GlobalScope, Rows[V]]
	}

	trait MatchSubselect[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends SubselectMatcher[F, Y] with CaseSubselectMapping[F, Y]
	{
		override def subselect[V](e :SubselectColumn[F, V]) :Y[GlobalScope, Rows[V]] =
			subselect(e :SubselectSQL[F, V])
	}

	trait CaseSubselect[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchSubselect[F, Y] {
		override def subselectMapping[M[O] <: MappingAt[O]](e :SubselectAs[F, M]) :Y[GlobalScope, Rows[M[()]#Subject]] =
			subselect(e)
	}



	trait SelectMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends SelectColumnMatcher[F, Y]
		with TopSelectMatcher[F, Y] with SubselectMatcher[F, Y] with SelectMappingMatcher[F, Y]
	{
		def select[V](e :SelectSQL[F, V]) :Y[GlobalScope, Rows[V]]
	}

	trait MatchSelect[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends SelectMatcher[F, Y]
		with MatchSelectMapping[F, Y] with CaseTopSelect[F, Y] with CaseSubselect[F, Y]
	{
		override def select[V](e :SelectColumn[F, V]) :Y[GlobalScope, Rows[V]] =
			select(e :SelectSQL[F, V])

		override def selectMapping[H[O] <: MappingAt[O]](e :SelectAs[F, H]) :Y[GlobalScope, Rows[H[()]#Subject]] =
			select(e)
	}

	trait CaseSelect[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchSelect[F, Y] {
		def subselect[V](e: SubselectSQL[F, V]): Y[GlobalScope, Rows[V]] = select(e)

		def topSelect[V](e: TopSelectSQL[V]): Y[GlobalScope, Rows[V]] = select(e)
	}


}

