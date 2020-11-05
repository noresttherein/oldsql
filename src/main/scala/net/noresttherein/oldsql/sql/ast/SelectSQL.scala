package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.ChainApplication
import net.noresttherein.oldsql.schema.{BaseMapping, ColumnMapping, ColumnReadForm, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.sql.{ColumnSQL, Dual, ExpressionColumnMapping, ExpressionMapping, FromSome, GroupByClause, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnMatcher
import net.noresttherein.oldsql.sql.RowProduct.{ExtendedBy, GroundFrom, NonEmptyFrom, PartOf, SubselectOf}
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionMatcher, GlobalScope, LocalScope, LocalSQL}
import net.noresttherein.oldsql.sql.ast.ConditionSQL.ExistsSQL
import net.noresttherein.oldsql.sql.ast.MappingSQL.{ColumnComponentSQL, ComponentSQL, RelationSQL}
import net.noresttherein.oldsql.sql.ast.QuerySQL.{ColumnMappingQuery, ColumnQuery, MappingQuery, Rows}
import net.noresttherein.oldsql.sql.mechanics.SQLScribe

//here be implicits
import net.noresttherein.oldsql.slang._






/** Representation of an SQL select as an SQL expression used in the context of source `F`. If the first type argument
  * is the wildcard `RowProduct`, this will be a `TopSelectSQL` instance - a select independent of any external
  * tables or parameters, in which all formulas (''select'' clause, ''where'' clause, etc) can be evaluated
  * based on the values of the tables in its ''from'' clause. If `F` is not `RowProduct`, but contains tables, this is
  * a subselect nested inside a select for source `F` - in its header, ''from'' or ''where'' clause. The source for
  * this expression, given by the member type `From`, is always an extension of `F`. Subclasses should extend the trait
  * for one of the above cases: [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]] or
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]], instead of being derived directly
  * from this trait.
  *
  * @tparam F the source of data for the ''enclosing'' select - tables from the ''from'' clause and any unbound parameters.
  * @tparam V the mapped header type representing a single row.
  */
sealed trait SelectSQL[-F <: RowProduct, V] extends QuerySQL[F, V] {

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

	//caution: in group by queries this returns the elements of the group by clause, not the actual from clause
	def tables :Seq[RelationSQL.AnyIn[from.Generalized]] = from.tableStack

	def isSubselect :Boolean = from.isSubselect


//	def as[X <: FlatSchemaMapping[_, _, _, _], M <: FlatSchemaMapping[T, R, C, A], T, R <: Chain, C <: Chain, A]
//	      (mapping :X)(implicit typer :Conforms[X, M, FlatSchemaMapping[T, R, C, A]], tuple :V =:= R) :SelectAs[F, S, M, O] = ???
//
//	def as[FC <: Chain, FR <: Chain, X <: SchemaMapping[_, _, _, _], M <: SchemaMapping[T, R, C, A], T, R <: Chain, C <: Chain, A]
//	      (mapping :X)(implicit typer :Conforms[X, M, SchemaMapping[T, R, C, A]],
//	                   flat :SchemaFlattening[R, C, FR, FC], tuple :V =:= R) :SelectAs[F, S, M, O] = ???
//
	def isDistinct :Boolean

	def distinct :SelectSQL[F, V]


	override def map[X](f :V => X) :SelectSQL[F, X]

	override def map[Fun, C <: Chain, X](f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
			:SelectSQL[F, X] =
		map(applyFun(f))


	override def isGlobal = true
	override def asGlobal :Option[SelectSQL[F, V]] = Some(this)


	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SelectSQL[E, V]

	override def extend[U <: F, E <: RowProduct]
	                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :SelectSQL[E, V]


//	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//	                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
//		matcher.select(this)


	override def isAnchored = true
	override def anchor(from :F) :SelectSQL[F, V] = this


	protected override def reverseCollect[X](fun: PartialFunction[SQLExpression.*, X], acc: List[X]): List[X] = {
		//we ignore filters in the implicit portion as, if this is a subselect, they would be collected by the enclosing expression.
		val headerItems = reverseCollect(header)(fun, super.reverseCollect(fun, acc))
		reverseCollect(from.filter)(fun, from match {
			case GroupByClause(ungrouped) => reverseCollect(ungrouped.filter)(fun, headerItems)
			case _ => headerItems
		})
	}


	override def isomorphic(expression: SQLExpression.*): Boolean = expression match {
		case s :SelectSQL[_, _] =>
			(s eq this) || (s canEqual this) && (s.header isomorphic header) && (s.from == from)
		case _ => false
	}

	private[oldsql] override def equivalent(expression: SQLExpression.*): Boolean = expression match {
		case s :SelectSQL[_, _] =>
			(s eq this) || (s canEqual this) && (s.header equivalent header) && (s.from == from)
		case _ => false
	}



	override def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def equals(that :Any) :Boolean = that match {
		case s :SelectSQL[_, _] => (s eq this) || (s canEqual this) && s.header == header && s.from == from
		case _ => false
	}

	override def hashCode :Int = header.hashCode * 31 + from.hashCode


	override def toString :String =
		if (isDistinct) s"SELECT DISTINCT $header FROM $from"
		else  s"SELECT $header FROM $from"

}






object SelectSQL {
	//todo: parameterized selects
	//todo: mapping indexed headers

	def apply[F <: GroundFrom, M[A] <: BaseMapping[V, A], V](from :F, header :ComponentSQL[F, M]) :SelectMapping[F, M, V] =
		new SelectComponent[F, M, V](from, header, false)

	def apply[F <: GroundFrom, M[A] <: ColumnMapping[V, A], V]
	         (from :F, column :ColumnComponentSQL[F, M, V]) :SelectColumnMapping[F, M, V] =
		new SelectColumnMappingImpl[F, M, V](from, column, false)

	def apply[F <: GroundFrom, V](from :F, header :TupleSQL[F, LocalScope, V]) :TopSelectSQL[V] =
		new ArbitraryTopSelect[F, V](from, header.anchor(from), false)

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
		new SubselectColumnMappingImpl[F, S, M, V](from, column, false)

	def subselect[F <: NonEmptyFrom, S <: SubselectOf[F], V]
	             (from :S, header :TupleSQL[S, LocalScope, V]) :SubselectSQL[F, V] =
		new ArbitrarySubselect[F, S, V](from, header.anchor(from), false)

	def subselect[F <: NonEmptyFrom, S <: SubselectOf[F], X, Y]
	             (from :S, header :ConversionSQL[S, LocalScope, X, Y]) :SubselectSQL[F, Y] =
		new ArbitrarySubselect[F, S, Y](from, header.anchor(from), false)

	def subselect[F <: NonEmptyFrom, S <: SubselectOf[F], V]
	             (from :S, header :ColumnSQL[S, LocalScope, V]) :SubselectColumn[F, V] =
		new ArbitrarySubselectColumn[F, S, V](from, header.anchor(from), false)



	type * = SelectSQL[_ <: RowProduct, _]



	trait SelectColumn[-F <: RowProduct, V] extends ColumnQuery[F, V] with SelectSQL[F, V] {
		override def readForm :ColumnReadForm[Rows[V]] = header.readForm.map(Rows(_))

		override val header :ColumnSQL[From, LocalScope, V]

		override def distinct :SelectColumn[F, V]

		override def single :ColumnSQL[F, GlobalScope, V] = to[V]

		override def map[X](f :V => X) :SelectColumn[F, X]

		override def map[Fun, C <: Chain, X](f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
				:SelectColumn[F, X] =
			map(applyFun(f))


		override def asGlobal :Option[SelectColumn[F, V]] = Some(this)

		override def anchor(from :F) :SelectColumn[F, V] = this


		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :PartOf[U, E]) :SelectColumn[E, V]

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :SelectColumn[E, V]

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.select(this)
	}



	/** Base trait for SQL select expressions whose header depends solely on the explicit FROM clause of the select,
	  * i.e. it is not dependent on any outside rows. Such an expression is a valid independent select statement
	  * in opposition to subselect expressions.
	  */
	trait TopSelectSQL[V] extends SelectSQL[RowProduct, V] {
		override type From <: GroundFrom

		override def distinct :TopSelectSQL[V]

		override def map[X](f :V => X) :TopSelectSQL[X] =
			new ArbitraryTopSelect[From, X](from, header.map(f), isDistinct)

		override def map[Fun, C <: Chain, X]
		                (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C) :TopSelectSQL[X] =
			map(applyFun(f))


		override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit ext :U PartOf E) :TopSelectSQL[V] =
			this

		override def extend[U <: RowProduct, S <: RowProduct]
		             (base :S)(implicit ev :U ExtendedBy S, global :GlobalScope <:< GlobalScope) :TopSelectSQL[V] =
			this

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[RowProduct, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.topSelect(this)

	}



	trait TopSelectColumn[V] extends TopSelectSQL[V] with SelectColumn[RowProduct, V] {
		override def distinct :TopSelectColumn[V]

		override def map[X](f :V => X) :TopSelectColumn[X] =
			new ArbitraryTopSelectColumn[From, X](from, header.map(f), isDistinct)

		override def map[Fun, C <: Chain, X]
		                (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
			:TopSelectColumn[X] =
			map(applyFun(f))


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
			new ArbitrarySubselect[F, From, X](from, header.map(f), isDistinct)

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
			new ArbitrarySubselectColumn[F, From, X](from, header.map(f), isDistinct)

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



	/** A `SelectSQL` interface exposing the mapping type `H` used as the header. */
	trait SelectAs[-F <: RowProduct, H[A] <: MappingAt[A]]
		extends MappingQuery[F, H] with SelectSQL[F, H[()]#Subject]
	{
		override def distinct :SelectAs[F, H]

		override def asGlobal :Option[SelectAs[F, H]] = Some(this)

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[H[Any]#Subject]] =
//			matcher.selectMapping(this)
	}


	trait SelectColumnAs[-F <: RowProduct, H[A] <: ColumnMapping[V, A], V]
		extends ColumnMappingQuery[F, H, V] with SelectAs[F, H] with SelectColumn[F, V]
	{
		override def distinct :SelectColumnAs[F, H, V]

		override def asGlobal :Option[SelectColumnAs[F, H, V]] = Some(this)

		override def canEqual(that :Any) :Boolean =
			that.isInstanceOf[SelectColumnAs[_, M, _] forSome { type M[O] <: ColumnMapping[_, O] }]

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.selectMapping(this)
	}



	//consider: renaming all FreeSelect classes to OuterSelect
	trait TopSelectAs[H[A] <: MappingAt[A]] extends TopSelectSQL[H[()]#Subject] with SelectAs[RowProduct, H] {
		override def distinct :TopSelectAs[H]
		override def asGlobal :Option[TopSelectAs[H]] = Some(this)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[RowProduct, Y]) :Y[GlobalScope, Rows[H[()]#Subject]] =
			matcher.topSelectMapping(this)
	}

	trait TopSelectColumnAs[H[A] <: ColumnMapping[V, A], V]
		extends TopSelectColumn[V] with TopSelectAs[H] with SelectColumnAs[RowProduct, H, V]
	{
		override def distinct :TopSelectColumnAs[H, V]
		override def asGlobal :Option[TopSelectColumnAs[H, V]] = Some(this)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ColumnMatcher[RowProduct, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.topSelectMapping(this)
	}


	trait SubselectAs[-F <: RowProduct, H[A] <: MappingAt[A]]
		extends SelectAs[F, H] with SubselectSQL[F, H[()]#Subject]
	{
		override def distinct :SubselectAs[F, H]
		override def asGlobal :Option[SubselectAs[F, H]] = Some(this)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[H[()]#Subject]] =
			matcher.subselectMapping(this)
	}

	trait SubselectColumnAs[-F <: RowProduct, H[A] <: ColumnMapping[V, A], V]
		extends SubselectColumn[F, V] with SubselectAs[F, H] with SelectColumnAs[F, H, V]
	{
		override def distinct :SubselectColumnAs[F, H, V]
		override def asGlobal :Option[SubselectColumnAs[F, H, V]] = Some(this)

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.subselectMapping(this)
	}




	trait SelectMapping[F <: GroundFrom, H[A] <: BaseMapping[V, A], V] extends TopSelectAs[H] {
		override type From = F
		override def distinct :SelectMapping[F, H, V]
	}

	trait SelectColumnMapping[F <: GroundFrom, H[A] <: ColumnMapping[V, A], V]
		extends TopSelectColumnAs[H, V] with SelectMapping[F, H, V]
	{
		override def distinct :SelectColumnMapping[F, H, V]
		override def asGlobal :Option[SelectColumnMapping[F, H, V]] = Some(this)
	}


	trait SubselectMapping[-F <: RowProduct, S <: SubselectOf[F], H[A] <: BaseMapping[V, A], V]
		extends SubselectAs[F, H]
	{
		override type From = S
		override def distinct :SubselectMapping[F, S, H, V]
		override def asGlobal :Option[SubselectMapping[F, S, H, V]] = Some(this)
	}

	trait SubselectColumnMapping[-F <: RowProduct, S <: SubselectOf[F], H[A] <: ColumnMapping[V, A], V]
		extends SubselectColumnAs[F, H, V] with SubselectMapping[F, S, H, V]
	{
		override def distinct :SubselectColumnMapping[F, S, H, V]
		override def asGlobal :Option[SubselectColumnMapping[F, S, H, V]] = Some(this)
	}






	private abstract class SelectComponentSQL[-F <: RowProduct, S <: SubselectOf[F],
	                                          H[A] <: BaseMapping[V, A], V]
	                                         (override val from :S, override val header :ComponentSQL[S, H])
		extends SelectSQL[F, V] with SelectAs[F, H]
	{
		override type From = S

		protected override def component[O] :HeaderMapping[O] = header.mapping.withOrigin[O]

		override val headerColumns: Seq[SelectedColumn[_]] =
			header.mapping.selectable.toSeq.map(include(_))

		private def include[X](column :ColumnMapping[X, header.Origin]) :SelectedColumn[X] = new SelectedColumn[X] {
			override val name :String = column.name
			override val expression  = header \ column
		}

	}



	private class SelectComponent[F <: GroundFrom, H[A] <: BaseMapping[V, A], V]
	                             (override val from :F, override val header :ComponentSQL[F, H],
	                              override val isDistinct :Boolean)
		extends SelectComponentSQL[RowProduct, F, H, V](from, header) with SelectMapping[F, H, V]
	{

		override def distinct :SelectMapping[F, H, V] =
			if (isDistinct) this else new SelectComponent(from, header, true)
	}



	private class SubselectComponent[-F <: RowProduct, S <: SubselectOf[F], H[A] <: BaseMapping[V, A], V]
	                                (subselect :S, component :ComponentSQL[S, H], override val isDistinct :Boolean)
		extends SelectComponentSQL[F, S, H, V](subselect, component)
		   with SubselectMapping[F, S, H, V]
	{
		override def distinct :SubselectMapping[F, S, H, V] =
			if (isDistinct) this else new SubselectComponent(from, header, true)

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectSQL[E, V] =
			from match { //would be safer to refactor this out as a RowProduct method
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
					new SubselectComponent[E, Ext, H, V](stretched, component, isDistinct)

				case empty :Dual =>
					val adaptedHeader = header.asInstanceOf[ComponentSQL[Dual, H]]
					new SubselectComponent[E, Dual, H, V](empty, adaptedHeader, isDistinct)

				case _ =>
					throw new UnsupportedOperationException(s"Cannot rebase clause $from :${from.unqualifiedClassName} onto $base.")
			}

	}



	private class SelectColumnMappingImpl[F <: GroundFrom, H[A] <: ColumnMapping[V, A], V]
	              (clause :F, override val header :ColumnComponentSQL[F, H, V], override val isDistinct :Boolean)
		extends SelectComponent[F, H, V](clause, header, isDistinct) with SelectColumnMapping[F, H, V]
	{
		override def distinct :SelectColumnMapping[F, H, V] =
			if (isDistinct) this else new SelectColumnMappingImpl(from, header, true)
	}



	private class SubselectColumnMappingImpl
	              [-F <: RowProduct, S <: SubselectOf[F], H[A] <: ColumnMapping[V, A], V]
	              (override val from :S, override val header :ColumnComponentSQL[S, H, V],
	               override val isDistinct :Boolean)
		extends SubselectComponent[F, S, H, V](from, header, isDistinct)
		   with SubselectColumnMapping[F, S, H, V]
	{
		override def distinct :SubselectColumnMapping[F, S, H, V] =
			if (isDistinct) this else new SubselectColumnMappingImpl(from, header, true)

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectColumn[E, V] =
			from match { //would be safer to refactor this out as a RowProduct method
				case some :NonEmptyFrom => //todo: refactor this together with SubselectComponent; if S <: NonEmptyFrom, than the casting could conceivably by omitted
					type Ext = SubselectOf[E] //pretend this is the actual type S after rebasing to the extension clause G
					implicit val extension = ev.asInstanceOf[some.Implicit ExtendedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val subselectTables = stretched.fullSize - base.fullSize
					val table = header.origin
					val replacement =
						if (table.shift < subselectTables)
							table.asInstanceOf[RelationSQL[Ext, MappingOf[Any]#TypedProjection, Any, Ext]]
						else
                            stretched.fullTableStack(table.shift + ev.length)
                                     .asInstanceOf[RelationSQL[Ext, MappingOf[Any]#TypedProjection, Any, Ext]]
					val component = replacement \ header.mapping.withOrigin[Ext]
					new SubselectColumnMappingImpl[E, Ext, H, V](stretched, component, isDistinct)

				case empty :Dual =>
					val adaptedHeader = header.asInstanceOf[ColumnComponentSQL[Dual, H, V]]
					new SubselectColumnMappingImpl[E, Dual, H, V](empty, adaptedHeader, isDistinct)

				case _ =>
					throw new UnsupportedOperationException(
						s"Cannot rebase clause $from :${from.unqualifiedClassName} onto $base."
					)
			}
	}





	/** A select expression based on the given row source and selecting an arbitrary expression `header` in its ''select''
	  * clause. This header will be translated by recursively flat mapping the header expression to obtain a flat sequence
	  * of columns. In particular, any sequences/tuples are inlined, and any `TypedComponentSQL`s referring to components
	  * of tables or whole table rows themselves are replaced with their columns. Column list declared by this mapping
	  * is thus created by recursively applying the following rules to the header expression:
	  *
	  *     1. If the expression is a `ColumnSQL`, it is taken as a basis for a `SelectedColumn`;
	  *     1. If the expression is a component mapping, create a column for every export column of the declared mapping;
	  *     1. If the expression is a tuple expression such as a tuple, recursively flatMap over it by applying these rules;
	  *     1. If the expression is a conversion, proceed with the base expression;
	  *     1. Other types of expressions encountered anywhere inside the `header` result in throwing an exception.
	  *
	  * Note that the above column list should be considered in the context of this instance as a mapping and represents
	  * all columns that potentially might be a part of the select clause. Existence of non-selectable and optional
	  * columns means that resulting select query may not contain all of the above. This distinction is also present
	  * when using this instance to map results of the created select statement; as individual columns
	  * in the select header may be any formulas, the source of values for evaluating the header expression are not values
	  * of the tables of the underlying source, but values for the whole column formulas. For example, a header expression
	  * in the form of `(current_date - birth_date, address, (first_name, family_name)) from users` could translate
	  * into a select expression declaring columns: `('col1', street, zip, city, country, first_name, family_name)`.
	  * Such columns would be available for any formulas using this mapping in their RowProduct and are considered
	  * 'available header columns'. However, when using this instance as a mapping for assembling the header value,
	  * we don't have values for individual columns of the users table in the above example, but values for the columns
	  * declared by this mapping. This means that we need a bit of creative term rewriting to map the scala value
	  * as it would be evaluated by the original header expression. In particular, in the above example, the address object
	  * would be reassembled based on the values of individual columns included in the final select.
	  */
	private abstract class ArbitrarySelect[-F <: RowProduct, S <: SubselectOf[F], V] protected
	                       (override val from :S, protected val mapping :ExpressionMapping[S, LocalScope, V, Any])
		extends SelectSQL[F, V]
	{
		def this(from :S, header :SQLExpression[S, LocalScope, V]) =
			this(from, ExpressionMapping[S, LocalScope, V, Any](header))

		override type From = S

		override val header = mapping.expr

		/** A column in the header of owning select.
		  * @param column the `ColumnMapping` implementation based on a `ColumnSQL` expression
		  *               providing the value for the column.
		  */
		protected class HeaderColumn[T](column :ExpressionColumnMapping[S, LocalScope, T, _])
			extends SelectedColumn[T]
		{
			override def expression :ColumnSQL[S, LocalScope, T] = column.expr
			override def name :String = column.name
		}

		override val headerColumns :Seq[SelectedColumn[_]] = mapping.columns.map { col => new HeaderColumn(col) }

	}



	private class ArbitraryTopSelect[S <: GroundFrom, V]
	              (override val from :S, override val header :LocalSQL[S, V], override val isDistinct :Boolean)
		extends ArbitrarySelect[RowProduct, S, V](from, header) with TopSelectSQL[V]
	{
		override type HeaderMapping[O] = ExpressionMapping[S, LocalScope, V, O]

		protected override def component[O] :HeaderMapping[O] = mapping.withOrigin[O]

		override def distinct :TopSelectSQL[V] =
			if (isDistinct) this else new ArbitraryTopSelect(from, header, true)
	}



	private class ArbitrarySubselect[-F <: RowProduct, S <: SubselectOf[F], V]
	                                (subclause :S, select :LocalSQL[S, V], override val isDistinct :Boolean)
		extends ArbitrarySelect[F, S, V](subclause, select) with SubselectSQL[F, V]
	{
		override type HeaderMapping[O] = ExpressionMapping[S, LocalScope, V, O]

		protected override def component[O] :HeaderMapping[O] = mapping.withOrigin[O]

		override def distinct :SubselectSQL[F, V] =
			if (isDistinct) this else new ArbitrarySubselect(from, header, true)

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ext :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectSQL[E, V] =
			from match { //would be safer to refactor this out as a RowProduct method
				case some :NonEmptyFrom =>
					type Ext = SubselectOf[E] //RowProduct { type Implicit = G }
					implicit val extension = ext.asInstanceOf[some.Implicit ExtendedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val substitute = SQLScribe.shiftBack[S, Ext](from, stretched, ext.length, some.size)
					new ArbitrarySubselect[E, Ext, V](stretched, substitute(header), isDistinct)

				case empty :Dual =>
					new ArbitrarySubselect[E, Dual, V](empty, header.asInstanceOf[LocalSQL[Dual, V]], isDistinct)

				case _ =>
					throw new UnsupportedOperationException(
						s"Cannot rebase clause $from :${from.unqualifiedClassName} onto $base."
					)
			}

	}






	private abstract class ArbitrarySelectColumn[-F <: RowProduct, S <: SubselectOf[F], V]
	                       (override val from :S, override val mapping :ExpressionColumnMapping[S, LocalScope, V, Any])
		extends ArbitrarySelect[F, S, V](from, mapping) with SelectColumn[F, V]
	{
		def this(from :S, expression :ColumnSQL[S, LocalScope, V]) =
			this(from, ExpressionColumnMapping[S, LocalScope, V, Any](expression))

		override type HeaderMapping[O] = ExpressionColumnMapping[S, LocalScope, V, O]

		protected override def component[O] :HeaderMapping[O] = mapping.withOrigin[O]

		override val header = mapping.expr
	}



	private class ArbitraryTopSelectColumn[S <: GroundFrom, V]
	              (override val from :S, override val header :ColumnSQL[S, LocalScope, V], override val isDistinct :Boolean)
		extends ArbitrarySelectColumn[RowProduct, S, V](from, header) with TopSelectColumn[V]
	{
		override def distinct :TopSelectColumn[V] =
			if (isDistinct) this else new ArbitraryTopSelectColumn(from, header, true)
	}



	private class ArbitrarySubselectColumn[-F <: RowProduct, S <: SubselectOf[F], V]
	              (clause :S, override val header :ColumnSQL[S, LocalScope, V], override val isDistinct :Boolean)
		extends ArbitrarySelectColumn[F, S, V](clause, header) with SubselectColumn[F, V]
	{
		override def distinct :SubselectColumn[F, V] =
			if (isDistinct) this else new ArbitrarySubselectColumn(from, header, true)

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ext :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:SubselectColumn[E, V] =
			from match {
				case some :FromSome =>
					type Ext = SubselectOf[E] //RowProduct { type Implicit = G }
					implicit val extension = ext.asInstanceOf[some.Implicit ExtendedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val substitute = SQLScribe.shiftBack[S, Ext](from, stretched, ext.length, some.size)
					new ArbitrarySubselectColumn[E, Ext, V](stretched, substitute(header), isDistinct)

				case empty :Dual =>
					val castHeader = header.asInstanceOf[ColumnSQL[Dual, LocalScope, V]]
					new ArbitrarySubselectColumn[E, Dual, V](empty, castHeader, isDistinct)
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

