package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{Mapping, RowSource, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAlias, MappingFrom, TypedMapping}
import net.noresttherein.oldsql.schema.RowSource.AnyRowSource
import net.noresttherein.oldsql.schema.support.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.sql.FromClause.{As, JoinedTables, SubselectFrom}
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.slang.SaferCasts._
import net.noresttherein.oldsql.sql.InnerJoin.AnyInnerJoin
import net.noresttherein.oldsql.sql.ProperJoin.AnyProperJoin
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation.{AnyRelationIn, LastRelation}
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple






/** Common upper type for `Join` subclasses which accept arbitrary `Mapping` subtypes as joined mappings.
  * This represents all joins of real relations, encompassing all proper joins and subselect 'joins', but
  * not the synthetic `WithParam` class which restricts itself to the `FromParam` class.
  */
trait Join[+L <: FromClause, R[O] <: MappingFrom[O]] extends With[L, R] { join =>


	override def copy[F <: FromClause](left :F, filter :BooleanFormula[F With R]) :F LikeJoin R =
		copy(left, right, filter)

	def copy[F <: FromClause, T[O] <: MappingFrom[O]]
	        (left :F, right :RowSource[T], filter: BooleanFormula[F With T]) :F LikeJoin T

	def copy[T[O] <: MappingFrom[O]](right :LastRelation[T, _], filter :BooleanFormula[L With T]) :JoinLeft[T]


	type LikeJoin[+F <: FromClause, T[O] <: MappingFrom[O]] <: (F Join T) {
		type LikeJoin[+S <: FromClause, M[O] <: MappingFrom[O]] = join.LikeJoin[S, M]
	}

	type JoinRight[+F <: FromClause] = F LikeJoin R
	type JoinLeft[T[O] <: MappingFrom[O]] <: L LikeJoin T
	override type This >: this.type <: L Join R



	/** Specify an alias for the last table in the join. This is not necessary and may be overriden in case of conflicts,
	  * but can be used as the default value and/or help with debugging.
	  * @param alias the alias for the table as in 'from users as u'
	  * @return a new join isomorphic with this instance, but with a new last table (not equal to this.last).
	  */
	def as[A <: Label](alias :A) :L LikeJoin (R As A)#T = {
		val aliased = (alias @: table.mapping.asInstanceOf[TypedMapping[Any, Any]]).asInstanceOf[A @: R[Any]]
		val source = FromClause.AliasedSource[R, A](table.source, alias)
		copy(LastRelation[(R As A)#T, Any](source, aliased, table.index), ???) //todo
	}


}






object Join {

	/** Create a cross join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some table proxy.
	  */
	def apply[L[O] <: MappingFrom[O], R[O] <: MappingFrom[O]]
	         (left :RowSource[L], right :RowSource[R]) :From[L] InnerJoin R =
		InnerJoin(left, right)

	def apply[L <: FromClause, R[O] <: MappingFrom[O]](left :L, right :RowSource[R]) :L InnerJoin R =
		InnerJoin(left, right)



	def unapply[L <: FromClause, R[O] <: MappingFrom[O]](join :L Join R) :Option[(L, RowSource[R])] =
		Some(join.left -> join.right)

	def unapply(from :FromClause) :Option[(FromClause, AnyRowSource)]  =
		from match {
			case join :AnyJoin => Some((join.left :FromClause, join.right))
			case _ => None
		}



	type AnyJoin = Join[_ <: FromClause, T] forSome { type T[O] <: MappingFrom[O] }

}






/** Base class for join implementations representing real SQL joins between relations, rather than a synthetic
  * `Subselect` representing a subselect of another select expression.
  */
sealed trait ProperJoin[+L <: FromClause, R[O] <: MappingFrom[O]] extends Join[L, R] { join =>


	override type LikeJoin[+F <: FromClause, T[O] <: MappingFrom[O]] <: (F ProperJoin T) {
		type LikeJoin[+S <: FromClause, M[O] <: MappingFrom[O]] = join.LikeJoin[S, M]
	}

	override type This >: this.type <: L ProperJoin R

	override type Outer = left.Outer

	override def outer :Outer = left.outer



	override type SubselectRow = left.SubselectRow ~ table.Subject //R[table.Origin]#Subject

	override def subselectRow :ChainTuple[this.type, SubselectRow] = //todo:
		left.subselectRow.asInstanceOf[ChainTuple[this.type, left.SubselectRow]] ~ table.upcast

	override def subselectTableStack :LazyList[AnyRelationIn[this.type]] =
		table #:: left.subselectTableStack.asInstanceOf[LazyList[AnyRelationIn[this.type]]]



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[AnyProperJoin]

}






object ProperJoin {

	/** Create a cross join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some table proxy.
	  */
	def apply[L[O] <: MappingFrom[O], R[O] <: MappingFrom[O]]
	         (left :RowSource[L], right :RowSource[R]) :From[L] InnerJoin R =
		InnerJoin(left, right)

	def apply[L <: FromClause, R[O] <: MappingFrom[O]](left :L, right :RowSource[R]) :L InnerJoin R =
		InnerJoin(left, right)



	def unapply[L <: FromClause, R[O] <: MappingFrom[O]](join :L ProperJoin R) :Option[(L, RowSource[R])] =
		Some(join.left -> join.right)

	def unapply(from :FromClause) :Option[(FromClause, AnyRowSource)]  =
		from match {
			case join :AnyProperJoin => Some((join.left :FromClause, join.right))
			case _ => None
		}



	type AnyProperJoin = ProperJoin[_ <: FromClause, M] forSome { type M[O] <: MappingFrom[O] }

}






/** A classic join of two SQL relations. Represents a cross join of all relations from the left side with the relation
  * on the right, narrowed down by the conjunction of this join's condition with the combined filter expression
  * of the left side.
  */
sealed trait InnerJoin[+L <: FromClause, R[O] <: MappingFrom[O]] extends ProperJoin[L, R] { join =>

	override type LikeJoin[+F <: FromClause, T[O] <: MappingFrom[O]] = F InnerJoin T
	override type This >: this.type <: L InnerJoin R

	override def copy[F <: FromClause](left :F, filter :BooleanFormula[F With R]) :F InnerJoin R =
		(left :FromClause) match {
			case Dual() => From(table.source).asInstanceOf[F InnerJoin R]
			case _ => InnerJoin(left, table, filter)
		}

	override def copy[F <: FromClause, T[O] <: MappingFrom[O]]
	                 (left :F, right :RowSource[T], filter :BooleanFormula[F With T]) :F InnerJoin T =
		left match {
			case Dual() => From(right, filter.asInstanceOf[BooleanFormula[Dual With T]]).asInstanceOf[F InnerJoin T]
			case _ => InnerJoin(left, LastRelation(right, left.size), filter)
		}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[AnyInnerJoin]

	protected override def joinType :String = "join"

}






object InnerJoin {

	/** Create a cross join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some table proxy.
	  */
	def apply[L[O] <: MappingFrom[O], R[O] <: MappingFrom[O]]
	         (left :RowSource[L], right :RowSource[R]) :From[L] InnerJoin R =
		new CrossJoin(From[L](left), LastRelation[R, Any](right, 1))

	def apply[L <: FromClause, R[O] <: MappingFrom[O]](left :L, right :RowSource[R]) :L InnerJoin R =
		new CrossJoin(left, LastRelation[R, Any](right, 1))

	private[sql] def apply[L <: FromClause, R[O] <: MappingFrom[O]]
	                      (left :L, right :RowSource[R], filter :BooleanFormula[L With R]) :L InnerJoin R =
		new CrossJoin(left, LastRelation[R, Any](right, 1), filter)

	private[sql] def apply[L <: FromClause, R[O] <: MappingFrom[O]]
	                      (left :L, right :LastRelation[R, _], filter :BooleanFormula[L With R] = True()) :L InnerJoin R =
		new CrossJoin(left, right, filter)



	def unapply[L <: FromClause, R[O] <: MappingFrom[O]](join :L With R) :Option[(L, RowSource[R])] = join match {
		case _ :CrossJoin[_, _] => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, AnyRowSource)] =
		from match {
			case join :AnyCrossJoin => Some(join.left -> join.right)
			case _ => None
		}



	type AnyInnerJoin = InnerJoin[_ <: FromClause, M] forSome { type M[O] <: MappingFrom[O] }
	private type AnyCrossJoin = CrossJoin[_ <: FromClause, M] forSome { type M[O] <: MappingFrom[O] }



	private class CrossJoin[L <: FromClause, R[O] <: MappingFrom[O]]
	                       (source :L, t :JoinedRelation[FromClause With R, R, _], pred :BooleanFormula[L With R] = True())
		extends With[L, R](source, t, pred) with InnerJoin[L, R]
	{
		override def copy(filter :BooleanFormula[L With R]) :L CrossJoin R = new CrossJoin(left, table, filter)

		override def copy[T[O] <: MappingFrom[O]](right :LastRelation[T, _], filter :BooleanFormula[L With T]) :L CrossJoin T =
			new CrossJoin(left, right, filter)


		override type JoinLeft[T[O] <: MappingFrom[O]] = L CrossJoin T
		override type This = L CrossJoin R

	}

}






sealed trait LeftJoin[+L <: FromClause, R[O] <: MappingFrom[O]] extends ProperJoin[L, R] {

	override type LikeJoin[+F <: FromClause, T[O] <: MappingFrom[O]] = F LeftJoin T
	override type This >: this.type <: L LeftJoin R

	protected override def joinType = "left join"

}






object LeftJoin {

	/** Create a left outer join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some table proxy.
	  */
	def apply[L[O] <: MappingFrom[O], R[O] <: MappingFrom[O]]
	         (left :RowSource[L], right :RowSource[R]) :From[L] LeftJoin R =
		new LeftOuterJoin(From[L](left), LastRelation[R, Any](right, 1))

	def apply[L <: FromClause, R[O] <: MappingFrom[O]](left :L, right :RowSource[R]) :L LeftJoin R =
		new LeftOuterJoin(left, LastRelation(right, left.size))

	private[sql] def apply[L <: FromClause, R[A] <: MappingFrom[A], O](left :L, right :LastRelation[R, O]) :L LeftJoin R =
		new LeftOuterJoin(left, right)




	def unapply[L <: FromClause, R[O] <: MappingFrom[O]](join :L With R) :Option[(L, RowSource[R])] = join match {
		case _ :LeftJoin[_, _] => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, AnyRowSource)] =
		from match {
			case join :AnyLeftJoin => Some(join.left -> join.right)
			case _ => None
		}



	type AnyLeftJoin = LeftJoin[_ <: FromClause, M] forSome { type M[O] <: MappingFrom[O] }



	private class LeftOuterJoin[L <: FromClause, R[O] <: MappingFrom[O]]
	                           (l :L, r :JoinedRelation[FromClause With R, R, _], cond :BooleanFormula[L With R] = True())
		extends With[L, R](l, r, cond) with LeftJoin[L, R]
	{
		override protected def copy(filter :BooleanFormula[L With R]) :L LeftOuterJoin R =
			new LeftOuterJoin(left, table, filter)

		override def copy[T[O] <: MappingFrom[O]](right :LastRelation[T, _], filter :BooleanFormula[L With T]) :L LeftOuterJoin T =
			new LeftOuterJoin(left, right, filter)

		override def copy[F <: FromClause](left :F, filter :BooleanFormula[F With R]) :F LeftJoin R =
			new LeftOuterJoin(left, LastRelation(table.source, left.size), filter)

		override def copy[F <: FromClause, T[O] <: MappingFrom[O]]
		                 (left :F, right :RowSource[T], filter :BooleanFormula[F With T]) :F LeftJoin T =
			new LeftOuterJoin(left, LastRelation(right, left.size), filter)


		override type JoinLeft[T[O] <: MappingFrom[O]] = L LeftOuterJoin T
		override type This = L LeftOuterJoin R
	}


}






sealed trait RightJoin[+L <: FromClause, R[O] <: MappingFrom[O]] extends ProperJoin[L, R] {

	override type LikeJoin[+F <: FromClause, T[O] <: MappingFrom[O]] = F RightJoin T
	override type This >: this.type <: L RightJoin R

	protected override def joinType = "right join"

}






object RightJoin {

	/** Create a right outer join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some table proxy.
	  */
	def apply[L[O] <: MappingFrom[O], R[O] <: MappingFrom[O]]
	         (left :RowSource[L], right :RowSource[R]) :From[L] RightJoin R =
		new RightOuterJoin(From(left), LastRelation(right, 1))

	def apply[L <: FromClause, R[O] <: MappingFrom[O]](left :L, right :RowSource[R]) :L RightJoin R =
		new RightOuterJoin(left, LastRelation(right, left.size))

	private[sql] def apply[L <: FromClause, R[O] <: MappingFrom[O]](left :L, right :LastRelation[R, _]) :L RightJoin R =
		new RightOuterJoin(left, right)



	def unapply[L <: FromClause, R[O] <: MappingFrom[O]](join :L With R) :Option[(L, RowSource[R])] = join match {
		case _ :RightJoin[_, _] => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, AnyRowSource)] =
		from match {
			case join :AnyRightJoin => Some(join.left -> join.right)
			case _ => None
		}



	type AnyRightJoin = RightJoin[_ <: FromClause, M] forSome { type M[O] <: MappingFrom[O] }



	private class RightOuterJoin[L <: FromClause, R[O] <: MappingFrom[O]]
	                            (l :L, r :JoinedRelation[FromClause With R, R, _], cond :BooleanFormula[L With R] = True())
		extends With[L, R](l, r, cond) with RightJoin[L, R]
	{
		protected override def copy(filter :BooleanFormula[L With R]) :L RightOuterJoin R =
			new RightOuterJoin(left, table, filter)

		override def copy[T[O] <: MappingFrom[O]]
		                 (right :LastRelation[T, _], filter :BooleanFormula[L With T]) :L RightOuterJoin T =
			new RightOuterJoin(left, right)

		override def copy[F <: FromClause](left :F, filter :BooleanFormula[F With R]) :F RightJoin R =
			new RightOuterJoin(left, LastRelation(right, left.size), filter)

		override def copy[F <: FromClause, T[O] <: MappingFrom[O]]
		                 (left :F, right :RowSource[T], filter :BooleanFormula[F With T]) :F RightJoin T =
			new RightOuterJoin(left, LastRelation(right, left.size), filter)


		override type JoinLeft[T[O] <: MappingFrom[O]] = L RightOuterJoin T
		override type This = L RightOuterJoin R

	}

}






/** A `FromClause` constituting of exactly one table or SQL relation.
  * This is just a bit of sugar for Join[Dual, T], so that we can write the type From[T] instead, especially
  * in larger clauses like `From[Children] Join Daemons`.
  */
final class From[T[O] <: MappingFrom[O]] protected
          (from :JoinedRelation[FromClause With T, T, _], filter :BooleanFormula[Dual With T] = True())
	extends With[Dual, T](Dual, from, filter) with InnerJoin[Dual, T]
{

	protected override def copy(filter :BooleanFormula[Dual With T]) :From[T] = new From(table, filter)

	override def copy[M[O] <: MappingFrom[O]](right :LastRelation[M, _], filter :BooleanFormula[Dual With M]) :From[M] =
		new From(right)



	override type JoinLeft[R[O] <: MappingFrom[O]] = From[R]
	override type This = From[T]

	def source :RowSource[T] = table.source


	protected override def joinType :String = "from"

	override def toString :String =
		if (condition == True()) "from " + table.source
		else "from " + table.source + " where " + condition

}






/** A `FromClause` factory for both single table queries, and starting points for arbitrary join lists.
  * Example (see the `With` class documentation for explanation of the filters):
  * {{{
  *     val users = From(Users) where (_(_.name)==="Jack")
  *     val guests = From(Hotels) join GuestsBook on (_(_.id)===_(_.hotelId)) join People on (_(_.guestId)===_(_.id))
  * }}}
  */
object From {

	def apply[M[O] <: MappingFrom[O]](source :RowSource[M]) :From[M] =
		new From(LastRelation[M, Any](source, 0))

	private[sql] def apply[M[O] <: MappingFrom[O]](source :RowSource[M], filter: BooleanFormula[Dual With M]) :From[M] =
		new From(LastRelation(source, 0), filter)



	def unapply[M[O] <: MappingFrom[O]](from :FromClause With M) :Option[RowSource[M]] = from match {
		case _ :AnyFrom => Some(from.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[AnyRowSource] = from match {
		case f :AnyFrom => Some(f.source)
		case _ => None
	}

	type AnyFrom = From[M] forSome { type M[O] <: MappingFrom[O] }

}





