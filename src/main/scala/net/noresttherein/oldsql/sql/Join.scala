package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{Mapping, RowSource, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.RowSource.AnyRowSource
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.sql.FromClause.{As, ExtendedBy, JoinedTables, SubselectFrom}
import net.noresttherein.oldsql.sql.InnerJoin.AnyInnerJoin
import net.noresttherein.oldsql.sql.ProperJoin.AnyProperJoin
import net.noresttherein.oldsql.sql.MappingFormula.{ComponentFormula, JoinedRelation}
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation.{AnyRelationIn, LastRelation}
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLScribe.ComponentSubstitutions
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple






/** Common upper type for `Join` subclasses which accept arbitrary `Mapping` subtypes as joined relations.
  * This represents all joins of real relations, encompassing all proper joins and subselect 'joins', but
  * not the synthetic `WithParam` class which restricts itself to the `FromParam` class.
  */
trait Join[+L <: FromClause, R[O] <: MappingFrom[O]] extends With[L, R] { join =>

	override def copy[F <: FromClause](left :F, filter :BooleanFormula[F With R]) :F LikeJoin R =
		copy(left, right, filter)

	def copy[F <: FromClause, T[O] <: MappingFrom[O]]
	        (left :F, right :RowSource[T], filter: BooleanFormula[F With T]) :F LikeJoin T

	def copy[T[O] <: MappingFrom[O]](right :LastRelation[T], filter :BooleanFormula[L With T]) :JoinLeft[T]


	type LikeJoin[+F <: FromClause, T[O] <: MappingFrom[O]] <: (F Join T) {
		type LikeJoin[+S <: FromClause, M[O] <: MappingFrom[O]] = join.LikeJoin[S, M]
	}

	type JoinRight[+F <: FromClause] = F LikeJoin R
	type JoinLeft[T[O] <: MappingFrom[O]] <: L LikeJoin T

	override type This >: this.type <: L Join R
//	override type Generalized >: this.type <: L#Generalized Join R


	/** Specify an alias for the last table in the join. This is not necessary and may be overriden in case of conflicts,
	  * but can be used as the default value and/or help with debugging.
	  * @param alias the alias for the table as in 'from users as u'
	  * @return a new join isomorphic with this instance, but with a new last table (not equal to this.last).
	  */
	def as[A <: Label](alias :A) :L LikeJoin (R As A)#T = {
		val source = FromClause.AliasedSource[R, A](table.source, alias)
		val aliased = LastRelation[(R As A)#T](source)
		val substitute = new ComponentSubstitutions[L With R, L With (R As A)#T] {
			override def relation[S >: With[L, R] <: FromClause, M[O] <: MappingFrom[O]](e :JoinedRelation[S, M])
					:ComponentFormula[L With (R As A)#T, T, M, S] forSome { type T[X] <: MappingFrom[X] } =
				(if (e.shift == 0) ComponentFormula(aliased, aliased.mapping.egg)
				else e).asInstanceOf[ComponentFormula[L With (R As A)#T, (R As A)#T, M, S]]
		}
		val condition = substitute(joinCondition)
		copy(aliased, condition)
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

	override def subselectRow[E <: FromClause](stretch :Generalized ExtendedBy E) :ChainTuple[E, SubselectRow] =
		left.subselectRow(stretch.stretchFront[left.Generalized, R]) ~ table.upcast.stretch(stretch)

	override def subselectTableStack[E <: FromClause](stretch :Generalized ExtendedBy E) :LazyList[AnyRelationIn[E]] =
		table.extend(stretch) #:: left.subselectTableStack(stretch.stretchFront[left.Generalized, R])


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
sealed trait InnerJoin[+L <: FromClause, R[O] <: MappingFrom[O]] extends ProperJoin[L, R] {

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
			case _ => InnerJoin(left, LastRelation(right), filter)
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
		apply(From[L](left), LastRelation[R](right))

	def apply[L <: FromClause, R[O] <: MappingFrom[O]](left :L, right :RowSource[R]) :L InnerJoin R =
		apply(left, LastRelation[R](right))

	private[sql] def apply[L <: FromClause, R[O] <: MappingFrom[O]]
	                      (left :L, right :RowSource[R], filter :BooleanFormula[L With R]) :L InnerJoin R =
		apply(left, LastRelation[R](right), filter)

	private[sql] def apply[L <: FromClause, R[O] <: MappingFrom[O]]
	                      (prefix :L, next :LastRelation[R], filter :BooleanFormula[L With R] = True) :L InnerJoin R =
		prefix match {
			case Dual => From(next, filter.asInstanceOf[BooleanFormula[Dual With R]]).asInstanceOf[L InnerJoin R]
			case _ => newJoin(prefix, next)(filter)
		}



	private def newJoin[R[O] <: MappingFrom[O]](prefix :FromClause, next :LastRelation[R])
	                                           (filter :BooleanFormula[prefix.type With R])
			:InnerJoin[prefix.type, R] =
		new CrossJoin[prefix.type, R] {
			override val left = prefix
			override val table = next
			override val joinCondition = filter

			override type This = left.type InnerJoin R
			override type JoinLeft[T[O] <: MappingFrom[O]] = left.type InnerJoin T

			protected override def self :left.type InnerJoin R = this


			override def copy[T[O] <: MappingFrom[O]]
			                 (right :LastRelation[T], filter :BooleanFormula[left.type With T]) :left.type InnerJoin T =
				newJoin(left, right)(filter)

		}



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

	private trait CrossJoin[+L <: FromClause, R[O] <: MappingFrom[O]] extends InnerJoin[L, R]

}






sealed trait LeftJoin[+L <: FromClause, R[O] <: MappingFrom[O]] extends ProperJoin[L, R] {

	override type LikeJoin[+F <: FromClause, T[O] <: MappingFrom[O]] = F LeftJoin T
	override type This >: this.type <: L LeftJoin R

	override def copy[F <: FromClause](left :F, filter :BooleanFormula[F With R]) :F LeftJoin R =
		LeftJoin(left, LastRelation(table.source), filter)

	override def copy[F <: FromClause, T[O] <: MappingFrom[O]]
	                 (left :F, right :RowSource[T], filter :BooleanFormula[F With T]) :F LeftJoin T =
		LeftJoin(left, LastRelation(right), filter)



	protected override def joinType = "left join"

}






object LeftJoin {

	/** Create a left outer join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some table proxy.
	  */
	def apply[L[O] <: MappingFrom[O], R[O] <: MappingFrom[O]]
	         (left :RowSource[L], right :RowSource[R]) :From[L] LeftJoin R =
		apply(From[L](left), LastRelation[R](right))

	def apply[L <: FromClause, R[O] <: MappingFrom[O]](left :L, right :RowSource[R]) :L LeftJoin R =
		apply(left, LastRelation(right))


	private[sql] def apply[L <: FromClause, R[A] <: MappingFrom[A]]
	                      (prefix :L, next :LastRelation[R], filter :BooleanFormula[L With R] = True) :L LeftJoin R =
		new LeftJoin[prefix.type, R] {
			override val left = prefix
			override val table = next
			override val joinCondition = filter

			override type JoinLeft[T[A] <: MappingFrom[A]] = left.type LeftJoin T
			override type This = left.type LeftJoin R

			override protected def self :left.type LeftJoin R = this


			override def copy[T[A] <: MappingFrom[A]]
			                 (right :LastRelation[T], filter :BooleanFormula[left.type With T]) :JoinLeft[T] =
				LeftJoin(left :left.type, right, filter)

		}



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

}






sealed trait RightJoin[+L <: FromClause, R[O] <: MappingFrom[O]] extends ProperJoin[L, R] {

	override type LikeJoin[+F <: FromClause, T[O] <: MappingFrom[O]] = F RightJoin T
	override type This >: this.type <: L RightJoin R


	override def copy[F <: FromClause](left :F, filter :BooleanFormula[F With R]) :F RightJoin R =
		RightJoin(left, LastRelation(right), filter)

	override def copy[F <: FromClause, T[O] <: MappingFrom[O]]
	                 (left :F, right :RowSource[T], filter :BooleanFormula[F With T]) :F RightJoin T =
		RightJoin(left, LastRelation(right), filter)


	protected override def joinType = "right join"

}






object RightJoin {

	/** Create a right outer join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some table proxy.
	  */
	def apply[L[O] <: MappingFrom[O], R[O] <: MappingFrom[O]]
	         (left :RowSource[L], right :RowSource[R]) :From[L] RightJoin R =
		RightJoin(From(left), LastRelation(right), True)

	def apply[L <: FromClause, R[O] <: MappingFrom[O]](left :L, right :RowSource[R]) :L RightJoin R =
		RightJoin(left, LastRelation(right), True)



	private[sql] def apply[L <: FromClause, R[O] <: MappingFrom[O]]
	                      (prefix :L, next :LastRelation[R], filter :BooleanFormula[L With R]) :L RightJoin R =
		new RightJoin[prefix.type, R] {
			override val left = prefix
			override val table = next
			override val joinCondition = filter

			override type JoinLeft[T[O] <: MappingFrom[O]] = left.type RightJoin T
			override type This = left.type RightJoin R

			override protected def self = this


			override def copy[T[O] <: MappingFrom[O]]
			                 (right :LastRelation[T], filter :BooleanFormula[left.type With T]) :JoinLeft[T] =
				RightJoin(left, right, filter)

		}



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

}






/** A `FromClause` constituting of exactly one table or SQL relation.
  * This is just a bit of sugar for Join[Dual, T], so that we can write the type From[T] instead, especially
  * in larger clauses like `From[Children] Join Daemons`.
  */
sealed trait From[T[O] <: MappingFrom[O]] extends InnerJoin[Dual, T] {
	val left :Dual = Dual

//	protected override def copy(filter :BooleanFormula[Dual With T]) :From[T] = From(table, filter)
//
//	override def copy[M[O] <: MappingFrom[O]](right :LastRelation[M], filter :BooleanFormula[Dual With M]) :From[M] =
//		From(right, filter)

	override type JoinLeft[R[O] <: MappingFrom[O]] <: From[R]
	override type This >: this.type <: From[T]
	override type Generalized = Dual With T


	def source :RowSource[T] = table.source


	protected override def joinType :String = "from"

	override def toString :String =
		if (condition == True()) "from " + table.source
		else "from " + table.source + " where " + condition

}






/** A `FromClause` factory for both single table queries, and starting points for arbitrary join lists.
  * Example (see the `With` class documentation for explanation of the filters):
  * {{{
  *     val users = From(Users) whereLast (_.name === "Jack")
  *     val guests = From(Hotels) join GuestsBook on (_.id === _.hotelId) join People on (_.guestId === _.id)
  * }}}
  */
object From {

	@inline def apply[M[O] <: MappingFrom[O]](source :RowSource[M]) :From[M] =
		apply(LastRelation[M](source), True)

	@inline private[sql] def apply[M[O] <: MappingFrom[O]]
	                              (source :RowSource[M], filter: BooleanFormula[Dual With M]) :From[M] =
		From(LastRelation(source), filter)

	@inline private[sql] def apply[M[O] <: MappingFrom[O]]
	                              (table :LastRelation[M], filter :BooleanFormula[Dual With M]) :From[M] =
		newJoin[M](table)(filter)



	private def newJoin[M[O] <: MappingFrom[O]]
	                   (relation :JoinedRelation[FromClause With M, M])(filter :BooleanFormula[Dual.type With M])
			:From[M] with (Dual.type InnerJoin M)=
		new From[M] with (Dual.type InnerJoin M) {
			override val left = Dual
			override val table = relation
			override val joinCondition = filter

			override def self :left.type InnerJoin M = this
			override type JoinLeft[R[O] <: MappingFrom[O]] = From[R] with (left.type InnerJoin R)
			override type This = From[M] with (left.type InnerJoin M)


			override def copy[T[O] <: MappingFrom[O]](right :LastRelation[T], filter :BooleanFormula[Dual.type With T]) =
				newJoin(right)(filter)

		}



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





