package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{RowSource, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.MappingFrom
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.Join.TypedJoin
import net.noresttherein.oldsql.sql.MappingFormula.SQLRelation
import net.noresttherein.oldsql.sql.MappingFormula.SQLRelation.LastRelation
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple



/** A special join type serving as a source for subselects (selects occurring inside of another select, either
  * in the ''select'' or ''where'' clause). It has direct access to all tables in the outer source, while providing
  * its own additional tables in its ''from'' clause. It is represented as an explicit synthetic join between
  * the outer source (left side), and the first table of the subselect from list.
  * This allows any expressions grounded in the outer select to be used as expressions grounded in this select,
  * as it doesn't differ from any other FromClause extension by joins. Note that it is possible to recursively nest
  * subselects to an arbitrary depth and it is modelled by a repeated use of this join type. In that case all
  * tables/mappings to the left of the first occurrence of `Subselect` in the type definition of a nested subselect,
  * while tables/mappings in between subsequent `Subselect`s form the ''from'' clauses of subsequent nested
  * selects, with any tables/mappings following its last occurrence in the type are exclusive to the deepest
  * subselect in the chain.
  *
  * @tparam F static type of outer select's source.
  * @tparam T Right side of this join - the first table of the ''from'' clause of the represented subselect.
  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf]]
  */
trait Subselect[+F <: FromClause, T[O] <: MappingFrom[O]] extends Join[F, T] {

	override type LikeJoin[+L <: FromClause, R[O] <: MappingFrom[O]] = L Subselect R

	override type This >: this.type <: F Subselect T

	override def copy[L <: FromClause, R[O] <: TypedMapping[X, O], X]
	                 (left :L, right :RowSource[R])(filter :BooleanFormula[left.Generalized With R]) :L Subselect R =
		Subselect[L, R, X](left, LastRelation[R, X](right))(filter)



	override def subselectSize = 1

	override type Outer = left.Generalized

	override def outer :Outer = left.generalized

	override type Inner = FromClause With T



	override def subselectFilter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
			:BooleanFormula[E] =
		condition.stretch(target)




	override type SubselectRow = @~ ~ last.Subject

	override def subselectRow[E <: FromClause]
	                         (target :E)(implicit stretch :Generalized ExtendedBy E) :ChainTuple[E, @~ ~ last.Subject] =
		ChainTuple.EmptyChain ~ last.stretch(target)(stretch)

	override type AsSubselectOf[O <: FromClause] = O Subselect T

	override def asSubselectOf[O <: FromClause](newOuter :O)(implicit extension :Outer ExtendedBy O)
		:(O Subselect T) { type Outer = newOuter.Generalized } =
	{
		val substitute = With.shiftAside[Generalized, newOuter.Generalized With T](1, extension.length)
		withLeft[newOuter.type](newOuter)(substitute(condition))
	}


	protected override def joinType = "subselect"

}






object Subselect {

	/** A template `Subselect` instance with a dummy mapping, for use as a polymorphic factory of `Subselect` joins. */
	final val template :Subselect.* = Subselect(Dual, RowSource.Dummy)


	def apply[L <: FromClause, R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S]
	         (left :L, right :RowSource[R], filter :BooleanFormula[L#Generalized With R] = True)
	         (implicit cast :InferSubject[left.type, Subselect, R, T, S]) :L Subselect R =
		cast(Subselect[left.type, T, S](left, LastRelation[T, S](cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromClause, R[O] <: TypedMapping[S, O], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (filter :BooleanFormula[prefix.Generalized With R]) :L Subselect R =
		newJoin[R, S](prefix, next)(filter)



	private[sql] def newJoin[R[O] <: TypedMapping[S, O], S]
	                        (prefix :FromClause, next :LastRelation[R, S])
	                        (filter :BooleanFormula[prefix.Generalized With R]) :prefix.type Subselect R =
		new Subselect[prefix.type, R] with TypedJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = filter
			override val size = left.size + 1

			override type WithRight[T[A] <: MappingFrom[A]] = left.type Subselect T
			override type This = left.type Subselect R

			override def self :left.type Subselect R = this

			override def withFilter(filter :BooleanFormula[left.Generalized With R]) :This =
				Subselect[left.type, R, S](left, last)(filter)

			override def withRight[T[O] <: TypedMapping[X, O], X]
			                      (right :LastRelation[T, X])(filter :BooleanFormula[left.Generalized With T])
					:left.type Subselect T =
				Subselect[left.type, T, X](left, right)(filter)

			override def withLeft[C <: FromClause]
			                     (left :C)(filter :BooleanFormula[left.Generalized With R]) :WithLeft[C] =
				Subselect[C, R, S](left, last)(filter)



			override def subselectTableStack[E <: FromClause]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[SQLRelation.AnyIn[E]] =
				last.stretch(target) #:: LazyList.empty[SQLRelation.AnyIn[E]]

		}



	def unapply[L <: FromClause, R[O] <: MappingFrom[O]](join :L With R) :Option[(L, RowSource[R])] = join match {
		case _ :Subselect.* => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, RowSource.*)] =
		from match {
			case join :Subselect.* => Some(join.left -> join.right)
			case _ => None
		}



	type * = Subselect[_ <: FromClause, M] forSome { type M[O] <: MappingFrom[O] }


}

