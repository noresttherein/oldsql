package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.schema.TypedMapping
import net.noresttherein.oldsql.schema.Mapping.MappingFrom
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation
import net.noresttherein.oldsql.sql.MappingFormula.TypedJoinedRelation.LastRelation
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple



/** An empty ''from'' clause, serving both as a base for SQL expressions not needing any input tables
  * (like 'SELECT _ FROM DUAL' in Oracle) and an initial element for `With` lists
  * (any chain of `With` classes starts by joining with `Dual`).
  */
sealed class Dual private () extends FromClause {

	override type LastMapping[O] = Nothing
	override type LastTable[-F <: FromClause] = Nothing
	override type FromLast = FromClause
	override type This = Dual

	override def last :Nothing = throw new NoSuchElementException("Dual.last")


	override type Generalized = FromClause

	override def generalized :Dual = this


	override type Outer = FromClause

	override def outer :Dual = this //throw new NoSuchElementException(s"No outer source for Dual")



	override type Row = @~

	override def row :ChainTuple[FromClause, @~] = ChainTuple.EmptyChain

	override def row[E <: FromClause](stretch :FromClause ExtendedBy E) :ChainTuple[E, @~] =
		ChainTuple.EmptyChain

	override def tableStack :LazyList[JoinedRelation.AnyIn[FromClause]] = LazyList.empty

	override def tableStack[E <: FromClause](stretch :FromClause ExtendedBy E) :LazyList[JoinedRelation.AnyIn[E]] = LazyList.empty



	override type SubselectRow = @~

	override def subselectRow :ChainTuple[FromClause, @~] = ChainTuple.EmptyChain

	override def subselectRow[E <: FromClause](stretch :FromClause ExtendedBy E) :ChainTuple[E, @~] =
		ChainTuple.EmptyChain

	override def subselectTableStack :LazyList[JoinedRelation.AnyIn[FromClause]] = LazyList.empty

	override def subselectTableStack[E <: FromClause](stretch :FromClause ExtendedBy E) :LazyList[JoinedRelation.AnyIn[E]] =
		LazyList.empty



	override def size = 0



	override def filteredBy :BooleanFormula[FromClause] = True

	override def filteredBy[E <: FromClause](extension :FromClause ExtendedBy E) :BooleanFormula[E] =
		filteredBy.stretch(extension)



	override type JoinFilter[T[O] <: MappingFrom[O]] = Nothing



//	override protected def joinAll_:(preceding :FromClause) :FromClause = preceding
//
//	override protected def joinSubselect_:(preceding :FromClause) :FromClause = preceding


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Dual]

	override def equals(that :Any) :Boolean = that match {
		case d :Dual => (d eq this) || d.canEqual(this) && d.filteredBy == filteredBy
		case _ => false
	}

	override def hashCode :Int = filteredBy.hashCode

	override def toString :String = filteredBy match {
		case True() => "Dual"
		case e => s"Dual where $e"
	}

}






/** An empty row source, serving both as a source for expressions not needing any input tables
  * (like 'SELECT _ ''from'' DUAL' in Oracle) and terminator element for With lists
  * (by default any chain of With[_, _] classes is eventually terminated by a Dual instance).
  */
object Dual extends Dual {

	protected[sql] override def selfInnerJoin[T[A] <: TypedMapping[X, A], X]
	                                         (right :LastRelation[T, X])(filter :BooleanFormula[FromClause With T])
			:From[T] with (this.type InnerJoin T) =
		From.newJoin[T, X](right)(filter)


	def unapply(source :FromClause) :Boolean = source.isInstanceOf[Dual]
}

