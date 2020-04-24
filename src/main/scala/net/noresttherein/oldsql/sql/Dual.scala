package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.schema.Mapping.MappingFrom
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, SubselectFrom}
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation.AnyRelationIn
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple


/** An empty ''from'' clause, serving both as a base for SQL expressions not needing any input tables
  * (like 'SELECT _ FROM DUAL' in Oracle) and an initial element for `With` lists
  * (any chain of `With` classes starts by joining with `Dual`).
  */
class Dual private (val filteredBy :BooleanFormula[Dual]) extends FromClause {

	def this() = this(True())

	override type LastMapping[O] = Nothing
	override type LastTable[-F <: FromClause] = Nothing
	override type FromLast = Dual
	override type This = Dual

	override def lastTable :Nothing = throw new NoSuchElementException("Dual.lastTable")
//	override def last[M[O] <: MappingFrom[O]] :Nothing = throw new NoSuchElementException("Dual.lastTable")

	override type Generalized = Dual

	override def generalized :Dual = this

	override type Outer = FromClause

	override def outer :Dual = this //throw new NoSuchElementException(s"No outer source for Dual")

	override type Row = @~

	override def row :ChainTuple[FromClause, @~] = ChainTuple.EmptyChain

	override def row[E <: FromClause](stretch :Dual ExtendedBy E) :ChainTuple[E, @~] =
		ChainTuple.EmptyChain

	override def tableStack :LazyList[AnyRelationIn[Dual]] = LazyList.empty

	override def tableStack[E <: FromClause](stretch :Dual ExtendedBy E) :LazyList[AnyRelationIn[E]] = LazyList.empty

	override type SubselectRow = @~

	override def subselectRow :ChainTuple[FromClause, @~] = ChainTuple.EmptyChain

	override def subselectRow[E <: FromClause](stretch :Dual ExtendedBy E) :ChainTuple[E, @~] =
		ChainTuple.EmptyChain

	override def subselectTableStack :LazyList[AnyRelationIn[Dual]] = LazyList.empty

	override def subselectTableStack[E <: FromClause](stretch :Dual ExtendedBy E) :LazyList[AnyRelationIn[E]] =
		LazyList.empty

	override def size = 0

	override type JoinFilter[T[O] <: MappingFrom[O]] = Nothing


//	protected override def filter[U >: Dual <: FromClause](condition :BooleanFormula[U]) :Dual =
//		if (condition == True()) this else new Dual(condition)



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
	def unapply(source :FromClause) :Boolean = source.isInstanceOf[Dual]
}

