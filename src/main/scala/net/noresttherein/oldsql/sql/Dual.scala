package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.sql.FromClause.SubselectFrom
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple


/** An empty source, serving both as a source for expressions not needing any input tables
  * (like 'SELECT _ ''from'' DUAL' in Oracle) and terminator element for Join lists
  * (by default any chain of Join[_, _] classes is eventually terminated by a Dual instance).
  */
class Dual(val filteredBy :BooleanFormula[Dual]) extends FromClause {

	def this() = this(True())

	override type LastMapping = Nothing
	override type LastTable[-F <: FromClause] = Nothing

	override def lastTable :Nothing = throw new NoSuchElementException("Dual.lastTable")

	override type Generalized = Dual

	override def generalized :Dual = this

	override type Outer = FromClause

	override def outer :Dual = this //throw new NoSuchElementException(s"No outer source for Dual")

	override type Row = @~

	override def row :ChainTuple[FromClause, @~] = ChainTuple.EmptyChain


	override def size = 0


/*
	override type LastMapping = Nothing
	override type LastTable[F <: FromClause] = Nothing //JoinedTable[S, Nothing]


	override def lastTable = throw new NoSuchElementException("Dual.last")

	override def allTables = Seq()

	override def toTableStream = Stream.empty

	override def toUntypedTableStream = Stream.empty

	override def subselectTableStream = Stream.empty

	override def headTable = throw new NoSuchElementException("Dual.head")

	override def mappings = Seq()

	override def size = 0

	override def occurrences(mapping: Mapping): Int = 0


	override def filterBy[S >: this.type <: FromClause](filter: BooleanFormula[S]): S =
		if (filter==filteredBy) this
		else new Dual(filteredBy).asInstanceOf[S]


	override def row: HListFormula[Dual, HNil] = SQLHNil

	override def joinAny(source: FromClause): source.type = source


	override def plant(prefix :PartialFunction[TableFormula[this.type, _<:Mapping], ComponentPath[_<:Mapping, _<:Mapping]]) : Dual = this

	override def plantMatching(prefix: ComponentPath[_ <: Mapping, _ <: Mapping]): Dual = this


	override def transplant[O <: FromClause](target: O, rewriter: SQLScribe[Outer, O]): SubselectFrom[O] =
		throw new UnsupportedOperationException(s"Can't transplant $this onto $target")


	override def substitute[T](table: TableFormula[this.type, _<:Mapping[T]], value: T): Dual = this
*/



	//	override def selectOne[T <: Mapping, C <: Mapping](mapping: (JoinedTables[this.type]) => ComponentFormula[this.type, T, C]): SelectMapping[Dual, C] =
	//		throw new UnsupportedOperationException(s"select from Dual")


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
  * (like 'SELECT _ ''from'' DUAL' in Oracle) and terminator element for Join lists
  * (by default any chain of Join[_, _] classes is eventually terminated by a Dual instance).
  */
object Dual extends Dual {
	def unapply(source :FromClause) :Boolean = source.isInstanceOf[Dual]
}

