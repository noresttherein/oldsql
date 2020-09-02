package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.schema.{Mapping, Mapping}
import net.noresttherein.oldsql.sql.FromClause.{SubselectOf, TableFormula}
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True


/** An empty source, serving both as a source for expressions not needing any input tables
  * (like 'SELECT _ FROM DUAL' in Oracle) and terminator element for JoinLike lists
  * (by default any chain of JoinLike[_, _] classes is eventually terminated by a Dual instance).
  */
class Dual(val filteredBy :BooleanFormula[Dual]) extends FromClause {

	def this() = this(True())

	type Row = @~

	type Outer = FromClause

	def outer :Dual = this //throw new NoSuchElementException(s"No outer source for Dual")

	type LastMapping = Nothing
	type LastTable[F <: FromClause] = Nothing //JoinedTable[S, Nothing]


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


	override def transplant[O <: FromClause](target: O, rewriter: SQLScribe[Outer, O]): SubselectOf[O] =
		throw new UnsupportedOperationException(s"Can't transplant $this onto $target")


	override def substitute[T](table: TableFormula[this.type, _<:Mapping[T]], value: T): Dual = this



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
  * (like 'SELECT _ FROM DUAL' in Oracle) and terminator element for JoinLike lists
  * (by default any chain of JoinLike[_, _] classes is eventually terminated by a Dual instance).
  */
case object Dual extends Dual {
	def unapply(source :FromClause) :Boolean = source.isInstanceOf[Dual]
}

