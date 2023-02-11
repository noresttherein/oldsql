package com.hcore.ogre.sql

import com.hcore.ogre.mapping.{Mapping, ComponentPath, AnyMapping}
import com.hcore.ogre.sql.RowSource.{SubsourceOf, TableFormula, RowTables}
import com.hcore.ogre.sql.SQLFormula._
import shapeless.HNil





/** An empty source, serving both as a source for expressions not needing any input tables
  * (like 'SELECT _ FROM DUAL' in Oracle) and terminator element for Join lists
  * (by default any chain of Join[_, _] classes is eventually terminated by a Dual instance).
  */
class Dual(val filteredBy :BooleanFormula[Dual]) extends RowSource {

	def this() = this(True())

	type Row = HNil

	type Parent = RowSource

	def parent = this //throw new NoSuchElementException(s"No parent source for Dual")

	type LastMapping = Nothing
	type LastTable[S<:RowSource] = Nothing //JoinedTable[S, Nothing]


	override def lastTable = throw new NoSuchElementException("Dual.last")

	override def allTables = Seq()

	override def toTableStream = Stream.empty

	override def toUntypedTableStream = Stream.empty

	override def subselectTableStream = Stream.empty

	override def headTable = throw new NoSuchElementException("Dual.head")

	override def mappings = Seq()

	override def size = 0

	override def occurences(mapping: AnyMapping): Int = 0


	override def filterBy[S >: this.type <: RowSource](filter: BooleanFormula[S]): S =
		if (filter==filteredBy) this
		else new Dual(filteredBy).asInstanceOf[S]


	override def row: HListFormula[Dual, HNil] = SQLHNil

	override def joinAny(source: RowSource): source.type = source


	override def plant(prefix :PartialFunction[TableFormula[this.type, _<:AnyMapping], ComponentPath[_<:AnyMapping, _<:AnyMapping]]) : Dual = this

	override def plantMatching(prefix: ComponentPath[_ <: AnyMapping, _ <: AnyMapping]): Dual = this


	override def transplant[O <: RowSource](target: O, rewriter: SQLScribe[Parent, O]): SubsourceOf[O] =
		throw new UnsupportedOperationException(s"Can't transplant $this onto $target")


	override def substitute[T](table: TableFormula[this.type, _<:Mapping[T]], value: T): Dual = this



//	override def selectOne[T <: Mapping, C <: Mapping](mapping: (JoinedTables[this.type]) => ComponentFormula[this.type, T, C]): SelectMapping[Dual, C] =
//		throw new UnsupportedOperationException(s"select from Dual")



	override def canEqual(that :Any) = that.isInstanceOf[Dual]

	override def equals(that :Any) = PartialFunction.cond(that) { case d:Dual => (d eq this) || d.canEqual(this) && d.filteredBy==filteredBy }

	override def hashCode = filteredBy.hashCode

	override def toString = filteredBy match {
		case True() => "Dual"
		case e => s"Dual where $e"
	}

}

/** An empty row source, serving both as a source for expressions not needing any input tables
  * (like 'SELECT _ FROM DUAL' in Oracle) and terminator element for Join lists
  * (by default any chain of Join[_, _] classes is eventually terminated by a Dual instance).
  */
case object Dual extends Dual {
	def unapply(source :RowSource) :Boolean = source.isInstanceOf[Dual]
}

