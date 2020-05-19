package com.hcore.ogre.sql


import com.hcore.ogre.mapping.AnyMapping
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.sql.RowSource.{SubsourceOf, RowTables, WithParam, TableFormula}
import com.hcore.ogre.sql.SQLFormula._

//implicits
import SaferCasts._


/** A special join type serving as a source for subselects (selects occuring inside of another select, either in the header or where clause).
  * It has implicit access to all tables in the parent source, while providing own additional tables in its from clause.
  * It is represented as an explicit artificial join between parent source (left side), and first last of the subselect from list.
  * This allows to use any expressions grounded in the parent select as expressions grounded in this select, as it doesn't differ
  * from any other RowSource extension by joins.
  *
  * @param source row source for parent select
  * @param table first last in the from list of this subselect
  * @param cond join condition joining this subselect (or rather, its first last) with parent select; may be expanded upon
  *             when additional tables are added to this subselect join (i.e. this source is expanded).
  * @tparam S static type of parent select source
  * @tparam R Right side of this join.
  */
class SubselectJoin[S<:RowSource, R<:AnyMapping] private (
		val source :S, table :TableFormula[S Join R, R], cond :BooleanFormula[S Join R]
	) extends Join[S, R](source, table, cond) //with SubsourceOf[S]
{ subsource =>

	val parent = source :Parent
	type Parent = S


	def this(source :S, table :R) = this(source, new TableFormula[RowSource Join R, R](table, source.size), True())


	override def filteredBy: BooleanFormula[this.type] = condition

	override def subselectTableStream: Seq[TableFormula[this.type, _ <: AnyMapping]] = Seq(lastTable)

	override def subselectTables: Seq[TableFormula[this.type, _ <: AnyMapping]] = Seq(lastTable)


	override def transplant[O <: RowSource](target: O, rewriter: SQLScribe[Parent, O]): SubsourceOf[O] = {
		val transplanted = target from right
		transplanted filterBy SQLScribe.subselect[Parent, this.type, O, SubsourceOf[O], Boolean](joinCondition, this, transplanted, rewriter)
	}

//	override def copyJoin[L <: RowSource, M <: Mapping](left: L, right: M): L SubselectJoin M =
//		new SubselectJoin[L, M](left, right)
//
//	override protected def copyJoin(replacement: TableFormula[S Join R, R], condition: BooleanFormula[S Join R]=True()): S SubselectJoin R =
//		new SubselectJoin[S, R](source, replacement, condition)

	override def copyJoin[L <: RowSource, M <: AnyMapping](left: L, right: M): L Join M =
		new SubselectJoin[L, M](left, right)

	override protected def copyJoin(replacement: TableFormula[S Join R, R], condition: BooleanFormula[S Join R]=True()): S Join R =
		new SubselectJoin[S, R](source, replacement, condition)


	override def on(condition: (S#LastTable[this.type], TableFormula[this.type, R]) => BooleanFormula[this.type]): S SubselectJoin R =
		super.on(condition).asInstanceOf[S SubselectJoin R]


	override def canEqual(that :Any) = that.isInstanceOf[SubselectJoin[_, _]]

	override def toString = s"$left subselect $lastTable" +  (if (condition==True) "" else " on "+condition)
}



