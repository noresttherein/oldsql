package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.sql.FromClause.{SubselectFrom, TableFormula}
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True


/** A special join type serving as a source for subselects (selects occurring inside of another select, either
  * in the ''select'' or ''where'' clause). It has direct access to all tables in the outer source, while providing
  * its own additional tables in its ''from'' clause. It is represented as an explicit synthetic join between
  * the outer source (left side), and the first table of the subselect from list.
  * This allows any expressions grounded in the outer select to be used as expressions grounded in this select,
  * as it doesn't differ from any other FromClause extension by joins. Note that it is possible to recursively nest
  * subselects to an arbitrary depth and it is modelled by a repeated use of this join type. In that case all
  * tables/mappings to the left of the first occurrence of `SubselectJoin` in the type definition of a nested subselect,
  * while tables/mappings in between subsequent `SubselectJoin`s form the ''from'' clauses of subsequent nested
  * selects, with any tables/mappings following its last occurrence in the type are exclusive to the deepest
  * subselect in the chain.
  *
  * @param source row source for outer select
  * @param table first table in the from list of this subselect
  * @param cond join condition joining this subselect (or rather, its first table) with outer select; may be expanded upon
  *             when additional tables are added to this subselect join (i.e. this source is expanded).
  * @tparam F static type of outer select's source.
  * @tparam S Right side of this join - the first table of the ''from'' clause of the represented subselect.
  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectFrom SubselectFrom]]
  */
class SubselectJoin[F <: FromClause, S <: Mapping] private
		(val source :F, table :TableFormula[F Join S, S], cond :BooleanFormula[F Join S])
	extends Join[F, S](source, table, cond) //with SubselectFrom[F]
{ subsource =>

	val outer = source :Outer
	type Outer = F


	def this(source :F, table :S) = this(source, new TableFormula[FromClause Join S, S](table, source.size), True())


	override def filteredBy: BooleanFormula[this.type] = condition

	override def subselectTableStream: Seq[TableFormula[this.type, _ <: Mapping]] = Seq(lastTable)

	override def subselectTables: Seq[TableFormula[this.type, _ <: Mapping]] = Seq(lastTable)


	override def transplant[O <: FromClause](target: O, rewriter: SQLScribe[Outer, O]): SubselectFrom[O] = {
		val transplanted = target from right
		transplanted filterBy SQLScribe.subselect[Outer, this.type, O, SubselectFrom[O], Boolean](joinCondition, this, transplanted, rewriter)
	}

	//	override def copyJoin[L <: FromClause, M <: Mapping](left: L, right: M): L SubselectJoin M =
	//		new SubselectJoin[L, M](left, right)
	//
	//	override protected def copyJoin(replacement: TableFormula[F Join S, S], condition: BooleanFormula[F Join S]=True()): F SubselectJoin S =
	//		new SubselectJoin[F, S](source, replacement, condition)

	override def copyJoin[L <: FromClause, R <: Mapping](left: L, right: R): L Join R =
		new SubselectJoin[L, R](left, right)

	override protected def copyJoin(replacement: TableFormula[F Join S, S], condition: BooleanFormula[F Join S]=True()): F Join S =
		new SubselectJoin[F, S](source, replacement, condition)


	override def on(condition: (F#LastTable[this.type], TableFormula[this.type, S]) => BooleanFormula[this.type]): F SubselectJoin S =
		super.on(condition).asInstanceOf[F SubselectJoin S]


	override def canEqual(that :Any) = that.isInstanceOf[SubselectJoin[_, _]]

	override def toString = s"$left subselect $lastTable" +  (if (condition==True) "" else " on "+condition)
}


