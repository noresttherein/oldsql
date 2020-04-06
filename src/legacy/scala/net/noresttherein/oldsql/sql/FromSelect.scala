package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.sql.FromClause.{RowTables, SelectFrom, SubselectFrom}




/** An intermediate representation of an sql select, capturing selected columns together with originating source,
  * which can be used to create an SQLFormula or a statement.
  */
trait FromSelect[F <: FromClause, H] {
	def header :SQLFormula[F, H]

	def from :F

	def asSQL[P <: FromClause, O](implicit subsourceOf :F <:< SubselectFrom[P]) :SelectFormula[P, O, H] =
		SelectFormula.subselect[P, SubselectFrom[P], O, H](subsourceOf(from), header.asInstanceOf[SQLFormula[SubselectFrom[P], H]])

	def asSubselectOf[P <: FromClause, O](source :P, alias :O)(implicit subsourceOf :F <:< SubselectFrom[P]) :SelectFormula[P, O, H] =
		asSQL[P, O]

	def asSelect[O](implicit ev :F <:< SelectFrom) :SelectFormula[FromClause, O, H] =
		asSQL[FromClause, O](ev)


	override def toString = s"select $header from $from"
}





object FromSelect {
	def apply[F <: FromClause, H](source :F, header :SQLFormula[F, H]) :FromSelect[F, H] =
		new SimpleSelect[F, H](source, header)

	def apply[F <: FromClause, H](source :F)(header :RowTables[F]=>SQLFormula[F, H]) :FromSelect[F, H] =
		new SimpleSelect[F, H](source, header(source.tables))


	implicit def asSQL[F <: SubselectFrom[P], P <: FromClause, O, H](select :FromSelect[F, H]) :SelectFormula[P, O, H] =
		select.asSQL[P, O]



	private class SimpleSelect[F <: FromClause, H](val from :F, val header :SQLFormula[F, H]) extends FromSelect[F, H]
}
