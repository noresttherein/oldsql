package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.sql.RowProduct.{RowTables, SelectFrom, SubselectOf}




/** An intermediate representation of an sql select, capturing selected columns together with originating source,
  * which can be used to create an SQLFormula or a statement.
  */
trait FromSelect[F <: RowProduct, H] {
	def header :SQLFormula[F, H]

	def from :F

	def asSQL[P <: RowProduct, O](implicit subsourceOf :F <:< SubselectOf[P]) :SelectFormula[P, O, H] =
		SelectFormula.subselect[P, SubselectOf[P], O, H](subsourceOf(from), header.asInstanceOf[SQLFormula[SubselectOf[P], H]])

	def asSubselectOf[P <: RowProduct, O](source :P, alias :O)(implicit subsourceOf :F <:< SubselectOf[P]) :SelectFormula[P, O, H] =
		asSQL[P, O]

	def asSelect[O](implicit ev :F <:< SelectFrom) :SelectFormula[RowProduct, O, H] =
		asSQL[RowProduct, O](ev)


	override def toString = s"select $header from $from"
}





object FromSelect {
	def apply[F <: RowProduct, H](source :F, header :SQLFormula[F, H]) :FromSelect[F, H] =
		new SimpleSelect[F, H](source, header)

	def apply[F <: RowProduct, H](source :F)(header :RowTables[F]=>SQLFormula[F, H]) :FromSelect[F, H] =
		new SimpleSelect[F, H](source, header(source.tables))


	implicit def asSQL[F <: SubselectOf[P], P <: RowProduct, O, H](select :FromSelect[F, H]) :SelectFormula[P, O, H] =
		select.asSQL[P, O]



	private class SimpleSelect[F <: RowProduct, H](val from :F, val header :SQLFormula[F, H]) extends FromSelect[F, H]
}
