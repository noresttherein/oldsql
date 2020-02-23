package com.hcore.ogre.sql

import com.hcore.ogre.sql.RowSource.{SelectSource, RowTables, SubsourceOf}
import com.hcore.ogre.sql.SQLFormula.SelectFormula


/** An intermediate representation of an sql select, capturing selected columns together with originating source,
  * which can be used to create an SQLFormula or a statement.
  */
trait SelectFrom[S<:RowSource, T] {
	def header :SQLFormula[S, T]

	def from :S

	def asSQL[P<:RowSource](implicit subsourceOf :S<:<SubsourceOf[P]) :SelectFormula[P, T] =
		SelectFormula.subselect[P, SubsourceOf[P], T](subsourceOf(from), header.asInstanceOf[SQLFormula[SubsourceOf[P], T]])

	def asSubselectOf[P<:RowSource](source :P)(implicit subsourceOf :S<:<SubsourceOf[P]) :SelectFormula[P, T] =
		asSQL[P]

	def asSelect(implicit ev :S<:<SelectSource) :SelectFormula[RowSource, T] =
		asSQL[RowSource](ev)


	override def toString = s"select $header from $from"
}





object SelectFrom {
	def apply[S<:RowSource, T](source :S, header :SQLFormula[S, T]) :SelectFrom[S, T] =
		new SimpleSelect[S, T](source, header)

	def apply[S<:RowSource, T](source :S)(header :RowTables[S]=>SQLFormula[S, T]) :SelectFrom[S, T] =
		new SimpleSelect[S, T](source, header(source.tables))


	implicit def asSQL[S<:SubsourceOf[P], P<:RowSource, H](select :SelectFrom[S, H]) :SelectFormula[P, H] =
		select.asSQL[P]



	class SimpleSelect[S<:RowSource, T](val from :S, val header :SQLFormula[S, T]) extends SelectFrom[S, T]
}