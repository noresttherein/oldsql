package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.AnyComponent
import net.noresttherein.oldsql.sql.FromClause.FromFormula.{CaseFrom, FromMatcher, MatchFrom}
import net.noresttherein.oldsql.sql.FromClause.FromFormula


/**
  * @author Marcin Mościcki
  */
trait MappingFormula[-F <: FromClause, M[O] <: AnyComponent[O]] extends SQLFormula[F, M[Any]#Subject] { //todo:
	type Subject = M[Any]#Subject

}






object MappingFormula {


	type MappingMatcher[+F <: FromClause, +Y[X]] = FromMatcher[F, Y]

	type MatchMapping[+F <: FromClause, +Y[X]] = MatchFrom[F, Y]

	trait CaseMapping[+F <: FromClause, +Y[X]] extends MatchMapping[F, Y] {
		def mapping[M[O] <: AnyComponent[O]](f :MappingFormula[F, M]) :Y[M[Any]#Subject]

		override def from[M[O] <: AnyComponent[O]](f :FromFormula[F, M]) :Y[M[Any]#Subject] =
			mapping(f)
	}

}