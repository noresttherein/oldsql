package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.sql.MappingFormula.{CaseMapping, ComponentFormula, FreeComponent, JoinedRelation}
import net.noresttherein.oldsql.sql.SQLFormula.{ColumnFormula, CompositeColumnFormula, CompositeFormula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula.{CaseColumnFormula, ColumnFormulaMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.CompositeColumnFormula.CaseCompositeColumn
import net.noresttherein.oldsql.sql.SQLFormula.CompositeFormula.CaseComposite
import net.noresttherein.oldsql.sql.SQLScribe.{ColumnScribe, FormulaResult}
import net.noresttherein.oldsql.sql.SQLTerm.{CaseTerm, ColumnTerm}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnTerm.CaseColumnTerm


/**
  * @author Marcin Mościcki
  */
trait SQLScribe[+F <: FromClause, S <: FromClause]
	extends SQLMapper[F, FormulaResult[S]#T] with FormulaMatcher[F, FormulaResult[S]#T]
{ outer =>
	def apply[X](e :ColumnFormula[F, X]) :ColumnFormula[S, X] = e.applyTo(columnScribe)

	def columnScribe :ColumnScribe[F, S]
}





object SQLScribe {
	type FormulaResult[S <: FromClause] = { type T[X] = SQLFormula[S, X] }
	type ColumnResult[S <: FromClause] = { type T[X] = ColumnFormula[S, X] }



	trait ColumnScribe[+F <: FromClause, S <: FromClause] extends ColumnFormulaMatcher[F, ColumnResult[S]#T] {
		def apply[X](e :ColumnFormula[F, X]) :ColumnFormula[S, X] = e.applyTo(this)
		def scribe :SQLScribe[F, S]
	}



	class AbstractColumnScribe[+F <: FromClause, S <: FromClause](override val scribe :SQLScribe[F, S])
		extends ColumnScribe[F ,S] with CaseCompositeColumn[F, ColumnResult[S]#T] with CaseColumnTerm[F, ColumnResult[S]#T]
	{ outer =>
		override def composite[X](e :CompositeColumnFormula[F, X]) :ColumnFormula[S, X] = e.map(scribe)

		override def term[X](e :ColumnTerm[X]) :ColumnFormula[S, X] = e
	}



	class IdentityColumnScribe[F <: FromClause](override val scribe :SQLScribe[F, F])
		extends ColumnScribe[F, F] with CaseColumnFormula[F, ColumnResult[F]#T]
	{
		override def column[X](e :ColumnFormula[F, X]) :ColumnFormula[F, X] = e
	}






	trait AbstractSQLScribe[+F <: FromClause, S <: FromClause]
		extends SQLScribe[F, S] with CaseTerm[F, FormulaResult[S]#T] with CaseComposite[F, FormulaResult[S]#T]
	{ outer =>
		override def term[X](e :SQLTerm[X]) :SQLFormula[S, X] = e

		override def composite[X](e :CompositeFormula[F, X]) :SQLFormula[S, X] = e.map(this)

		override val columnScribe :ColumnScribe[F, S] = new DefaultColumnScribe

		protected class DefaultColumnScribe
			extends ColumnScribe[F, S] with CaseColumnTerm[F, ColumnResult[S]#T] with CaseCompositeColumn[F, ColumnResult[S]#T]
		{
			override def scribe :SQLScribe[F, S] = outer

			override def term[X](e :ColumnTerm[X]) :ColumnFormula[S, X] = e

			override def composite[X](e :CompositeColumnFormula[F, X]) :ColumnFormula[S, X] = e.map(outer)
		}

	}






	abstract class ComponentSubstitutions[+F <: FromClause, S <: FromClause] extends AbstractSQLScribe[F, S] {
		override def component[T[A] <: MappingFrom[A], M[A] <: MappingFrom[A], O]
		                      (e :ComponentFormula[F, T, M, O]) :SQLFormula[S, M[O]#Subject] =
		{
			val table = relation(e.from).from.asInstanceOf[JoinedRelation[FromClause, T]]
			if (table eq e.from)
				e.asInstanceOf[ComponentFormula[S, T, M, O]]
			else
				ComponentFormula(table, e.mapping.asInstanceOf[MappingFrom[FromClause]]).asInstanceOf[SQLFormula[S, M[O]#Subject]]
		}


		override def relation[O >: F <: FromClause, M[A] <: MappingFrom[A]](e :JoinedRelation[O, M])
				:ComponentFormula[S, T, M, O] forSome { type T[A] <: MappingFrom[A] }


		override def freeComponent[O >: F <: FromClause, M[A] <: TypedMapping[X, A], X]
		                          (e :FreeComponent[O, M, X]) :MappingFormula[S, M[O]] =
			unhandled(e)

	}






	def groundFreeComponents[F <: FromClause, X](from :F)(e :ColumnFormula[F, X]) :ColumnFormula[F, X] =
		e.applyTo(new ComponentGrounding(from).columnScribe)



	class ComponentGrounding[F <: FromClause](from :F)
		extends CaseMapping[F, FormulaResult[F]#T] with AbstractSQLScribe[F, F]
	{
		override def mapping[M <: Mapping](e :MappingFormula[F, M]) :SQLFormula[F, M#Subject] = ???

		override def freeComponent[J >: F <: FromClause, M[A] <: TypedMapping[X, A], X]
		                          (e :FreeComponent[J, M, X]) :SQLFormula[F, X] =
			ComponentFormula(from.tableStack(e.shift).asInstanceOf[JoinedRelation[J, MappingFrom]], e.mapping)
	}



}


