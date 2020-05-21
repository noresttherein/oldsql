package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf}
import net.noresttherein.oldsql.schema.{ColumnMapping, Mapping, TypedMapping}
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, SubselectOf}
import net.noresttherein.oldsql.sql.MappingFormula.{AbstractComponentFormula, CaseMapping, ColumnComponentFormula, ComponentFormula, FreeColumn, FreeComponent, JoinedRelation, SQLRelation, TypedJoinedRelation}
import net.noresttherein.oldsql.sql.SelectFormula.{CaseFreeSelect, CaseFreeSelectColumn, FreeSelectColumn, FreeSelectFormula, SubselectFormula}
import net.noresttherein.oldsql.sql.SQLFormula.{CaseFormula, ColumnFormula, CompositeColumnFormula, CompositeFormula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula.{CaseColumnFormula, ColumnFormulaMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.CompositeColumnFormula.CaseCompositeColumn
import net.noresttherein.oldsql.sql.SQLFormula.CompositeFormula.CaseComposite
import net.noresttherein.oldsql.sql.SQLScribe.{ColumnResult, FormulaResult}
import net.noresttherein.oldsql.sql.SQLTerm.{CaseTerm, ColumnTerm}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnTerm.CaseColumnTerm
import slang._



/**
  * @author Marcin Mościcki
  */
trait SQLScribe[+F <: FromClause, -R <: FromClause]
	extends FormulaMatcher[F, FormulaResult[R]#T] with ColumnFormulaMatcher[F, ColumnResult[R]#T]
{
	def apply[X](e :ColumnFormula[F, X]) :ColumnFormula[R, X] = e.applyTo(this)
}





object SQLScribe {
	type FormulaResult[F <: FromClause] = { type T[X] = SQLFormula[F, X] }
	type ColumnResult[F <: FromClause] = { type T[X] = ColumnFormula[F, X] }



	trait AbstractSQLScribe[+F <: FromClause, R <: FromClause] extends SQLScribe[F, R]
		with CaseComposite[F, FormulaResult[R]#T] with CaseCompositeColumn[F, ColumnResult[R]#T]
		with CaseTerm[F, FormulaResult[R]#T] with CaseColumnTerm[F, ColumnResult[R]#T]
		with CaseFreeSelect[F, FormulaResult[R]#T] with CaseFreeSelectColumn[F, FormulaResult[R]#T]
	{
		override def composite[X](e :CompositeFormula[F, X]) :SQLFormula[R, X] = e.map(this)

		override def composite[X](e :CompositeColumnFormula[F, X]) :ColumnFormula[R, X] = e.map(this)

		override def term[X](e :SQLTerm[X]) :SQLFormula[R, X] = e

		override def term[X](e :ColumnTerm[X]) :ColumnFormula[R, X] = e

		override def select[V, O](e :FreeSelectFormula[V, O]) :SQLFormula[R, Rows[V]] = e

		override def select[V, O](e :FreeSelectColumn[V, O]) :ColumnFormula[R, Rows[V]] = e
	}






	trait ComponentSubstitutions[+F <: FromClause, R <: FromClause] extends AbstractSQLScribe[F, R] {

		override def component[T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, O]
		                      (e :ComponentFormula[F, T, E, M, V, O]) :SQLFormula[R, V] =
		{
			val table = relation(e.from).from.asInstanceOf[SQLRelation[R, T, E, O]]
			if (table eq e.from)
				e.asInstanceOf[ComponentFormula[R, T, E, M, V, O]]
			else
				(table \ e.mapping).upcast
		}


		override def component[T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O]
		                      (e :ColumnComponentFormula[F, T, E, M, V, O]) :ColumnFormula[R, V] =
		{
			val table = relation(e.from).from.asInstanceOf[SQLRelation[R, T, E, O]]
			if (table eq e.from)
				e.asInstanceOf[ColumnFormula[R, V]]
			else
				(table \ e.mapping).upcast
		}


		//todo: this should not be of origin O, as it surely will need a cast in every subclass
		override def relation[T[A] <: TypedMapping[E, A], E, O](e :SQLRelation[F, T, E, O])
				:AbstractComponentFormula[R, M, T, O] forSome { type M[A] <: MappingFrom[A] }




		override def freeComponent[O >: F <: FromClause, M[A] <: TypedMapping[X, A], X]
		                          (e :FreeComponent[O, M, X]) :MappingFormula[R, M[O]] =
			unhandled(e)

		override def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :FreeColumn[O, M, V]) :ColumnFormula[R, V] =
			unhandled(e)
	}






	def stretcher[F <: FromClause, R <: FromClause](clause :R)(implicit extension :F ExtendedBy R) :SQLScribe[F, R] =
		new Stretcher[F, R](clause)

	private class Stretcher[+F <: FromClause, R <: FromClause](clause :R)(implicit extension :F ExtendedBy R)
		extends CaseFormula[F, FormulaResult[R]#T] with CaseColumnFormula[F, ColumnResult[R]#T]
		   with AbstractSQLScribe[F, R] //overrides the catch-all from the preceding traits
	{
		override def formula[X](e :SQLFormula[F, X]) :SQLFormula[R, X] = e.stretch(clause)

		override def column[X](e :ColumnFormula[F, X]) :ColumnFormula[R, X] = e.stretch(clause)
	}






	def groundFreeComponents[F <: FromClause, X](from :F)(e :ColumnFormula[F, X]) :ColumnFormula[F, X] =
		e.applyTo(new ComponentGrounding(from))



	private class ComponentGrounding[F <: FromClause](from :F)
		extends CaseMapping[F, FormulaResult[F]#T] with AbstractSQLScribe[F, F]
	{
		override def mapping[M <: Mapping](e :MappingFormula[F, M]) :SQLFormula[F, M#Subject] = e

		override def component[T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O]
		                      (e :ColumnComponentFormula[F, T, E, M, V, O]) :ColumnFormula[F, V] = e

		override def freeComponent[J >: F <: FromClause, M[A] <: TypedMapping[X, A], X]
		                          (e :FreeComponent[J, M, X]) :SQLFormula[F, X] =
		{
			val table = from.tableStack(e.shift)
				.asInstanceOf[TypedJoinedRelation[J, MappingOf[Any]#TypedProjection, Any]]
			(table \ e.mapping).upcast
		}

		override def freeComponent[J >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :FreeColumn[J, M, V]) :ColumnFormula[F, V] =
		{
			val table = from.tableStack(e.shift).asInstanceOf[TypedJoinedRelation[J, MappingOf[Any]#TypedProjection, Any]]
			(table \ e.mapping).upcast
		}

		override def subselect[S <: SubselectOf[F], V, O](e :SubselectFormula[F, S, V, O]) = e

		override def subselect[S <: SubselectOf[F], V, O](e :SelectFormula.SubselectColumn[F, S, V, O]) = e
	}



}


