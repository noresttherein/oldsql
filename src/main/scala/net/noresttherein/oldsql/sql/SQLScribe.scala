package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf}
import net.noresttherein.oldsql.schema.{ColumnMapping, Mapping, TypedMapping}
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, SubselectOf}
import net.noresttherein.oldsql.sql.MappingFormula.{BaseComponentFormula, CaseMapping, ColumnComponentFormula, ComponentFormula, FreeColumn, FreeComponent, JoinedRelation, SQLRelation}
import net.noresttherein.oldsql.sql.SelectFormula.{CaseFreeSelect, CaseFreeSelectColumn, FreeSelectColumn, FreeSelectFormula, SubselectColumn, SubselectFormula}
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

		override def freeSelect[V, O](e :FreeSelectFormula[V, O]) :SQLFormula[R, Rows[V]] = e

		override def freeSelect[V, O](e :FreeSelectColumn[V, O]) :ColumnFormula[R, Rows[V]] = e
	}






	trait SubstituteComponents[+F <: FromClause, G <: FromClause] extends AbstractSQLScribe[F, G] {

		override def component[T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, O >: F <: FromClause]
		                      (e :ComponentFormula[F, T, E, M, V, O]) :SQLFormula[G, V] =
		{
			val table = relation(e.from).from.asInstanceOf[SQLRelation[G, T, E, G]]
			if (table eq e.from)
				e.asInstanceOf[ComponentFormula[G, T, E, M, V, G]]
			else
				(table \ e.mapping.withOrigin[G]).upcast
		}


		override def component[T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentFormula[F, T, E, M, V, O]) :ColumnFormula[G, V] =
		{
			val table = relation(e.from).from.asInstanceOf[SQLRelation[G, T, E, G]]
			if (table eq e.from)
				e.asInstanceOf[ColumnFormula[G, V]]
			else
				(table \ e.mapping.withOrigin[G]).upcast
		}



		override def relation[T[A] <: TypedMapping[E, A], E, O >: F <: FromClause](e :SQLRelation[F, T, E, O])
				:BaseComponentFormula[G, M, T, _ >: G <: FromClause] forSome { type M[A] <: MappingFrom[A] }



		override def freeComponent[O >: F <: FromClause, M[A] <: TypedMapping[X, A], X]
		                          (e :FreeComponent[O, M, X]) :MappingFormula[G, M[O]] =
			unhandled(e)

		override def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :FreeColumn[O, M, V]) :ColumnFormula[G, V] =
			unhandled(e)
	}




/*
	class SubstituteRelation[+F <: _ With R, G <: _ With N, R[A] <: TypedMapping[X, A], X, N[A] <: MappingFrom[A]]
	                        (oldClause :F, newClause :G,
	                         oldRelation :JoinedRelation[_ >: F, R], newRelation :BaseComponentFormula[G, N, R, _])
		extends SubstituteComponents[F, G]
	{
		override def relation[T[A] <: TypedMapping[E, A], E, O >: F <: FromClause](e :SQLRelation[F, T, E, O])
				:BaseComponentFormula[G, M, T, _ >: G <: FromClause] forSome { type M[A] <: MappingFrom[A] } =
			(if (oldRelation.shift == e.shift) newRelation else e).asInstanceOf[BaseComponentFormula[G, N, T, G]]

		override def subselect[S <: SubselectOf[F], V, O](e :SubselectFormula[F, S, V, O]) :SQLFormula[G, Rows[V]] = {
			val oldsubselect = e.from
			val oldOuter = oldsubselect.outer //:oldsubselect.Outer
			implicit val oldOuterExtension = oldsubselect.subselectSpan
			val subclause = e.from.asSubselectOf(newClause)(oldsubselect.subselectSpan.asInstanceOf[e.from.Outer ExtendedBy G])
		}

		override def subselect[S <: SubselectOf[F], V, O](e :SubselectColumn[F, S, V, O]) :ColumnFormula[G, Rows[V]] = ???
	}
*/





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
		e.applyTo(new GroundFreeComponents(from))



	private class GroundFreeComponents[F <: FromClause](from :F)
		extends CaseMapping[F, FormulaResult[F]#T] with AbstractSQLScribe[F, F]
	{
		override def mapping[M <: Mapping](e :MappingFormula[F, M]) :SQLFormula[F, M#Subject] = e

		override def component[T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentFormula[F, T, E, M, V, O]) :ColumnFormula[F, V] = e

		override def freeComponent[J >: F <: FromClause, M[A] <: TypedMapping[X, A], X]
		                          (e :FreeComponent[J, M, X]) :SQLFormula[F, X] =
		{
			val table = from.tableStack(e.shift).asInstanceOf[SQLRelation[F, MappingOf[Any]#TypedProjection, Any, J]]
			(table \ e.mapping).upcast
		}

		override def freeComponent[J >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :FreeColumn[J, M, V]) :ColumnFormula[F, V] =
		{
			val table = from.tableStack(e.shift).asInstanceOf[SQLRelation[F, MappingOf[Any]#TypedProjection, Any, J]]
			(table \ e.mapping).upcast
		}

		override def subselect[S <: SubselectOf[F], V, O](e :SubselectFormula[F, S, V, O]) = e

		override def subselect[S <: SubselectOf[F], V, O](e :SelectFormula.SubselectColumn[F, S, V, O]) = e
	}



}


