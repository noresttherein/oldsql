package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf}
import net.noresttherein.oldsql.schema.{ColumnMapping, Mapping, TypedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, SubselectOf}
import net.noresttherein.oldsql.sql.MappingFormula.{BaseComponentFormula, CaseMapping, ColumnComponentFormula, ComponentFormula, FreeColumn, FreeComponent, JoinedRelation, SQLRelation}
import net.noresttherein.oldsql.sql.SelectFormula.{CaseFreeSelect, CaseFreeSelectColumn, FreeSelectColumn, FreeSelectFormula, SubselectColumn, SubselectFormula}
import net.noresttherein.oldsql.sql.SQLFormula.{CaseFormula, ColumnFormula, CompositeColumnFormula, CompositeFormula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.ColumnFormula.{CaseColumnFormula, ColumnFormulaMatcher}
import net.noresttherein.oldsql.sql.SQLFormula.CompositeColumnFormula.CaseCompositeColumn
import net.noresttherein.oldsql.sql.SQLFormula.CompositeFormula.CaseComposite
import net.noresttherein.oldsql.sql.SQLScribe.{ColumnResult, FormulaResult}
import net.noresttherein.oldsql.sql.SQLTerm.{CaseTerm, ColumnTerm, True}
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



	trait AbstractSQLScribe[+F <: FromClause, -R <: FromClause] extends SQLScribe[F, R]
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






	trait SubstituteComponents[+F <: FromClause, -G <: FromClause] extends AbstractSQLScribe[F, G] {
		protected[this] val oldClause :F
		protected[this] val newClause :G

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



		override def subselect[V, O](e :SubselectColumn[F, V, O]) :ColumnFormula[G, Rows[V]] =
			subselect(e :SubselectFormula[F, V, O]).asInstanceOf[ColumnFormula[G, Rows[V]]]

		override def subselect[V, O](e :SubselectFormula[F, V, O]) :SQLFormula[G, Rows[V]] = {
			val replacement = subselectClause(e.from)//(e.from.outer)
			val newSubselect :replacement.clause.Generalized = replacement.clause.generalized
			val oldExtension = replacement.newExtension.asInstanceOf[oldClause.Generalized ExtendedBy e.From]
			val scribe = extended(e.from, newSubselect)(oldExtension, replacement.newExtension)
			scribe(e.header).subselectFrom(newSubselect).asInstanceOf[SQLFormula[G, Rows[V]]]
		}

		private def subselectClause(subselect :FromClause)
				:SubstituteComponentsSubselectExtension[oldClause.Generalized, newClause.Generalized] =
			subselect match {
				case j :ProperJoin[_, _] =>
					val join = j.asInstanceOf[FromClause ProperJoin MappingFrom]
					val sub = subselectClause(join.left)
					val newExtension = sub.newExtension.stretch[join.LastMapping]
					val oldExtension = newExtension.asInstanceOf[oldClause.Generalized ExtendedBy join.Generalized]
					val unfiltered = join.withLeft[sub.clause.type](sub.clause)(True)

					val scribe = extended(join.generalized, unfiltered.generalized)(oldExtension, newExtension)
					val res = join.withLeft[sub.clause.type](sub.clause)(scribe(join.condition))
					SubstituteComponentsSubselectExtension(res)(newExtension)
			}



		override def freeComponent[O >: F <: FromClause, M[A] <: TypedMapping[X, A], X]
		                          (e :FreeComponent[O, M, X]) :MappingFormula[G, M[O]] =
			unhandled(e)

		override def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :FreeColumn[O, M, V]) :ColumnFormula[G, V] =
			unhandled(e)



		protected[this] def extended[S <: FromClause, N <: FromClause]
		                            (subselect :S, replacement :N)
		                            (implicit oldExt :oldClause.Generalized ExtendedBy S,
		                                      newExt :newClause.Generalized ExtendedBy N) :SQLScribe[S, N]

	}



	private trait SubstituteComponentsSubselectExtension[O <: FromClause, N <: FromClause] {
		val clause :FromClause { type Implicit = N }
		implicit val newExtension :N ExtendedBy clause.Generalized
	}

	private def SubstituteComponentsSubselectExtension[O <: FromClause, N <: FromClause]
	            (result :FromClause { type Implicit = N })(implicit extension :N ExtendedBy result.Generalized)
			:SubstituteComponentsSubselectExtension[O, N] =
		new SubstituteComponentsSubselectExtension[O, N] {
			override val clause :result.type = result
			override val newExtension :N ExtendedBy clause.Generalized = extension
		}




	class ReplaceRelation[T[X] <: TypedMapping[E, X], E, N[X] <: TypedMapping[V, X], V, F <: FromClause, G <: FromClause]
	                     (override val oldClause :F, override val newClause :G)
                         (relation :SQLRelation[F, T, E, _ >: F <: FromClause],
                          replacement :ComponentFormula[G, N, V, T, E, _ >: G <: FromClause])
		extends SubstituteComponents[F, G]
	{
		override def relation[M[X] <: TypedMapping[S, X], S, J >: F <: FromClause](e :SQLRelation[F, M, S, J]) =
			(if (e.shift == relation.shift) replacement else e)
				.asInstanceOf[BaseComponentFormula[G, MappingFrom, M, G]]

		protected[this] override def extended[S <: FromClause, H <: FromClause]
		                                     (subselect :S, replacement :H)
		                                     (implicit oldExt :ExtendedBy[oldClause.Generalized, S],
		                                      newExt :ExtendedBy[newClause.Generalized, H]) =
		{
			val shift = SQLRelation[S, T, E](relation.source, relation.shift + oldExt.length)
			val newRelation = SQLRelation[H, N, V](this.replacement.from.source, shift.shift)
			val comp = newRelation \ this.replacement.mapping.asInstanceOf[T[H]]
			new ReplaceRelation[T, E, N, V, S, H](subselect, replacement)(shift, comp)
		}
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

		override def subselect[V, O](e :SubselectFormula[F, V, O]) = e

		override def subselect[V, O](e :SubselectColumn[F, V, O]) = e
	}



}


