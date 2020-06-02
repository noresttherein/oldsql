package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.{ColumnMapping, Mapping, TypedMapping}
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.sql.ColumnSQL.{CaseColumn, ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.CaseCompositeColumn
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.JoinParam.FromParam
import net.noresttherein.oldsql.sql.MappingSQL.{BaseComponentSQL, CaseMapping, ColumnComponentSQL, ComponentSQL, FreeColumn, FreeComponent, SQLRelation}
import net.noresttherein.oldsql.sql.MappingSQL.ColumnComponentSQL.CaseColumnComponent
import net.noresttherein.oldsql.sql.MappingSQL.ComponentSQL.CaseComponent
import net.noresttherein.oldsql.sql.SelectSQL.{CaseFreeSelect, CaseFreeSelectColumn, FreeSelectColumn, FreeSelectSQL, SubselectColumn, SubselectSQL}
import net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.CaseComposite
import net.noresttherein.oldsql.sql.SQLExpression.{CaseExpression, CompositeSQL, ExpressionMatcher}
import net.noresttherein.oldsql.sql.SQLScribe.{ColumnResult, ExpressionResult}
import net.noresttherein.oldsql.sql.SQLTerm.{CaseTerm, ColumnLiteral, ColumnTerm, CompositeNULL, NULL, SQLLiteral, True}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnTerm.CaseColumnTerm


//here be implicits
import slang._



/**
  * @author Marcin Mościcki
  */
trait SQLScribe[+F <: FromClause, -R <: FromClause]
	extends ExpressionMatcher[F, ExpressionResult[R]#T] with ColumnMatcher[F, ColumnResult[R]#T]
{
	def apply[X](e :ColumnSQL[F, X]) :ColumnSQL[R, X] = e.applyTo(this)
}





object SQLScribe {
	type ExpressionResult[F <: FromClause] = { type T[X] = SQLExpression[F, X] }
	type ColumnResult[F <: FromClause] = { type T[X] = ColumnSQL[F, X] }



	trait AbstractSQLScribe[+F <: FromClause, -R <: FromClause] extends SQLScribe[F, R]
		with CaseComposite[F, ExpressionResult[R]#T] with CaseCompositeColumn[F, ColumnResult[R]#T]
		with CaseTerm[F, ExpressionResult[R]#T] with CaseColumnTerm[F, ColumnResult[R]#T]
		with CaseFreeSelect[F, ExpressionResult[R]#T] with CaseFreeSelectColumn[F, ColumnResult[R]#T]
	{
		override def composite[X](e :CompositeSQL[F, X]) :SQLExpression[R, X] = e.rephrase(this)

		override def composite[X](e :CompositeColumnSQL[F, X]) :ColumnSQL[R, X] = e.rephrase(this)

		override def term[X](e :SQLTerm[X]) :SQLExpression[R, X] = e

		override def term[X](e :ColumnTerm[X]) :ColumnSQL[R, X] = e

		override def freeSelect[V, O](e :FreeSelectSQL[V, O]) :SQLExpression[R, Rows[V]] = e

		override def freeSelect[V, O](e :FreeSelectColumn[V, O]) :ColumnSQL[R, Rows[V]] = e
	}






	trait RecursiveScribe[+F <: FromClause, -G <: FromClause] extends AbstractSQLScribe[F, G] {
		protected[this] val oldClause :F
		protected[this] val newClause :G


		override def freeComponent[O >: F <: FromClause, M[A] <: TypedMapping[X, A], X]
		                          (e :FreeComponent[O, M, X]) :MappingSQL[G, M[O]] =
			unhandled(e)

		override def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :FreeColumn[O, M, V]) :ColumnSQL[G, V] =
			unhandled(e)



		override def subselect[V, O](e :SubselectColumn[F, V, O]) :ColumnSQL[G, Rows[V]] =
			subselect(e :SubselectSQL[F, V, O]).asInstanceOf[ColumnSQL[G, Rows[V]]]

		override def subselect[V, O](e :SubselectSQL[F, V, O]) :SQLExpression[G, Rows[V]] = {
			val replacement = subselectClause(e.from)//(e.from.outer)
			val newSubselect :replacement.clause.Generalized = replacement.clause.generalized
			val oldExtension = replacement.newExtension.asInstanceOf[oldClause.Generalized ExtendedBy e.From]
			val scribe = extended(e.from, newSubselect)(oldExtension, replacement.newExtension)
			scribe(e.header).subselectFrom(newSubselect).asInstanceOf[SQLExpression[G, Rows[V]]]
		}

		private def subselectClause(subselect :FromClause)
				:RecursiveScribeSubselectExtension[oldClause.Generalized, newClause.Generalized] =
			subselect match {
				case j :ProperJoin[_, _] =>
					val join = j.asInstanceOf[FromClause ProperJoin MappingAt]
					val sub = subselectClause(join.left)
					val newExtension = sub.newExtension.stretch[join.LastMapping]
					val oldExtension = newExtension.asInstanceOf[oldClause.Generalized ExtendedBy join.Generalized]
					val unfiltered = join.withLeft[sub.clause.type](sub.clause)(True)

					val scribe = extended(join.generalized, unfiltered.generalized)(oldExtension, newExtension)
					val res = join.withLeft[sub.clause.type](sub.clause)(scribe(join.condition))
					RecursiveScribeSubselectExtension(res)(newExtension)
			}



		protected[this] def extended[S <: FromClause, N <: FromClause]
		                            (subselect :S, replacement :N)
		                            (implicit oldExt :oldClause.Generalized ExtendedBy S,
		                                      newExt :newClause.Generalized ExtendedBy N) :SQLScribe[S, N]

	}



	private trait RecursiveScribeSubselectExtension[O <: FromClause, N <: FromClause] {
		val clause :FromClause { type Implicit = N }
		implicit val newExtension :N ExtendedBy clause.Generalized
	}

	private def RecursiveScribeSubselectExtension[O <: FromClause, N <: FromClause]
	            (result :FromClause { type Implicit = N })(implicit extension :N ExtendedBy result.Generalized)
			:RecursiveScribeSubselectExtension[O, N] =
		new RecursiveScribeSubselectExtension[O, N] {
			override val clause :result.type = result
			override val newExtension :N ExtendedBy clause.Generalized = extension
		}






	trait SubstituteComponents[+F <: FromClause, -G <: FromClause] extends RecursiveScribe[F, G] {

		override def component[T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, O >: F <: FromClause]
		                      (e :ComponentSQL[F, T, E, M, V, O]) :SQLExpression[G, V] =
		{
			val table = relation(e.from).from.asInstanceOf[SQLRelation[G, T, E, G]]
			if (table eq e.from)
				e.asInstanceOf[ComponentSQL[G, T, E, M, V, G]]
			else
				(table \ e.mapping.withOrigin[G]).upcast
		}


		override def component[T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[G, V] =
		{
			val table = relation(e.from).from.asInstanceOf[SQLRelation[G, T, E, G]]
			if (table eq e.from)
				e.asInstanceOf[ColumnSQL[G, V]]
			else
				(table \ e.mapping.withOrigin[G]).upcast
		}



		override def relation[T[A] <: TypedMapping[E, A], E, O >: F <: FromClause](e :SQLRelation[F, T, E, O])
				:BaseComponentSQL[G, M, T, _ >: G <: FromClause] forSome { type M[A] <: MappingAt[A] }

	}






	class ReplaceRelation[T[X] <: TypedMapping[E, X], E, N[X] <: TypedMapping[V, X], V, F <: FromClause, G <: FromClause]
	                     (override val oldClause :F, override val newClause :G)
                         (relation :SQLRelation[F, T, E, _ >: F <: FromClause],
                          replacement :ComponentSQL[G, N, V, T, E, _ >: G <: FromClause])
		extends SubstituteComponents[F, G]
	{
		override def relation[M[X] <: TypedMapping[S, X], S, J >: F <: FromClause](e :SQLRelation[F, M, S, J]) =
			(if (e.shift == relation.shift) replacement else e)
				.asInstanceOf[BaseComponentSQL[G, MappingAt, M, G]]

		protected[this] override def extended[S <: FromClause, H <: FromClause]
		                                     (subselect :S, replacement :H)
		                                     (implicit oldExt :ExtendedBy[oldClause.Generalized, S],
		                                      newExt :ExtendedBy[newClause.Generalized, H]) =
		{
			val shift = SQLRelation[S, T, E, S](relation.source, relation.shift + oldExt.length)
			val newRelation = SQLRelation[H, N, V, H](this.replacement.from.source, shift.shift)
			val comp = newRelation \ this.replacement.mapping.asInstanceOf[T[H]]
			new ReplaceRelation[T, E, N, V, S, H](subselect, replacement)(shift, comp)
		}
	}






	class SubstituteParams[+F <: FromClause, -G <: FromClause] private
	                      (protected[this] override val oldClause :F, protected[this] override val newClause :G,
	                       params :IndexedSeq[Any], followingParams :IndexedSeq[Int])
		extends RecursiveScribe[F, G] with CaseColumnComponent[F, ColumnResult[G]#T]
	{

		def this(oldClause :F, newClause :G)(params :oldClause.Params) =
			this(oldClause, newClause, params.all.toIndexedSeq,
				/*{
					def rec(clause :FromClause) :List[Int] = clause match {
						case Dual => Nil
						case left JoinParam _ => rec(left) match {
							case Nil => 1::Nil
							case list @ (h::_) => h + 1 :: list
						}
						case left With _ => rec(left) match {
							case Nil => 0::Nil
							case list @ (h::_) => h::list
						}
					}
					rec(oldClause).reverse.toIndexedSeq
				}*/
		        (List.empty[Int] /: oldClause.tableStack) {
					case (acc, FromParam(_, _, _)) =>
				        if (acc.isEmpty) 1::Nil else acc.head + 1 :: acc
					case (acc, _) =>
				        if (acc.isEmpty) 0::Nil else acc.head::acc
				}.toIndexedSeq)



		override protected[this] def extended[S <: FromClause, N <: FromClause]
		                                     (subselect :S, replacement :N)
		                                     (implicit oldExt :ExtendedBy[oldClause.Generalized, S],
		                                      newExt :ExtendedBy[newClause.Generalized, N]) :SQLScribe[S, N] =
			new SubstituteParams[S, N](subselect, replacement, params, followingParams ++ List.fill(oldExt.length)(0))



		override def relation[T[A] <: TypedMapping[E, A], E, O >: F <: FromClause]
		                     (e :SQLRelation[F, T, E, O]) :SQLExpression[G, E] =
			e match {
				case FromParam(param, _, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					SQLLiteral(params(params.length - shift).asInstanceOf[E])(param.form)
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.shift)
					SQLRelation[G, T, E, G](e.source, e.shift - shift)
			}



		override def component[T[A] <: TypedMapping[E, A], E, M[A] <: TypedMapping[V, A], V, O >: F <: FromClause]
		                      (e :ComponentSQL[F, T, E, M, V, O]) :SQLExpression[G, V] =
			e match {
				case FromParam(_, extract, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					val rank = params.length - shift
					val param = params(rank)
					extract.get(param.asInstanceOf[E]) match {
						case Some(v) => SQLLiteral(v)(extract.export.form)
						case _ => CompositeNULL[V](extract.export.form)
					}
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.from.shift)
					if (shift == 0)
						e.asInstanceOf[SQLExpression[G, V]]
					else {
						val table = SQLRelation[G, T, E, G](e.source, e.from.shift - shift)
						(table \ e.mapping.withOrigin[G]).upcast
					}
			}



		override def component[T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[G, V] =
			e match {
				case FromParam(_, extract, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					extract.get(params(params.length - shift).asInstanceOf[E]) match {
						case Some(v) => ColumnLiteral(v)(extract.export.form)
						case _ => NULL[V](extract.export.form)
					}
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.from.shift)
					if (shift == 0)
						e.asInstanceOf[ColumnSQL[G, V]]
					else {
						val table = SQLRelation[G, T, E, G](e.source, e.from.shift - shift)
						(table \ e.mapping.withOrigin[G]).upcast
					}
			}

	}






	def stretcher[F <: FromClause, R <: FromClause](clause :R)(implicit extension :F ExtendedBy R) :SQLScribe[F, R] =
		new Stretcher[F, R](clause)

	private class Stretcher[+F <: FromClause, R <: FromClause](clause :R)(implicit extension :F ExtendedBy R)
		extends CaseExpression[F, ExpressionResult[R]#T] with CaseColumn[F, ColumnResult[R]#T]
		   with AbstractSQLScribe[F, R] //overrides the catch-all from the preceding traits
	{
		override def expression[X](e :SQLExpression[F, X]) :SQLExpression[R, X] = e.stretch(clause)

		override def column[X](e :ColumnSQL[F, X]) :ColumnSQL[R, X] = e.stretch(clause)
	}






	def groundFreeComponents[F <: FromClause, X](from :F)(e :ColumnSQL[F, X]) :ColumnSQL[F, X] =
		e.applyTo(new GroundFreeComponents(from))



	private class GroundFreeComponents[F <: FromClause](from :F)
		extends CaseMapping[F, ExpressionResult[F]#T] with AbstractSQLScribe[F, F]
	{
		override def mapping[M <: Mapping](e :MappingSQL[F, M]) :SQLExpression[F, M#Subject] = e

		override def component[T[A] <: TypedMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[F, V] = e

		override def freeComponent[J >: F <: FromClause, M[A] <: TypedMapping[X, A], X]
		                          (e :FreeComponent[J, M, X]) :SQLExpression[F, X] =
		{
			val table = from.tableStack(e.shift).asInstanceOf[SQLRelation[F, MappingOf[Any]#TypedProjection, Any, J]]
			(table \ e.mapping).upcast
		}

		override def freeComponent[J >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :FreeColumn[J, M, V]) :ColumnSQL[F, V] =
		{
			val table = from.tableStack(e.shift).asInstanceOf[SQLRelation[F, MappingOf[Any]#TypedProjection, Any, J]]
			(table \ e.mapping).upcast
		}

		override def subselect[V, O](e :SubselectSQL[F, V, O]) = e

		override def subselect[V, O](e :SubselectColumn[F, V, O]) = e
	}



}


