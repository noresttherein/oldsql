package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.{BaseMapping, ColumnMapping, Mapping}
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.sql.ColumnSQL.{CaseColumn, ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.CaseCompositeColumn
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FromSome}
import net.noresttherein.oldsql.sql.JoinParam.FromParam
import net.noresttherein.oldsql.sql.MappingSQL.{BaseComponentSQL, CaseMapping, ColumnComponentSQL, ComponentSQL, FreeColumn, FreeComponent, RelationSQL}
import net.noresttherein.oldsql.sql.SelectSQL.{CaseFreeSelect, CaseFreeSelectColumn, FreeSelectColumn, FreeSelectSQL, SubselectColumn, SubselectSQL}
import net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.CaseComposite
import net.noresttherein.oldsql.sql.SQLExpression.{CaseExpression, CompositeSQL, ExpressionMatcher}
import net.noresttherein.oldsql.sql.SQLScribe.{ColumnResult, ExpressionResult}
import net.noresttherein.oldsql.sql.SQLTerm.{CaseTerm, ColumnLiteral, ColumnTerm, CompositeNULL, NULL, SQLLiteral, SQLParameter, SQLParameterColumn, True}
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



	/** Base `SQLScribe` trait implementing methods for all `SQLExpression` types which do not depend on the clause `F`
	  * (such as terms) as well as for `CompositeSQL` subclasses by recursively applying itself using their
	  * [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.rephrase rephrase]] method.
	  */
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






	/** Base `SQLScribe` trait which uses an identity transformation for all `SQLExpression` types where it is
	  * applicable (such as `SQLTerm` and its subclasses), and recursively applies itself to both
	  * `CompositeSQL` and `SubselectSQL`. The former uses the `rephrase` method to rebuild the composite expression
	  * from parts transformed with this instance, while the latter creates a new instance with its
	  * [[net.noresttherein.oldsql.sql.SQLScribe.RecursiveScribe#extended extended]] method for the subselect's
	  * ''from'' clause. The subselect clause is then transplanted onto the result clause `G` in a way similar
	  * to [[net.noresttherein.oldsql.sql.FromClause.asSubselectOf FromClause.asSubselectOf]], rewriting all
	  * join conditions and the subselect header before creating a new `SubselectSQL`.
	  * In order to be able to rewrite subselect expressions, it requires the output clause instance `G` to use
	  * as their `Outer`/`Implicit` portion. This can be achieved either by using this scribe in a `AndFrom`
	  * constructor and passing `this` (with uninitialized join condition), or it can be a template clause with an empty
	  * join condition, as the join conditions from the outer portion of subselect clauses are not used in the
	  * generation of the SQL for the subselect.
	  */
	trait RecursiveScribe[+F <: FromClause, -G <: FromClause] extends AbstractSQLScribe[F, G] {
		protected[this] val oldClause :F
		protected[this] val newClause :G


		override def freeComponent[O >: F <: FromClause, M[A] <: BaseMapping[X, A], X]
		                          (e :FreeComponent[O, M, X]) :MappingSQL[G, M[O]] =
			unhandled(e)

		override def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :FreeColumn[O, M, V]) :ColumnSQL[G, V] =
			unhandled(e)



		override def subselect[V, O](e :SubselectColumn[F, V, O]) :ColumnSQL[G, Rows[V]] =
			subselect(e :SubselectSQL[F, V, O]).asInstanceOf[ColumnSQL[G, Rows[V]]]

		override def subselect[V, O](e :SubselectSQL[F, V, O]) :SQLExpression[G, Rows[V]] = {
			val replacement = subselectClause(e.from)//(e.from.outer)
			val newSubselect = replacement.clause.generalized
			val oldExtension = replacement.newExtension.asInstanceOf[oldClause.Generalized ExtendedBy e.From]
			val scribe = extended(e.from, newSubselect)(oldExtension, replacement.newExtension)
			scribe(e.header).subselectFrom(newSubselect).asInstanceOf[SQLExpression[G, Rows[V]]]
		}

		private def subselectClause(subselect :FromClause)
				:RecursiveScribeSubselectExtension[oldClause.Generalized, newClause.Generalized] =
			subselect match {
				case j :TrueJoin[_, _] =>
					val join = j.asInstanceOf[FromSome TrueJoin MappingAt]
					val sub = subselectClause(join.left)
					val newExtension = sub.newExtension.stretch[join.LastMapping]
					val oldExtension = newExtension.asInstanceOf[oldClause.Generalized ExtendedBy join.Generalized]
					val unfiltered = join.withLeft[sub.clause.type](sub.clause)(True) //todo: condition from a function

					val scribe = extended(join.generalized, unfiltered.generalized)(oldExtension, newExtension)
					val res = join.withLeft[sub.clause.type](sub.clause)(scribe(join.condition))
					RecursiveScribeSubselectExtension(res)(newExtension)

				case j :Subselect[_, _] if newClause.nonEmpty =>
					val join = j.asInstanceOf[(oldClause.Generalized with FromSome) Subselect MappingAt]
					val base = newClause.asInstanceOf[FromSome]
					val unfiltered = join.withLeft[base.type](base)(True)
					implicit val extension = unfiltered.subselectSpan.asInstanceOf[newClause.Generalized ExtendedBy unfiltered.Generalized]
					val scribe = extended(join.generalized, unfiltered.generalized) //todo: condition from a function
					val res = join.withLeft[base.type](base)(scribe(join.condition)).asInstanceOf[newClause.Nested]
					RecursiveScribeSubselectExtension(res)(extension.asInstanceOf[newClause.Generalized ExtendedBy res.Generalized])

				case j :Subselect[_, _] => //newClause :Dual => subselect becomes a free select
					val join = j.asInstanceOf[(oldClause.Generalized with FromSome) Subselect MappingOf[Any]#TypedProjection]
					val unfiltered = From[MappingOf[Any]#TypedProjection, Any](join.right)
					implicit val extension = unfiltered.subselectSpan.asInstanceOf[newClause.Generalized ExtendedBy unfiltered.Generalized]
					val scribe = extended(join.generalized, unfiltered.generalized)
					val condition = newClause.filter.asInstanceOf[SQLBoolean[FromClause]] && scribe(join.condition)
					val res = From[MappingOf[Any]#TypedProjection, Any](join.right, condition).asInstanceOf[newClause.Nested]
					RecursiveScribeSubselectExtension(res)(extension.asInstanceOf[newClause.Generalized ExtendedBy res.Generalized])

				//cases with From/Dual are covered by the freeSelect rather than subselect and this method is not called.
				case bogus =>
					throw new IllegalArgumentException(
						s"Unsupported clause in a subselect: $bogus.\nTransplanting a subselect of $oldClause\n onto $newClause."
					)
			}



		/** Factory method for compatible rewriters extending their implementation to subselect clauses extending
		  * the input clause `F`.
		  * @param subselect a subselect of `F`, that is a ''from'' clause of type `F Subselect ...`.
		  * @param replacement a subselect of the output clause `G`, that is a ''from'' clause of type `F Subselect ...`,
		  *                    which contains all the same relations as `S` in its subselect span (`Inner`/`Explicit`),
		  *                    but with the last join condition uninitialized/unchanged.
		  */
		protected def extended[S <: FromClause, N <: FromClause]
		                      (subselect :S, replacement :N)
		                      (implicit oldExt :oldClause.Generalized ExtendedBy S,
		                                newExt :newClause.Generalized ExtendedBy N) :SQLScribe[S, N]

	}



	private trait RecursiveScribeSubselectExtension[O <: FromClause, N <: FromClause] {
		val clause :FromSome { type Implicit = N }
		implicit val newExtension :N ExtendedBy clause.Generalized
	}

	private def RecursiveScribeSubselectExtension[O <: FromClause, N <: FromClause]
	            (result :FromSome { type Implicit = N })(implicit extension :N ExtendedBy result.Generalized)
			:RecursiveScribeSubselectExtension[O, N] =
		new RecursiveScribeSubselectExtension[O, N] {
			override val clause :result.type = result
			override val newExtension :N ExtendedBy clause.Generalized = extension
		}






	/** Base `SQLScribe` trait for implementations substituting one or more of the relations in the input clause
	  * with mappings containing the replaced mappings as their components. The only remaining method left to implement
	  * by subclasses is [[net.noresttherein.oldsql.sql.SQLScribe.SubstituteComponents#relation relation]], return type
	  * of which has been narrowed down to a `BaseComponentSQL`. The handler methods for `ComponentSQL` and
	  * `ColumnComponentSQL` in this trait assume that the substitute relation also contains the original component
	  * and simply graft it onto the new relation.
	  */
	trait SubstituteComponents[+F <: FromClause, -G <: FromClause] extends RecursiveScribe[F, G] {

		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		                      (e :ComponentSQL[F, T, E, M, V, O]) :SQLExpression[G, V] =
		{
			val table = relation(e.from).from.asInstanceOf[RelationSQL[G, T, E, G]]
			if (table eq e.from)
				e.asInstanceOf[ComponentSQL[G, T, E, M, V, G]]
			else
				(table \ e.mapping.withOrigin[G]).upcast
		}


		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[G, V] =
		{
			val table = relation(e.from).from.asInstanceOf[RelationSQL[G, T, E, G]]
			if (table eq e.from)
				e.asInstanceOf[ColumnSQL[G, V]]
			else
				(table \ e.mapping.withOrigin[G]).upcast
		}



		override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: FromClause](e :RelationSQL[F, T, E, O])
				:BaseComponentSQL[G, M, T, _ >: G <: FromClause] forSome { type M[A] <: MappingAt[A] }

	}






	class ReplaceRelation[T[X] <: BaseMapping[E, X], E, N[X] <: BaseMapping[V, X], V, F <: FromClause, G <: FromClause]
	                     (override val oldClause :F, override val newClause :G)
                         (relation :RelationSQL[F, T, E, _ >: F <: FromClause],
                          replacement :ComponentSQL[G, N, V, T, E, _ >: G <: FromClause])
		extends SubstituteComponents[F, G]
	{
		override def relation[M[X] <: BaseMapping[S, X], S, J >: F <: FromClause](e :RelationSQL[F, M, S, J]) =
			(if (e.shift == relation.shift) replacement else e)
				.asInstanceOf[BaseComponentSQL[G, MappingAt, M, G]]

		protected override def extended[S <: FromClause, H <: FromClause]
		                               (subselect :S, replacement :H)
		                               (implicit oldExt :ExtendedBy[oldClause.Generalized, S],
		                                         newExt :ExtendedBy[newClause.Generalized, H]) =
		{
			val shift = RelationSQL[S, T, E, S](relation.relation, relation.shift + oldExt.length)
			val newRelation = RelationSQL[H, N, V, H](this.replacement.from.relation, shift.shift)
			val comp = newRelation \ this.replacement.mapping.asInstanceOf[T[H]]
			new ReplaceRelation[T, E, N, V, S, H](subselect, replacement)(shift, comp)
		}

		override def toString = s"ReplaceRelation($relation with $replacement)($oldClause => $newClause)"
	}






	private[sql]
	class ReplaceParam[+F <: FromClause, -G <: FromClause, O >: G <: FromClause, M[A] <: FromParam[X, A], X, P]
	                  (protected[this] override val oldClause :F, protected[this] override val newClause :G)
	                  (param :RelationSQL[G, M, X, O], extractor :X =?> P)
		extends RecursiveScribe[F, G]
	{
		protected override def extended[S <: FromClause, N <: FromClause]
		                               (subselect :S, replacement :N)
		                               (implicit oldExt :oldClause.Generalized ExtendedBy S,
		                                         newExt :newClause.Generalized ExtendedBy N) =
			new ReplaceParam[S, N, N, M, X, P](subselect, replacement)(
				RelationSQL[N, M, X, N](param.relation, param.shift + newExt.length), extractor
			)


		override def component[T[A] <: BaseMapping[E, A], E, C[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, C, V, O]) :ColumnSQL[G, V] =
			e match {
				case param.mapping(old) if e.from.shift == param.shift =>
					val column = param.mapping.col(extractor andThen old.extract.asInstanceOf[P =?> V])(old.form)
					(param \ column).upcast
				case _ =>
					e.asInstanceOf[ColumnSQL[G, V]]
			}


		override def component[T[A] <: BaseMapping[E, A], E, C[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		                      (e :ComponentSQL[F, T, E, C, V, O]) :SQLExpression[G, V] =
			e match {
				case param.mapping(old) if e.from.shift == param.shift =>
					val component = param.mapping(extractor andThen old.extract.asInstanceOf[P =?> V])(old.form)
					(param \ component).upcast
				case _ =>
					e.asInstanceOf[SQLExpression[G, V]]
			}

		override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: FromClause]
		                     (e :RelationSQL[F, T, E, O]) :SQLExpression[G, E] =
			if (e.shift == param.shift)
				param.asInstanceOf[SQLExpression[G, E]]
			else
				RelationSQL[G, T, E, G](e.relation, e.shift)

		override def toString = s"ReplaceParam($param)($oldClause => $newClause)"
	}






	/** Removes all references to unbound statement parameters (synthetic mappings joined with `JoinParam`),
	  * replacing all their components with bound parameter expressions (`SQLParameter`) using the values for `F#Params`
	  * provided as constructor arguments.
	  */
	private[sql] class RemoveParams[+F <: FromClause, -G <: FromClause] private (
	                                protected[this] override val oldClause :F, protected[this] override val newClause :G,
	                                params :IndexedSeq[Any], followingParams :IndexedSeq[Int])
		extends RecursiveScribe[F, G] //with CaseColumnComponent[F, ColumnResult[G]#T]
	{

		def this(oldClause :F, newClause :G)(params :oldClause.Params) =
			this(oldClause, newClause, params.toList.toIndexedSeq,
				/*{
					def rec(clause :FromClause) :List[Int] = clause match {
						case Dual => Nil
						case left JoinParam _ => rec(left) match {
							case Nil => 1::Nil
							case list @ (h::_) => h + 1 :: list
						}
						case left AndFrom _ => rec(left) match {
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



		protected override def extended[S <: FromClause, N <: FromClause]
		                               (subselect :S, replacement :N)
		                               (implicit oldExt :ExtendedBy[oldClause.Generalized, S],
		                                         newExt :ExtendedBy[newClause.Generalized, N]) :SQLScribe[S, N] =
			new RemoveParams[S, N](subselect, replacement, params, followingParams ++ List.fill(oldExt.length)(0))



		override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: FromClause]
		                     (e :RelationSQL[F, T, E, O]) :SQLExpression[G, E] =
			e match {
				case FromParam(param, _, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					SQLParameter(params(params.length - shift).asInstanceOf[E])(param.form)
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.shift)
					RelationSQL[G, T, E, G](e.relation, e.shift - shift)
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		                      (e :ComponentSQL[F, T, E, M, V, O]) :SQLExpression[G, V] =
			e match {
				case FromParam(_, extract, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					val rank = params.length - shift
					val param = params(rank)
					extract.get(param.asInstanceOf[E]) match {
						case Some(v) => SQLParameter(v)(extract.export.form)
						case _ => CompositeNULL[V](extract.export.form)
					}
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.from.shift)
					if (shift == 0)
						e.asInstanceOf[SQLExpression[G, V]]
					else {
						val table = RelationSQL[G, T, E, G](e.relation, e.from.shift - shift)
						(table \ e.mapping.withOrigin[G]).upcast
					}
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[G, V] =
			e match {
				case FromParam(_, extract, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					extract.get(params(params.length - shift).asInstanceOf[E]) match {
						case Some(v) => SQLParameterColumn(v)(extract.export.form)
						case _ => NULL[V](extract.export.form)
					}
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.from.shift)
					if (shift == 0)
						e.asInstanceOf[ColumnSQL[G, V]]
					else {
						val table = RelationSQL[G, T, E, G](e.relation, e.from.shift - shift)
						(table \ e.mapping.withOrigin[G]).upcast
					}
			}


		override def toString = s"RemoveParams($params)($oldClause => $newClause)"
	}






	private[sql] class RemoveParam[+F <: FromClause, -G <: FromClause, X](
	                               protected[this] override val oldClause :F, protected[this] override val newClause :G,
	                               param :X, idx :Int)
		extends RecursiveScribe[F, G] //with CaseColumnComponent[F, ColumnResult[G]#T]
	{

		protected override def extended[S <: FromClause, N <: FromClause]
		                               (subselect :S, replacement :N)
		                               (implicit oldExt :ExtendedBy[oldClause.Generalized, S],
		                                         newExt :ExtendedBy[newClause.Generalized, N]) :SQLScribe[S, N] =
			new RemoveParam[S, N, X](subselect, replacement, param, idx + oldExt.length)



		override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: FromClause]
		                     (e :RelationSQL[F, T, E, O]) :SQLExpression[G, E] =
			e match {
				case FromParam(param, _, this.idx) =>
					SQLParameter(this.param.asInstanceOf[E])(param.form)
				case _ if e.shift < idx =>
					e.asInstanceOf[SQLExpression[G, E]]
				case _ =>
					RelationSQL[G, T, E, G](e.relation, e.shift - idx)
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		                      (e :ComponentSQL[F, T, E, M, V, O]) :SQLExpression[G, V] =
			e match {
				case FromParam(_, extract, this.idx) =>
					extract.get(param.asInstanceOf[E]) match {
						case Some(v) => SQLParameter(v)(extract.export.form)
						case _ => CompositeNULL[V](extract.export.form)
					}
				case _ if e.from.shift < idx =>
					e.asInstanceOf[SQLExpression[G, V]]
				case _ =>
					val table = RelationSQL[G, T, E, G](e.relation, e.from.shift - idx)
					(table \ e.mapping.withOrigin[G]).upcast
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[G, V] =
			e match {
				case FromParam(_, extract, this.idx) =>
					extract.get(param.asInstanceOf[E]) match {
						case Some(v) => SQLParameterColumn(v)(extract.export.form)
						case _ => NULL[V](extract.export.form)
					}
				case _ if e.from.shift < idx =>
					e.asInstanceOf[ColumnSQL[G, V]]
				case _ =>
					val table = RelationSQL[G, T, E, G](e.relation, e.from.shift - idx)
					(table \ e.mapping.withOrigin[G]).upcast
			}

		override def toString = s"RemoveParam(#$idx=$param)($oldClause => $newClause)"
	}






	/** A scribe rewriting `SQLExpression` instances based on a ''from'' clause `F` into expressions based on
	  * some its extension clause `E`. It relies on the [[net.noresttherein.oldsql.sql.SQLExpression.stretch stretch]]
	  * method of `SQLExpression` and recursively applies itself to parts of composite expressions and subselects of `F`.
	  */
	def stretcher[F <: FromClause, E <: FromSome](clause :E)(implicit extension :F ExtendedBy E) :SQLScribe[F, E] =
		new Stretcher[F, E](clause)

	private class Stretcher[+F <: FromClause, R <: FromSome](clause :R)(implicit extension :F ExtendedBy R)
		extends CaseExpression[F, ExpressionResult[R]#T] with CaseColumn[F, ColumnResult[R]#T]
		   with AbstractSQLScribe[F, R] //overrides the catch-all from the preceding traits
	{
		override def expression[X](e :SQLExpression[F, X]) :SQLExpression[R, X] = e.stretch(clause)

		override def column[X](e :ColumnSQL[F, X]) :ColumnSQL[R, X] = e.stretch(clause)
	}






	/** Replaces all occurrences of [[net.noresttherein.oldsql.sql.MappingSQL.FreeComponent FreeComponent]] class
	  * in the expression with a appropriate [[net.noresttherein.oldsql.sql.MappingSQL.ComponentSQL ComponentSQL]]
	  * counterpart using the relation from `F` at the offset specified by the free component.
	  */
	def groundFreeComponents[F <: FromClause, X](from :F, e :ColumnSQL[F, X]) :ColumnSQL[F, X] =
		e.applyTo(new GroundFreeComponents(from))

	/** Replaces all occurrences of [[net.noresttherein.oldsql.sql.MappingSQL.FreeComponent FreeComponent]] class
	  * in the expression with a appropriate [[net.noresttherein.oldsql.sql.MappingSQL.ComponentSQL ComponentSQL]]
	  * counterpart using the relation from `F` at the offset specified by the free component.
	  */
	def groundFreeComponents[F <: FromClause, X](from :F, e :SQLExpression[F, X]) :SQLExpression[F, X] =
		e.applyTo(new GroundFreeComponents(from))



	private class GroundFreeComponents[F <: FromClause](from :F)
		extends CaseMapping[F, ExpressionResult[F]#T] with AbstractSQLScribe[F, F]
	{
		override def mapping[M <: Mapping](e :MappingSQL[F, M]) :SQLExpression[F, M#Subject] = e

		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[F, V] = e

		override def freeComponent[J >: F <: FromClause, M[A] <: BaseMapping[X, A], X]
		                          (e :FreeComponent[J, M, X]) :SQLExpression[F, X] =
		{
			val table = from.tableStack(e.shift).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, J]]
			(table \ e.mapping).upcast
		}

		override def freeComponent[J >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :FreeColumn[J, M, V]) :ColumnSQL[F, V] =
		{
			val table = from.tableStack(e.shift).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, J]]
			(table \ e.mapping).upcast
		}

		override def subselect[V, O](e :SubselectSQL[F, V, O]) = e

		override def subselect[V, O](e :SubselectColumn[F, V, O]) = e

		override def toString = s"GroundFreeComponents($from)"
	}



}


