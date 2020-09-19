package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.{BaseMapping, ColumnMapping, Mapping}
import net.noresttherein.oldsql.slang
import net.noresttherein.oldsql.sql.ColumnSQL.{CaseColumn, ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.CaseCompositeColumn
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.UnboundParam.{FromParam, UnboundParamSQL}
import net.noresttherein.oldsql.sql.MappingSQL.{BaseComponentSQL, CaseMapping, ColumnComponentSQL, ComponentSQL, FreeColumn, FreeComponent, RelationSQL}
import net.noresttherein.oldsql.sql.SelectSQL.{CaseFreeSelect, CaseFreeSelectColumn, FreeSelectColumn, FreeSelectSQL, SubselectColumn, SubselectSQL}
import net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.CaseComposite
import net.noresttherein.oldsql.sql.SQLExpression.{CaseExpression, CompositeSQL, ExpressionMatcher, GlobalScope, GlobalSQL, LocalScope}
import net.noresttherein.oldsql.sql.SQLScribe.{ColumnResult, ExpressionResult}
import net.noresttherein.oldsql.sql.SQLTerm.{CaseTerm, ColumnTerm, CompositeNULL, NULL, SQLParameter, SQLParameterColumn, True}
import net.noresttherein.oldsql.sql.SQLTerm.ColumnTerm.CaseColumnTerm


//here be implicits
import slang._



/**
  * @author Marcin Mościcki
  */
trait SQLScribe[+F <: FromClause, -R <: FromClause]
	extends ExpressionMatcher[F, ExpressionResult[R]#T] with ColumnMatcher[F, ColumnResult[R]#T]
{
	def apply[S >: LocalScope <: GlobalScope, V](e :ColumnSQL[F, S, V]) :ColumnSQL[R, S, V] = e.applyTo(this)
}





object SQLScribe {
	type ExpressionResult[F <: FromClause] = { type T[-S >: LocalScope <: GlobalScope, X] = SQLExpression[F, S, X] }
	type ColumnResult[F <: FromClause] = { type T[-S >: LocalScope <: GlobalScope, X] = ColumnSQL[F, S, X] }



	/** Base `SQLScribe` trait implementing methods for all `SQLExpression` types which do not depend on the clause `F`
	  * (such as terms) as well as for `CompositeSQL` subclasses by recursively applying itself using their
	  * [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL#rephrase rephrase]] method.
	  */
	trait AbstractSQLScribe[+F <: FromClause, -R <: FromClause] extends SQLScribe[F, R]
		with CaseComposite[F, ExpressionResult[R]#T] with CaseCompositeColumn[F, ColumnResult[R]#T]
		with CaseTerm[F, ExpressionResult[R]#T] with CaseColumnTerm[F, ColumnResult[R]#T]
		with CaseFreeSelect[F, ExpressionResult[R]#T] with CaseFreeSelectColumn[F, ColumnResult[R]#T]
	{
		override def composite[S >: LocalScope <: GlobalScope, X](e :CompositeSQL[F, S, X]) :SQLExpression[R, S, X] =
			e.rephrase(this)

		override def composite[S >: LocalScope <: GlobalScope, X](e :CompositeColumnSQL[F, S, X]) :ColumnSQL[R, S, X] =
			e.rephrase(this)

		override def term[X](e :SQLTerm[X]) :GlobalSQL[R, X] = e

		override def term[X](e :ColumnTerm[X]) :ColumnSQL[R, GlobalScope, X] = e

		override def freeSelect[V, O](e :FreeSelectSQL[V, O]) :GlobalSQL[R, Rows[V]] = e

		override def freeSelect[V, O](e :FreeSelectColumn[V, O]) :ColumnSQL[R, GlobalScope, Rows[V]] = e
	}






	/** Base `SQLScribe` trait which uses an identity transformation for all `SQLExpression` types where it is
	  * applicable (such as `SQLTerm` and its subclasses), and recursively applies itself to both
	  * `CompositeSQL` and `SubselectSQL`. The former uses the `rephrase` method to rebuild the composite expression
	  * from parts transformed with this instance, while the latter creates a new instance with its
	  * [[net.noresttherein.oldsql.sql.SQLScribe.RecursiveScribe#extended extended]] method for the subselect's
	  * ''from'' clause. The subselect clause is then transplanted onto the result clause `G` in a way similar
	  * to [[net.noresttherein.oldsql.sql.FromClause#asSubselectOf FromClause.asSubselectOf]], rewriting all
	  * join conditions and the subselect header before creating a new `SubselectSQL`.
	  * In order to be able to rewrite subselect expressions, it requires the output clause instance `G` to use
	  * as their `Outer`/`Implicit` portion. This can be achieved either by using this scribe in a `Compound`
	  * constructor and passing `this` (with uninitialized join condition), or it can be a template clause with an empty
	  * join condition, as the join conditions from the outer portion of subselect clauses are not used in the
	  * generation of the SQL for the subselect.
	  */
	trait RecursiveScribe[+F <: FromClause, -G <: FromClause] extends AbstractSQLScribe[F, G] {
		protected[this] val oldClause :F
		protected[this] val newClause :G


		override def freeComponent[O >: F <: FromClause, M[A] <: BaseMapping[X, A], X]
		                          (e :FreeComponent[O, M, X]) :MappingSQL[G, GlobalScope, M[O]] =
			unhandled(e)

		override def freeComponent[O >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :FreeColumn[O, M, V]) :ColumnSQL[G, GlobalScope, V] =
			unhandled(e)



		override def subselect[V, O](e :SubselectColumn[F, V, O]) :ColumnSQL[G, GlobalScope, Rows[V]] =
			subselect(e :SubselectSQL[F, V, O]).asInstanceOf[ColumnSQL[G, GlobalScope, Rows[V]]]

		override def subselect[V, O](e :SubselectSQL[F, V, O]) :GlobalSQL[G, Rows[V]] = {
			val replacement = subselectClause(e.from)//(e.from.outer)
			val newSubselect = replacement.clause.generalized
			val oldExtension = replacement.newExtension.asInstanceOf[oldClause.Generalized ExtendedBy e.From]
			val scribe = extended(e.from, newSubselect)(oldExtension, replacement.newExtension)
			scribe(e.header).subselectFrom(newSubselect).asInstanceOf[GlobalSQL[G, Rows[V]]]
		}
		//todo: rewrite it with some sort of generic dispatch, because it's fugly
		private def subselectClause(subselect :FromClause)
				:RecursiveScribeSubselectExtension[oldClause.Generalized, newClause.Generalized] =
			subselect match {
				case j :Join[_, _] =>
					val join = j.asInstanceOf[FromSome Join MappingAt]
					val sub = subselectClause(join.left)
					val newExtension = sub.newExtension.extend[join.LastMapping]
					val oldExtension = newExtension.asInstanceOf[oldClause.Generalized ExtendedBy join.Generalized]
					val unfiltered = join.withLeft[sub.clause.type](sub.clause)(True) //todo: condition from a function

					val scribe = extended(join.generalized, unfiltered.generalized)(oldExtension, newExtension)
					val res = join.withLeft[sub.clause.type](sub.clause)(scribe(join.condition))
					RecursiveScribeSubselectExtension(res)(newExtension)

				case j :Subselect[_, _] if newClause.nonEmpty =>
					val join = j.asInstanceOf[(oldClause.Generalized with FromSome) Subselect MappingAt]
					val base = newClause.asInstanceOf[FromSome]
					val unfiltered = join.withLeft[base.type](base)(True)
					implicit val extension = unfiltered.explicitSpan
						.asInstanceOf[newClause.Generalized ExtendedBy unfiltered.Generalized]
					val scribe = extended(join.generalized, unfiltered.generalized) //todo: condition from a function
					val res = join.withLeft[base.type](base)(scribe(join.condition))
						.asInstanceOf[FromSome { type Implicit = newClause.Generalized }]
					RecursiveScribeSubselectExtension(res)(extension.asInstanceOf[newClause.Generalized ExtendedBy res.Generalized])

				case j :Subselect[_, _] => //newClause :Dual => subselect becomes a free select
					val join = j.asInstanceOf[(oldClause.Generalized with FromSome) Subselect MappingOf[Any]#TypedProjection]
					val unfiltered = From[MappingOf[Any]#TypedProjection, Any](join.right)
					implicit val extension = unfiltered.explicitSpan
						.asInstanceOf[newClause.Generalized ExtendedBy unfiltered.Generalized]
					val scribe = extended(join.generalized, unfiltered.generalized)
					val condition = newClause.filter.asInstanceOf[GlobalBoolean[FromClause]] && scribe(join.condition)
					val res = From[MappingOf[Any]#TypedProjection, Any](join.right, condition)
						.asInstanceOf[FromSome { type Implicit = newClause.Generalized }]
					RecursiveScribeSubselectExtension(res)(extension.asInstanceOf[newClause.Generalized ExtendedBy res.Generalized])

				case d :DecoratedFrom[_] =>
					val wrap = d.asInstanceOf[DecoratedFrom[FromClause]]
					val sub = subselectClause(wrap.clause)
					val res = wrap.withClause(sub.clause.asInstanceOf[wrap.clause.FromLast])
						.asInstanceOf[FromSome { type Implicit = newClause.Generalized }]
					val newExtension = sub.newExtension.asInstanceOf[newClause.Generalized ExtendedBy res.Generalized]
					RecursiveScribeSubselectExtension(res)(newExtension)

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
		protected def extended[S <: FromClause, E <: FromClause]
		                      (subselect :S, replacement :E)
		                      (implicit oldExt :oldClause.Generalized ExtendedBy S,
		                                newExt :newClause.Generalized ExtendedBy E) :SQLScribe[S, E]

	}



	private trait RecursiveScribeSubselectExtension[O <: FromClause, E <: FromClause] {
		val clause :FromSome { type Implicit = E }
		implicit val newExtension :E ExtendedBy clause.Generalized
	}

	private def RecursiveScribeSubselectExtension[O <: FromClause, E <: FromClause]
	            (result :FromSome { type Implicit = E })(implicit extension :E ExtendedBy result.Generalized)
			:RecursiveScribeSubselectExtension[O, E] =
		new RecursiveScribeSubselectExtension[O, E] {
			override val clause :result.type = result
			override val newExtension :E ExtendedBy clause.Generalized = extension
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
		                      (e :ComponentSQL[F, T, E, M, V, O]) :GlobalSQL[G, V] =
		{
			val table = relation(e.from).from.asInstanceOf[RelationSQL[G, T, E, G]]
			if (table eq e.from)
				e.asInstanceOf[ComponentSQL[G, T, E, M, V, G]]
			else
				(table \ e.mapping.withOrigin[G]).upcast
		}


		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[G, GlobalScope, V] =
		{
			val table = relation(e.from).from.asInstanceOf[RelationSQL[G, T, E, G]]
			if (table eq e.from)
				e.asInstanceOf[ColumnSQL[G, GlobalScope, V]]
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






	/** Replaces an unbound parameter, together with all its 'components', with one given as the relation `param`
	  * (with the same shift/position). The value of the new parameter must be derivable from the old one.
	  * This scribe is used when aliasing a JoinParam with the `as` method.
	  */
	private[sql] def replaceParam[F <: FromClause, T[A] <: FromParam[P, A], P,
	                              G <: FromClause, M[A] <: FromParam[X, A], X, O >: G <: FromClause]
	                             (oldClause :F, newClause :G,
								  oldParam :RelationSQL[F, T, P, _ >: F <: FromClause],
								  newParam :RelationSQL[G, M, X, O], substitute :X =?> P)
			:SQLScribe[F, G] =
		new ReplaceParam[F, T, P, G, M, X, O](oldClause, newClause)(oldParam, newParam, substitute)


	private class ReplaceParam[+F <: FromClause, M[A] <: FromParam[P, A], P,
	                           -G <: FromClause, N[A] <: FromParam[X, A], X, O >: G <: FromClause]
	                          (protected[this] override val oldClause :F, protected[this] override val newClause :G)
	                          (oldParam :RelationSQL[F, M, P, _ >: F <: FromClause],
							   newParam :RelationSQL[G, N, X, O], extractor :X =?> P)
		extends RecursiveScribe[F, G]
	{
		protected override def extended[S <: FromClause, E <: FromClause]
		                               (subselect :S, replacement :E)
		                               (implicit oldExt :oldClause.Generalized ExtendedBy S,
		                                         newExt :newClause.Generalized ExtendedBy E) =
			new ReplaceParam[S, M, P, E, N, X, E](subselect, replacement)(
				RelationSQL[S, M, P, S](oldParam.relation, oldParam.shift + oldExt.length),
				RelationSQL[E, N, X, E](newParam.relation, newParam.shift + newExt.length),
				extractor
			)


		override def component[T[A] <: BaseMapping[R, A], R, C[A] <: ColumnMapping[V, A], V, U >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, R, C, V, U]) :ColumnSQL[G, GlobalScope, V] =
			e match {
				case oldParam.mapping(old) if e.from.shift == oldParam.shift =>
					val column = newParam.mapping.col(extractor andThen old.extract.asInstanceOf[P =?> V])(old.form)
					(newParam \ column).upcast
				case _ =>
					e.asInstanceOf[ColumnSQL[G, GlobalScope, V]]
			}


		override def component[T[A] <: BaseMapping[R, A], R, C[A] <: BaseMapping[V, A], V, U >: F <: FromClause]
		                      (e :ComponentSQL[F, T, R, C, V, U]) :GlobalSQL[G, V] =
			e match {
				case oldParam.mapping(old) if e.from.shift == oldParam.shift =>
					val component = newParam.mapping(extractor andThen old.extract.asInstanceOf[P =?> V])(old.form)
					(newParam \ component).upcast
				case _ =>
					e.asInstanceOf[GlobalSQL[G, V]]
			}

		override def relation[T[A] <: BaseMapping[R, A], R, U >: F <: FromClause]
		                     (e :RelationSQL[F, T, R, U]) :GlobalSQL[G, R] =
			if (e.shift == oldParam.shift)
				newParam.asInstanceOf[GlobalSQL[G, R]]
			else
				RelationSQL[G, T, R, G](e.relation, e.shift)

		override def toString = s"ReplaceParam($oldParam => $newParam)($oldClause => $newClause)"
	}






	/** Removes all references to unbound statement parameters (synthetic mappings joined with `JoinParam`),
	  * replacing all their components with bound parameter expressions (`SQLParameter`) using the values for `F#Params`
	  * provided as constructor arguments.
	  */
	private[sql] def applyParams[F <: FromClause, N <: FromClause]
	                             (parameterized :F, parameterless :N)(params :parameterized.Params) :SQLScribe[F, N] =
		new ApplyParams(parameterized, parameterless)(params)


	private class ApplyParams[+F <: FromClause, -G <: FromClause] private (
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
		        (List.empty[Int] /: oldClause.fullTableStack) {
					case (acc, UnboundParamSQL(_, _, _)) =>
				        if (acc.isEmpty) 1::Nil else acc.head + 1 :: acc
					case (acc, _) =>
				        if (acc.isEmpty) 0::Nil else acc.head::acc
				}.toIndexedSeq)



		protected override def extended[S <: FromClause, N <: FromClause]
		                               (subselect :S, replacement :N)
		                               (implicit oldExt :ExtendedBy[oldClause.Generalized, S],
		                                         newExt :ExtendedBy[newClause.Generalized, N]) :SQLScribe[S, N] =
			new ApplyParams[S, N](subselect, replacement, params, followingParams ++ List.fill(oldExt.length)(0))



		override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: FromClause]
		                     (e :RelationSQL[F, T, E, O]) :GlobalSQL[G, E] =
			e match {
				case UnboundParamSQL(param, _, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					SQLParameter(params(params.length - shift).asInstanceOf[E])(param.form)
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.shift)
					RelationSQL[G, T, E, G](e.relation, e.shift - shift)
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		                      (e :ComponentSQL[F, T, E, M, V, O]) :GlobalSQL[G, V] =
			e match {
				case UnboundParamSQL(_, extract, idx) =>
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
						e.asInstanceOf[GlobalSQL[G, V]]
					else {
						val table = RelationSQL[G, T, E, G](e.relation, e.from.shift - shift)
						(table \ e.mapping.withOrigin[G]).upcast
					}
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[G, GlobalScope, V] =
			e match {
				case UnboundParamSQL(_, extract, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					extract.get(params(params.length - shift).asInstanceOf[E]) match {
						case Some(v) => SQLParameterColumn(v)(extract.export.form)
						case _ => NULL[V](extract.export.form)
					}
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.from.shift)
					if (shift == 0)
						e.asInstanceOf[ColumnSQL[G, GlobalScope, V]]
					else {
						val table = RelationSQL[G, T, E, G](e.relation, e.from.shift - shift)
						(table \ e.mapping.withOrigin[G]).upcast
					}
			}


		override def toString = s"ApplyParams($params)($oldClause => $newClause)"
	}





	/** Removes a single unbound parameter with the relation shift given as `idx` (counting from the ''right'')
	  * and all its components with bound parameter(s) of/derived from the given value.
	  */
	private[sql] def applyParam[F <: FromClause, N <: FromClause, X]
	                            (from :F, without :N, param :X, idx :Int) :SQLScribe[F, N] =
		new ApplyParam(from, without, param, idx)


	private class ApplyParam[+F <: FromClause, -G <: FromClause, X](
	                         protected[this] override val oldClause :F, protected[this] override val newClause :G,
							 param :X, idx :Int)
		extends RecursiveScribe[F, G] //with CaseColumnComponent[F, ColumnResult[G]#T]
	{

		protected override def extended[S <: FromClause, N <: FromClause]
		                               (subselect :S, replacement :N)
		                               (implicit oldExt :ExtendedBy[oldClause.Generalized, S],
		                                         newExt :ExtendedBy[newClause.Generalized, N]) :SQLScribe[S, N] =
			new ApplyParam[S, N, X](subselect, replacement, param, idx + oldExt.length)



		override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: FromClause]
		                     (e :RelationSQL[F, T, E, O]) :GlobalSQL[G, E] =
			e match {
				case UnboundParamSQL(param, _, this.idx) =>
					SQLParameter(this.param.asInstanceOf[E])(param.form)
				case _ if e.shift < idx =>
					e.asInstanceOf[GlobalSQL[G, E]]
				case _ =>
					RelationSQL[G, T, E, G](e.relation, e.shift - idx)
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: FromClause]
		                      (e :ComponentSQL[F, T, E, M, V, O]) :GlobalSQL[G, V] =
			e match {
				case UnboundParamSQL(_, extract, this.idx) =>
					extract.get(param.asInstanceOf[E]) match {
						case Some(v) => SQLParameter(v)(extract.export.form)
						case _ => CompositeNULL[V](extract.export.form)
					}
				case _ if e.from.shift < idx =>
					e.asInstanceOf[GlobalSQL[G, V]]
				case _ =>
					val table = RelationSQL[G, T, E, G](e.relation, e.from.shift - idx)
					(table \ e.mapping.withOrigin[G]).upcast
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[G, GlobalScope, V] =
			e match {
				case UnboundParamSQL(_, extract, this.idx) =>
					extract.get(param.asInstanceOf[E]) match {
						case Some(v) => SQLParameterColumn(v)(extract.export.form)
						case _ => NULL[V](extract.export.form)
					}
				case _ if e.from.shift < idx =>
					e.asInstanceOf[ColumnSQL[G, GlobalScope, V]]
				case _ =>
					val table = RelationSQL[G, T, E, G](e.relation, e.from.shift - idx)
					(table \ e.mapping.withOrigin[G]).upcast
			}

		override def toString = s"ApplyParam(#$idx=$param)($oldClause => $newClause)"
	}






	/** A scribe rewriting `SQLExpression` instances based on a ''from'' clause `F` into expressions based on
	  * some its extension clause `E`. It relies on the [[net.noresttherein.oldsql.sql.SQLExpression#stretch stretch]]
	  * method of `SQLExpression` and recursively applies itself to parts of composite expressions and subselects of `F`.
	  */
	def stretcher[F <: FromClause, E <: FromClause](clause :E)(implicit extension :F ExtendedBy E) :SQLScribe[F, E] =
		new Stretcher[F, E](clause)

	private class Stretcher[+F <: FromClause, R <: FromClause](clause :R)(implicit extension :F ExtendedBy R)
		extends CaseExpression[F, ExpressionResult[R]#T] with CaseColumn[F, ColumnResult[R]#T]
		   with AbstractSQLScribe[F, R] //overrides the catch-all from the preceding traits
	{
		override def expression[S >: LocalScope <: GlobalScope, X](e :SQLExpression[F, S, X]) :SQLExpression[R, S, X] =
			e.stretch(clause)

		override def column[S >: LocalScope <: GlobalScope, X](e :ColumnSQL[F, S, X]) :ColumnSQL[R, S, X] =
			e.stretch(clause)
	}






	/** Replaces all occurrences of [[net.noresttherein.oldsql.sql.MappingSQL.FreeComponent FreeComponent]] class
	  * in the expression with a appropriate [[net.noresttherein.oldsql.sql.MappingSQL.ComponentSQL ComponentSQL]]
	  * counterpart using the relation from `F` at the offset specified by the free component.
	  */
	def groundFreeComponents[F <: FromClause](from :F) :SQLScribe[F, F] = new GroundFreeComponents(from)


	private class GroundFreeComponents[F <: FromClause](from :F)
		extends CaseMapping[F, ExpressionResult[F]#T] with AbstractSQLScribe[F, F]
	{
		override def mapping[S >: LocalScope <: GlobalScope, M <: Mapping]
		                    (e :MappingSQL[F, S, M]) :SQLExpression[F, S, M#Subject] = e

		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: FromClause]
		                      (e :ColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[F, GlobalScope, V] = e

		override def freeComponent[J >: F <: FromClause, M[A] <: BaseMapping[X, A], X]
		                          (e :FreeComponent[J, M, X]) :GlobalSQL[F, X] =
		{
			val table = from.fullTableStack(e.shift).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, J]]
			(table \ e.mapping).upcast
		}

		override def freeComponent[J >: F <: FromClause, M[A] <: ColumnMapping[V, A], V]
		                          (e :FreeColumn[J, M, V]) :ColumnSQL[F, GlobalScope, V] =
		{
			val table = from.fullTableStack(e.shift).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, J]]
			(table \ e.mapping).upcast
		}

		override def subselect[V, O](e :SubselectSQL[F, V, O]) = e

		override def subselect[V, O](e :SubselectColumn[F, V, O]) = e

		override def toString = s"GroundFreeComponents($from)"
	}






	/** An SQL expression rewriter shifting back references to all relations before the last `Subselect` join
	  * by `extension` positions. Used when a subselect clause is 'transplanted' onto another clause,
	  * extending the `Implicit` clause of the subselect.
	  * @param old a subselect clause serving as SQL expression base.
	  * @param extending a new subselect clause with some additional relations inserted between `F#Implicit`
	  *                  and the mapping joined in with a `Subselect` join.
	  * @param extension the difference in relations number between `F` and `G`.
	  * @param subselectSize number of relations in the explicit ''from'' clause of subselects `F` and `G`.
	  */
	private[sql] def shiftBack[F <: FromClause, G <: FromClause]
	                          (old :F, extending :G, extension :Int, subselectSize :Int) :SQLScribe[F, G] =
		new SubstituteComponents[F, G] {
			protected[this] override val oldClause = old
			protected[this] override val newClause = extending

			private[this] val relations = extending.fullTableStack.to(Array)

			override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: FromClause](e :RelationSQL[F, T, E, O])
					:BaseComponentSQL[G, M, T, _ >: G <: FromClause] forSome { type M[A] <: MappingAt[A] } =
//				(if (e.shift < innerSize) e
//				 else RelationSQL[G, T, E, G](e.relation, e.shift + extension)).asInstanceOf[RelationSQL[G, T, E, G]]
				(if (e.shift < subselectSize) e.asInstanceOf[RelationSQL[G, T, E, G]]
				 else relations(e.shift + extension).asInstanceOf[RelationSQL[G, T, E, G]]).asInstanceOf[RelationSQL[G, T, E, G]]


			protected override def extended[S <: FromClause, N <: FromClause]
			                               (subselect :S, replacement :N)
			                               (implicit oldExt :oldClause.Generalized ExtendedBy S,
			                                         newExt :newClause.Generalized ExtendedBy N) =
				shiftBack[S, N](subselect, replacement, extension, subselectSize + oldExt.length)
		}


}


