package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.ColumnMapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.{AggregateClause, Aggregated, By, ColumnSQL, From, FromSome, GlobalBoolean, GroupBy, GroupByClause, Join, RowProduct, SQLExpression, Subselect}
import net.noresttherein.oldsql.sql.ColumnSQL.{CaseColumn, ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.CaseCompositeColumn
import net.noresttherein.oldsql.sql.DecoratedFrom.FromSomeDecorator
import net.noresttherein.oldsql.sql.RowProduct.{ExtendedBy, NonEmptyFrom, PartOf}
import net.noresttherein.oldsql.sql.SQLExpression.{CaseExpression, CompositeSQL, ExpressionMatcher, GlobalScope, GlobalSQL, LocalScope}
import net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.CaseComposite
import net.noresttherein.oldsql.sql.UnboundParam.{FromParam, UnboundParamSQL}
import net.noresttherein.oldsql.sql.ast.{AggregateSQL, MappingSQL, QuerySQL, SQLTerm}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{CaseMapping, ComponentSQL, LooseColumn, LooseComponent, RelationSQL, TypedColumnComponentSQL, TypedComponentSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.TypedColumnComponentSQL.CaseColumnComponent
import net.noresttherein.oldsql.sql.ast.QuerySQL.{CaseColumnQuery, CaseQuery, ColumnQuery, Rows}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{CaseTopSelect, CaseTopSelectColumn, SubselectColumn, SubselectSQL, TopSelectColumn, TopSelectSQL}
import net.noresttherein.oldsql.sql.ast.SQLTerm.{CaseTerm, ColumnTerm, CompositeNull, SQLNull, SQLParameter, SQLParameterColumn, True}
import net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm.CaseColumnTerm
import net.noresttherein.oldsql.sql.mechanics.SQLScribe.{ColumnResult, ExpressionResult}






/** A term rewriter translating any [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, T]`
  * for the input ''from'' clause `F`, into an `SQLExpression[R, S, T]` for the output ''from'' clause `R`.
  * If the expression is a subtype of [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]], the result will
  * also be a `ColumnSQL` of the same value type and scope, but based on the clause `R`.
  * It is implemented through the visitor pattern as defined by the
  * [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionMatcher ExpressionMatcher]] trait.
  * @author Marcin Mościcki
  */
trait SQLScribe[+F <: RowProduct, -R <: RowProduct]
	extends ExpressionMatcher[F, ExpressionResult[R]#T] with ColumnMatcher[F, ColumnResult[R]#T]
{   //overriden so that the column variant is correctly picked from between overloads
	override def apply[S >: LocalScope <: GlobalScope, V](e :SQLExpression[F, S, V]) :SQLExpression[R, S, V] =
		e.applyTo(this)

	def apply[S >: LocalScope <: GlobalScope, V](e :ColumnSQL[F, S, V]) :ColumnSQL[R, S, V] =
		e.applyTo(this)
}





object SQLScribe {
	type ExpressionResult[F <: RowProduct] = { type T[-S >: LocalScope <: GlobalScope, X] = SQLExpression[F, S, X] }
	type ColumnResult[F <: RowProduct] = { type T[-S >: LocalScope <: GlobalScope, X] = ColumnSQL[F, S, X] }



	/** Base `SQLScribe` trait implementing methods for all `SQLExpression` types which do not depend on the clause `F`
	  * (such as terms) as well as for `CompositeSQL` subclasses by recursively applying itself using their
	  * [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.rephrase rephrase]] method.
	  */
	trait AbstractSQLScribe[+F <: RowProduct, -R <: RowProduct] extends SQLScribe[F, R]
		with CaseComposite[F, ExpressionResult[R]#T] with CaseCompositeColumn[F, ColumnResult[R]#T]
		with CaseTerm[F, ExpressionResult[R]#T] with CaseColumnTerm[F, ColumnResult[R]#T]
		with CaseTopSelect[F, ExpressionResult[R]#T] with CaseTopSelectColumn[F, ColumnResult[R]#T]
	{
		override def *(e :ColumnSQL[RowProduct, LocalScope, Nothing]) :ColumnSQL[R, LocalScope, Nothing] = e


		override def composite[S >: LocalScope <: GlobalScope, X](e :CompositeSQL[F, S, X]) :SQLExpression[R, S, X] =
			e.rephrase(this)

		override def composite[S >: LocalScope <: GlobalScope, X](e :CompositeColumnSQL[F, S, X]) :ColumnSQL[R, S, X] =
			e.rephrase(this)

		override def term[X](e :SQLTerm[X]) :GlobalSQL[R, X] = e

		override def term[X](e :ColumnTerm[X]) :ColumnSQL[R, GlobalScope, X] = e

		override def topSelect[V](e :TopSelectSQL[V]) :GlobalSQL[R, Rows[V]] = e

		override def topSelect[V](e :TopSelectColumn[V]) :ColumnSQL[R, GlobalScope, Rows[V]] = e


	}






	/** Base `SQLScribe` trait which uses an identity transformation for all `SQLExpression` types where it is
	  * applicable (such as `SQLTerm` and its subclasses), and recursively applies itself to both
	  * `CompositeSQL` and `SubselectSQL`. The former uses the `rephrase` method to rebuild the composite expression
	  * from parts transformed with this instance, while the latter creates a new instance with its
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLScribe.RecursiveScribe.extended extended]] method for the subselect's
	  * ''from'' clause. The subselect clause is then transplanted onto the result clause `G` in a way similar
	  * to [[net.noresttherein.oldsql.sql.RowProduct.asSubselectOf RowProduct.asSubselectOf]], rewriting all
	  * join conditions and the subselect header before creating a new `SubselectSQL`.
	  * In order to be able to rewrite subselect expressions, it requires the output clause instance `G` to use
	  * as their `Outer`/`Implicit` portion. This can be achieved either by using this scribe in a `Compound`
	  * constructor and passing `this` (with uninitialized join condition), or it can be a template clause with an empty
	  * join condition, as the join conditions from the outer portion of subselect clauses are not used in the
	  * generation of the SQL for the subselect.
	  */
	trait RecursiveScribe[+F <: RowProduct, -G <: RowProduct]
		extends CaseQuery[F, ExpressionResult[G]#T] with CaseColumnQuery[F, ColumnResult[G]#T]
		   with AbstractSQLScribe[F, G]
	{ self =>
		protected[this] val oldClause :F
		protected[this] val newClause :G

		override def expression[S >: LocalScope <: GlobalScope, X](e :SQLExpression[F, S, X]) :SQLExpression[G, S, X] =
			unhandled(e)

		override def column[S >: LocalScope <: GlobalScope, X](e :ColumnSQL[F, S, X]) :ColumnSQL[G, S, X] =
			unhandled(e)

		override def mapping[S >: LocalScope <: GlobalScope, M[O] <: MappingAt[O]]
		                    (e :MappingSQL[F, S, M]) :SQLExpression[G, S, M[()]#Subject] =
			unhandled(e)

		override def query[V](e :QuerySQL[F, V]) :SQLExpression[G, GlobalScope, Rows[V]] = unhandled(e)

		override def query[V](e :ColumnQuery[F, V]) :ColumnSQL[G, GlobalScope, Rows[V]] = unhandled(e)

		override def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[X, A], X]
		                          (e :LooseComponent[O, M, X]) :MappingSQL[G, GlobalScope, M] =
			unhandled(e)

		override def looseComponent[O >: F <: RowProduct, M[A] <: ColumnMapping[V, A], V]
		                          (e :LooseColumn[O, M, V]) :ColumnSQL[G, GlobalScope, V] =
			unhandled(e)



		override def aggregate[D <: FromSome, X, Y](e :AggregateSQL[D, F, X, Y]) :ColumnSQL[G, LocalScope, Y] =
			oldClause match {
				case oldAggr :AggregateClause => newClause match {
					case newAggr :AggregateClause =>
						val groupings = oldAggr.size
						val scribe = group(oldAggr.fromClause, newAggr.fromClause, groupings, newAggr.size, groupings)
						val arg = scribe(e.arg.asInstanceOf[ColumnSQL[oldAggr.Discrete, LocalScope, Y]])
						val res = AggregateSQL(e.function, arg, e.isDistinct)(e.readForm)
						res.asInstanceOf[ColumnSQL[G, LocalScope, Y]]

					case _ =>
						throw new IllegalArgumentException(
							s"Cannot transcribe aggregate expression $e to a non-aggregate FROM clause $newClause with $this"
						)
				}
				case _ =>
					throw new IllegalArgumentException(
						s"Cannot transcribe aggregate expression $e from a non-aggregate FROM clause $oldClause with $this"
					)
			}


		/** A scribe rewriting expressions aggregated under the clause `F`. Used for `e.arg` of received instances of
		  * `e :AggregateSQL[F, O, _, _]`, as well as expressions of any subselects of `oldClause.Discrete`.
		  * @param oldGrouped base clause of input - either `oldClause.Discrete`, or its subselect (possibly indirect).
		  * @param newGrouped base clause of the output. initially, when `oldGrouped = oldClause.from`,
		  *                   `newGrouped = newClause.from`. Afterwards, these are both arguments given to
		  *                   `extend` - that is a clause resulting from rebasing the input subselect clause onto their
		  *                   common prefix, `this.oldClause.outer`.
		  * @param oldOffset the index of the first relation from the outer clause of `this.oldClause` (i.e., `F`),
		  *                  but inside the clause `O`.
		  * @param newOffset the index of the first relation from the outer clause of `this.newClause` (i.e., `E`),
		  *                  but inside the clause `N`. This is the same relation as `oldGrouped.fullTableStack(oldOffset)`.
		  * @param groupings number of pseudo relations in the ''group by'' clause of `this.oldClause`, that is
		  *                  `this.oldClause.size` (cached).
		  */
		private def group[O <: RowProduct, N <: RowProduct]
		                 (oldGrouped :O, newGrouped :N, oldOffset :Int, newOffset :Int, groupings :Int) :SQLScribe[O, N] =
			new RecursiveScribe[O, N] {
				override val oldClause = oldGrouped
				override val newClause = newGrouped

				override def component[T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, U >: O <: RowProduct]
				                      (e :TypedComponentSQL[O, T, R, M, V, U]) =
					if (e.origin.shift >= oldOffset) { //relation is from an enclosing select of oldAggr, not the grouped portion
						val adapted = RelationSQL[F, T, R, F](e.relation, e.origin.shift - oldOffset + groupings)
						val comp = adapted \ e.mapping.withOrigin[F]
						self.component(comp).asInstanceOf[SQLExpression[N, GlobalScope, V]]
					} else { //relation is in the grouped portion. All that needs to be done is to update the index
						val adapted = RelationSQL[N, T, R, N](e.relation, e.origin.shift - oldOffset + newOffset)
						val res = adapted \ e.mapping.withOrigin[N]
						res
					}

				override def component[T[A] <: BaseMapping[R, A], R, M[A] <: ColumnMapping[V, A], V, U >: O <: RowProduct]
				                      (e :TypedColumnComponentSQL[O, T, R, M, V, U]) =
					if (e.origin.shift >= oldOffset) { //relation is from an enclosing select of oldAggr, not the grouped portion
						val adapted = RelationSQL[F, T, R, F](e.relation, e.origin.shift - oldOffset + groupings)
						val comp = adapted \ e.mapping.withOrigin[F]
						self(comp).asInstanceOf[ColumnSQL[N, GlobalScope, V]]
					} else { //relation is in the grouped portion. All that needs to be done is to update the index
						val adapted = RelationSQL[N, T, R, N](e.relation, e.origin.shift - oldOffset + newOffset)
						adapted \ e.mapping.withOrigin[N]
					}

				override def relation[T[A] <: BaseMapping[R, A], R, U >: O <: RowProduct]
				                     (e :RelationSQL[O, T, R, U]) =
					if (e.shift >= oldOffset) {//relation is from an enclosing select of oldAggr, not the grouped portion
						val adapted = RelationSQL[F, T, R, F](e.relation, e.shift - oldOffset + groupings)
						self.relation(adapted).asInstanceOf[SQLExpression[N, GlobalScope, R]]
					} else //relation is in the grouped portion. All that needs to be done is to update the index
						RelationSQL[N, T, R, N](e.relation, e.shift - oldOffset + newOffset)


				override def extended[S <: RowProduct, E <: RowProduct]
				                     (subselect :S, replacement :E)
				                     (implicit oldExt :oldClause.Generalized ExtendedBy S,
				                               newExt :newClause.Generalized ExtendedBy E) =
					group(subselect, replacement, oldOffset + oldExt.length, newOffset + newExt.length, groupings)

			}



		override def subselect[V](e :SubselectColumn[F, V]) :ColumnSQL[G, GlobalScope, Rows[V]] =
			subselect(e :SubselectSQL[F, V]).asInstanceOf[ColumnSQL[G, GlobalScope, Rows[V]]]

		override def subselect[V](e :SubselectSQL[F, V]) :GlobalSQL[G, Rows[V]] = {
			val replacement = e.from match {
				case discrete :FromSome => rebaseSubselect(discrete)//(e.from.outer)
				case grouped :GroupByClause => rebaseGroupedSubselect(grouped)
				case aggregated :Aggregated[_] =>
					val discrete = rebaseSubselect(aggregated.clause)
					val res = Aggregated[discrete.clause.type](discrete.clause)
					implicit val newExtension = res.explicitSpan
					RecursiveScribeSubselectExtension[RowProduct, newClause.Generalized](res)
				case _ =>
					throw new IllegalArgumentException(s"Unexpected FROM clause type of a subselect: ${e.from}.")
			}
			val newSubselect = replacement.clause.generalized
			val oldExtension = replacement.newExtension.asInstanceOf[oldClause.Generalized ExtendedBy e.From]
			val scribe = extended(e.from, newSubselect)(oldExtension, replacement.newExtension)
			scribe(e.selectClause).selectFrom(newSubselect).asInstanceOf[GlobalSQL[G, Rows[V]]]
		}


		//todo: rewrite it with some sort of generic dispatch, because it's fugly
		/** Given a clause `subselect` such that `oldClause.Generalized ExtendedBy subselect.Generalized` and
		  * the first 'join' of the extension is a `Subselect`, create a new clause, based on `newClause.Generalized`,
		  * which applies all relations and 'joins' from `subselect` following `oldClause`, including rewriting all
		  * join conditions (`filter` properties) in it.
		  */
		private def rebaseSubselect(subselect :FromSome)
				:RecursiveScribeSubselectExtension[FromSome, newClause.Generalized] =
			subselect match {
				case j :Join[_, _] =>
					val join = j.asInstanceOf[FromSome Join MappingAt]
					val sub = rebaseSubselect(join.left)
					val newExtension = sub.newExtension.extend[join.LastMapping]
					val oldExtension = newExtension.asInstanceOf[oldClause.Generalized ExtendedBy join.Generalized]
					val unfiltered = join.withLeft[sub.clause.type](sub.clause)(True) //todo: condition from a function

					val scribe = extended(join.generalized, unfiltered.generalized)(oldExtension, newExtension)
					val res = join.withLeft[sub.clause.type](sub.clause)(scribe(join.condition))
					RecursiveScribeSubselectExtension(res)(newExtension)

				case j :Subselect[_, _] if newClause.nonEmpty =>
					val join = j.asInstanceOf[(oldClause.Generalized with NonEmptyFrom) Subselect MappingAt]
					val base = newClause.asInstanceOf[FromSome]
					val unfiltered = join.withLeft[base.type](base)(True)
					implicit val extension = unfiltered.explicitSpan
						.asInstanceOf[newClause.Generalized ExtendedBy unfiltered.Generalized]
					implicit val oldExtension = extension.asInstanceOf[oldClause.Generalized ExtendedBy join.Generalized]
					val scribe = extended(join.generalized, unfiltered.generalized) //todo: condition from a function
					val res = join.withLeft[base.type](base)(scribe(join.condition))
						.asInstanceOf[FromSome { type Implicit = newClause.Generalized }]
					val resExtension = extension.asInstanceOf[newClause.Generalized ExtendedBy res.Generalized]
					RecursiveScribeSubselectExtension(res)(resExtension)

				case j :Subselect[_, _] => //newClause :Dual => subselect becomes a free select
					val join = j.asInstanceOf[(oldClause.Generalized with FromSome) Subselect MappingOf[Any]#TypedProjection]
					val unfiltered = From[MappingOf[Any]#TypedProjection, Any](join.right)
					implicit val extension = unfiltered.explicitSpan
						.asInstanceOf[newClause.Generalized ExtendedBy unfiltered.Generalized]
					implicit val oldExtension = extension.asInstanceOf[oldClause.Generalized ExtendedBy join.Generalized]
					val scribe = extended(join.generalized, unfiltered.generalized)
					val condition = newClause.filter.asInstanceOf[GlobalBoolean[RowProduct]] && scribe(join.condition)
					val res = From[MappingOf[Any]#TypedProjection, Any](join.right, condition)
						.asInstanceOf[FromSome { type Implicit = newClause.Generalized }]
					RecursiveScribeSubselectExtension(res)(extension.asInstanceOf[newClause.Generalized ExtendedBy res.Generalized])

				case d :FromSomeDecorator[_] =>
					val wrap = d.asInstanceOf[FromSomeDecorator[FromSome]]
					val sub = rebaseSubselect(wrap.clause)
					val res = wrap.withClause(sub.clause.asInstanceOf[wrap.clause.FromLast])
						.asInstanceOf[FromSome { type Implicit = newClause.Generalized }]
					val newExtension = sub.newExtension.asInstanceOf[newClause.Generalized ExtendedBy res.Generalized]
					RecursiveScribeSubselectExtension(res)(newExtension)

				//cases with From/Dual are covered by the topSelect rather than subselect and this method is not called.
				case bogus =>
					throw new IllegalArgumentException(
						s"Unsupported clause in a subselect: $bogus.\nTransplanting a subselect of $oldClause\n onto $newClause."
					)
			}


		private def rebaseGroupedSubselect(subselect :GroupByClause)
				:RecursiveScribeSubselectExtension[GroupByClause, newClause.Generalized] =
			subselect match {
				case j :By[_, _] => //same as Join, really. A pity we can't implement polymorphism without implicits
					val join = j.asInstanceOf[GroupByClause By MappingAt]
					val sub = rebaseGroupedSubselect(join.left)
					val newExtension = sub.newExtension.extend[join.LastMapping]
					val oldExtension = newExtension.asInstanceOf[oldClause.Generalized ExtendedBy join.Generalized]
					val unfiltered = join.unsafeLeftSwap[sub.clause.type](sub.clause)(True)

					val scribe = extended(join.generalized, unfiltered.generalized)(oldExtension, newExtension)
					val res = join.unsafeLeftSwap[sub.clause.type](sub.clause)(scribe(join.condition))
					RecursiveScribeSubselectExtension(res)(newExtension)

				case j :GroupBy[_, _] =>
					val join = j.asInstanceOf[FromSome GroupBy MappingAt]
					val sub = rebaseSubselect(join.left)
					val unfiltered = join.unsafeLeftSwap[sub.clause.type](sub.clause)(True)
					implicit val newExtension = unfiltered.explicitSpan //this part differs from other cases
					implicit val oldExtension = newExtension.asInstanceOf[oldClause.Generalized ExtendedBy join.Generalized]

					val scribe = extended(join.generalized, unfiltered.generalized)
					val res = join.unsafeLeftSwap[sub.clause.type](sub.clause)(scribe(join.condition))
					RecursiveScribeSubselectExtension(res)

				case bogus =>
					throw new IllegalArgumentException(
						s"Unsupported clause in a subselect: $bogus.\nTransplanting a subselect of $oldClause\n onto $newClause."
					)
			}



		/** Factory method for compatible rewriters extending their implementation to subselect clauses extending
		  * the input clause `F`.
		  * @param subselectClause a subselect of `F`, that is a ''from'' clause of type `F Subselect ...`.
		  * @param replacementClause a subselect of the output clause `G`, that is a ''from'' clause of type
		  *                          `F Subselect ...`, which contains all the same relations as `S`
		  *                          in its subselect span (`Inner`/`Explicit`), but with the last join condition
		  *                          uninitialized/unchanged.
		  */
		protected def extended[S <: RowProduct, E <: RowProduct]
		                      (subselectClause :S, replacementClause :E)
		                      (implicit oldExt :oldClause.Generalized ExtendedBy S,
		                                newExt :newClause.Generalized ExtendedBy E) :SQLScribe[S, E]

	}



	private trait RecursiveScribeSubselectExtension[+U <: RowProduct, E <: RowProduct] {
		val clause :U { type Implicit = E }
		implicit val newExtension :E ExtendedBy clause.Generalized
	}

	private def RecursiveScribeSubselectExtension[U <: RowProduct, E <: RowProduct]
	            (result :U { type Implicit = E })(implicit extension :E ExtendedBy result.Generalized)
			:RecursiveScribeSubselectExtension[U, E] =
		new RecursiveScribeSubselectExtension[U, E] {
			override val clause :result.type = result
			override val newExtension :E ExtendedBy clause.Generalized = extension
		}







	/** Base `SQLScribe` trait for implementations substituting one or more of the relations in the input clause
	  * with mappings containing the replaced mappings as their components. The only remaining method left to implement
	  * by subclasses is [[net.noresttherein.oldsql.sql.mechanics.SQLScribe.SubstituteComponents.relation relation]],
	  * return type of which has been narrowed down to a `ComponentSQL`. The handler methods for `TypedComponentSQL` and
	  * `TypedColumnComponentSQL` in this trait assume that the substitute relation also contains the original component
	  * and simply graft it onto the new relation.
	  */
	trait SubstituteComponents[+F <: RowProduct, -G <: RowProduct] extends RecursiveScribe[F, G] { self =>

		override def component[T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedComponentSQL[F, T, R, M, V, O]) :GlobalSQL[G, V] =
		{
			val table = relation(e.origin).origin.asInstanceOf[RelationSQL[G, T, R, G]]
			if (table eq e.origin)
				e.asInstanceOf[TypedComponentSQL[G, T, R, M, V, G]]
			else
				(table \ e.mapping.withOrigin[G]).upcast
		}


		override def component[T[A] <: BaseMapping[R, A], R, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedColumnComponentSQL[F, T, R, M, V, O]) :ColumnSQL[G, GlobalScope, V] =
		{
			val table = relation(e.origin).origin.asInstanceOf[RelationSQL[G, T, R, G]]
			if (table eq e.origin)
				e.asInstanceOf[ColumnSQL[G, GlobalScope, V]]
			else
				(table \ e.mapping.withOrigin[G]).upcast
		}



		override def relation[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct](e :RelationSQL[F, T, R, O])
				:ComponentSQL[G, T]

	}






	def replaceRelation[T[X] <: BaseMapping[E, X], E, N[X] <: BaseMapping[V, X], V, F <: RowProduct, G <: RowProduct]
	                   (oldClause :F, newClause :G,
	                    relation :RelationSQL[F, T, E, _ >: F <: RowProduct],
	                    replacement :TypedComponentSQL[G, N, V, T, E, _ >: G <: RowProduct]) :SQLScribe[F, G] =
		new ReplaceRelation[T, E, N, V, F, G](oldClause, newClause)(relation, replacement)


	class ReplaceRelation[T[X] <: BaseMapping[E, X], E, N[X] <: BaseMapping[V, X], V, F <: RowProduct, G <: RowProduct]
	                     (override val oldClause :F, override val newClause :G)
	                     (relation :RelationSQL[F, T, E, _ >: F <: RowProduct],
	                      replacement :TypedComponentSQL[G, N, V, T, E, _ >: G <: RowProduct])
		extends SubstituteComponents[F, G]
	{
		override def relation[M[X] <: BaseMapping[S, X], S, J >: F <: RowProduct](e :RelationSQL[F, M, S, J]) =
			(if (e.shift == relation.shift) replacement else e).asInstanceOf[ComponentSQL[G, M]]


		protected override def extended[S <: RowProduct, H <: RowProduct]
		                               (subselect :S, replacement :H)
		                               (implicit oldExt :ExtendedBy[oldClause.Generalized, S],
		                                         newExt :ExtendedBy[newClause.Generalized, H]) =
		{
			val shift = RelationSQL[S, T, E, S](relation.relation, relation.shift + oldExt.length)
			val newRelation = RelationSQL[H, N, V, H](this.replacement.origin.relation, shift.shift)
			val comp = newRelation \ this.replacement.mapping.asInstanceOf[T[H]]
			new ReplaceRelation[T, E, N, V, S, H](subselect, replacement)(shift, comp)
		}

		override def toString = s"ReplaceRelation($relation with $replacement)($oldClause => $newClause)"
	}






	/** Replaces an unbound parameter, together with all its 'components', with one given as the relation `param`
	  * (with the same shift/position). The value of the new parameter must be derivable from the old one.
	  * This scribe is used when aliasing a JoinParam with the `as` method.
	  */
	private[sql] def replaceParam[F <: RowProduct, T[A] <: FromParam[P, A], P,
	                              G <: RowProduct, M[A] <: FromParam[X, A], X, O >: G <: RowProduct]
	                             (oldClause :F, newClause :G,
	                              oldParam :RelationSQL[F, T, P, _ >: F <: RowProduct],
	                              newParam :RelationSQL[G, M, X, O], substitute :X =?> P)
			:SQLScribe[F, G] =
		new ReplaceParam[F, T, P, G, M, X, O](oldClause, newClause)(oldParam, newParam, substitute)


	private class ReplaceParam[+F <: RowProduct, M[A] <: FromParam[P, A], P,
	                           -G <: RowProduct, N[A] <: FromParam[X, A], X, O >: G <: RowProduct]
	                          (protected[this] override val oldClause :F, protected[this] override val newClause :G)
	                          (oldParam :RelationSQL[F, M, P, _ >: F <: RowProduct],
	                           newParam :RelationSQL[G, N, X, O], extractor :X =?> P)
		extends RecursiveScribe[F, G]
	{
		protected override def extended[S <: RowProduct, E <: RowProduct]
		                               (subselect :S, replacement :E)
		                               (implicit oldExt :oldClause.Generalized ExtendedBy S,
		                                         newExt :newClause.Generalized ExtendedBy E) =
			new ReplaceParam[S, M, P, E, N, X, E](subselect, replacement)(
				RelationSQL[S, M, P, S](oldParam.relation, oldParam.shift + oldExt.length),
				RelationSQL[E, N, X, E](newParam.relation, newParam.shift + newExt.length),
				extractor
			)


		override def component[T[A] <: BaseMapping[R, A], R, C[A] <: ColumnMapping[V, A], V, U >: F <: RowProduct]
		                      (e :TypedColumnComponentSQL[F, T, R, C, V, U]) :ColumnSQL[G, GlobalScope, V] =
			e match {
				case oldParam.mapping(old) if e.origin.shift == oldParam.shift =>
					val column = newParam.mapping.col(extractor andThen old.extract.asInstanceOf[P =?> V])(old.form)
					(newParam \ column)(OriginProjection.isomorphism[MappingOf[V]#ColumnProjection, V, O]).upcast
				case _ =>
					e.asInstanceOf[ColumnSQL[G, GlobalScope, V]]
			}


		override def component[T[A] <: BaseMapping[R, A], R, C[A] <: BaseMapping[V, A], V, U >: F <: RowProduct]
		                      (e :TypedComponentSQL[F, T, R, C, V, U]) :GlobalSQL[G, V] =
			e match {
				case oldParam.mapping(old) if e.origin.shift == oldParam.shift =>
					val component = newParam.mapping(extractor andThen old.extract.asInstanceOf[P =?> V])(old.form)
					(newParam \ component).upcast
				case _ =>
					e.asInstanceOf[GlobalSQL[G, V]]
			}

		override def relation[T[A] <: BaseMapping[R, A], R, U >: F <: RowProduct]
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
	private[sql] def applyParams[F <: RowProduct, N <: RowProduct]
	                             (parameterized :F, parameterless :N)(params :parameterized.Params) :SQLScribe[F, N] =
		new ApplyParams(parameterized, parameterless)(params)


	private class ApplyParams[+F <: RowProduct, -G <: RowProduct] private(
	                          protected[this] override val oldClause :F, protected[this] override val newClause :G,
	                          params :IndexedSeq[Any], followingParams :IndexedSeq[Int])
		extends RecursiveScribe[F, G] //with CaseColumnComponent[F, ColumnResult[G]#T]
	{

		def this(oldClause :F, newClause :G)(params :oldClause.Params) =
			this(oldClause, newClause, params.toList.toIndexedSeq,
				/*{
					def rec(clause :RowProduct) :List[Int] = clause match {
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



		protected override def extended[S <: RowProduct, N <: RowProduct]
		                               (subselect :S, replacement :N)
		                               (implicit oldExt :ExtendedBy[oldClause.Generalized, S],
		                                         newExt :ExtendedBy[newClause.Generalized, N]) :SQLScribe[S, N] =
			new ApplyParams[S, N](subselect, replacement, params, followingParams ++ List.fill(oldExt.length)(0))



		override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: RowProduct]
		                     (e :RelationSQL[F, T, E, O]) :GlobalSQL[G, E] =
			e match {
				case UnboundParamSQL(param, _, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					SQLParameter(param.form)(params(params.length - shift).asInstanceOf[E])
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.shift)
					RelationSQL[G, T, E, G](e.relation, e.shift - shift)
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedComponentSQL[F, T, E, M, V, O]) :GlobalSQL[G, V] =
			e match {
				case UnboundParamSQL(_, extract, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					val rank = params.length - shift
					val param = params(rank)
					extract.get(param.asInstanceOf[E]) match {
						case Some(v) => SQLParameter(extract.export.form)(v)
						case _ => CompositeNull[V](extract.export.form)
					}
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.origin.shift)
					if (shift == 0)
						e.asInstanceOf[GlobalSQL[G, V]]
					else {
						val table = RelationSQL[G, T, E, G](e.relation, e.origin.shift - shift)
						(table \ e.mapping.withOrigin[G]).upcast
					}
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[G, GlobalScope, V] =
			e match {
				case UnboundParamSQL(_, extract, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					extract.get(params(params.length - shift).asInstanceOf[E]) match {
						case Some(v) => SQLParameterColumn(v)(extract.export.form)
						case _ => SQLNull[V](extract.export.form)
					}
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.origin.shift)
					if (shift == 0)
						e.asInstanceOf[ColumnSQL[G, GlobalScope, V]]
					else {
						val table = RelationSQL[G, T, E, G](e.relation, e.origin.shift - shift)
						(table \ e.mapping.withOrigin[G]).upcast
					}
			}


		override def toString = s"ApplyParams($params)($oldClause => $newClause)"
	}





	/** Removes a single unbound parameter with the relation shift given as `idx` (counting from the ''right'')
	  * and all its components with bound parameter(s) of/derived from the given value.
	  */
	private[sql] def applyParam[F <: RowProduct, N <: RowProduct, X]
	                            (from :F, without :N, param :X, idx :Int) :SQLScribe[F, N] =
		new ApplyParam(from, without, param, idx)


	private class ApplyParam[+F <: RowProduct, -G <: RowProduct, X](
	                         protected[this] override val oldClause :F, protected[this] override val newClause :G,
	                         param :X, idx :Int)
		extends RecursiveScribe[F, G] //with CaseColumnComponent[F, ColumnResult[G]#T]
	{

		protected override def extended[S <: RowProduct, N <: RowProduct]
		                               (subselect :S, replacement :N)
		                               (implicit oldExt :ExtendedBy[oldClause.Generalized, S],
		                                         newExt :ExtendedBy[newClause.Generalized, N]) :SQLScribe[S, N] =
			new ApplyParam[S, N, X](subselect, replacement, param, idx + oldExt.length)



		override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: RowProduct]
		                     (e :RelationSQL[F, T, E, O]) :GlobalSQL[G, E] =
			e match {
				case UnboundParamSQL(param, _, this.idx) =>
					SQLParameter(param.form)(this.param.asInstanceOf[E])
				case _ if e.shift < idx =>
					e.asInstanceOf[GlobalSQL[G, E]]
				case _ =>
					RelationSQL[G, T, E, G](e.relation, e.shift - idx)
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedComponentSQL[F, T, E, M, V, O]) :GlobalSQL[G, V] =
			e match {
				case UnboundParamSQL(_, extract, this.idx) =>
					extract.get(param.asInstanceOf[E]) match {
						case Some(v) => SQLParameter(extract.export.form)(v)
						case _ => CompositeNull[V](extract.export.form)
					}
				case _ if e.origin.shift < idx =>
					e.asInstanceOf[GlobalSQL[G, V]]
				case _ =>
					val table = RelationSQL[G, T, E, G](e.relation, e.origin.shift - idx)
					(table \ e.mapping.withOrigin[G]).upcast
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[G, GlobalScope, V] =
			e match {
				case UnboundParamSQL(_, extract, this.idx) =>
					extract.get(param.asInstanceOf[E]) match {
						case Some(v) => SQLParameterColumn(v)(extract.export.form)
						case _ => SQLNull[V](extract.export.form) :SQLNull[V]
					}
				case _ if e.origin.shift < idx =>
					e.asInstanceOf[ColumnSQL[G, GlobalScope, V]]
				case _ =>
					val table = RelationSQL[G, T, E, G](e.relation, e.origin.shift - idx)
					(table \ e.mapping.withOrigin[G]).upcast
			}

		override def toString = s"ApplyParam(#$idx=$param)($oldClause => $newClause)"
	}






	/** A scribe rewriting `SQLExpression` instances based on a ''from'' clause `F` into expressions based on
	  * some its extension clause `E`. It relies on the [[net.noresttherein.oldsql.sql.SQLExpression.extend extend]]
	  * method of `SQLExpression` and recursively applies itself to parts of composite expressions and subselects of `F`.
	  */
	def rebase[F <: RowProduct, E <: RowProduct](clause :E)(implicit extension :F PartOf E) :SQLScribe[F, E] =
		new RebaseExpression[F, E](clause)

	private class RebaseExpression[+F <: RowProduct, E <: RowProduct](clause :E)(implicit extension :F PartOf E)
		extends CaseExpression[F, ExpressionResult[E]#T] with CaseColumn[F, ColumnResult[E]#T]
		   with AbstractSQLScribe[F, E] //overrides the catch-all from the preceding traits
	{
		override def expression[S >: LocalScope <: GlobalScope, X](e :SQLExpression[F, S, X]) =
			e.basedOn(clause)

		override def column[S >: LocalScope <: GlobalScope, X](e :ColumnSQL[F, S, X]) :ColumnSQL[E, S, X] =
			e.basedOn(clause)


		override def toString = s"Rebase[to $clause]"
	}



	/** A scribe rewriting `SQLExpression` instances based on a ''from'' clause `F` into expressions based on
	  * some its extension clause `E`. It relies on the [[net.noresttherein.oldsql.sql.SQLExpression.extend extend]]
	  * method of `SQLExpression` and recursively applies itself to parts of composite expressions and subselects of `F`.
	  * For this reason it can only be applied to global expressions, i.e. those for which
	  * [[net.noresttherein.oldsql.sql.SQLExpression.asGlobal asGlobal]] returns `Some`. For all other expressions
	  * it will throw an `IllegalArgumentException`
	  */
	def extend[F <: RowProduct, E <: RowProduct](clause :E)(implicit extension :F ExtendedBy E) :SQLScribe[F, E] =
		new ExtendExpression[F, E](clause)


	private class ExtendExpression[+F <: RowProduct, E <: RowProduct](clause :E)(implicit extension :F ExtendedBy E)
		extends CaseExpression[F, ExpressionResult[E]#T] with CaseColumn[F, ColumnResult[E]#T]
		   with AbstractSQLScribe[F, E] //overrides the catch-all from the preceding traits
	{
		private def nonGlobal(e :SQLExpression.*) :Nothing =
			throw new IllegalArgumentException(
				s"Cannot extend non global expression $e by ${extension.length} relations to to clause $clause."
			)

		override def apply[S >: LocalScope <: GlobalScope, V](e :SQLExpression[F, S, V]) :GlobalSQL[E, V] =
			e.asGlobal match {
				case Some(global) => global.applyTo(this)
				case _ => nonGlobal(e)
			}

		override def apply[S >: LocalScope <: GlobalScope, V](e :ColumnSQL[F, S, V]) :ColumnSQL[E, GlobalScope, V] =
			e.asGlobal match {
				case Some(global) => global.applyTo(this)
				case _ => nonGlobal(e)
			}


		override def expression[S >: LocalScope <: GlobalScope, X](e :SQLExpression[F, S, X]) :SQLExpression[E, S, X] =
			e.asGlobal match {
				case Some(global) => global.extend(clause)
				case _ => nonGlobal(e)
			}

		override def column[S >: LocalScope <: GlobalScope, X](e :ColumnSQL[F, S, X]) :ColumnSQL[E, S, X] =
			e.asGlobal match {
				case Some(global) => global.extend(clause)
				case _ => nonGlobal(e)
			}


		override def toString = s"Extend[$clause]"
	}






	/** Replaces all occurrences of [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent LooseComponent]] class
	  * in the expression with a appropriate [[net.noresttherein.oldsql.sql.ast.MappingSQL.TypedComponentSQL TypedComponentSQL]]
	  * counterpart using the relation from `F` at the offset specified by the free component.
	  */
	def anchorLooseComponents[F <: RowProduct](from :F) :SQLScribe[F, F] = new AnchorLooseComponents(from)


	private class AnchorLooseComponents[F <: RowProduct](from :F)
		extends ExpressionMatcher[F, ExpressionResult[F]#T]// with CaseMapping[F, ExpressionResult[F]#T]
		   with CaseQuery[F, ExpressionResult[F]#T] with CaseColumnQuery[F, ColumnResult[F]#T]
		   with CaseMapping[F, ExpressionResult[F]#T] with CaseColumnComponent[F, ColumnResult[F]#T]
		   with AbstractSQLScribe[F, F]
	{ //todo: ugly! make anchoring a method in SQLExpression.

		override def aggregate[D <: FromSome, X, V](e :AggregateSQL[D, F, X, V]) :ColumnSQL[F, LocalScope, V] = {
			val aggregate = from.asInstanceOf[AggregateClause]
			val arg = e.arg.asInstanceOf[ColumnSQL[aggregate.Discrete, LocalScope, V]]
			val grounded = anchorLooseComponents[aggregate.Discrete](aggregate.fromClause)(arg)
			AggregateSQL(e.function, grounded, e.isDistinct)(e.readForm).asInstanceOf[ColumnSQL[F, LocalScope, V]]
		}

		override def mapping[S >: LocalScope <: GlobalScope, M[O] <: MappingAt[O]]
		                    (e :MappingSQL[F, S, M]) :SQLExpression[F, S, M[()]#Subject] = e

		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[F, GlobalScope, V] = e

		override def looseComponent[J >: F <: RowProduct, M[A] <: BaseMapping[X, A], X]
		                          (e :LooseComponent[J, M, X]) :GlobalSQL[F, X] =
		{
			val table = from.fullTableStack(e.shift).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, J]]
			(table \ e.mapping).upcast
		}

		override def looseComponent[J >: F <: RowProduct, M[A] <: ColumnMapping[V, A], V]
		                          (e :LooseColumn[J, M, V]) :ColumnSQL[F, GlobalScope, V] =
		{
			val table = from.fullTableStack(e.shift).asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, J]]
			(table \ e.mapping).upcast
		}

		//these - and free selects - are safe as the grounding must have happened when creating the select.
		override def query[V](e :QuerySQL[F, V]) = e

		override def query[V](e :ColumnQuery[F, V]) = e


		override def expression[S >: LocalScope <: GlobalScope, X](e :SQLExpression[F, S, X]) = unhandled(e)

		override def column[S >: LocalScope <: GlobalScope, X](e :ColumnSQL[F, S, X]) = unhandled(e)


		override def toString = s"AnchorLooseComponents($from)"
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
	private[sql] def shiftBack[F <: RowProduct, G <: RowProduct]
	                          (old :F, extending :G, extension :Int, subselectSize :Int) :SQLScribe[F, G] =
		new SubstituteComponents[F, G] {
			protected[this] override val oldClause = old
			protected[this] override val newClause = extending

			private[this] val relations = extending.fullTableStack.to(Array)

			override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: RowProduct](e :RelationSQL[F, T, E, O])
					:ComponentSQL[G, T] =
				if (e.shift < subselectSize) e.asInstanceOf[RelationSQL[G, T, E, G]]
				else relations(e.shift + extension).asInstanceOf[RelationSQL[G, T, E, G]]


			protected override def extended[S <: RowProduct, N <: RowProduct]
			                               (subselect :S, replacement :N)
			                               (implicit oldExt :oldClause.Generalized ExtendedBy S,
			                                         newExt :newClause.Generalized ExtendedBy N) =
				shiftBack[S, N](subselect, replacement, extension, subselectSize + oldExt.length)
		}


}


