package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.ColumnMapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.{AggregateClause, Aggregated, By, ColumnSQL, From, FromSome, GlobalBoolean, GroupBy, GroupByClause, Join, RowProduct, SQLExpression, Subselect}
import net.noresttherein.oldsql.sql.ColumnSQL.{CaseColumn, ColumnVisitor, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.CompositeColumnSQL.CaseCompositeColumn
import net.noresttherein.oldsql.sql.DecoratedFrom.FromSomeDecorator
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, NonEmptyFrom, PartOf}
import net.noresttherein.oldsql.sql.SQLExpression.{CaseExpression, CompositeSQL, ExpressionMapper, ExpressionVisitor, GlobalScope, GlobalSQL, LocalScope}
import net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.CaseComposite
import net.noresttherein.oldsql.sql.UnboundParam.{FromParam, UnboundParamSQL}
import net.noresttherein.oldsql.sql.ast.{AggregateSQL, MappingSQL, QuerySQL, SQLTerm}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{ComponentSQL, LooseColumn, LooseComponent, RelationSQL, TypedColumnComponentSQL, TypedComponentSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.RelationSQL.CaseRelation
import net.noresttherein.oldsql.sql.ast.QuerySQL.{CaseColumnQuery, CaseQuery, ColumnQuery, Rows}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{CaseTopSelect, CaseTopSelectColumn, SubselectColumn, SubselectSQL, TopSelectColumn, TopSelectSQL}
import net.noresttherein.oldsql.sql.ast.SQLTerm.{CaseTerm, ColumnTerm, CompositeNull, SQLNull, SQLParameter, SQLParameterColumn, True}
import net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm.CaseColumnTerm
import net.noresttherein.oldsql.sql.mechanics.SQLScribe.ExpressionResult






/** An expression rewriter translating any [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, T]`
  * for the input ''from'' clause `F`, into an `SQLExpression[R, S, T]` for the output ''from'' clause `R`.
  * If the expression is a subtype of [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]], the result will
  * also be a `ColumnSQL` of the same value type and scope, but based on the clause `R`.
  * It is implemented through the visitor pattern as defined by the
  * [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionVisitor ExpressionVisitor]] trait.
  * @author Marcin Mościcki
  */
trait SQLScribe[+F <: RowProduct, -R <: RowProduct] extends ExpressionMapper[F, ExpressionResult[R]#T] {
	//overriden so that the column variant is correctly picked from between overloads
	override def apply[S >: LocalScope <: GlobalScope, V](e :SQLExpression[F, S, V]) :SQLExpression[R, S, V]

	def apply[S >: LocalScope <: GlobalScope, V](e :ColumnSQL[F, S, V]) :ColumnSQL[R, S, V]
}





object SQLScribe {
	type ExpressionResult[F <: RowProduct] = { type T[-S >: LocalScope <: GlobalScope, X] = SQLExpression[F, S, X] }
	type ColumnResult[F <: RowProduct] = { type T[-S >: LocalScope <: GlobalScope, X] = ColumnSQL[F, S, X] }



	/** Base `SQLScribe` trait implementing methods for all `SQLExpression` types which do not depend on the clause `F`
	  * (such as terms) as well as for `CompositeSQL` subclasses by recursively applying itself using their
	  * [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL.rephrase rephrase]] method.
	  */
	trait AbstractSQLScribe[+F <: RowProduct, -R <: RowProduct] extends SQLScribe[F, R]
		with ExpressionVisitor[F, ExpressionResult[R]#T] with ColumnVisitor[F, ColumnResult[R]#T]
		with CaseComposite[F, ExpressionResult[R]#T] with CaseCompositeColumn[F, ColumnResult[R]#T]
		with CaseTerm[F, ExpressionResult[R]#T] with CaseColumnTerm[F, ColumnResult[R]#T]
		with CaseTopSelect[F, ExpressionResult[R]#T] with CaseTopSelectColumn[F, ColumnResult[R]#T]
		with CaseRelation[F, ExpressionResult[R]#T]
	{
		override def apply[S >: LocalScope <: GlobalScope, V](e :SQLExpression[F, S, V]) :SQLExpression[R, S, V] =
			e.applyToForwarder(this)

		override def apply[S >: LocalScope <: GlobalScope, V](e :ColumnSQL[F, S, V]) :ColumnSQL[R, S, V] =
			e.applyToForwarder(this)


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
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLScribe.RecursiveScribe.expanded expanded]] method for the subselect's
	  * ''from'' clause. The subselect clause is then transplanted onto the result clause `G` in a way similar
	  * to [[net.noresttherein.oldsql.sql.RowProduct.asSubselectOf RowProduct.asSubselectOf]], rewriting all
	  * join conditions and the subselect header before creating a new `SubselectSQL`.
	  * In order to be able to rewrite subselect expressions, it requires the output clause instance `G` to use
	  * as their `Outer`/`Implicit` portion. This can be achieved either by using this scribe in a `Adjoin`
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

		override def expression[S >: LocalScope <: GlobalScope, V](e :SQLExpression[F, S, V]) :SQLExpression[G, S, V] =
			unhandled(e)

		override def column[S >: LocalScope <: GlobalScope, V](e :ColumnSQL[F, S, V]) :ColumnSQL[G, S, V] =
			unhandled(e)

		override def mapping[S >: LocalScope <: GlobalScope, M[O] <: MappingAt[O]]
		                    (e :MappingSQL[F, S, M]) :SQLExpression[G, S, M[Unit]#Subject] =
			unhandled(e)

		override def query[V](e :QuerySQL[F, V]) :SQLExpression[G, GlobalScope, Rows[V]] = unhandled(e)

		override def query[V](e :ColumnQuery[F, V]) :ColumnSQL[G, GlobalScope, Rows[V]] = unhandled(e)

		override def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[V, A], V]
		                          (e :LooseComponent[O, M, V]) :SQLExpression[G, GlobalScope, V] =
			this(e.anchor(oldClause))

		override def looseComponent[O >: F <: RowProduct, M[A] <: ColumnMapping[V, A], V]
		                          (e :LooseColumn[O, M, V]) :ColumnSQL[G, GlobalScope, V] =
			this(e.anchor(oldClause))


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
		  * @param newGrouped base clause of the output. Initially, when `oldGrouped = oldClause.from`,
		  *                   `newGrouped = newClause.from`. Afterwards, these are both arguments given to
		  *                   `expand` - that is a clause resulting from rebasing the input subselect clause onto their
		  *                   common prefix, `this.oldClause.outer`.
		  * @param oldOffset the index of the rightmost relation from the outer clause of `this.oldClause` (i.e., `F`),
		  *                  but inside clause `O`.
		  * @param newOffset the index of the rightmost relation from the outer clause of `this.newClause` (i.e., `E`),
		  *                  but inside clause `N`. This is the same relation as `oldGrouped.fullTableStack(oldOffset)`.
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
					if (e.origin.offset >= oldOffset) { //relation is from an enclosing select of oldAggr, not the grouped portion
						val rebased = e.moveTo(new TableOffset[F, T](e.origin.offset - oldOffset + groupings))
						self.component(rebased).asInstanceOf[SQLExpression[N, GlobalScope, V]]
					} else  //relation is in the grouped portion. All that needs to be done is to update the index
						e.moveTo(new TableOffset[N, T](e.origin.offset - oldOffset + newOffset))

				override def component[T[A] <: BaseMapping[R, A], R, M[A] <: ColumnMapping[V, A], V, U >: O <: RowProduct]
				                      (e :TypedColumnComponentSQL[O, T, R, M, V, U]) =
					if (e.origin.offset >= oldOffset) { //relation is from an enclosing select of oldAggr, not the grouped portion
						val rebased = e.moveTo(new TableOffset[F, T](e.origin.offset - oldOffset + groupings))
						self(rebased).asInstanceOf[ColumnSQL[N, GlobalScope, V]]
					} else  //relation is in the grouped portion. All that needs to be done is to update the index
						e.moveTo(new TableOffset[N, T](e.origin.offset - oldOffset + newOffset))

				override def relation[T[A] <: BaseMapping[R, A], R, U >: O <: RowProduct]
				                     (e :RelationSQL[O, T, R, U]) =
					if (e.offset >= oldOffset) {//relation is from an enclosing select of oldAggr, not the grouped portion
						val adapted = e.moveTo(new TableOffset[F, T](e.offset - oldOffset + groupings))
						self.relation(adapted).asInstanceOf[SQLExpression[N, GlobalScope, R]]
					} else //relation is in the grouped portion. All that needs to be done is to update the index
						e.moveTo(new TableOffset[N, T](e.offset - oldOffset + groupings))


				override def expanded[S <: RowProduct, E <: RowProduct]
				                     (subselect :S, replacement :E)
				                     (implicit oldExt :oldClause.Generalized ExpandedBy S,
				                      newExt :newClause.Generalized ExpandedBy E) =
					group(subselect, replacement, oldOffset + oldExt.length, newOffset + newExt.length, groupings)

				override def toString = s"$self.group($oldClause, $newClause, $oldOffset, $newOffset, $groupings)"
			}



		override def subselect[V](e :SubselectColumn[F, V]) :ColumnSQL[G, GlobalScope, Rows[V]] =
			subselect(e :SubselectSQL[F, V]).asInstanceOf[ColumnSQL[G, GlobalScope, Rows[V]]]

		override def subselect[V](e :SubselectSQL[F, V]) :GlobalSQL[G, Rows[V]] = {
			val replacement = e.from match { //e.from is non-empty because otherwise it would be a top select
				case discrete :FromSome => rebaseSubselect(discrete)
				case grouped :GroupByClause => rebaseGroupedSubselect(grouped)
				case aggregated :Aggregated[_] =>
					val discrete = rebaseSubselect(aggregated.clause)
					val res = Aggregated[discrete.clause.type](discrete.clause)
					implicit val newExpansion = res.explicitSpan
					RecursiveScribeSubselectExpansion[RowProduct, newClause.Generalized](res)
				case _ => //this will never happen - every RowProduct must be either FromClause, GroupByClause or Aggregated
					throw new IllegalArgumentException(s"Unexpected FROM clause type of a subselect: ${e.from}.")
			}
			val newSubselect = replacement.clause.generalized
			val oldExpansion = replacement.newExpansion.asInstanceOf[oldClause.Generalized ExpandedBy e.From]
			val scribe = expanded(e.from, newSubselect)(oldExpansion, replacement.newExpansion)
			scribe(e.selectClause).selectFrom(newSubselect).asInstanceOf[GlobalSQL[G, Rows[V]]]
		}


		//todo: rewrite it with some sort of generic dispatch, because it's fugly
		/** Given a clause `subselect` such that `oldClause.Generalized ExpandedBy subselect.Generalized` and
		  * the first 'join' of the expansion is a `Subselect`, create a new clause, based on `newClause.Generalized`,
		  * which applies all relations and 'joins' from `subselect` following `oldClause`, including rewriting all
		  * join conditions (`filter` properties) in it.
		  */
		private def rebaseSubselect(subselect :FromSome)
				:RecursiveScribeSubselectExpansion[FromSome, newClause.Generalized] =
			subselect match {
				case j :Join[_, _] =>
					val join = j.asInstanceOf[FromSome Join MappingAt]
					val sub = rebaseSubselect(join.left)
					val newExpansion = sub.newExpansion.expand[join.LastMapping]
					val oldExpansion = newExpansion.asInstanceOf[oldClause.Generalized ExpandedBy join.Generalized]
					val unfiltered = join.withLeft[sub.clause.type](sub.clause)(True) //todo: condition from a function

					val scribe = expanded(join.generalized, unfiltered.generalized)(oldExpansion, newExpansion)
					val res = join.withLeft[sub.clause.type](sub.clause)(scribe(join.condition))
					RecursiveScribeSubselectExpansion(res)(newExpansion)

				case j :Subselect[_, _] if newClause.nonEmpty =>
					val join = j.asInstanceOf[(oldClause.Generalized with NonEmptyFrom) Subselect MappingAt]
					val base = newClause.asInstanceOf[FromSome]
					val unfiltered = join.withLeft[base.type](base)(True)
					implicit val expansion = unfiltered.explicitSpan
						.asInstanceOf[newClause.Generalized ExpandedBy unfiltered.Generalized]
					implicit val oldExpansion = expansion.asInstanceOf[oldClause.Generalized ExpandedBy join.Generalized]
					val scribe = expanded(join.generalized, unfiltered.generalized) //todo: condition from a function
					val res = join.withLeft[base.type](base)(scribe(join.condition))
						.asInstanceOf[FromSome { type Implicit = newClause.Generalized }]
					val resExpansion = expansion.asInstanceOf[newClause.Generalized ExpandedBy res.Generalized]
					RecursiveScribeSubselectExpansion(res)(resExpansion)

				case j :Subselect[_, _] => //newClause :Dual => subselect becomes a free select
					val join = j.asInstanceOf[(oldClause.Generalized with FromSome) Subselect MappingOf[Any]#TypedProjection]
					val unfiltered = From[MappingOf[Any]#TypedProjection, Any](join.right)
					implicit val expansion = unfiltered.explicitSpan
						.asInstanceOf[newClause.Generalized ExpandedBy unfiltered.Generalized]
					implicit val oldExpansion = expansion.asInstanceOf[oldClause.Generalized ExpandedBy join.Generalized]
					val scribe = expanded(join.generalized, unfiltered.generalized)
					val condition = newClause.filter.asInstanceOf[GlobalBoolean[RowProduct]] && scribe(join.condition)
					val res = From[MappingOf[Any]#TypedProjection, Any](join.right, condition)
						.asInstanceOf[FromSome { type Implicit = newClause.Generalized }]
					RecursiveScribeSubselectExpansion(res)(expansion.asInstanceOf[newClause.Generalized ExpandedBy res.Generalized])

				case d :FromSomeDecorator[_] =>
					val wrap = d.asInstanceOf[FromSomeDecorator[FromSome]]
					val sub = rebaseSubselect(wrap.clause)
					val res = wrap.withClause(sub.clause.asInstanceOf[wrap.clause.FromLast])
						.asInstanceOf[FromSome { type Implicit = newClause.Generalized }]
					val newExpansion = sub.newExpansion.asInstanceOf[newClause.Generalized ExpandedBy res.Generalized]
					RecursiveScribeSubselectExpansion(res)(newExpansion)

				//cases with From/Dual are covered by the topSelect rather than subselect and this method is not called.
				case bogus =>
					throw new IllegalArgumentException(
						s"Unsupported clause in a subselect: $bogus.\nTransplanting a subselect of $oldClause\n onto $newClause."
					)
			}


		private def rebaseGroupedSubselect(subselect :GroupByClause)
				:RecursiveScribeSubselectExpansion[GroupByClause, newClause.Generalized] =
			subselect match {
				case j :By[_, _] => //same as Join, really. A pity we can't implement polymorphism without implicits
					val join = j.asInstanceOf[GroupByClause By MappingAt]
					val sub = rebaseGroupedSubselect(join.left)
					val newExpansion = sub.newExpansion.expand[join.LastMapping]
					val oldExpansion = newExpansion.asInstanceOf[oldClause.Generalized ExpandedBy join.Generalized]
					val unfiltered = join.unsafeLeftSwap[sub.clause.type](sub.clause)(True)

					val scribe = expanded(join.generalized, unfiltered.generalized)(oldExpansion, newExpansion)
					val res = join.unsafeLeftSwap[sub.clause.type](sub.clause)(scribe(join.condition))
					RecursiveScribeSubselectExpansion(res)(newExpansion)

				case j :GroupBy[_, _] =>
					val join = j.asInstanceOf[FromSome GroupBy MappingAt]
					val sub = rebaseSubselect(join.left)
					val unfiltered = join.unsafeLeftSwap[sub.clause.type](sub.clause)(True)
					implicit val newExpansion = unfiltered.explicitSpan //this part differs from other cases
					implicit val oldExpansion = newExpansion.asInstanceOf[oldClause.Generalized ExpandedBy join.Generalized]

					val scribe = expanded(join.generalized, unfiltered.generalized)
					val res = join.unsafeLeftSwap[sub.clause.type](sub.clause)(scribe(join.condition))
					RecursiveScribeSubselectExpansion(res)

				case bogus =>
					throw new IllegalArgumentException(
						s"Unsupported clause in a subselect: $bogus.\nTransplanting a subselect of $oldClause\n onto $newClause."
					)
			}



		/** Factory method for compatible rewriters expanding their implementation to subselect clauses expanding
		  * the input clause `F`.
		  * @param subselectClause a subselect of `F`, that is a ''from'' clause of type `F Subselect ...`.
		  * @param replacementClause a subselect of the output clause `G`, that is a ''from'' clause of type
		  *                          `F Subselect ...`, which contains all the same relations as `S`
		  *                          in its subselect span (`Inner`/`Explicit`), but with the last join condition
		  *                          uninitialized/unchanged.
		  */
		protected def expanded[S <: RowProduct, E <: RowProduct]
		                      (subselectClause :S, replacementClause :E)
		                      (implicit oldExt :oldClause.Generalized ExpandedBy S,
		                       newExt :newClause.Generalized ExpandedBy E) :SQLScribe[S, E]

	}



	private trait RecursiveScribeSubselectExpansion[+U <: RowProduct, E <: RowProduct] {
		val clause :U { type Implicit = E }
		implicit val newExpansion :E ExpandedBy clause.Generalized
	}

	private def RecursiveScribeSubselectExpansion[U <: RowProduct, E <: RowProduct]
	            (result :U { type Implicit = E })(implicit expansion :E ExpandedBy result.Generalized)
			:RecursiveScribeSubselectExpansion[U, E] =
		new RecursiveScribeSubselectExpansion[U, E] {
			override val clause :result.type = result
			override val newExpansion :E ExpandedBy clause.Generalized = expansion
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
				e.graft(table)
		}


		override def component[T[A] <: BaseMapping[R, A], R, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedColumnComponentSQL[F, T, R, M, V, O]) :ColumnSQL[G, GlobalScope, V] =
		{
			val table = relation(e.origin).origin.asInstanceOf[RelationSQL[G, T, R, G]]
			if (table eq e.origin)
				e.asInstanceOf[ColumnSQL[G, GlobalScope, V]]
			else
				e.graft(table)
		}



		override def relation[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct](e :RelationSQL[F, T, R, O])
				:ComponentSQL[G, T]

	}





	//used in as/aliased
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
			(if (e.offset == relation.offset) replacement else e).asInstanceOf[ComponentSQL[G, M]]


		protected override def expanded[S <: RowProduct, H <: RowProduct]
		                               (subselect :S, replacement :H)
		                               (implicit oldExt :ExpandedBy[oldClause.Generalized, S],
		                                newExt :ExpandedBy[newClause.Generalized, H]) =
		{
			val shift = RelationSQL[S, T, E, S](relation.relation, relation.offset + oldExt.length)
			val comp = this.replacement.moveTo(new TableOffset[H, N](relation.offset + oldExt.length))
			new ReplaceRelation[T, E, N, V, S, H](subselect, replacement)(shift, comp)
		}

		override def toString = s"ReplaceRelation($relation with $replacement)($oldClause => $newClause)"
	}






	/** Replaces an unbound parameter, together with all its 'components', with one given as the relation `newParam`
	  * (with the same offset/position). The value of the new parameter must be derivable from the old one.
	  * This scribe is used when aliasing a JoinParam with the `as` method.
	  */
	private[sql] def replaceParam[F <: RowProduct, T[A] <: FromParam[P, A], P,
	                              G <: RowProduct, M[A] <: FromParam[X, A], X, O >: G <: RowProduct]
	                             (oldClause :F, newClause :G,
	                              oldParam :RelationSQL[F, T, P, _ >: F <: RowProduct],
	                              newParam :RelationSQL[G, M, X, O])
	                             (substitute :X =?> P)
			:SQLScribe[F, G] =
		new ReplaceParam[F, T, P, G, M, X, O](oldClause, newClause)(oldParam, newParam, substitute)


	private class ReplaceParam[+F <: RowProduct, M[A] <: FromParam[P, A], P,
	                           -G <: RowProduct, N[A] <: FromParam[X, A], X, O >: G <: RowProduct]
	                          (protected[this] override val oldClause :F, protected[this] override val newClause :G)
	                          (oldParam :RelationSQL[F, M, P, _ >: F <: RowProduct],
	                           newParam :RelationSQL[G, N, X, O], extractor :X =?> P)
		extends RecursiveScribe[F, G]
	{
		protected override def expanded[S <: RowProduct, E <: RowProduct]
		                               (subselect :S, replacement :E)
		                               (implicit oldExt :oldClause.Generalized ExpandedBy S,
		                                newExt :newClause.Generalized ExpandedBy E) =
			new ReplaceParam[S, M, P, E, N, X, E](subselect, replacement)(
				oldParam.moveTo(new TableOffset[S, M](oldParam.offset + oldExt.length)),
				newParam.moveTo(new TableOffset[E, N](newParam.offset + newExt.length)),
				extractor
			)


		override def component[T[A] <: BaseMapping[R, A], R, C[A] <: ColumnMapping[V, A], V, U >: F <: RowProduct]
		                      (e :TypedColumnComponentSQL[F, T, R, C, V, U]) :ColumnSQL[G, GlobalScope, V] =
			e match {
				case oldParam.mapping(old) if e.origin.offset == oldParam.offset =>
					val column = newParam.mapping.col(extractor andThen old.extract.asInstanceOf[P =?> V])(old.form)
					(newParam \ column)(OriginProjection.isomorphism[MappingOf[V]#ColumnProjection, V, O]).upcast
				case _ =>
					e.asInstanceOf[ColumnSQL[G, GlobalScope, V]]
			}


		override def component[T[A] <: BaseMapping[R, A], R, C[A] <: BaseMapping[V, A], V, U >: F <: RowProduct]
		                      (e :TypedComponentSQL[F, T, R, C, V, U]) :GlobalSQL[G, V] =
			e match {
				case oldParam.mapping(old) if e.origin.offset == oldParam.offset =>
					val component = newParam.mapping.comp(extractor andThen old.extract.asInstanceOf[P =?> V])(old.form)
					(newParam \ component).upcast
				case _ =>
					e.asInstanceOf[GlobalSQL[G, V]]
			}

		override def relation[T[A] <: BaseMapping[R, A], R, U >: F <: RowProduct]
		                     (e :RelationSQL[F, T, R, U]) :GlobalSQL[G, R] =
			if (e.offset == oldParam.offset)
				newParam.asInstanceOf[GlobalSQL[G, R]]
			else
				e.moveTo(new TableOffset[G, T](e.offset))

		override def toString = s"ReplaceParam($oldParam => $newParam)($oldClause => $newClause)"
	}






	/** Removes all references to unbound statement parameters (synthetic mappings joined with `JoinParam`),
	  * replacing all their components with bound parameter expressions (`SQLParameter`) using the values for `F#Params`
	  * provided as constructor arguments.
	  */ //todo: make it a method in SQLExpression
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



		protected override def expanded[S <: RowProduct, N <: RowProduct]
		                               (subselect :S, replacement :N)
		                               (implicit oldExt :ExpandedBy[oldClause.Generalized, S],
		                                newExt :ExpandedBy[newClause.Generalized, N]) :SQLScribe[S, N] =
			new ApplyParams[S, N](subselect, replacement, params, followingParams ++ List.fill(oldExt.length)(0))



		override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: RowProduct]
		                     (e :RelationSQL[F, T, E, O]) :GlobalSQL[G, E] =
			e match {
				case UnboundParamSQL(param, _, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
						SQLParameter(param.form)(params(params.length - shift).asInstanceOf[E])
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.offset)
					e.moveTo(new TableOffset[G, T](e.offset - shift))
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedComponentSQL[F, T, E, M, V, O]) :GlobalSQL[G, V] =
			e match {
				case UnboundParamSQL(_, extract, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					val rank = params.length - shift
					val param = params(rank)
					extract.opt(param.asInstanceOf[E]) match {
						case Got(v) => SQLParameter(extract.export.form)(v)
						case _ => CompositeNull[V](extract.export.form)
					}
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.origin.offset)
					if (shift == 0)
						e.asInstanceOf[GlobalSQL[G, V]]
					else
						e.moveTo(new TableOffset[G, T](e.origin.offset - shift))
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[G, GlobalScope, V] =
			e match {
				case UnboundParamSQL(_, extract, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					extract.opt(params(params.length - shift).asInstanceOf[E]) match {
						case Got(v) => SQLParameterColumn(v)(extract.export.form)
						case _ => SQLNull[V](extract.export.form)
					}
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.origin.offset)
					if (shift == 0)
						e.asInstanceOf[ColumnSQL[G, GlobalScope, V]]
					else
						e.moveTo(new TableOffset[G, T](e.origin.offset - shift))
			}


		override def toString = s"ApplyParams($params)($oldClause => $newClause)"
	}





	/** Removes a single unbound parameter with the relation offset given as `idx` (counting from the ''right'')
	  * and all its components with bound parameter(s) of/derived from the given value.
	  *///todo: make it a method in SQLExpression
	private[sql] def applyParam[F <: RowProduct, N <: RowProduct, X]
	                            (from :F, without :N, param :X, idx :Int) :SQLScribe[F, N] =
		new ApplyParam(from, without, param, idx)


	private class ApplyParam[+F <: RowProduct, -G <: RowProduct, X](
	                         protected[this] override val oldClause :F, protected[this] override val newClause :G,
	                         param :X, idx :Int)
		extends RecursiveScribe[F, G] //with CaseColumnComponent[F, ColumnResult[G]#T]
	{

		protected override def expanded[S <: RowProduct, N <: RowProduct]
		                               (subselect :S, replacement :N)
		                               (implicit oldExt :ExpandedBy[oldClause.Generalized, S],
		                                newExt :ExpandedBy[newClause.Generalized, N]) :SQLScribe[S, N] =
			new ApplyParam[S, N, X](subselect, replacement, param, idx + oldExt.length)



		override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: RowProduct]
		                     (e :RelationSQL[F, T, E, O]) :GlobalSQL[G, E] =
			e match {
				case UnboundParamSQL(param, _, this.idx) =>
					SQLParameter(param.form)(this.param.asInstanceOf[E])
				case _ if e.offset < idx =>
					e.asInstanceOf[GlobalSQL[G, E]]
				case _ =>
					e.moveTo(new TableOffset[G, T](e.offset - idx))
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedComponentSQL[F, T, E, M, V, O]) :GlobalSQL[G, V] =
			e match {
				case UnboundParamSQL(_, extract, this.idx) =>
					extract.opt(param.asInstanceOf[E]) match {
						case Got(v) => SQLParameter(extract.export.form)(v)
						case _ => CompositeNull[V](extract.export.form)
					}
				case _ if e.origin.offset < idx =>
					e.asInstanceOf[GlobalSQL[G, V]]
				case _ =>
					e.moveTo(new TableOffset[G, T](e.origin.offset - idx))
			}



		override def component[T[A] <: BaseMapping[E, A], E, M[A] <: ColumnMapping[V, A], V, O >: F <: RowProduct]
		                      (e :TypedColumnComponentSQL[F, T, E, M, V, O]) :ColumnSQL[G, GlobalScope, V] =
			e match {
				case UnboundParamSQL(_, extract, this.idx) =>
					extract.opt(param.asInstanceOf[E]) match {
						case Got(v) => SQLParameterColumn(v)(extract.export.form)
						case _ => SQLNull[V](extract.export.form) :SQLNull[V]
					}
				case _ if e.origin.offset < idx =>
					e.asInstanceOf[ColumnSQL[G, GlobalScope, V]]
				case _ =>
					e.moveTo(new TableOffset[G, T](e.origin.offset - idx))
			}

		override def toString = s"ApplyParam(#$idx=$param)($oldClause => $newClause)"
	}




	/** A very simple expression mapper which invokes [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchor]]
	  * on all its arguments, at root [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] and
	  * [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] levels.
	  */
	def anchor[F <: RowProduct](from :F) :SQLScribe[F, F] = new Anchorage[F](from)

	private class Anchorage[F <: RowProduct](from :F)
		extends AbstractSQLScribe[F, F]
		   with CaseExpression[F, ExpressionResult[F]#T] with CaseColumn[F, ColumnResult[F]#T]
	{
		override def expression[S >: LocalScope <: GlobalScope, X](e :SQLExpression[F, S, X]) = e.anchor(from)

		override def column[S >: LocalScope <: GlobalScope, X](e :ColumnSQL[F, S, X]) = e.anchor(from)

		override def toString = s"Anchorage($from)"
	}




	/** A scribe rewriting `SQLExpression` instances based on a ''from'' clause `F` into expressions based on
	  * some its expansion clause `E`. It relies on the [[net.noresttherein.oldsql.sql.SQLExpression.basedOn basedOn]]
	  * method of `SQLExpression` and recursively applies itself to parts of composite expressions and subselects of `F`.
	  */ //unused
	def rebase[F <: RowProduct, E <: RowProduct](clause :E)(implicit expansion :F PartOf E) :SQLScribe[F, E] =
		new RebaseExpression[F, E](clause)

	private class RebaseExpression[+F <: RowProduct, E <: RowProduct](clause :E)(implicit expansion :F PartOf E)
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
	  * some its expansion clause `E`. It relies on the [[net.noresttherein.oldsql.sql.SQLExpression.expand expand]]
	  * method of `SQLExpression` and recursively applies itself to parts of composite expressions and subselects of `F`.
	  * For this reason it can only be applied to global expressions, i.e. those for which
	  * [[net.noresttherein.oldsql.sql.SQLExpression.asGlobal asGlobal]] returns `Some`. For all other expressions
	  * it will throw an `IllegalArgumentException`
	  */
	def expand[F <: RowProduct, E <: RowProduct](clause :E)(implicit expansion :F ExpandedBy E) :SQLScribe[F, E] =
		new ExpandExpression[F, E](clause)


	private class ExpandExpression[+F <: RowProduct, E <: RowProduct](clause :E)(implicit expansion :F ExpandedBy E)
		extends CaseExpression[F, ExpressionResult[E]#T] with CaseColumn[F, ColumnResult[E]#T]
		   with AbstractSQLScribe[F, E] //overrides the catch-all from the preceding traits
	{
		private def nonGlobal(e :SQLExpression.*) :Nothing =
			throw new IllegalArgumentException(
				s"Cannot expand non global expression $e by ${expansion.length} relations to to clause $clause."
			)

		override def apply[S >: LocalScope <: GlobalScope, V](e :SQLExpression[F, S, V]) :GlobalSQL[E, V] =
			e.asGlobal match {
				case Some(global) => this(global)
				case _ => nonGlobal(e)
			}

		override def apply[S >: LocalScope <: GlobalScope, V](e :ColumnSQL[F, S, V]) :ColumnSQL[E, GlobalScope, V] =
			e.asGlobal match {
				case Some(global) => this(global)
				case _ => nonGlobal(e)
			}


		override def expression[S >: LocalScope <: GlobalScope, X](e :SQLExpression[F, S, X]) :SQLExpression[E, S, X] =
			e.asGlobal match {
				case Some(global) => global.expand(clause)
				case _ => nonGlobal(e)
			}

		override def column[S >: LocalScope <: GlobalScope, X](e :ColumnSQL[F, S, X]) :ColumnSQL[E, S, X] =
			e.asGlobal match {
				case Some(global) => global.expand(clause)
				case _ => nonGlobal(e)
			}


		override def toString = s"Expand[$clause]"
	}




	/** An SQL expression rewriter shifting back references to all relations before the last `Subselect` join
	  * by `expansion` positions. Used when a subselect clause is 'transplanted' onto another clause,
	  * expanding the `Implicit` clause of the subselect.
	  * @param old a subselect clause serving as SQL expression base.
	  * @param expanding a new subselect clause with some additional relations inserted between `F#Implicit`
	  *                  and the mapping joined in with a `Subselect` join.
	  * @param expansion the difference in relations number between `F` and `G`.
	  * @param subselectSize number of relations in the explicit ''from'' clause of subselects `F` and `G`.
	  */
	private[sql] def shiftBack[F <: RowProduct, G <: RowProduct]
	                          (old :F, expanding :G, expansion :Int, subselectSize :Int) :SQLScribe[F, G] =
		new SubstituteComponents[F, G] {
			protected[this] override val oldClause = old
			protected[this] override val newClause = expanding

			private[this] val relations = expanding.fullTableStack.to(Array)

			override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: RowProduct](e :RelationSQL[F, T, E, O])
					:ComponentSQL[G, T] =
				if (e.offset < subselectSize) e.asInstanceOf[RelationSQL[G, T, E, G]]
				else relations(e.offset + expansion).asInstanceOf[RelationSQL[G, T, E, G]]


			protected override def expanded[S <: RowProduct, N <: RowProduct]
			                               (subselect :S, replacement :N)
			                               (implicit oldExt :oldClause.Generalized ExpandedBy S,
			                                newExt :newClause.Generalized ExpandedBy N) =
				shiftBack[S, N](subselect, replacement, expansion, subselectSize + oldExt.length)
		}


}


