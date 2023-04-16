package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.exceptions.Bug
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.sql.{AggregateClause, Aggregated, By, ColumnSQL, Expanded, From, FromSome, GroupBy, GroupByClause, Join, RowProduct, SingleBoolean, SingleSQL, SQLExpression, Subselect}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, CaseAnyColumn}
import net.noresttherein.oldsql.sql.DecoratedRow.FromSomeDecorator
import net.noresttherein.oldsql.sql.ParamClause.UnboundParam
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, NonEmptyRow, PartOf}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, CaseAnyExpression, Grouped, Single}
import net.noresttherein.oldsql.sql.SQLExpression.AnyExpressionVisitor.Result
import net.noresttherein.oldsql.sql.ast.{AggregateSQL, BoundColumnParam, BoundParam, ColumnLValueSQL, ColumnMappingSQL, ColumnQuery, ColumnTerm, ComponentSQL, CompositeColumnSQL, CompositeSQL, CompoundSelectColumn, CompoundSelectSQL, GenericColumnComponentSQL, LooseColumn, LooseComponent, LValueSQL, MappingSQL, MultiNull, QuerySQL, RelationSQL, SQLNull, SQLTerm, UnboundParamSQL}
import net.noresttherein.oldsql.sql.ast.ColumnQuery.CaseAnyColumnQuery
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.ColumnTerm.CaseAnyColumnTerm
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.CaseAnyCompositeColumn
import net.noresttherein.oldsql.sql.ast.CompositeSQL.CaseAnyComposite
import net.noresttherein.oldsql.sql.ast.QuerySQL.{CaseAnyQuery, Rows}
import net.noresttherein.oldsql.sql.ast.RelationSQL.CaseAnyRelation
import net.noresttherein.oldsql.sql.ast.SelectColumn.{CaseAnyTopSelectColumn, SubselectColumn, TopSelectColumn}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{CaseAnyTopSelect, SubselectSQL, TopSelectSQL}
import net.noresttherein.oldsql.sql.ast.SQLTerm.CaseAnyTerm






/** An expression rewriter translating any [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, T]`
  * for the input ''from'' clause `F`, into an `SQLExpression[R, S, T]` for the output ''from'' clause `R`.
  * If the expression is a subtype of [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]], the result will
  * also be a `ColumnSQL` of the same value type and scope, but based on the clause `R`.
  *
  * Most classes will likely extend either the minimal
  * [[net.noresttherein.oldsql.sql.mechanics.SQLScribe.AbstractSQLScribe AbstractSQLScribe]], implementing
  * the visitor pattern, as defined
  * by the [[net.noresttherein.oldsql.sql.SQLExpression.AnyExpressionVisitor AnyExpressionVisitor]] trait,
  * or [[net.noresttherein.oldsql.sql.mechanics.SQLScribe.RecursiveScribe RecursiveScribe]]
  * which provides sensible implementations for all standard expression types except
  * for [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] subtypes.
  * @tparam F the clause used as the domain of the expressions accepted as arguments.
  * @tparam R the clause used as the domain of the returned expression(s).
  * @author Marcin Mościcki
  */
trait SQLScribe[+F <: RowProduct, -R <: RowProduct] {
	def apply[S >: Grouped <: Single, V](e :SQLExpression[F, S, V]) :SQLExpression[R, S, V]

	def apply[S >: Grouped <: Single, V](e :ColumnSQL[F, S, V]) :ColumnSQL[R, S, V]
}





object SQLScribe {

	/** Base `SQLScribe` trait implementing as identity methods
	  * for all [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] types which do not depend on the clause `F` -
	  * such as terms - as well as for [[net.noresttherein.oldsql.sql.ast.CompositeSQL CompositeSQL]] subclasses
	  * by recursively applying itself using their [[net.noresttherein.oldsql.sql.ast.CompositeSQL.rephrase rephrase]]
	  * method.
	  * @tparam F the clause used as the domain of the expressions accepted as arguments.
	  * @tparam R the clause used as the domain of the returned expression(s).
	  */
	trait AbstractSQLScribe[+F <: RowProduct, -R <: RowProduct] extends SQLScribe[F, R]
		with AnyExpressionVisitor[F, Result[R]#E] with AnyColumnVisitor[F, Result[R]#C]
		with CaseAnyComposite[F, Result[R]#E] with CaseAnyCompositeColumn[F, Result[R]#C]
		with CaseAnyTerm[Result[R]#E] with CaseAnyColumnTerm[Result[R]#C]
		with CaseAnyTopSelect[F, Result[R]#E] with CaseAnyTopSelectColumn[F, Result[R]#C]
		with CaseAnyRelation[F, Result[R]#E]
	{
		override def apply[S >: Grouped <: Single, V](e :SQLExpression[F, S, V]) :SQLExpression[R, S, V] =
			e.`->visit`(this)

		override def apply[S >: Grouped <: Single, V](e :ColumnSQL[F, S, V]) :ColumnSQL[R, S, V] =
			e.`->visit`(this)


		override def *(e :ColumnSQL[RowProduct, Grouped, Nothing]) :ColumnSQL[R, Grouped, Nothing] = e

		override def composite[S >: Grouped <: Single, X](e :CompositeSQL[F, S, X]) :SQLExpression[R, S, X] =
			e.rephrase(this)

		override def compositeColumn[S >: Grouped <: Single, X](e :CompositeColumnSQL[F, S, X]) :ColumnSQL[R, S, X] =
			e.rephrase(this)

		override def term[X](e :SQLTerm[X]) :SingleSQL[R, X] = e

		override def columnTerm[X](e :ColumnTerm[X]) :ColumnSQL[R, Single, X] = e

		override def topSelect[V](e :TopSelectSQL[V]) :SingleSQL[R, Rows[V]] = e

		override def topSelectColumn[V](e :TopSelectColumn[V]) :ColumnSQL[R, Single, Rows[V]] = e
	}




	/** Base `SQLScribe` trait which uses an identity transformation for all `SQLExpression` types where it is
	  * applicable (such as `SQLTerm` and its subclasses), and recursively applies itself to
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL CompositeSQL]],
	  * [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]] and
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL AggregateSQL]]. The first case relies on
	  * the [[net.noresttherein.oldsql.sql.ast.CompositeSQL.rephrase rephrase]] method to rebuild a composite expression
	  * of the same kind from parts transformed with this instance, while the latter two create a new visitor instance
	  * with its [[net.noresttherein.oldsql.sql.mechanics.SQLScribe.RecursiveScribe.expanded expanded]] and
	  * [[net.noresttherein.oldsql.sql.mechanics.SQLScribe.RecursiveScribe.grouping grouping]] methods
	  * for the subselect's ''from'' clause/the ungrouped clause used as the domain of an argument
	  * to a [[net.noresttherein.oldsql.sql.AggregateFunction AggregateFunction]].
	  * The subselect clause is then transplanted onto the result clause `G` in a way similar
	  * to [[net.noresttherein.oldsql.sql.RowProduct.asSubselectOf RowProduct.asSubselectOf]], rewriting all
	  * join conditions and the subselect header before creating a new `SubselectSQL`.
	  * In order to be able to rewrite subselect expressions, it requires the output clause instance `G` to use
	  * as their `Outer`/`Implicit` portion. This can be achieved either by using this scribe in
	  * a [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] constructor and passing `this` (with an uninitialized
	  * join condition), or it can be a template clause with an empty join condition, as the join conditions
	  * from the outer portion of subselect clauses are not used in the generation of the SQL for the subselect.
	  * @tparam F the clause used as the domain of the expressions accepted as arguments.
	  * @tparam G the clause used as the domain of the returned expression(s).
	  */
	trait RecursiveScribe[+F <: RowProduct, -G <: RowProduct]
		extends CaseAnyQuery[F, Result[G]#E] with CaseAnyColumnQuery[F, Result[G]#C]
		   with AbstractSQLScribe[F, G]
	{ self =>
		protected[this] val oldClause :F
		protected[this] val newClause :G

		override def expression[S >: Grouped <: Single, V](e :SQLExpression[F, S, V]) :SQLExpression[G, S, V] =
			unhandled(e)

		override def column[S >: Grouped <: Single, V](e :ColumnSQL[F, S, V]) :ColumnSQL[G, S, V] =
			unhandled(e)

		override def mapping[S >: Grouped <: Single, M[O] <: MappingAt[O], V]
		                    (e :MappingSQL[F, S, M, V]) :SQLExpression[G, S, V] =
			unhandled(e)

		override def columnMapping[S >: Grouped <: Single, M[O] <: ColumnAt[O], V]
		                          (e :ColumnMappingSQL[F, S, M, V]) :ColumnSQL[G, S, V] =
			unhandled(e)

		//compound selects handles as CompositeSQL
		override def query[V](e :QuerySQL[F, V]) :SQLExpression[G, Single, Rows[V]] = unhandled(e)

		override def columnQuery[V](e :ColumnQuery[F, V]) :ColumnSQL[G, Single, Rows[V]] = unhandled(e)


		override def looseComponent[O >: F <: RowProduct, M[A] <: BaseMapping[V, A], V]
		                          (e :LooseComponent[O, M, V]) :SQLExpression[G, Single, V] =
			this(e.anchor(oldClause))

		override def looseColumn[O >: F <: RowProduct, M[A] <: BaseColumn[V, A], V]
		                          (e :LooseColumn[O, M, V]) :ColumnSQL[G, Single, V] =
			this(e.anchor(oldClause))


		override def aggregate[D <: FromSome, X, Y](e :AggregateSQL[D, F, X, Y]) :ColumnSQL[G, Grouped, Y] =
			oldClause match {
				case oldAggr :AggregateClause => newClause match {
					case newAggr :AggregateClause =>
						val scribe = group(oldAggr.fromClause, newAggr.fromClause)
						val arg = scribe(e.arg.asInstanceOf[ColumnSQL[oldAggr.Discrete, Grouped, Y]])
						val res = AggregateSQL(e.function, arg, e.isDistinct)(e.selectForm)
						res.asInstanceOf[ColumnSQL[G, Grouped, Y]]

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


		protected def group[O <: RowProduct, N <: RowProduct](oldGrouped :O, newGrouped :N) :SQLScribe[O, N] =
			group(oldGrouped, newGrouped, oldGrouped.size, newGrouped.size, oldClause.size, newClause.size)

		/** A scribe rewriting expressions aggregated under the clause `F`. Used for `e.arg` of received instances of
		  * `e :AggregateSQL[F, O, _, _]`, as well as expressions of any subselects of `oldClause.Discrete`.
		  * @param oldUngrouped base clause of input - either `oldClause.Discrete`, or its subselect (possibly indirect).
		  * @param newUngrouped base clause of the output. Initially, when `oldUngrouped = oldClause.fromClause`,
		  *                     `newUngrouped = newClause.fromClause`. Afterwards, these are both arguments given to
		  *                     `expand` - that is a clause resulting from rebasing the input subselect clause
		  *                     onto their common prefix, `this.oldClause.outer`.
		  * @param oldOffset    the index of the rightmost relation from the outer clause of `this.oldClause`
		  *                     (i.e., `F`), but inside clause `O`. Initially, this is the (inner) size of `O`.
		  * @param newOffset    the index of the rightmost relation from the outer clause of `this.newClause`
		  *                     (i.e., `E`), but inside clause `N`. This is the same relation
		  *                     as `oldUngrouped.fullTableStack(oldOffset)`.
		  * @param oldGroupings number of pseudo relations in the ''group by'' clause of `this.oldClause`,
		  *                     that is `this.oldClause.size` (cached).
		  * @param newGroupings number of pseudo relations in the ''group by'' clause of `this.newClause`.
		  */
		protected def group[O <: RowProduct, N <: RowProduct](oldUngrouped :O, newUngrouped :N, oldOffset :Int,
		                                                      newOffset :Int, oldGroupings :Int, newGroupings :Int)
				:SQLScribe[O, N] =
			new RecursiveScribe[O, N] {
				override val oldClause = oldUngrouped
				override val newClause = newUngrouped

				override def component[U >: O <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, L <: RowProduct]
				                      (e :TypedComponentSQL[U, T, R, M, V, L]) :SQLExpression[N, Single, V] =
					if (e.origin.index >= oldOffset) {
						//relation is from an enclosing select of self.oldClause/oldUngrouped, not the grouped portion
						// it can be thus expanded to the expression over F and transcribed as such,
						// then having the index readjusted to reflect its position in N
						val newIndex = e.origin.index - oldOffset + oldGroupings
						val inF = e.moveTo(RelationOffset.unsafe[F, e.FromLast, e.origin.position.Rel, T](newIndex))
						val inG = self.component(inF) //if the result is not a component, we need to shift recursively
						val scribe = shiftBack[G, N](self.newClause, newClause, newOffset - newGroupings, newGroupings)
						scribe(inG)
					} else {  //relation is in the grouped portion. All that needs to be done is to update the index
						val newIndex = e.origin.index - oldOffset + newOffset
						e.moveTo(RelationOffset.unsafe[N, e.FromLast, e.origin.position.Rel, T](newIndex))
					}

				override def columnComponent[U >: O <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
				                            (e :TypedColumnComponentSQL[U, T, R, M, V, L]) :ColumnSQL[N, Single, V] =
					if (e.origin.index >= oldOffset) { //relation is from an enclosing select of oldUngrouped, not the grouped portion
						val newIndex = e.origin.index - oldOffset + oldGroupings
						val inF = e.moveTo(RelationOffset.unsafe[F, e.FromLast, e.origin.position.Rel, T](newIndex))
						val inG = self(inF)
						val scribe = shiftBack[G, N](self.newClause, newClause, newOffset - newGroupings, newGroupings)
						scribe(inG)
					} else {  //relation is in the grouped portion. All that needs to be done is to update the index
						val newIndex = e.origin.index - oldOffset + newOffset
						e.moveTo(RelationOffset.unsafe[N, e.FromLast, e.origin.position.Rel, T](newIndex))
					}

				override def relation[U >: O <: RowProduct, T[A] <: BaseMapping[R, A], R, L <: RowProduct]
				                     (e :RelationSQL[U, T, R, L]) =
					component(e)


				override def expanded[S <: RowProduct, E <: RowProduct]
				                     (subselect :S, replacement :E)
				                     (implicit oldExt :oldClause.Generalized ExpandedBy S,
				                      newExt :newClause.Generalized ExpandedBy E) =
					self.group(
						subselect, replacement,
						oldOffset + oldExt.length, newOffset + newExt.length, oldGroupings, newGroupings
					)

				override def toString =
					s"$self.group($oldClause, $newClause, $oldOffset, $newOffset, $oldGroupings, $newGroupings)"
			}


		override def subselectColumn[V](e :SubselectColumn[F, V]) :ColumnSQL[G, Single, Rows[V]] =
			subselect(e :SubselectSQL[F, V]).asInstanceOf[ColumnSQL[G, Single, Rows[V]]]

		override def subselect[V](e :SubselectSQL[F, V]) :SingleSQL[G, Rows[V]] = {
			val replacement = e.from match { //e.from is non-empty because otherwise it would be a top select
				case discrete :FromSome => rebaseSubselect(discrete)
				case grouped :GroupByClause => rebaseGroupedSubselect(grouped)
				case aggregated :Aggregated[_] =>
					val discrete = rebaseSubselect(aggregated.clause)
					val res = Aggregated[discrete.clause.type](discrete.clause :discrete.clause.type)
					implicit val newExpansion = res.explicitSpan
					RecursiveScribeSubselectExpansion[RowProduct, newClause.Generalized](res)
				case _ => //this will never happen - every RowProduct must be either FromClause, GroupByClause or Aggregated
					throw new IllegalArgumentException(s"Unexpected FROM clause type of a subselect: ${e.from}.")
			}
			val newSubselect = replacement.clause.generalized
			val oldExpansion = replacement.newExpansion.asInstanceOf[oldClause.Generalized ExpandedBy e.From]
			val scribe = expanded(e.from, newSubselect)(oldExpansion, replacement.newExpansion)
			scribe(e.selectClause).selectFrom(newSubselect).asInstanceOf[SingleSQL[G, Rows[V]]]
		}


		//todo: rewrite it with some sort of a generic dispatch, because it's fugly
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
					val join = j.asInstanceOf[(oldClause.Generalized with NonEmptyRow) Subselect MappingAt]
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
					val condition = newClause.filter.asInstanceOf[SingleBoolean[RowProduct]] && scribe(join.condition)
					val res = From[MappingOf[Any]#TypedProjection, Any](join.right, condition)
						.asInstanceOf[FromSome { type Implicit = newClause.Generalized }]
					RecursiveScribeSubselectExpansion(res)(expansion.asInstanceOf[newClause.Generalized ExpandedBy res.Generalized])

				case d :FromSomeDecorator[_] =>
					val sub = rebaseSubselect(d.clause)
					val res = d.decorate(sub.clause.asInstanceOf[d.clause.FromLast])
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
					type M[O] = BaseMapping[Any, O]
					val join = j.asInstanceOf[GroupByClause By M]
					val sub = rebaseGroupedSubselect(join.left)
					val newExpansion = sub.newExpansion.expand[join.LastMapping]
					val oldExpansion = newExpansion.asInstanceOf[oldClause.Generalized ExpandedBy join.Generalized]
					val res = join.rewriteLeft[sub.clause.type](sub.clause)(
						group[join.Discrete, sub.clause.GeneralizedDiscrete](join.fromClause, sub.clause.fromClause),
						expanded(join.generalized, _)(oldExpansion, newExpansion)
					)
					RecursiveScribeSubselectExpansion(res)(newExpansion)

				case j :GroupBy[_, _] =>
					type M[O] = BaseMapping[Any, O]
					val join = j.asInstanceOf[FromSome GroupBy M]
					val sub = rebaseSubselect(join.left)
					implicit val oldExpansion = //==1
						join.explicitSpan.asInstanceOf[oldClause.Generalized ExpandedBy join.Generalized]
					implicit val newExpansion =
						oldExpansion.asInstanceOf[sub.clause.Implicit ExpandedBy (sub.clause.Generalized GroupBy M)]

					val res = join.rewriteLeft[sub.clause.type](sub.clause)(
						group[join.Discrete, sub.clause.Generalized](join.fromClause, sub.clause.generalized),
						expanded(join.generalized, _)(oldExpansion, newExpansion)
					)
					RecursiveScribeSubselectExpansion(res)(newExpansion)

				case bogus =>
					throw new IllegalArgumentException(
						s"Unsupported clause in a subselect: $bogus when transplanting a subselect of $oldClause\n onto $newClause."
					)
			}


		/** Factory method for compatible rewriters expanding their implementation to subselect clauses expanding
		  * the input clause `F`.
		  * @param subselectClause   a subselect of `F`, that is a ''from'' clause of type `F Subselect ...`.
		  * @param replacementClause a subselect of the output clause `G`, that is a ''from'' clause of type
		  *                          `G Subselect ...`, which contains all the same relations as `S`
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

		override def component[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseMapping[V, A], V, L <: RowProduct]
		                      (e :TypedComponentSQL[O, T, R, M, V, L]) :SingleSQL[G, V] =
		{
			val table = relation(e.origin).origin.asInstanceOf[RelationSQL[G, T, R, L]]
			if (table eq e.origin)
				e.asInstanceOf[TypedComponentSQL[G, T, R, M, V, L]]
			else
				e.graft(table)
		}
		override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
		                            (e :TypedColumnComponentSQL[O, T, R, M, V, L]) :ColumnSQL[G, Single, V] =
		{
			val table = relation(e.origin).origin.asInstanceOf[RelationSQL[G, T, R, L]]
			if (table eq e.origin)
				e.asInstanceOf[ColumnSQL[G, Single, V]]
			else
				e.graft(table)
		}
		override def relation[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, L <: RowProduct]
		                     (e :RelationSQL[O, T, R, L]) :ComponentSQL[G, T]

	}




	//used in as/aliased
	def replaceRelation[T[X] <: BaseMapping[E, X], E, N[X] <: BaseMapping[V, X], V, L <: RowProduct,
	                    F <: RowProduct, G <: RowProduct]
	                   (oldClause :F, newClause :G, relation :RelationSQL[F, T, E, L],
	                    replacement :TypedComponentSQL[G, N, V, T, E, L]) :SQLScribe[F, G] =
		new ReplaceRelation[T, E, N, V, L, F, G](oldClause, newClause)(relation, replacement)


	class ReplaceRelation[T[X] <: BaseMapping[E, X], E, N[X] <: BaseMapping[V, X], V,
	                      L <: RowProduct, F <: RowProduct, G <: RowProduct]
	                     (override val oldClause :F, override val newClause :G)
	                     (relation :RelationSQL[F, T, E, L], replacement :TypedComponentSQL[G, N, V, T, E, L])
		extends SubstituteComponents[F, G]
	{
		override def relation[J >: F <: RowProduct, M[X] <: BaseMapping[S, X], S, FL <: RowProduct]
		                     (e :RelationSQL[J, M, S, FL]) =
			(if (e.index == relation.index) replacement else e).asInstanceOf[ComponentSQL[G, M]]


		protected override def expanded[S <: RowProduct, H <: RowProduct]
		                               (subselect :S, replacement :H)
		                               (implicit oldExt :ExpandedBy[oldClause.Generalized, S],
		                                newExt :ExpandedBy[newClause.Generalized, H]) =
		{
			val r = this.replacement
			val index = relation.index + oldExt.length
			val shift = relation.moveTo(RelationOffset.unsafe[S, L, relation.position.Rel, T](index))
			val comp = r.moveTo(RelationOffset.unsafe[H, r.FromLast, r.origin.position.Rel, N](index))
			new ReplaceRelation[T, E, N, V, L, S, H](subselect, replacement)(shift, comp)
		}

		override def toString = s"ReplaceRelation($relation with $replacement)($oldClause => $newClause)"
	}




	/** Replaces an unbound parameter, together with all its 'components', with one given as the relation `newParam`
	  * (with the same offset/position). The value of the new parameter must be derivable from the old one.
	  * This scribe is used when aliasing a JoinParam with the `as` method.
	  */
	private[sql] def replaceParam[F1 <: RowProduct, M1[A] <: UnboundParam[X1, A], X1, L1 <: RowProduct,
	                              F2 <: RowProduct, M2[A] <: UnboundParam[X2, A], X2, L2 <: RowProduct]
	                             (oldClause :F1, newClause :F2,
	                              oldParam :RelationSQL[_ >: F1, M1, X1, L1],
	                              newParam :RelationSQL[_ >: F2, M2, X2, L2])
	                             (substitute :X2 =?> X1)
			:SQLScribe[F1, F2] =
		new ReplaceParam[F1, M1, X1, L1, F2, M2, X2, L2](oldClause, newClause)(oldParam, newParam, substitute)


	private class ReplaceParam[+F1 <: RowProduct, M1[A] <: UnboundParam[X1, A], X1, L1 <: RowProduct,
	                           -F2 <: RowProduct, M2[A] <: UnboundParam[X2, A], X2, L2 <: RowProduct]
	                          (protected[this] override val oldClause :F1, protected[this] override val newClause :F2)
	                          (oldParam :RelationSQL[_ >: F1, M1, X1, L1],
	                           newParam :RelationSQL[_ >: F2, M2, X2, L2], extractor :X2 =?> X1)
		extends RecursiveScribe[F1, F2]
	{
		protected override def expanded[S <: RowProduct, E <: RowProduct]
		                               (subselect :S, replacement :E)
		                               (implicit oldExt :oldClause.Generalized ExpandedBy S,
		                                newExt :newClause.Generalized ExpandedBy E) =
			new ReplaceParam[S, M1, X1, L1, E, M2, X2, L2](subselect, replacement)(
				oldParam.moveTo(
					RelationOffset.unsafe[S, L1, oldParam.position.Rel, M1](oldParam.index + oldExt.length)
				),
				newParam.moveTo(
					RelationOffset.unsafe[E, L2, newParam.position.Rel, M2](newParam.index + newExt.length)
				),
				extractor
			)


		override def columnComponent[U >: F1 <: RowProduct, T[A] <: BaseMapping[R, A], R, C[A] <: BaseColumn[V, A], V, L <: RowProduct]
		                            (e :TypedColumnComponentSQL[U, T, R, C, V, L]) :ColumnSQL[F2, Single, V] =
			e match {
				case oldParam.mapping.components(old) if e.origin.index == oldParam.index =>
					val column = newParam.mapping.col(extractor andThen old.extract.asInstanceOf[X1 =?> V])(old.form)
					newParam \ column
				case _ =>
					e.asInstanceOf[ColumnSQL[F2, Single, V]]
			}


		override def component[U >: F1 <: RowProduct, T[A] <: BaseMapping[R, A], R, C[A] <: BaseMapping[V, A], V, L <: RowProduct]
		                      (e :TypedComponentSQL[U, T, R, C, V, L]) :SingleSQL[F2, V] =
			e match {
				case oldParam.mapping.components(old) if e.origin.index == oldParam.index =>
					val component = newParam.mapping.comp(extractor andThen old.extract.asInstanceOf[X1 =?> V])(old.form)
					(newParam \ component)//.upcast
				case _ =>
					e.asInstanceOf[SingleSQL[F2, V]]
			}

		override def relation[U >: F1 <: RowProduct, T[A] <: BaseMapping[R, A], R, L <: RowProduct]
		                     (e :RelationSQL[U, T, R, L]) :SingleSQL[F2, R] =
			if (e.index == oldParam.index)
				newParam.asInstanceOf[SingleSQL[F2, R]]
			else
				e.moveTo(RelationOffset.unsafe[F2, e.FromLast, e.position.Rel, T](e.index))

		override def toString = s"ReplaceParam($oldParam => $newParam)($oldClause => $newClause)"
	}






	/** Removes all references to unbound statement parameters (synthetic mappings joined with `JoinParam`),
	  * replacing all their components with bound parameter expressions (`BoundParam`) using the values for `F#Params`
	  * provided as constructor arguments.
	  */ //todo: make it a method in SQLExpression
	private[sql] def applyParams[F <: RowProduct, N <: RowProduct]
	                             (parameterized :F, parameterless :N)(params :parameterized.Params) :SQLScribe[F, N] =
		new ApplyParams(parameterized, parameterless)(params)

	//fixme: this falls apart if a param is under grouping
	private class ApplyParams[+F <: RowProduct, -G <: RowProduct] private(
	                          protected[this] override val oldClause :F, protected[this] override val newClause :G,
	                          params :IndexedSeq[Any], followingParams :IndexedSeq[Int])
		extends RecursiveScribe[F, G] //with CaseAnyColumnComponent[F, ColumnResult[G]#T]
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

		override def relation[O >: F <: RowProduct, T[A] <: BaseMapping[E, A], E, L <: RowProduct]
		                     (e :RelationSQL[O, T, E, L]) :SingleSQL[G, E] =
			e match {
				case UnboundParamSQL(param, _, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
						BoundParam(param.form, params(params.length - shift).asInstanceOf[E])
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.index)
					e.moveTo(RelationOffset.unsafe[G, e.FromLast, e.position.Rel, T](e.index - shift))
			}

		override def component[O >: F <: RowProduct, T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, L <: RowProduct]
		                      (e :TypedComponentSQL[O, T, E, M, V, L]) :SingleSQL[G, V] =
			e match {
				case UnboundParamSQL(_, extract, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					val rank = params.length - shift
					val param = params(rank)
					extract.opt(param.asInstanceOf[E]) match {
						case Got(v) => BoundParam(extract.export.form, v)
						case _ => MultiNull[V](extract.export.form)
					}
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.origin.index)
					if (shift == 0)
						e.asInstanceOf[SingleSQL[G, V]]
					else
						e.moveTo(RelationOffset.unsafe[G, e.FromLast, e.origin.position.Rel, T](e.origin.index - shift))
			}

		override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[E, A], E, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
		                            (e :TypedColumnComponentSQL[O, T, E, M, V, L]) :ColumnSQL[G, Single, V] =
			e match {
				case UnboundParamSQL(_, extract, idx) =>
					val shift = followingParams(followingParams.length - 1 - idx)
					extract.opt(params(params.length - shift).asInstanceOf[E]) match {
						case Got(v) => BoundColumnParam(extract.export.form, v)
						case _ => SQLNull[V](extract.export.form)
					}
				case _ =>
					val shift = followingParams(followingParams.length - 1 - e.origin.index)
					if (shift == 0)
						e.asInstanceOf[ColumnSQL[G, Single, V]]
					else
						e.moveTo(RelationOffset.unsafe[G, e.FromLast, e.origin.position.Rel, T](e.origin.index - shift))
			}

		override def toString = s"ApplyParams($params)($oldClause => $newClause)"
	}





	/** Removes a single unbound parameter with the relation offset given as `idx` (counting from the ''right'')
	  * and all its components with bound parameter(s) of/derived from the given value.
	  *///todo: make it a method in SQLExpression
	private[sql] def applyParam[F <: RowProduct, N <: RowProduct, X]
	                            (from :F, without :N, param :X) :SQLScribe[F, N] =
		new ApplyParam(from, without, param)

	//consider: note that JoinedGrouping relations and their components are here only shift to new positions,
	//but the underlying GroupingRelation in newClause will be completely new comparing to oldClause (with a new mapping!)
	//because the underlying expression is rewritten. This is not *very* bad because the semantics of JoinedRelation
	//specify that it is only a placeholder to be replaced with the one in the final RowProduct anyway,
	//so the expressions simply becomes unanchored, but it's easy to forget and might cause unforeseen issues elsewhere.
	//fixme: this should anchor the expression in newClause
	private class ApplyParam[+F <: RowProduct, -G <: RowProduct, X](
	                         protected[this] override val oldClause :F, protected[this] override val newClause :G,
	                         param :X)
		extends RecursiveScribe[F, G] //with CaseAnyColumnComponent[F, ColumnResult[G]#T]
	{
		val idx = oldClause.lastParamOffset

		protected override def expanded[S <: RowProduct, N <: RowProduct]
		                               (subselect :S, replacement :N)
		                               (implicit oldExt :ExpandedBy[oldClause.Generalized, S],
		                                newExt :ExpandedBy[newClause.Generalized, N]) :SQLScribe[S, N] =
			if (subselect.isExplicitParameterized)
				throw Bug(
					"Cannot bind the last parameter of " + oldClause +
					": encountered a parameterized subselect clause " + subselect + "."
				)
			else
				new ApplyParam[S, N, X](subselect, replacement, param)

		protected override def group[O <: RowProduct, N <: RowProduct]
		                            (oldUngrouped :O, newUngrouped :N, oldOffset :Int, newOffset :Int,
		                             oldGroupings :Int, newGroupings :Int) :SQLScribe[O, N] =
			new ApplyParam[O, N, X](oldUngrouped, newUngrouped, param)

		override def relation[O >: F <: RowProduct, T[A] <: BaseMapping[E, A], E, L <: RowProduct]
		                     (e :RelationSQL[O, T, E, L]) :SingleSQL[G, E] =
			e match {
				case _ if idx < 0 => //the last parameter is in a from clause under grouping, it doesn't change indexing
					e.asInstanceOf[RelationSQL[G, T, E, L]]
				case UnboundParamSQL(param, _, this.idx) =>
					BoundParam(param.form, this.param.asInstanceOf[E])
				case _ if e.index < idx =>
					e.asInstanceOf[SingleSQL[G, E]]
				case _ =>
					e.moveTo(RelationOffset.unsafe[G, e.FromLast, e.position.Rel, T](e.index - 1))
			}

//		override def grouping[D <: FromSome, T[A] <: BaseMapping[E, A], E, O >: F <: RowProduct]
//		                     (e :GroupingSQL[D, F, T, E, O]) :GlobalSQL[G, E] =
//			e match {
//				case _ if idx < 0 =>
//
//			}

		override def component[O >: F <: RowProduct, T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[V, A], V, L <: RowProduct]
		                      (e :TypedComponentSQL[O, T, E, M, V, L]) :SingleSQL[G, V] =
			e match {
				case _ if idx < 0 =>
					e.asInstanceOf[TypedComponentSQL[G, T, E, M, V, L]]
				case UnboundParamSQL(_, extract, this.idx) =>
					extract.opt(param.asInstanceOf[E]) match {
						case Got(v) => BoundParam(extract.export.form, v)
						case _ => MultiNull[V](extract.export.form)
					}
				case _ if e.origin.index < idx =>
					e.asInstanceOf[SingleSQL[G, V]]
				case _ =>
					e.moveTo(RelationOffset.unsafe[G, e.FromLast, e.origin.position.Rel, T](e.origin.index - 1))
			}

		override def columnComponent[O >: F <: RowProduct, T[A] <: BaseMapping[E, A], E, M[A] <: BaseColumn[V, A], V, L <: RowProduct]
		                            (e :TypedColumnComponentSQL[O, T, E, M, V, L]) :ColumnSQL[G, Single, V] =
			e match {
				case _ if idx < 0 =>
					e.asInstanceOf[TypedColumnComponentSQL[G, T, E, M, V, L]]
				case UnboundParamSQL(_, extract, this.idx) =>
					extract.opt(param.asInstanceOf[E]) match {
						case Got(v) => BoundColumnParam(extract.export.form, v)
						case _ => SQLNull[V](extract.export.form) :SQLNull[V]
					}
				case _ if e.origin.index < idx =>
					e.asInstanceOf[ColumnSQL[G, Single, V]]
				case _ =>
					e.moveTo(RelationOffset.unsafe[G, e.FromLast, e.origin.position.Rel, T](e.origin.index - 1))
			}

		override def aggregate[D <: FromSome, A, B](e :AggregateSQL[D, F, A, B]) =
			if (idx < 0 || idx >= oldClause.size)
				super.aggregate(e)
			else  //aggregated expression can't use anything from the group by clause
				e.asInstanceOf[AggregateSQL[D, G, A, B]]

		override def toString = s"ApplyParam(#$idx=$param)($oldClause => $newClause)"
	}





	/** A very simple expression mapper which invokes [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchor]]
	  * on all its arguments, at root [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] and
	  * [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] levels.
	  */
	def anchor[F <: RowProduct](from :F) :SQLScribe[F, F] = new Anchorage[F](from)

	private class Anchorage[F <: RowProduct](from :F)
		extends SQLScribe[F, F]
	{
		override def apply[S >: Grouped <: Single, V](e :SQLExpression[F, S, V]) = e.anchor(from)
		override def apply[S >: Grouped <: Single, V](e :ColumnSQL[F, S, V]) = e.anchor(from)
		override def toString = s"Anchorage($from)"
	}




	/** A scribe rewriting `SQLExpression` instances based on a ''from'' clause `F` into expressions based on
	  * some its expansion clause `E`. It relies on the [[net.noresttherein.oldsql.sql.SQLExpression.basedOn basedOn]]
	  * method of `SQLExpression` and recursively applies itself to parts of composite expressions and subselects of `F`.
	  */ //unused
	def rebase[F <: RowProduct, E <: RowProduct](clause :E)(implicit expansion :F PartOf E) :SQLScribe[F, E] =
		new RebaseExpression[F, E](clause)

	private class RebaseExpression[+F <: RowProduct, E <: RowProduct](clause :E)(implicit expansion :F PartOf E)
		extends CaseAnyExpression[F, Result[E]#E] with CaseAnyColumn[F, Result[E]#C]
		   with AbstractSQLScribe[F, E] //overrides the catch-all from the preceding traits
	{
		override def expression[S >: Grouped <: Single, X](e :SQLExpression[F, S, X]) =
			e.basedOn(clause)

		override def column[S >: Grouped <: Single, X](e :ColumnSQL[F, S, X]) :ColumnSQL[E, S, X] =
			e.basedOn(clause)

		override def toString = s"Rebase[to $clause]"
	}



	/** A scribe rewriting `SQLExpression` instances based on a ''from'' clause `F` into expressions based on
	  * some its expansion clause `E`. It relies on the [[net.noresttherein.oldsql.sql.SQLExpression.expand expand]]
	  * method of `SQLExpression` and recursively applies itself to parts of composite expressions and subselects of `F`.
	  * For this reason it can only be applied to global expressions, i.e. those for which
	  * [[net.noresttherein.oldsql.sql.SQLExpression.asSingleRow asGlobal]] returns `Some`. For all other expressions
	  * it will throw an `IllegalArgumentException`
	  */
	def expand[F <: RowProduct, E <: RowProduct](clause :E)(implicit expansion :F ExpandedBy E) :SQLScribe[F, E] =
		new ExpandExpression[F, E](clause)


	private class ExpandExpression[+F <: RowProduct, E <: RowProduct](clause :E)(implicit expansion :F ExpandedBy E)
		extends CaseAnyExpression[F, Result[E]#E] with CaseAnyColumn[F, Result[E]#C]
		   with AbstractSQLScribe[F, E] //overrides the catch-all from the preceding traits
	{
		private def nonGlobal(e :SQLExpression.__) :Nothing =
			throw new IllegalArgumentException(
				s"Cannot expand non global expression $e by ${expansion.length} relations to to clause $clause."
			)

		override def apply[S >: Grouped <: Single, V](e :SQLExpression[F, S, V]) :SingleSQL[E, V] =
			e.asSingleRow match {
				case Some(global) => this(global)
				case _ => nonGlobal(e)
			}

		override def apply[S >: Grouped <: Single, V](e :ColumnSQL[F, S, V]) :ColumnSQL[E, Single, V] =
			e.asSingleRow match {
				case Some(global) => this(global)
				case _ => nonGlobal(e)
			}


		override def expression[S >: Grouped <: Single, X](e :SQLExpression[F, S, X]) :SQLExpression[E, S, X] =
			e.asSingleRow match {
				case Some(global) => global.expand(clause)
				case _ => nonGlobal(e)
			}

		override def column[S >: Grouped <: Single, X](e :ColumnSQL[F, S, X]) :ColumnSQL[E, S, X] =
			e.asSingleRow match {
				case Some(global) => global.expand(clause)
				case _ => nonGlobal(e)
			}

		override def toString = s"Expand[$clause]"
	}


	/** An SQL expression rewriter shifting back references to all relations before the last `Subselect` in `old`
	  * after its relations following the `Subselect` were transplanted onto ''from'' clause `expanding.outer`,
	  * which is an expansion of `old.outer`. All relations from `old.outer` (with indices higher or equal to
	  * `old.size`) are moved to indices larger by `expanding.size - old.size`. Relations within the most
	  * inner subselect are left unchanged (assuming that `expanding` contains the same relations as `old` within
	  * the most inner subselect).
	  */
	private[sql] def shiftBack[F <: RowProduct, E <: RowProduct](old :F, expanding :E) :SQLScribe[F, E] =
		shiftBack(old, expanding, expanding.fullSize - old.fullSize, old.size)

	/** An SQL expression rewriter shifting back references to all relations before a certain `Subselect` join
	  * by `expansion` positions. Used when a subselect clause is 'transplanted' onto another clause,
	  * expanding the `Implicit` clause of the subselect. In the initial and most basic scenario,
	  * all relations with index higher or equal to the size of the `old` clause are shift (i.e, the boundary
	  * is the last `Subselect` in `old`), but subselect expressions encountered in ''where''/''having'' clauses
	  * of shift tables result in recursive calls to this method and progressively longer `subselectSize` parameter.
	  * @param old       a subselect clause serving as an SQL expression base.
	  * @param expanding a new subselect clause with some additional relations inserted between `F#Implicit`
	  *                  and the mapping joined in with a `Subselect` join.
	  * @param expansion the difference in relations number between `F` and `E`.
	  * @param subselectSize number of relations in the explicit ''from'' clause of subselects `F` and `E`.
	  */
	private[sql] def shiftBack[F <: RowProduct, E <: RowProduct]
	                          (old :F, expanding :E, expansion :Int, subselectSize :Int) :SQLScribe[F, E] =
		new SubstituteComponents[F, E] {
			protected[this] override val oldClause = old
			protected[this] override val newClause = expanding

			private[this] val relations = expanding.fullTableStack.to(Array)

			override def relation[O >: F <: RowProduct, T[A] <: BaseMapping[S, A], S, L <: RowProduct]
			                     (e :RelationSQL[O, T, S, L]) :ComponentSQL[E, T] =
				if (e.index < subselectSize)
					e.asInstanceOf[RelationSQL[E, T, S, L]]
				else try {
					relations(e.index + expansion).asInstanceOf[RelationSQL[E, T, S, L]]
				} catch { //this never happens normally, but we abuse this method in RecursiveScribe.group with a possibly negative expansion
					case e :IndexOutOfBoundsException =>
						throw new IndexOutOfBoundsException(
							"Failed to shift " + e + " by " + expansion + " from " + oldClause + " to " + newClause + "."
						).initCause(e)
				}


			protected override def expanded[S <: RowProduct, N <: RowProduct]
			                               (subselect :S, replacement :N)
			                               (implicit oldExt :oldClause.Generalized ExpandedBy S,
			                                newExt :newClause.Generalized ExpandedBy N) =
				shiftBack[S, N](subselect, replacement, expansion, subselectSize + oldExt.length)
		}


}


