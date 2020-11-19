package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.{ColumnMapping, ColumnReadForm, Mapping, Relation, SQLForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.AggregateFunction.{Avg, Count, Max, Min, StdDev, Sum, Var}
import net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate
import net.noresttherein.oldsql.sql.ColumnSQL.GlobalColumn
import net.noresttherein.oldsql.sql.RowProduct.{AggregateOf, As, ExtendedBy, GroundFrom, JoinedMappings, NonEmptyFrom, NonEmptyFromTemplate, ParamlessFrom, PartOf, RowProductTemplate}
import net.noresttherein.oldsql.sql.mechanics.GetTable.ByIndex
import net.noresttherein.oldsql.sql.JoinParam.WithParam
import net.noresttherein.oldsql.sql.UnboundParam.{?:, NamedParamRelation, ParamRelation}
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, GlobalSQL, LocalScope}
import net.noresttherein.oldsql.sql.ast.{AggregateSQL, ConversionSQL, MappingSQL, SelectSQL, SQLTerm, TupleSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{ComponentSQL, JoinedRelation, LooseColumn, RelationSQL, TypedComponentSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.ast.SelectSQL.SelectColumn
import net.noresttherein.oldsql.sql.ast.SQLTerm.True
import net.noresttherein.oldsql.sql.mechanics.{GroupingExpression, RowProductMatcher, SQLScribe, TableCount}
import net.noresttherein.oldsql.sql.FromClause.FromClauseTemplate






/** Marker trait for all true ''from'' clauses, without a ''group by'' clause. Most implementations extend
  * its non-empty subtype, [[net.noresttherein.oldsql.sql.FromSome FromSome]].
  * @see [[net.noresttherein.oldsql.sql.GroupByClause]]
  */
trait FromClause extends RowProduct with FromClauseTemplate[FromClause] { thisClause =>

	override def filter :GlobalBoolean[Generalized] = filter(generalized)

	override def filter[E <: RowProduct](target :E)(implicit extension :Generalized PartOf E) :GlobalBoolean[E]



	override type BoundParamless >: Paramless <: FromClause { type Params = @~ }
	override type Paramless <: FromClause { type Params = @~ }

	/** A join between this clause and the relation for mapping `T`. For non empty clauses, this is simply `J[Self, T]`,
	  * but `Dual` defines it as `From[T]` instead.
	  */
	type Extend[+J[+L <: FromSome, R[O] <: T[O]] <: L Extended R, T[O] <: MappingAt[O]] <: Self Extended T

	/** Joins this clause with another relation `next` for mapping `T`.
	  * @param next the relation to add to the clause.
	  * @param filter the additional filter condition for the ''where'' clause of the created clause; defaults to `True`.
	  * @param join a template `JoinLike` instance used as a factory for the returned clause; defaults to `InnerJoin`.
	  * @return `From(next)` if this clause is empty and `join.likeJoin(self, next)` for non empty clauses.
	  */
	def extend[T[O] <: BaseMapping[S, O], S]
	    (next :Relation[T], filter :GlobalBoolean[Generalized AndFrom T] = True, join :JoinLike.* = InnerJoin.template)
			:Extend[join.LikeJoin, T]

	/** Used to add any relation to any clause, creates the clause of a type depending on this clause:
	  * empty clauses return `From[T]`, while non empty clauses create `this.type InnerJoin T`.
	  */ //this method serves also as a seal ensuring that every discrete clause extends either EmptyFrom or FromSome
	protected[sql] def extend[T[O] <: BaseMapping[S, O], S, A <: Label]
	                         (right :LastRelation[T, S], alias :Option[A],
	                          filter :GlobalBoolean[Generalized AndFrom T]) :this.type AndFrom T As A



	/** A join between this clause and the clause `F`. This type is the result of replacing `Dual` in `F` with `Self`.
	  * Non-empty clauses define it as `F#JoinedWith[Self, J]]`, while `Dual` defines it as `F` - the indirection
	  * enforced by the join type `J` (and `Join` subclasses) having `FromSome` as the upper bound of their left side.
	  */
	type JoinWith[+J[+L <: FromSome, R[O] <: MappingAt[O]] <: L AndFrom R, F <: RowProduct] <: RowProduct

	/** Joins the clause given as the parameter with this clause. If any of the clauses is empty, the other is
	  * returned. Otherwise the created clause contains this clause as its prefix, followed by all relations
	  * from `suffix` joined with the same join kinds. The first `From` pseudo join in suffix is replaced with
	  * the join type specified by the template parameter (`join.LikeJoin`); if the first relation in `suffix` is
	  * however joined with `Dual` using another join type (such as `JoinParam`), it is preserved. This is a dispatch
	  * call to [[net.noresttherein.oldsql.sql.RowProduct.joinedWith suffix.joinedWith(self, join)]]. This extra level
	  * of indirection is enforced by the upper bound of `FromSome` on the left type parameters in `Join` classes,
	  * while this method can be called also if this clause is empty. Additionally, the join kind to use between
	  * the last relation in this clause and the first relation in `suffix` can be specified as `Subselect`,
	  * while `joinedWith` allows only `Join` subclasses.
	  *
	  * It is a low level method and client code should prefer the eponymously named extension methods
	  * for individual join kinds defined in
	  * [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension FromSomeExtension]]: `join`, `outerJoin`,
	  * `innerJoin`, `leftJoin`, `rightJoin`, `subselect`, as they have more friendly return types.
	  * @param suffix the clause with relations which should be added to this clause
	  * @param join a template instance to use as the factory for the join between the last relation in this clause
	  *             and the first relation in `suffix`.
	  */
	def joinWith[F <: FromSome](suffix :F, join :JoinLike.* = InnerJoin.template) :JoinWith[join.LikeJoin, F]



	protected override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.fromClause(this)


	private[sql] def concrete_FromClause_subclass_must_extend_DiscreteFrom_or_GroupByClause :Nothing =
		throw new UnsupportedOperationException

}






object FromClause {

	trait FromClauseTemplate[+F <: FromClause] extends RowProductTemplate[F] { this :F with FromClauseTemplate[F] =>
		override def fromClause :F = this
	}



	/** Extension methods performing most generic join between any ungrouped (i.e., pure) `RowProduct`,
	  * and other relations.
	  */
	implicit class FromClauseExtension[F <: FromClause](val thisClause :F) extends AnyVal {

		/** Performs an inner join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side. The real type of the result depends on the type of this clause:
		  * for `Dual`, the result is `From[R]`, for non-empty clauses the result is `F InnerJoin R`.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @return `From[R]` if this clause is empty or `F InnerJoin R` otherwise.
		  */
		@inline def andFrom[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		            (table :Relation[R])
		            (implicit cast :JoinedRelationSubject[AndFrom.WithLeft[F]#F, R, T, MappingOf[S]#TypedProjection])
				:F AndFrom R =
			AndFrom(thisClause, table)

		/** Performs a join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side. The join type between the clauses will be an `InnerJoin` if the dynamic
		  * type of the first join of the given clause is `From[_]`, otherwise the join type is preserved.
		  * If either of the clauses is empty, the other is returned unchanged.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be subsequently
		  * specified using the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def andFrom[R <: RowProduct](other :R) :other.JoinedWith[F, AndFrom] = other.appendedTo(thisClause)
	}



	/** A `RowProduct` of a top level, independent ''select'' without a ''group by'' clause - one which doesn't contain
	  * any [[net.noresttherein.oldsql.sql.Subselect Subselect]] or [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]
	  * joins (is not a ''from'' clause of a subselect of some other select). In order to conform naturally
	  * (rather than by refinement) to `OuterDiscreteForm`, the clause must be ''complete'', that is its static type
	  * must start with either [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.From From]]
	  * - rather than a wildcard or abstract type hiding an unknown prefix - and its `Generalized` supertype
	  * must be known, meaning all join kinds should be narrowed down at least to
	  * [[net.noresttherein.oldsql.sql.Join Join]]/[[net.noresttherein.oldsql.sql.JoinParam JoinParam]].
	  * Despite the requirement for completeness, this type does not conform in itself to
	  * [[net.noresttherein.oldsql.sql.RowProduct.GeneralizedFrom GeneralizedFrom]], maintaining a minimalistic
	  * definition to preserve the property of being an outer clause by as many transformation functions as possible.
	  * This is an 'ungrouped' subtype of the more generic [[net.noresttherein.oldsql.sql.RowProduct.TopFrom TopFrom]].
	  * See [[net.noresttherein.oldsql.sql.FromSome.TopFromSome TopFromSome]] for a variant with a
	  * [[net.noresttherein.oldsql.sql.FromSome FromSome]] upper bound. An `TopFromClause` can still
	  * contain (unbound) parameters; [[net.noresttherein.oldsql.sql.FromClause.GroundFromClause GroundFromClause]]
	  * is the specialization of this type without any `JoinParam` 'joins'.
	  * @see [[net.noresttherein.oldsql.sql.FromClause FromClause]]
	  */
	type TopFromClause = FromClause {
		type Implicit = RowProduct
	}

	/** A `RowProduct` without any [[net.noresttherein.oldsql.sql.Subselect Subselect]],
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]],
	  * or [[net.noresttherein.oldsql.sql.GroupBy, GroupBy]]/[[net.noresttherein.oldsql.sql.By By All]]
	  * joins. Representing a single ''from'' clause (and not one of a nested subselect), without a ''group by'' clause,
	  * it can be used as a basis of (top level) SQL ''selects''. In order to conform naturally (rather than by refinement),
	  * a type must be ''complete'', and the [[net.noresttherein.oldsql.sql.RowProduct.Generalized generalized]] form
	  * of every component clause must be known; such types will automatically also conform to
	  * [[net.noresttherein.oldsql.sql.RowProduct.GeneralizedFrom GeneralizedFrom]], but there is no actual subtype
	  * relationship between the two in the type system. A `GroundFromClause` will however always by a subtype of
	  * its more generic variants, [[net.noresttherein.oldsql.sql.RowProduct.GroundFrom GroundFrom]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.TopFromClause TopFromClause]] (which groups all outer clauses,
	  * including those with unbound parameters), as well as
	  * [[net.noresttherein.oldsql.sql.RowProduct.ParamlessFrom ParamlessFrom]].
	  */
	type GroundFromClause = FromClause {
		type Implicit = RowProduct
		type Base = RowProduct
		type DefineBase[+I <: RowProduct] = I
		type Params = @~
	}

}







/** Common upper bound for all ''from'' clauses containing at least one relation, but no ''group by'' clause.
  * Extended by every [[net.noresttherein.oldsql.sql.FromClause FromClause]] implementations
  * other than [[net.noresttherein.oldsql.sql.Dual Dual]]. Most types do not do this directly however, but
  * through the [[net.noresttherein.oldsql.sql.Extended Extended]], which is the base trait for recursively built
  * clauses by adding a [[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedRelation JoinedRelation]] to a prefix
  * `RowProduct`.
  */
trait FromSome
	extends FromClause with NonEmptyFrom with FromClauseTemplate[FromSome] with NonEmptyFromTemplate[FromSome, FromSome]
{ thisClause =>

	override type LastTable[F <: RowProduct] = JoinedRelation[F, LastMapping]
	override type FromLast >: Generalized <: FromSome
	override type FromNext[E[+L <: FromSome] <: RowProduct] = E[FromLast]

	override type Generalized >: Dealiased <: FromSome {
		type FromLast = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = RowProduct
		type Base <: thisClause.Base
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	type Dealiased >: Self <: FromSome {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Implicit = thisClause.Implicit
		type Base = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: FromSome {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Base = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}



	override type FilterNext[E[+L <: FromSome] <: L Extended N, S <: RowProduct Extended N, G <: S, N[O] <: MappingAt[O]] =
		                    (JoinedRelation[FromNext[E], LastMapping], JoinedRelation[S, N]) => GlobalBoolean[G]

	protected override def filterNext[F <: RowProduct AndFrom N, N[O] <: MappingAt[O]]
	                       (next :F)(filter :FilterNext[next.GeneralizedLeft, next.FromLast, next.Generalized, N])
			:next.Copy =
	{
		val condition = filter(last.asIn(next.generalizedExtension[FromLast]), next.last)
		val anchored = SQLScribe.anchorLooseComponents(next.generalized)(condition)
		next.filtered(anchored)
	}

	override type Paramless <: BoundParamless
	override type BoundParamless = FromSome { type Params = @~ } //only because JoinParam requires FromSome on the left


	override type Extend[+J[+L <: FromSome, R[O] <: T[O]] <: L Extended R, T[O] <: MappingAt[O]] = Self J T

	override def extend[T[O] <: BaseMapping[S, O], S]
	                   (next :Relation[T], filter :GlobalBoolean[Generalized AndFrom T], join :JoinLike.*)
			:join.LikeJoin[Self, T] =
		join.likeJoin[Self, T, S](self, next)(filter)

	protected[sql] def extend[T[O] <: BaseMapping[S, O], S, A <: Label]
	                   (right :LastRelation[T, S], alias :Option[A], filter :GlobalBoolean[Generalized AndFrom T])
			:this.type AndFrom T As A =
		InnerJoin[this.type, T, S, A](this, right, alias)(filter)



	override type JoinWith[+J[+L <: FromSome, R[O] <: MappingAt[O]] <: L AndFrom R, F <: RowProduct] =
		F#JoinedWith[Self, J]

	override def joinWith[F <: FromSome](suffix :F, join :JoinLike.*) :suffix.JoinedWith[Self, join.LikeJoin] =
		join.likeJoin(self, suffix)


	/** The upper bound for all `RowProduct` subtypes representing a ''group by'' clause grouping a clause
	  * with the same [[net.noresttherein.oldsql.sql.RowProduct.Generalized Generalized]] type as this clause.
	  * This type is additionally extended to include the special [[net.noresttherein.oldsql.sql.Aggregated Aggregated]]
	  * clause, representing ''from'' clauses of SQL ''selects'' with an aggregate function in their ''select'' clause.
	  */
	type GeneralizedAggregate = AggregateClause {
		type GeneralizedDiscrete = thisClause.Generalized
	}

	/** The upper bound for all `RowProduct` subtypes representing a ''group by'' clause grouping this clause. */
	type Aggregate = AggregateClause {
		type GeneralizedDiscrete = thisClause.Generalized
		type Discrete = thisClause.Self
	}

}






object FromSome {


	/** Extension methods for `FromSome` classes (non-empty ''from'' clauses) which benefit from having a static,
	  * invariant self type. These include methods for joining with other relations and clauses as well as
	  * select methods creating SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]] using
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions.
	  */
	implicit class FromSomeExtension[F <: FromSome](val thisClause :F) extends AnyVal {
		import thisClause.{Base, Generalized, FromLast, LastMapping}

		/** Performs an inner join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def join[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                (table :Relation[R])(implicit cast :InferSubject[F, InnerJoin, R, T, S]) :F InnerJoin R =
			InnerJoin(thisClause, table)

		/** Performs an inner join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be subsequently
		  * specified using the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def join[R <: RowProduct](other :R) :other.JoinedWith[F, InnerJoin] =
			other.joinedWith(thisClause, InnerJoin.template)


		/** Performs a symmetric outer join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def outerJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :Relation[R])
		                     (implicit cast :InferSubject[F, OuterJoin, R, T, S]) :F OuterJoin R =
			OuterJoin(thisClause, table)

		/** Performs a symmetric outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be subsequently
		  * specified using the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def outerJoin[R <: RowProduct](other :R) :other.JoinedWith[F, OuterJoin] =
			other.joinedWith(thisClause, OuterJoin.template)


		/** Performs a left outer join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def leftJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                    (table :Relation[R])
		                    (implicit cast :InferSubject[F, LeftJoin, R, T, S]) :F LeftJoin R =
			LeftJoin(thisClause, table)

		/** Performs a left outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be subsequently
		  * specified using the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def leftJoin[R <: RowProduct](other :R) :other.JoinedWith[F, LeftJoin] =
			other.joinedWith(thisClause, LeftJoin.template)


		/** Performs a right outer join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def rightJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :Relation[R])
		                     (implicit cast :InferSubject[F, RightJoin, R, T, S]) :F RightJoin R =
			RightJoin(thisClause, table)

		/** Performs a right outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be subsequently
		  * specified using the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def rightJoin[R <: RowProduct](other :R) :other.JoinedWith[F, RightJoin] =
			other.joinedWith(thisClause, RightJoin.template)



		/** Performs a natural inner join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side. The join condition of the created instance will compare all columns of the last
		  * relation in this clause with columns with matching names from the given relation. If the column types
		  * (associated `ColumnForm` objects) of any of these column pairs differ, an `IllegalArgumentException`
		  * is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                       (table :Relation[R])
		                       (implicit cast :InferSubject[thisClause.type, InnerJoin, R, T, S],
		                                 last :ByIndex[F, Generalized, -1] { type O >: Generalized <: FromSome })
				:F InnerJoin R =
			cast(InnerJoin[thisClause.type, T, T, S](thisClause, cast(table)) where naturalFilter[T] _)


		/** Performs a natural symmetric outer join between this clause on the left side, and the relation given
		  * as a `Relation` object on the right side. The join condition of the created instance will compare
		  * all columns of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalOuterJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                            (table :Relation[R])
		                            (implicit cast :InferSubject[thisClause.type, OuterJoin, R, T, S],
		                                      last :ByIndex[F, Generalized, -1] { type O >: Generalized <: FromSome })
				:F OuterJoin R =
			cast(OuterJoin[thisClause.type, T, T, S](thisClause, cast(table)) where naturalFilter[T] _)


		/** Performs a natural left outer join between this clause on the left side, and the relation given
		  * as a `Relation` object on the right side. The join condition of the created instance will compare
		  * all columns of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalLeftJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                           (table :Relation[R])
		                           (implicit cast :InferSubject[thisClause.type, LeftJoin, R, T, S],
		                                     last :ByIndex[F, Generalized, -1] { type O >: Generalized <: FromSome })
				:F LeftJoin R =
			cast(LeftJoin[thisClause.type, T, T, S](thisClause, cast(table)) where naturalFilter[T] _)


		/** Performs a natural right outer join between this clause on the left side, and the relation given
		  * as a `Relation` object on the right side. The join condition of the created instance will compare
		  * all columns of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalRightJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                            (table :Relation[R])
		                            (implicit cast :InferSubject[thisClause.type, RightJoin, R, T, S],
		                                      last :ByIndex[F, Generalized, -1] { type O >: Generalized <: FromSome })
				:F RightJoin R =
			cast(RightJoin[thisClause.type, T, T, S](thisClause, cast(table))(cast.self) where naturalFilter[T] _)


		private def naturalFilter[T[O] <: BaseMapping[_, O]]
		                         (tables :JoinedMappings[thisClause.type Join T])
		                         (implicit prev :ByIndex[F Join T, Generalized Join T, -2])
				:GlobalBoolean[Generalized Join T] =
		{
			val firstTable = tables.prev
			val secondTable = tables.last

			val firstColumns = firstTable.columns.map(c => c.name -> c).toMap //todo: below - nondeterministic compilation
			val secondColumns = secondTable.columns.map(c => c.name -> (c :ColumnMapping[_, RowProduct AndFrom T])).toMap
			val common = firstColumns.keySet & secondColumns.keySet

			val joins = common map { name =>

				val first = firstColumns(name).asInstanceOf[ColumnMapping[Any, prev.O]]
				val second = secondColumns(name).asInstanceOf[ColumnMapping[Any, RowProduct AndFrom T]]
				if (first.form != second.form)
					throw new IllegalArgumentException(
						s"Can't perform a natural join of $firstTable and $secondTable: " +
						s"columns $first and $second have different types (forms): ${first.form} and ${second.form}."
					)
				//todo: why explicit conversions are necessary here?
				LooseColumn(first, 1) === LooseColumn(second, 0)
			}
			(True[Generalized Join T] /: joins)(_ && _)
		}



		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based on this clause.
		  * The explicit list of relations in the clause is initialized with the relation given as a `Relation` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and ''select'' clauses.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]]
		  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast()]] methods.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def subselect[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :Relation[R])
		                     (implicit cast :InferSubject[F, Subselect, R, T, S]) :F Subselect R =
			Subselect(thisClause, table)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based on this clause.
		  * The explicit list of relations in the clause is initialized with the relations given as a `RowProduct`
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relations - following the `Subselect`
		  * pseudo join and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]]
		  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast()]] methods.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order). The clause cannot be empty to enforce that the `Subselect` join
		  *              is actually applied and that any relations joined later will be part of the new subselect
		  *              rather than the currently most deeply nested select.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  * @throws UnsupportedOperationException if `other` is empty or its first join is a `JoinParam`.
		  */
		@inline def subselect[R <: NonEmptyFrom](other :R) :other.JoinedWithSubselect[F] =
			other.joinedWithSubselect(thisClause)



		/** Adds a [[net.noresttherein.oldsql.sql.GroupBy group by]] clause to this ''from'' clause.
		  * @tparam E an expression used for grouping, for which
		  *           a [[net.noresttherein.oldsql.sql.mechanics.GroupingExpression GroupingExpression]]
		  *           type class exist. The [[net.noresttherein.oldsql.sql.mechanics.GroupingExpression$ companion]]
		  *           object contains definitions for:
		  *             - `M <: `[[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[_, O]`, having an implicit
		  *               [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] (which exists
		  *               for all subtypes of `BaseMapping` taking the `Origin` type as its last type parameter),
		  *             - components of relations:
		  *               [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]]`[F, _]` and
		  *               [[net.noresttherein.oldsql.sql.ast.MappingSQL.ColumnComponentSQL ColumnComponentSQL]]`[F, _]`,
		  *             - any single column expressions [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, _]`,
		  *             - base [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, _]`,
		  *           where type `F` is this clause, and `O` is its some supertype, with the origin relation
		  *           of the component expression being the first relation following a wildcard type (typically `FromSome`).
		  * @param expr a function accepting the facade to the this clause
		  *             [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]], which provides
		  *             accessors to the mappings for all relations in this clause, and which returns
		  *             either a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] with a supertype
		  *             of this clause as its `Origin` type argument,
		  *             or an [[net.noresttherein.oldsql.sql.SQLExpression SQL expression]] based on this clause which
		  *             will be used as the grouping expression. The expression may be
		  *             a [[net.noresttherein.oldsql.sql.ColumnSQL single column]], but it doesn't have to,
		  *             in which case all columns of the expression will be inlined in the ''group by'' clause
		  *             in the order defined by its [[net.noresttherein.oldsql.schema.SQLReadForm form]].
		  *             If the returned value is a a mapping `M[O] <: MappingAt[O]` or
		  *             a [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL component expression]]
		  *             for such a mapping, then the return type of the method will be
		  *             `G `[[net.noresttherein.oldsql.sql.By By]]` M`, allowing selecting of any
		  *             of its components/columns, just as with components of tables joined using
		  *             the [[net.noresttherein.oldsql.sql.Join Join]] classes (and through the same
		  *             mechanisms).
		  *             Note that the argument, `JoinedMappings[F]`, is parameterised not with this ''group by''
		  *             clause, but the discrete ''from'' clause underneath it.
		  * @param grouping a type class responsible for creating the returned ''group by'' clause, which defines
		  *                 the return type based on the type of the expression returned by the function passed
		  *                 as the first argument. See the `returns` section for a listing.
		  * @return a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] instance using this clause as its left side.
		  *         The mapping type on the right side will be the mapping for the expression `E` returned
		  *         by the passed function: if it is a
		  *         [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]], it is used directly after anchoring
		  *         to the relation based on its `Origin` type. In case of
		  *         [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]] (including its column
		  *         subtype), the mapping is the mapping type parameter of the component expression.
		  *         Otherwise a generic [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]
		  *         (or [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] if `E` is
		  *         a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]) is used. In the latter case, the return type
		  *         is abbreviated as `F `[[net.noresttherein.oldsql.sql.GroupByVal GroupByVal]]` V`
		  *         (or `F `[[net.noresttherein.oldsql.sql.GroupByOne GroupByOne]]` V`), where `V` is the value type
		  *         of the expression `E`.
		  */
		def groupBy[E](expr :JoinedMappings[F] => E)
		              (implicit grouping :GroupingExpression[Generalized, F, E]) :grouping.Result =
			grouping(thisClause, expr(thisClause.mappings))

		/** Adds a [[net.noresttherein.oldsql.sql.GroupBy group by]] clause to this ''from'' clause.
		  * The grouping expression is based solely on the last relation in this clause, but
		  * @tparam E an expression used for grouping, for which
		  *           a [[net.noresttherein.oldsql.sql.mechanics.GroupingExpression GroupingExpression]]
		  *           type class exist. The [[net.noresttherein.oldsql.sql.mechanics.GroupingExpression$ companion]]
		  *           object contains definitions for:
		  *             - `M <: `[[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[_, O]`, having an implicit
		  *               [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] (which exists
		  *               for all subtypes of `BaseMapping` taking the `Origin` type as its last type parameter),
		  *             - components of relations:
		  *               [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]]`[F, _]` and
		  *               [[net.noresttherein.oldsql.sql.ast.MappingSQL.ColumnComponentSQL ColumnComponentSQL]]`[F, _]`,
		  *             - any single column expressions [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, _]`,
		  *             - base [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, _]`,
		  *           where type `F` is this clause, and `O` is its some supertype, with the origin relation
		  *           of the component expression being the first relation following a wildcard type (typically `FromSome`).
		  * @param expr     a function accepting the last [[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedRelation relation]]
		  *                 of the this clause, and which returns either
		  *                 a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] with a supertype of this clause
		  *                 as its `Origin` type argument, or an [[net.noresttherein.oldsql.sql.SQLExpression SQL expression]]
		  *                 based on this clause which will be used as the grouping expression. The expression may be
		  *                 a [[net.noresttherein.oldsql.sql.ColumnSQL single column]], but it doesn't have to,
		  *                 in which case all columns of the expression will be inlined in the ''group by'' clause
		  *                 in the order defined by its [[net.noresttherein.oldsql.schema.SQLReadForm form]].
		  *                 If the returned value is a a mapping `M[O] <: MappingAt[O]` or
		  *                 a [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL component expression]]
		  *                 for such a mapping, then the return type of the method will be
		  *                 `F `[[net.noresttherein.oldsql.sql.GroupBy GroupBy]]` M`, allowing selecting of any
		  *                 of its components/columns, just as with components of tables joined using
		  *                 the [[net.noresttherein.oldsql.sql.Join Join]] classes (and through the same
		  *                 mechanisms).
		  * @param grouping a type class responsible for creating the returned ''group by'' clause, which defines
		  *                 the return type based on the type of the expression returned by the function passed
		  *                 as the first argument. See the `returns` section for a listing.
		  * @return a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] instance using this clause as its left side.
		  *         The mapping type on the right side will be the mapping for the expression `E` returned
		  *         by the passed function: if it is a
		  *         [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]], it is used directly after anchoring
		  *         to the relation based on its `Origin` type. In case of
		  *         [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]] (including its column
		  *         subtype), the mapping is the mapping type parameter of the component expression.
		  *         Otherwise a generic [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]
		  *         (or [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] if `E` is
		  *         a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]) is used. In the latter case, the return type
		  *         is abbreviated as `F `[[net.noresttherein.oldsql.sql.GroupByVal GroupByVal]]` V`
		  *         (or `F `[[net.noresttherein.oldsql.sql.GroupByOne GroupByOne]]` V`), where `V` is the value type
		  *         of the expression `E`.
		  */
		def groupByLast[E](expr :JoinedRelation[FromLast, LastMapping] => E)
		                  (implicit grouping :GroupingExpression[Generalized, F, E]) :grouping.Result =
			grouping(thisClause, expr(thisClause.last))

		/** Adds a ''group by'' clause to this ''from'' clause with all
		  * [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]] columns of the given component.
		  * @param component a mapping for a component of one of the relations listed by this clause.
		  *                  It must be a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[S, O]`, where
		  *                  the `Origin` type `O` is a supertype of the `Generalized` type of this clause starting with
		  *                  `RowProduct AndFrom T`, where `T` is the mapping type for the relation this component
		  *                  comes from.
		  * @param typeParams used to instantiate the necessary `Subject` and `Origin` types `S`, `O` of the argument mapping.
		  * @param shift implicit evidence with the number of relations listed in the `Origin` type.
		  * @param projection a casting type class for `M` which provides its necessary type constructor accepting
		  *                   an `Origin` type.
		  */ //todo: this currently is not picked over the overload with ComponentSQL for some reason
		def groupBy[M <: Mapping, S, O <: RowProduct]
		           (component :M)
		           (implicit typeParams :M <:< BaseMapping[S, O], belongs :Generalized <:< O,
		                     shift :TableCount[O, _ <: Numeral], projection :OriginProjection[M, S])
				:F GroupBy projection.WithOrigin =
		{
			val relation = thisClause.fullTableStack(shift.offset).toRelationSQL
				.asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
			val expr = TypedComponentSQL(relation, projection[F](component))(projection.isomorphism)
			GroupBy[F, projection.WithOrigin, projection.WithOrigin, S](thisClause, expr.groupingRelation)
		}

		/** Adds a ''group by'' clause to this ''from'' clause with all
		  * [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]] columns of the component from
		  * the given component expression based on this clause.
		  */
		def groupBy[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		           (component :ComponentSQL[Generalized, M])
		           (implicit cast :InferSubject[F, GroupBy, M, T, S]) :F GroupBy M =
			GroupBy(thisClause, component.groupingRelation)

		/** Adds a ''group by'' clause to this ''from'' clause with the given single column expression. */
		def groupBy[V](column :GlobalColumn[Generalized, V]) :F GroupByOne V =
 			GroupByOne[Generalized, thisClause.type, V](thisClause, column)

		/** Adds a ''group by'' clause to this ''from'' clause with all member columns of the given expression.
		  * The expression is traversed structurally until a [[net.noresttherein.oldsql.sql.ColumnSQL column expression]]
		  * is encountered, which is then added to the ''group by'' clause of the generated SQL.
		  * Not all possible expressions are supported; the expression may consist of
		  *   - any single [[net.noresttherein.oldsql.sql.ColumnSQL column expressions]] (atomic SQL values),
		  *     in particular [[net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm terms]],
		  *   - [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL components]] (ranging from whole entities
		  *     to single columns),
		  *   - [[ConversionSQL conversion]] nodes,
		  *   - any [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL composites]] combining the above, in particular:
		  *   - [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple tuples]] and
		  *     [[ast.TupleSQL.IndexedChainTuple indexed tuples]].
		  */
		def groupBy[V](expr :GlobalSQL[Generalized, V]) :F GroupByVal V =
			GroupByVal[Generalized, thisClause.type, V](thisClause, expr)


		/** Wraps this clause in a special adapter allowing the use of
		  * [[net.noresttherein.oldsql.sql.AggregateFunction aggregate functions]] in its ''select'' clause.
		  * While available methods such as `count`, `sum`, `avg` etc. allow selecting of a single statistic
		  * directly from this instance, if results of more than one aggregate functions are required,
		  * they must be passed using the standard `select` methods defined in
		  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]:
		  * {{{
		  *     import AggregateFunction._
		  *     this.aggregate select { tables =>
		  *         val weapon = tables[Weapons]
		  *         Max(weapon.damage) ~ Min(weapon.damage) ~ Avg(weapon.damage)
		  *     }
		  * }}}
		  * Note that the general purpose [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]]
		  * method, which accepts a function of [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]],
		  * supports directly aggregate expressions based on the aggregation of this ''from'' clause.
		  */
		@inline def aggregate :Aggregated[F] = Aggregated(thisClause)



		/** Creates an aggregated SQL ''select'' with a single column and a single row, containing the result
		  * of applying the aggregate function of the argument to all rows from the ''from'' clause.
		  * If there are any unbound parameters inside the explicit portion of this clause, the resulting ''select''
		  * will be based on the type `Nothing` (rather than `Implicit`), making it statically impossible to use as a part
		  * of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] (and, in particular, inside
		  * the `Outer` clause).
		  * @return a `SelectColumn` subtype depending on whether this instance represents an outer select
		  *         ([[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectColumn TopSelectColumn]])
		  *         or a subselect ([[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectColumn]])
		  */
		def select[V](header :AggregateSQL[Generalized, Aggregated[Generalized], _, V])
				:SelectColumn[Base, V] =
			Aggregated(thisClause.self).select(header)


		/** Creates a single column SQL `select count(*) from` with this instance as the from clause.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Count]]
		  */
		def count :SelectColumn[Base, Int] = select(Count.*)

		/** Creates a single column SQL ''select'' counting all rows with non-null values in the given column.
		  * This translates to `select count(column) from this`.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Count]]
		  */
		def count(column :ColumnSQL[Generalized, LocalScope, _]) :SelectColumn[Base, Int] =
			select(Count(column))


		/** Creates a single column SQL ''select'' counting all rows with non-null values in the given column.
		  * This translates to `select count(column) from this`.
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Count]]
		  */
		def count(column :JoinedMappings[F] => ColumnSQL[Generalized, LocalScope, _]) :SelectColumn[Base, Int] =
			select(Count(column(thisClause.mappings)))

		/** Creates a single column SQL ''select'' returning the smallest value of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]].
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  *               The column must be of a type with [[net.noresttherein.oldsql.sql.SQLOrdering SQLOrdering]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               and the actual ordering is not influenced by the type class.
		  * @return an SQL expression representing `select min(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Min]]
		  */
		def min[X :SQLOrdering](column :ColumnSQL[Generalized, LocalScope, X]) :SelectColumn[Base, X] =
			select(Min(column))

		/** Creates a single column SQL ''select'' returning the smallest value of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]].
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  *               The column must be of a type with [[net.noresttherein.oldsql.sql.SQLOrdering SQLOrdering]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general
		  *               and the actual ordering is not influenced by the type class.
		  * @return SQL expression representing `select min(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Min]]
		  */
		def min[X :SQLOrdering](column :JoinedMappings[F] => ColumnSQL[Generalized, LocalScope, X]) :SelectColumn[Base, X] =
			select(Min(column(thisClause.mappings)))


		/** Creates a single column SQL ''select'' returning the largest value of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]].
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  *               The column must be of a type with [[net.noresttherein.oldsql.sql.SQLOrdering SQLOrdering]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  * @return an SQL expression representing `select max(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Max]]
		  */
		def max[X :SQLOrdering](column :ColumnSQL[Generalized, LocalScope, X]) :SelectColumn[Base, X] =
			select(Max(column))

		/** Creates a single column SQL ''select'' returning the largest value of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]].
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  *               The column must be of a type with [[net.noresttherein.oldsql.sql.SQLOrdering SQLOrdering]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.		  *
		  * @return an SQL expression representing `select max(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Max]]
		  */
		def max[X :SQLOrdering](column :JoinedMappings[F] => ColumnSQL[Generalized, LocalScope, X]) :SelectColumn[Base, X] =
			select(Max(column(thisClause.mappings)))


		/** Creates a single column SQL ''select'' returning the sum of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  *               The column must be of a numeric type with [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select sum(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Sum]]
		  */
		def sum[X :SQLNumber](column :ColumnSQL[Generalized, LocalScope, X]) :SelectColumn[Base, X] =
			select(Sum(column))

		/** Creates a single column SQL ''select'' returning the sum of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  *               The column must be of a numeric type with [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.		  *
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select sum(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Sum]]
		  */
		def sum[X :SQLNumber](column :JoinedMappings[F] => ColumnSQL[Generalized, LocalScope, X]) :SelectColumn[Base, X] =
			select(Sum(column(thisClause.mappings)))


		/** Creates a single column SQL ''select'' returning the average of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  *               The column must be of a numeric type with [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select avg(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Avg]]
		  */
		def avg[X :SQLNumber](column :ColumnSQL[Generalized, LocalScope, X]) :SelectColumn[Base, BigDecimal] =
			select(Avg(column))

		/** Creates a single column SQL ''select'' returning the average of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  *               The column must be of a numeric type with [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.		  *
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select avg(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Var]]
		  */
		def avg[X :SQLNumber](column :JoinedMappings[F] => ColumnSQL[Generalized, LocalScope, X])
				:SelectColumn[Base, BigDecimal] =
			select(Avg(column(thisClause.mappings)))


		/** Creates a single column SQL ''select'' returning the variance of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  *               The column must be of a numeric type with [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select var(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Var]]
		  */
		def variance[X :SQLNumber](column :ColumnSQL[Generalized, LocalScope, X]) :SelectColumn[Base, BigDecimal] =
			select(Var(column))

		/** Creates a single column SQL ''select'' returning the variance of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  *               The column must be of a numeric type with [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.		  *
		  *               It cannot be used to provide custom arithmetic.
		  * @return a SQL expression representing `select var(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.StdDev]]
		  */
		def variance[X :SQLNumber](column :JoinedMappings[F] => ColumnSQL[Generalized, LocalScope, X])
				:SelectColumn[Base, BigDecimal] =
			select(Var(column(thisClause.mappings)))


		/** Creates a single column SQL ''select'' returning the standard deviation of the gaussian approximation
		  * of the value distribution of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  *               The column must be of a numeric type with [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select stddev(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.StdDev]]
		  */
		def stddev[X :SQLNumber](column :ColumnSQL[Generalized, LocalScope, X]) :SelectColumn[Base, BigDecimal] =
			select(StdDev(column))

		/** Creates a single column SQL ''select'' returning the standard deviation of the gaussian approximation
		  * of the value distribution of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all rows returned by this ''from'' clause.
		  * Null values are ignored; the created SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] can be modified
		  * to ignore duplicate values using its [[net.noresttherein.oldsql.sql.ast.SelectSQL.distinct distinct]] method.
		  * @param column a function from a facade to this clause providing access to the mappings of its relations,
		  *               returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on this clause.
		  *               The column must be of a numeric type with [[net.noresttherein.oldsql.sql.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.		  *
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select stddev(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.StdDev]]
		  */
		def stddev[X :SQLNumber](column :JoinedMappings[F] => ColumnSQL[Generalized, LocalScope, X])
				:SelectColumn[Base, BigDecimal] =
			select(StdDev(column(thisClause.mappings)))

	}






	/** Extension methods for `TopFromSome` objects (''from'' clauses without any `Subselect`s which can serve
	  * as the basis for independent selects). It provides methods for introducing unbound parameters
	  * to the clause in the form of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] 'joins',
	  * which can be substituted with
	  */
	implicit class TopFromSomeExtension[F <: TopFromSome](private val thisClause :F) extends AnyVal {

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */
		@inline def param[X :SQLForm] :F WithParam X = JoinParam(thisClause, ParamRelation[X]())

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  *
		  * The artificial pseudo relation [[net.noresttherein.oldsql.sql.UnboundParam.ParamRelation ParamRelation]]
		  * is best obtained using the [[net.noresttherein.oldsql.sql.UnboundParam.?: ?:]] factory method
		  * defined in the [[net.noresttherein.oldsql.sql.UnboundParam UnboundParam]] object:
		  * {{{
		  *     From(Hamsters) param ?:[String] on (_.name === _)
		  * }}}
		  * @param relation a pseudo relation dedicated to `UnboundParam` joins, representing a future parameter
		  *                 of type `X`, which can be later accessed as any other mappings.
		  * @tparam X the parameter type.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */
		@inline def param[X](relation :ParamRelation[X]) :F WithParam X = JoinParam(thisClause, relation)

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @param name the suggested name for the parameter in the generated SQL, as specified by JDBC.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */
		@inline def param[X :SQLForm](name :String) :F WithParam X = JoinParam(thisClause, ParamRelation[X](name))

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @tparam N a string literal used as the label for the mapping and suggested parameter name.
		  * @tparam X parameter type.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */ //the order of implicits is important to avoid double definition
		@inline def param[N <: Label, X](implicit form :SQLForm[X], name :ValueOf[N]) :F JoinParam (N ?: X)#P =
			JoinParam(thisClause, form ?: (name.value :N))


		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  *
		  * The recommended ways of creating these relations are the
		  * [[net.noresttherein.oldsql.sql.UnboundParam.?: ?:]] factory method in object
		  * [[net.noresttherein.oldsql.sql.UnboundParam UnboundParam]] and an extension method for `String` literals
		  * with the same name: [[net.noresttherein.oldsql.sql.UnboundParam.method_?:.?: ?:]].
		  * {{{
		  *     def parameterize[N <: Label :ValueOf, X :SQLForm, F <: TopFromSome](from :F) :F JoinParam (N ?: X)#P =
		  *         from param ?:[N, X]
		  *
		  *     From(Characters) param "XP".?:[Int] where { t => t[Characters].XP >= t.labeled["XP"] }
		  * }}}
		  * Importing the `?:` symbol imports at the same time the factory methods for named and unnamed parameter
		  * relations, the implicit conversion enriching `String` literals, and the container type
		  * with the type constructor for the type of `Mapping` used by `JoinParam`.
		  * @param relation a pseudo relation dedicated to `UnboundParam` joins, representing a future parameter
		  *                 of type `X`, with its similarly synthetic `Mapping` being labeled with `N`,
		  *                 used at the same time for the suggested parameter name.
		  * @tparam N a string literal used as the label for the mapping and suggested parameter name.
		  * @tparam X parameter type.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */
		@inline def param[N <: Label, X](relation :NamedParamRelation[N, X]) :F JoinParam (N ?: X)#P =
			JoinParam(thisClause, relation)

	}



	/** A refinement of the [[net.noresttherein.oldsql.sql.FromSome FromSome]] type, representing a top level,
	  * independent and non-empty ''from'' clause of a ''select'' without a ''group by'' clause - one which doesn't contain
	  * any [[net.noresttherein.oldsql.sql.Subselect Subselect]] or [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]
	  * joins (is not a ''from'' clause of a subselect of some other select). In order to conform naturally
	  * (rather than by refinement) to `TopFromSome`, the clause must be ''complete'', that is its static type
	  * must start with either [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.From From]]
	  * - rather than a wildcard or abstract type hiding an unknown prefix - and its `Generalized` supertype
	  * must be known, meaning all join kinds should be narrowed down at least to
	  * [[net.noresttherein.oldsql.sql.Join Join]]/[[net.noresttherein.oldsql.sql.JoinParam JoinParam]].
	  * Despite the requirement for completeness, this type does not conform in itself to
	  * [[net.noresttherein.oldsql.sql.RowProduct.GeneralizedFrom GeneralizedFrom]], maintaining a minimalistic
	  * definition to preserve the property of being an outer clause by as many transformation functions as possible.
	  * This is an 'ungrouped' subtype of the more generic [[net.noresttherein.oldsql.sql.RowProduct.TopFrom TopFrom]].
	  *  An `TopFromClause` can still contain (unbound) parameters;
	  *  [[net.noresttherein.oldsql.sql.FromClause.GroundFromClause GroundFromClause]] is the specialization of this type
	  *  without any `JoinParam` 'joins'.
	  */
	type TopFromSome = FromSome {
		type Implicit = RowProduct
	}

	/** A non empty `RowProduct` without any [[net.noresttherein.oldsql.sql.Subselect Subselect]],
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]],
	  * or [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]]
	  * joins. Representing a single ''from'' clause (and not one of a nested subselect), without a ''group by'' clause,
	  * it can be used as a basis of (top level) SQL ''selects''. In order to conform naturally (rather than by refinement),
	  * a type must be ''complete'', and the [[net.noresttherein.oldsql.sql.RowProduct.Generalized generalized]] form
	  * of every component clause must be known; such types will automatically also conform to
	  * [[net.noresttherein.oldsql.sql.RowProduct.GeneralizedFrom GeneralizedFrom]], but there is no subtype
	  * relationship between the two in the type system. A `GroundFromSome` will however always by a subtype of
	  * its more generic variants, [[net.noresttherein.oldsql.sql.RowProduct.GroundFrom GroundFrom]],
	  * [[net.noresttherein.oldsql.sql.FromClause.GroundFromClause GroundFromClause]] and
	  * [[net.noresttherein.oldsql.sql.FromSome.TopFromSome TopFromSome]] (which groups all outer clauses,
	  * including those with unbound parameters), as well as
	  * [[net.noresttherein.oldsql.sql.RowProduct.ParamlessFrom ParamlessFrom]].
	  */
	type GroundFromSome = FromSome {
		type Implicit = RowProduct
		type Base = RowProduct
		type DefineBase[+I <: RowProduct] = I
		type Params = @~
	}


}