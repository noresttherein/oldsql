package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.{BaseMapping, ColumnMapping, Relation, SQLForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.{ApplyJoinParams, ExtendedBy, FreeFrom, FreeFromSome, JoinedMappings, NonEmptyFrom, OuterFromSome, ParameterlessFrom, PartOf}
import net.noresttherein.oldsql.sql.FromClause.GetTable.ByIndex
import net.noresttherein.oldsql.sql.JoinParam.WithParam
import net.noresttherein.oldsql.sql.MappingSQL.{FreeColumn, JoinedRelation}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.UnboundParam.{?:, ParamRelation}
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject.InferSubject






/** Marker trait for all true ''from'' clauses, without a ''group by'' clause. Most implementations extend
  * its non-empty subtype, [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome FromSome]].
  * @see [[net.noresttherein.oldsql.sql.GroupByClause]]
  */ //FromClause is redundant but makes the signature more clear.
trait DiscreteFrom extends FromClause { thisClause =>
	//override type FromLast <: DiscreteFrom //can't have this because Dual.FromLast = FromClause to be a fixed point.

	override type This <: DiscreteFrom {
		type LastMapping[O] = thisClause.LastMapping[O]
		type LastTable[F <: FromClause] = thisClause.LastTable[F]
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Self = thisClause.Self
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Outer = thisClause.Outer
		type Base = thisClause.Base
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
		type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] = thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
	}


	override def filter :GlobalBoolean[Generalized]

	override def filter[E <: FromClause](target :E)(implicit extension :Generalized PartOf E) :GlobalBoolean[E]



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
	protected[sql] def extend[T[O] <: BaseMapping[S, O], S]
	                   (right :LastRelation[T, S], filter :GlobalBoolean[Generalized AndFrom T]) :this.type AndFrom T



	/** A join between this clause and the clause `F`. This type is the result of replacing `Dual` in `F` with `Self`.
	  * Non-empty clauses define it as `F#JoinedWith[Self, J]]`, while `Dual` defines it as `F` - the indirection
	  * enforced by the join type `J` (and `Join` subclasses) having `FromSome` as the upper bound of their left side.
	  */
	type JoinWith[+J[+L <: FromSome, R[O] <: MappingAt[O]] <: L AndFrom R, F <: FromClause] <: FromClause

	/** Joins the clause given as the parameter with this clause. If any of the clauses is empty, the other is
	  * returned. Otherwise the created clause contains this clause as its prefix, followed by all relations
	  * from `suffix` joined with the same join kinds. The first `From` pseudo join in suffix is replaced with
	  * the join type specified by the template parameter (`join.LikeJoin`); if the first relation in `suffix` is
	  * however joined with `Dual` using another join type (such as `JoinParam`), it is preserved. This is a dispatch
	  * call to [[net.noresttherein.oldsql.sql.FromClause.joinedWith suffix.joinedWith(self, join)]]. This extra level
	  * of indirection is enforced by the upper bound of `FromSome` on the left type parameters in `Join` classes,
	  * while this method can be called also if this clause is empty. Additionally, the join kind to use between
	  * the last relation in this clause and the first relation in `suffix` can be specified as `Subselect`,
	  * while `joinedWith` allows only `Join` subclasses.
	  *
	  * It is a low level method and client code should prefer the eponymously named extension methods
	  * for individual join kinds defined in
	  * [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension FromSomeExtension]]: `join`, `outerJoin`,
	  * `innerJoin`, `leftJoin`, `rightJoin`, `subselect`, as they have more friendly return types.
	  * @param suffix the clause with relations which should be added to this clause
	  * @param join a template instance to use as the factory for the join between the last relation in this clause
	  *             and the first relation in `suffix`.
	  */
	def joinWith[F <: FromSome](suffix :F, join :JoinLike.* = InnerJoin.template) :JoinWith[join.LikeJoin, F]


	override type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] <: Generalized



	private[sql] def concrete_FromClause_subclass_must_extend_DiscreteFrom_or_GroupedFrom :Nothing =
		throw new UnsupportedOperationException
}






object DiscreteFrom {



	/** Common upper bound for all ''from'' clauses containing at least one relation, but no ''group by'' clause.
	  * Extended by every [[net.noresttherein.oldsql.sql.DiscreteFrom DiscreteFrom]] implementations
	  * other than [[net.noresttherein.oldsql.sql.Dual Dual]]. Most types do not do this directly however, but
	  * through the [[net.noresttherein.oldsql.sql.Extended Extended]], which is the base trait for recursively built
	  * clauses by adding a [[net.noresttherein.oldsql.sql.MappingSQL.JoinedRelation JoinedRelation]] to a prefix
	  * `FromClause`.
	  */
	trait FromSome extends NonEmptyFrom with DiscreteFrom { thisClause =>
		override type LastTable[F <: FromClause] = JoinedRelation[F, LastMapping]
		//override type FromLast >: this.type <: FromSome //>: this.type doesn't hold for decorators
		override type FromLast <: FromSome
		override type FromNext[E[+L <: FromSome] <: FromClause] = E[FromLast]

		override type This <: FromSome {
			type LastMapping[O] = thisClause.LastMapping[O]
			type FromLast = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Self = thisClause.Self
			type Params = thisClause.Params
			type FullRow = thisClause.FullRow
			type Explicit = thisClause.Explicit
			type Inner = thisClause.Inner
			type Implicit = thisClause.Implicit
			type Outer = thisClause.Outer
			type Base = thisClause.Base
			type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
			type InnerRow = thisClause.InnerRow
			type OuterRow = thisClause.OuterRow
			type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] = thisClause.JoinedWith[P, J]
			type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
			type FromRelation[T[O] <: MappingAt[O]] = thisClause.FromRelation[T]
			type FromSubselect[+F <: NonEmptyFrom] = thisClause.FromSubselect[F]
		}


		override def filter :GlobalBoolean[Generalized] = filter(generalized)

		override type JoinFilter[E[+L <: FromSome] <: L Extended N, S <: FromClause Extended N, G <: S, N[O] <: MappingAt[O]] =
			                    (JoinedRelation[FromNext[E], LastMapping], JoinedRelation[S, N]) => GlobalBoolean[G]

		protected override def filterNext[F <: FromClause AndFrom N, N[O] <: MappingAt[O]]
		                       (next :F)(filter :JoinFilter[next.GeneralizedLeft, next.FromLast, next.Generalized, N])
				:next.This =
		{
			val condition = filter(last.asIn(next.generalizedExtension[FromLast]), next.last)
			val grounded = SQLScribe.groundFreeComponents(next.generalized)(condition)
			next.where(grounded)
		}

		override type Extend[+J[+L <: FromSome, R[O] <: T[O]] <: L Extended R, T[O] <: MappingAt[O]] = Self J T

		override def extend[T[O] <: BaseMapping[S, O], S]
		                   (next :Relation[T], filter :GlobalBoolean[Generalized AndFrom T], join :JoinLike.*)
				:join.LikeJoin[Self, T] =
			join.likeJoin[Self, T, S](self, next)(filter)

		protected[sql] def extend[T[O] <: BaseMapping[S, O], S]
		                         (right :LastRelation[T, S], filter :GlobalBoolean[Generalized AndFrom T])
				:this.type AndFrom T =
			InnerJoin[this.type, T, S](this, right)(filter)



		override type JoinWith[+J[+L <: FromSome, R[O] <: MappingAt[O]] <: L AndFrom R, F <: FromClause] =
			F#JoinedWith[Self, J]

		override def joinWith[F <: FromSome](suffix :F, join :JoinLike.*) :suffix.JoinedWith[Self, join.LikeJoin] =
			join.likeJoin(self, suffix)


	}






	/** Extension methods performing most generic join between any ungrouped (i.e., pure) `FromClause`,
	  * and other relations. */
	implicit class DiscreteFromExtension[F <: DiscreteFrom](val clause :F) extends AnyVal {

		/** Performs an inner join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side. The real type of the result depends on the type of this clause:
		  * for `Dual`, the result is `From[R]`, for non-empty clauses the result is `F InnerJoin R`.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on()]],
		  * [[net.noresttherein.oldsql.sql.AndFrom.where where()]] or
		  * [[net.noresttherein.oldsql.sql.AndFrom.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @return `From[R]` if this clause is empty or `F InnerJoin R` otherwise.
		  */
		@inline def andFrom[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		            (table :Relation[R])
		            (implicit cast :JoinedRelationSubject[AndFrom.WithLeft[F]#F, R, T, MappingOf[S]#TypedProjection])
				:F AndFrom R =
			AndFrom(clause, table)

		/** Performs a join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side. The join type between the clauses will be an `InnerJoin` if the dynamic
		  * type of the first join of the given clause is `From[_]`, otherwise the join type is preserved.
		  * If either of the clauses is empty, the other is returned unchanged.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be
		  * subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def andFrom[R <: DiscreteFrom](other :R) :other.JoinedWith[F, AndFrom] = other.appendedTo(clause)
	}




	/** Extension methods for `FromSome` classes (non-empty ''from'' clauses) which benefit from having a static,
	  * invariant self type. Most notably, this includes methods for joining it with other relations.
	  */
	implicit class FromSomeExtension[F <: FromSome](val clause :F) extends AnyVal {

		/** Performs an inner join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on()]],
		  * [[net.noresttherein.oldsql.sql.AndFrom.where where()]] or
		  * [[net.noresttherein.oldsql.sql.AndFrom.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def join[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                (table :Relation[R])(implicit cast :InferSubject[F, InnerJoin, R, T, S]) :F InnerJoin R =
			InnerJoin(clause, table)

		/** Performs an inner join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be
		  * subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def join[R <: FromSome](other :R) :other.JoinedWith[F, InnerJoin] =
			other.joinedWith(clause, InnerJoin.template)



		/** Performs a symmetric outer join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on()]],
		  * [[net.noresttherein.oldsql.sql.AndFrom.where where()]] or
		  * [[net.noresttherein.oldsql.sql.AndFrom.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def outerJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :Relation[R])
		                     (implicit cast :InferSubject[F, OuterJoin, R, T, S]) :F OuterJoin R =
			OuterJoin(clause, table)

		/** Performs a symmetric outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be
		  * subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def outerJoin[R <: FromSome](other :R) :other.JoinedWith[F, OuterJoin] =
			other.joinedWith(clause, OuterJoin.template)



		/** Performs a left outer join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on()]],
		  * [[net.noresttherein.oldsql.sql.AndFrom.where where()]] or
		  * [[net.noresttherein.oldsql.sql.AndFrom.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def leftJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                    (table :Relation[R])
		                    (implicit cast :InferSubject[F, LeftJoin, R, T, S]) :F LeftJoin R =
			LeftJoin(clause, table)

		/** Performs a left outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be
		  * subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def leftJoin[R <: FromSome](other :R) :other.JoinedWith[F, LeftJoin] =
			other.joinedWith(clause, LeftJoin.template)



		/** Performs a right outer join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on()]],
		  * [[net.noresttherein.oldsql.sql.AndFrom.where where()]] or
		  * [[net.noresttherein.oldsql.sql.AndFrom.whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def rightJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :Relation[R])
		                     (implicit cast :InferSubject[F, RightJoin, R, T, S]) :F RightJoin R =
			RightJoin(clause, table)

		/** Performs a right outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be
		  * subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def rightJoin[R <: FromSome](other :R) :other.JoinedWith[F, RightJoin] =
			other.joinedWith(clause, RightJoin.template)



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
		                       (implicit cast :InferSubject[clause.type, InnerJoin, R, T, S],
		                        last :ByIndex[clause.Generalized, -1] { type G >: clause.Generalized <: FromSome })
				:F InnerJoin R =
			cast(InnerJoin[clause.type, T, T, S](clause, cast(table)) where naturalFilter[T] _)


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
		                            (implicit cast :InferSubject[clause.type, OuterJoin, R, T, S],
		                             last :ByIndex[clause.Generalized, -1] { type G >: clause.Generalized <: FromSome })
				:F OuterJoin R =
			cast(OuterJoin[clause.type, T, T, S](clause, cast(table)) where naturalFilter[T] _)


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
		                           (implicit cast :InferSubject[clause.type, LeftJoin, R, T, S],
		                            last :ByIndex[clause.Generalized, -1] { type G >: clause.Generalized <: FromSome })
				:F LeftJoin R =
			cast(LeftJoin[clause.type, T, T, S](clause, cast(table)) where naturalFilter[T] _)


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
		                            (implicit cast :InferSubject[clause.type, RightJoin, R, T, S],
		                             last :ByIndex[clause.Generalized, -1] { type G >: clause.Generalized <: FromSome })
				:F RightJoin R =
			cast(RightJoin[clause.type, T, T, S](clause, cast(table))(cast.self) where naturalFilter[T] _)


		private def naturalFilter[T[O] <: BaseMapping[_, O]]
		                         (tables :JoinedMappings[clause.Generalized Join T])
		                         (implicit prev :ByIndex[clause.Generalized Join T, -2])
				:GlobalBoolean[clause.Generalized Join T] =
		{
			val firstTable = tables.prev
			val secondTable = tables.last

			val firstColumns = firstTable.columns.map(c => c.name -> c).toMap //todo: below - nondeterministic compilation
			val secondColumns = secondTable.columns.map(c => c.name -> (c :ColumnMapping[_, FromClause AndFrom T])).toMap
			val common = firstColumns.keySet & secondColumns.keySet

			val joins = common map { name =>

				val first = firstColumns(name).asInstanceOf[ColumnMapping[Any, prev.G]]
				val second = secondColumns(name).asInstanceOf[ColumnMapping[Any, DiscreteFrom AndFrom T]]
				if (first.form != second.form)
					throw new IllegalArgumentException(s"Can't perform a natural join of $firstTable and $secondTable: " +
							s"columns $first and $second have different types (forms): ${first.form} and ${second.form}.")

				FreeColumn(first, 1) === FreeColumn(second, 0)
			}
			(True[clause.Generalized Join T]() /: joins)(_ && _)
		}



		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using this clause.
		  * The explicit list of relations in the clause is initialized with the relation given as a `Relation` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.AndFrom.on on()]], [[net.noresttherein.oldsql.sql.AndFrom.where where()]]
		  * and [[net.noresttherein.oldsql.sql.AndFrom.whereLast whereLast()]] methods.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def subselect[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :Relation[R])
		                     (implicit cast :InferSubject[F, Subselect, R, T, S]) :F Subselect R =
			Subselect(clause, table)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using on clause.
		  * The explicit list of relations in the clause is initialized with the relations given as a `FromClause`
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relations - following the `Subselect`
		  * pseudo join and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.AndFrom.on on()]], [[net.noresttherein.oldsql.sql.AndFrom.where where()]]
		  * and [[net.noresttherein.oldsql.sql.AndFrom.whereLast whereLast()]] methods.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  * @throws UnsupportedOperationException if `other` is empty or its first join is a `JoinParam`.
		  */
		@inline def subselect[R <: FromSome](other :R) :other.JoinedWithSubselect[F] =
			other.joinedWithSubselect(clause)



		/** Applies this clause to its parameters: removes all `JoinParam` joins and substitutes all references
		  * to parameter components with SQL literals extracted from parameter values.
		  * @param params a chain consisting of subject types of all parameter mappings of all `JoinParam` joins
		  *               in this clause in order of their appearance.
		  * @tparam U `FromClause` subtype obtained by removing all `JoinParam` instances from this clause's type.
		  */
		def apply[U <: ParameterlessFrom](params :clause.Params)
		                                 (implicit apply :ApplyJoinParams[F, clause.Params, U]) :U =
			apply(clause, params)

	}






	/** Extension methods for `OuterFrom` objects (''from'' clauses without any `Subselect`s which can serve
	  * as the basis for independent selects). It provides methods for introducing unbound parameters
	  * to the clause in the form of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] 'joins',
	  * which can be substituted with
	  */
	implicit class OuterFromSomeExtension[F <: OuterFromSome](private val clause :F) extends AnyVal {

		/** Creates a parameterized `FromClause` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  */
		@inline def param[X :SQLForm] :F WithParam X = JoinParam(clause, ParamRelation[X]())

		/** Creates a parameterized `FromClause` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @param name the suggested name for the parameter in the generated SQL, as specified by JDBC.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  */
		@inline def param[X :SQLForm](name :String) :F WithParam X = JoinParam(clause, ParamRelation[X](name))

		/** Creates a parameterized `FromClause` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @tparam N a string literal used as the label for the mapping and suggested parameter name.
		  * @tparam X parameter type.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  */
		@inline def param[N <: Label, X](implicit form :SQLForm[X], name :ValueOf[N]) :F JoinParam (N ?: X)#T =
			JoinParam(clause, form ?: (name.value :N))

	}


}