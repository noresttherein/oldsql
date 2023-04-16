package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Listing
import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, OriginProjection}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.IndexedMapping
import net.noresttherein.oldsql.sql.{By, ByOne, ByVal, ColumnSQL, FromSome, SingleSQL, GroupBy, GroupByClause, GroupByOne, GroupByVal, RowProduct}
import net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation
import net.noresttherein.oldsql.sql.RowProduct.GroupingOfGeneralized
import net.noresttherein.oldsql.sql.SQLExpression.Single
import net.noresttherein.oldsql.sql.ast.{ComponentSQL, IndexedSQL}






/** A type class for type `E`, allowing it to be used when creating ''group by'' clause elements
  * using [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.groupBy groupBy]] and
  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseExtension.by by]] methods.
  * @tparam F the actual ''from'' clause `<: `[[net.noresttherein.oldsql.sql.FromSome FromSome]]
  *           in the [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form,
  *           which the grouping expression created by this type class is based on. It must be a supertype of
  *           `G#GeneralizedDiscrete`.
  * @tparam G the clause which is being expanded with a new grouping expression. It will become the left side
  *           of the returned `this.Result`. It may be either the type `F` when adding a first grouping expression
  *           using [[net.noresttherein.oldsql.sql.GroupBy GroupBy]], or
  *           a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]], in which case it will be expanded
  *           with a [[net.noresttherein.oldsql.sql.By By]] class.
  * @tparam E the type of the expression which can be turned into a grouping expansion. Predefined implicit values
  *           of this type exist in the companion [[net.noresttherein.oldsql.sql.mechanics.CanGroup$ object]]
  *           for:
  *             - [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[V, O]` subtypes,
  *               where `O >: F <: RowProduct`,
  *             - [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] and
  *               [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL ColumnMappingSQL]],
  *             - [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, `[[net.noresttherein.oldsql.sql.SQLExpression.Single Single]]`, V]`
  *               and [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, Single, V]`.
  */
@implicitNotFound("Type ${E} cannot be used as a grouping expression in ${G}:\n" +
                  "Missing implicit CanGroup[${F}, ${G}, ${E}]")
trait CanGroup[-F <: RowProduct, -G <: RowProduct, -E] {
	type Result <: GroupByClause
	def apply(clause :G, expr :E) :Result
}




private[mechanics] sealed abstract class CanGroupArbitraryImplicits {

	implicit def groupByVal[O <: FromSome, F <: FromSome { type Generalized <: O }, V]
			:CanGroup[O, F, SingleSQL[O, V]] { type Result = F GroupByVal V } =
		new CanGroup[O, F, SingleSQL[O, V]] {
			override type Result = F GroupByVal V

			override def apply(clause :F, expr :SingleSQL[O, V]) = GroupByVal(clause, expr)
		}

	implicit def byVal[F <: FromSome, G <: GroupingOfGeneralized[F], V]
			:CanGroup[F, G, SingleSQL[F, V]] { type Result = G ByVal V } =
		new CanGroup[F, G, SingleSQL[F, V]] {
			override type Result = G ByVal V

			override def apply(clause :G, expr :SingleSQL[F, V]) = ByVal(clause, expr)
		}
}


private[mechanics] sealed abstract class CanGroupIndexedImplicits extends CanGroupArbitraryImplicits {
}


private[mechanics] sealed abstract class CanGroupColumnImplicits extends CanGroupIndexedImplicits {

	implicit def groupByOne[O <: FromSome, F <: FromSome { type Generalized <: O }, V]
			:CanGroup[O, F, ColumnSQL[O, Single, V]] { type Result = F GroupByOne V } =
		new CanGroup[O, F, ColumnSQL[O, Single, V]] {
			override type Result = F GroupByOne V

			override def apply(clause :F, expr :ColumnSQL[O, Single, V]) = GroupByOne(clause, expr)
		}

	implicit def byOne[F <: FromSome, G <: GroupingOfGeneralized[F], V]
			:CanGroup[F, G, ColumnSQL[F, Single, V]] { type Result = G ByOne V } =
		new CanGroup[F, G, ColumnSQL[F, Single, V]] {
			override type Result = G ByOne V

			override def apply(clause :G, expr :ColumnSQL[F, Single, V]) = ByOne(clause, expr)
		}
}


object CanGroup extends CanGroupColumnImplicits {

	implicit def groupByMapping[U <: FromSome, F <: FromSome { type Generalized = U }, O <: RowProduct, C <: Mapping, S]
	                           (implicit origin :C <:< MappingAt[O], belongs :U <:< O,
	                            shift :RelationCount.In[O], projection :OriginProjection[C, S])
			:CanGroup[U, F, C] { type Result = F GroupBy projection.WithOrigin } =
		new CanGroup[U, F, C] {
			override type Result = F GroupBy projection.WithOrigin

			override def apply(clause :F, expr :C) = clause.groupBy[C, S, O](expr)
//			{ //todo: clause groupBy expr <- requires Generalized <: O
//				val relation = clause.fullTableStack(shift.offset).toRelationSQL
//				                     .asInstanceOf[RelationSQL[U, MappingOf[Any]#TypedProjection, Any, U]]
//				val component = TypedComponentSQL(relation, projection[U](expr))(projection.isomorphism)
//				//todo: replace this with component in Scala 3 once overloading works
//				GroupBy[F, projection.WithOrigin, projection.WithOrigin, S](clause, component.asGrouping)
//			}
		}

	implicit def groupByComponent[O <: FromSome, F <: FromSome { type Generalized <: O }, M[A] <: BaseMapping[S, A], S]
	                             (implicit subject :M[O] <:< BaseMapping[S, O])
			:CanGroup[O, F, ComponentSQL[O, M]] { type Result = F GroupBy M } =
		new CanGroup[O, F, ComponentSQL[O, M]] {
			override type Result = F GroupBy M

			override def apply(clause :F, expr :ComponentSQL[O, M]) = GroupBy(clause, expr)
		}

	implicit def groupByListing[O <: FromSome, F <: FromSome { type Generalized = O }, S <: Listing]
			:CanGroup[O, F, IndexedSQL[O, Single, S]] { type Result = F GroupBy IndexedMapping.of[S]#M } =
		new CanGroup[O, F, IndexedSQL[O, Single, S]] {
			override type Result = F GroupBy IndexedMapping.of[S]#M

			override def apply(clause :F, expr :IndexedSQL[O, Single, S]) =
				GroupBy(clause)(GroupingRelation[O, IndexedMapping.of[S]#M, S](expr))
		}



	implicit def byMapping[F <: FromSome, G <: GroupingOfGeneralized[F], O <: RowProduct, C <: Mapping, S]
	                      (implicit origin :C <:< MappingAt[O], belongs :F <:< O,
	                       shift :RelationCount.In[O], projection :OriginProjection[C, S])
			:CanGroup[F, G, C] { type Result = G By projection.WithOrigin } =
		new CanGroup[F, G, C] {
			override type Result = G By projection.WithOrigin

			override def apply(clause :G, expr :C) = clause.by[C, S, O](expr)
		}

	implicit def byComponent[F <: FromSome, G <: GroupingOfGeneralized[F], M[O] <: BaseMapping[S, O], S]
	                        (implicit subject :M[F] <:< BaseMapping[S, F])
			:CanGroup[F, G, ComponentSQL[F, M]] { type Result = G By M } =
		new CanGroup[F, G, ComponentSQL[F, M]] {
			override type Result = G By M

			override def apply(clause :G, expr :ComponentSQL[F, M]) = By(clause, expr)
		}

	implicit def byListing[F <: FromSome, G <: GroupingOfGeneralized[F], S <: Listing]
			:CanGroup[F, G, IndexedSQL[F, Single, S]] { type Result = G By IndexedMapping.of[S]#M } =
		new CanGroup[F, G, IndexedSQL[F, Single, S]] {
			override type Result = G By IndexedMapping.of[S]#M

			override def apply(clause :G, expr :IndexedSQL[F, Single, S]) :G By IndexedMapping.of[S]#M =
				By(clause)(GroupingRelation[F, IndexedMapping.of[S]#M, S](expr))
		}
}

