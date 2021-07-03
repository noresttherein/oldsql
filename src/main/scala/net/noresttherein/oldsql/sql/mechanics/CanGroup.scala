package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.{By, ByOne, ByVal, ColumnSQL, FromSome, GroupBy, GroupByClause, GroupByOne, GroupByVal, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.RowProduct.GroupingOfGeneralized
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, GlobalSQL}
import net.noresttherein.oldsql.sql.ast.{ComponentSQL, RelationSQL}
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL






/** A type class for type `E`, allowing it to be used when creating ''group by'' clause elements
  * using [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.groupBy groupBy]] and
  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseExtension.by by]] methods.
  * @tparam F the actual ''from'' clause `<: `[[net.noresttherein.oldsql.sql.FromSome FromSome]]
  *           in the [[net.noresttherein.oldsql.sql.RowProduct.Generalized generalized]] form,
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
  *               [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL ColumnComponentSQL]],
  *             - [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, `[[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope GlobalScope]]`, V]`
  *               and [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, GlobalScope, V]`.
  */
@implicitNotFound("Type ${E} cannot be used as a grouping expression in ${G}:\n" +
                  "Missing implicit CanGroup[${F}, ${G}, ${E}]")
trait CanGroup[-F <: RowProduct, -G <: RowProduct, -E] {
	type Result <: GroupByClause
	def apply(clause :G, expr :E) :Result
}




sealed abstract class ArbitraryCanGroupImplicits {

	implicit def groupByVal[O <: FromSome, F <: FromSome { type Generalized <: O }, V]
			:CanGroup[O, F, GlobalSQL[O, V]] { type Result = F GroupByVal V } =
		new CanGroup[O, F, GlobalSQL[O, V]] {
			override type Result = F GroupByVal V

			override def apply(clause :F, expr :GlobalSQL[O, V]) =
				GroupByVal(clause, expr.anchor(clause.generalized))
		}

	implicit def byVal[F <: RowProduct, G <: GroupingOfGeneralized[F], V]
			:CanGroup[F, G, GlobalSQL[F, V]] { type Result = G ByVal V } =
		new CanGroup[F, G, GlobalSQL[F, V]] {
			override type Result = G ByVal V

			override def apply(clause :G, expr :GlobalSQL[F, V]) =
				ByVal(clause, expr.anchor(clause.fromClause.generalized))
		}
}


sealed abstract class CanGroupColumnImplicits extends ArbitraryCanGroupImplicits {

	implicit def groupByOne[O <: RowProduct, F <: FromSome { type Generalized <: O }, V]
			:CanGroup[O, F, ColumnSQL[O, GlobalScope, V]] { type Result = F GroupByOne V } =
		new CanGroup[O, F, ColumnSQL[O, GlobalScope, V]] {
			override type Result = F GroupByOne V

			override def apply(clause :F, expr :ColumnSQL[O, GlobalScope, V]) =
				GroupByOne(clause, expr.anchor(clause.generalized))
		}

	implicit def byOne[F <: RowProduct, G <: GroupingOfGeneralized[F], V]
			:CanGroup[F, G, ColumnSQL[F, GlobalScope, V]] { type Result = G ByOne V } =
		new CanGroup[F, G, ColumnSQL[F, GlobalScope, V]] {
			override type Result = G ByOne V

			override def apply(clause :G, expr :ColumnSQL[F, GlobalScope, V]) =
				ByOne(clause, expr.anchor(clause.fromClause.generalized))
		}
}


object CanGroup extends CanGroupColumnImplicits {

	implicit def groupByMapping[U <: FromSome, F <: FromSome { type Generalized = U }, O <: RowProduct, C <: Mapping, S]
	                           (implicit origin :C <:< MappingAt[O], belongs :U <:< O,
	                                     shift :TableCount[O, _ <: Numeral], projection :OriginProjection[C, S])
			:CanGroup[U, F, C] { type Result = F GroupBy projection.WithOrigin } =
		new CanGroup[U, F, C] {
			override type Result = F GroupBy projection.WithOrigin

			override def apply(clause :F, expr :C) = { //todo: clause groupBy expr <- requires Generalized <: O
				val relation = clause.fullTableStack(shift.offset).toRelationSQL
				                     .asInstanceOf[RelationSQL[U, MappingOf[Any]#TypedProjection, Any, U]]
				val component = TypedComponentSQL(relation, projection[U](expr))(projection.isomorphism)
				//todo: replace this with component in Scala 3 once overloading works
				GroupBy[F, projection.WithOrigin, projection.WithOrigin, S](clause, component.groupingRelation, component)
			}
		}

	implicit def groupByComponent[O <: RowProduct, F <: FromSome { type Generalized <: O },
	                              M[A] <: BaseMapping[S, A], S]
	                             (implicit subject :M[O] <:< BaseMapping[S, O])
			:CanGroup[O, F, ComponentSQL[O, M]] { type Result = F GroupBy M } =
		new CanGroup[O, F, ComponentSQL[O, M]] {
			override type Result = F GroupBy M

			override def apply(clause :F, expr :ComponentSQL[O, M]) = GroupBy(clause, expr)
		}



	implicit def byMapping[F <: RowProduct, G <: GroupingOfGeneralized[F], O <: RowProduct, C <: Mapping, S]
	                      (implicit origin :C <:< MappingAt[O], belongs :F <:< O,
	                                shift :TableCount[O, _ <: Numeral], projection :OriginProjection[C, S])
			:CanGroup[F, G, C] { type Result = G By projection.WithOrigin } =
		new CanGroup[F, G, C] {
			override type Result = G By projection.WithOrigin

			override def apply(clause :G, expr :C) = clause by[C, S, O] expr //(origin, shift, projection)
		}

	implicit def byComponent[F <: RowProduct, G <: GroupingOfGeneralized[F], M[O] <: BaseMapping[S, O], S]
	                        (implicit subject :M[F] <:< BaseMapping[S, F])
			:CanGroup[F, G, ComponentSQL[F, M]] { type Result = G By M } =
		new CanGroup[F, G, ComponentSQL[F, M]] {
			override type Result = G By M

			override def apply(clause :G, expr :ComponentSQL[F, M]) = By(clause, expr)
		}

}

