package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.{Adjoin, ColumnSQL, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.RowProduct.{As, NonEmptyRow}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}






/**
  * @author Marcin Mościcki
  */ //todo: migrate to this class from JoinedRelationSubject
sealed abstract class MappingReveal[X[O] <: MappingAt[O], Y[O] <: U[O], +U[O] <: MappingAt[O]] {
	def back :MappingReveal[Y, X, MappingAt]

	def apply[F[M[O] <: MappingAt[O]]](thing :F[X]) :F[Y]

	@inline final def join[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]]](join :L J X) :L J Y =
		apply[({ type F[M[O] <: MappingAt[O]] = L J M })#F](join)

	def alias[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: NonEmptyRow, N <: Label]
	          (join :L J X As N) :L J Y As N =
		apply[({ type F[M[O] <: MappingAt[O]] = L J M As N })#F](join)

	def expr[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Adjoin B, S >: LocalScope <: GlobalScope, T]
	        (e :SQLExpression[L J X, S, T]) :SQLExpression[L J Y, S, T] =
		apply[({ type F[M[O] <: MappingAt[O]] = SQLExpression[L J M, S, T] })#F](e)

	def column[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Adjoin B, S >: LocalScope <: GlobalScope, T]
	          (e :ColumnSQL[L J X, S, T]) :ColumnSQL[L J Y, S, T] =
		apply[({ type F[M[O] <: MappingAt[O]] = ColumnSQL[L J M, S, T] })#F](e)
}





object MappingReveal {

	implicit def evidence[X[O] <: MappingAt[O]] :MappingReveal[X, X, X] = instance.asInstanceOf[MappingReveal[X, X, X]]

	private[this] val instance = new MappingReveal[MappingAt, MappingAt, MappingAt] {
		override def back = this

		override def apply[F[M[O] <: MappingAt[O]]](thing :F[MappingAt]) = thing
//		override def back[F[M[O] <: MappingAt[O]]](thing :F[MappingAt]) = thing
	}

	@inline implicit def reveal[F[M[O] <: MappingAt[O]], X[O] <: MappingAt[O], Y[O] <: U[O], U[O] <: MappingAt[O]]
	                           (thing :F[X])(implicit reveal :MappingReveal[X, Y, U]) :F[Y] =
		reveal(thing)

	@inline implicit def hide[F[M[O] <: MappingAt[O]], X[O] <: MappingAt[O], Y[O] <: U[O], U[O] <: MappingAt[O]]
	                         (thing :F[Y])(implicit reveal :MappingReveal[X, Y, U]) :F[X] =
		reveal.back(thing)

	@inline implicit def revealJoin[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]],
	                                X[O] <: MappingAt[O], Y[O] <: BaseMapping[_, O]]
	                               (join :L J X)(implicit reveal :MappingReveal[X, Y, MappingAt]) :L J Y =
		reveal.join(join)

	@inline implicit def hideJoin[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]],
	                              X[O] <: MappingAt[O], Y[O] <: BaseMapping[_, O]]
	                             (join :L J Y)(implicit reveal :MappingReveal[X, Y, MappingAt]) :L J X =
		reveal.back.join(join)

	@inline implicit def revealJoinExpression
	                     [L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Adjoin B,
	                      S >: LocalScope <: GlobalScope, T, X[O] <: MappingAt[O], Y[O] <: BaseMapping[_, O]]
	                     (expr :SQLExpression[L J X, S, T])(implicit reveal :MappingReveal[X, Y, MappingAt])
			:SQLExpression[L J Y, S, T] =
		reveal.expr(expr)

	@inline implicit def hideJoinExpression
	                     [L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Adjoin B,
	                      S >: LocalScope <: GlobalScope, T, X[O] <: MappingAt[O], Y[O] <: BaseMapping[_, O]]
	                     (expr :SQLExpression[L J Y, S, T])(implicit reveal :MappingReveal[X, Y, MappingAt])
			:SQLExpression[L J X, S, T] =
		reveal.back.expr(expr)

	@inline implicit def revealJoinColumn[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Adjoin B,
	                                      S >: LocalScope <: GlobalScope, T,
	                                      X[O] <: MappingAt[O], Y[O] <: BaseMapping[_, O]]
	                                     (expr :ColumnSQL[L J X, S, T])(implicit reveal :MappingReveal[X, Y, MappingAt])
			:ColumnSQL[L J Y, S, T] =
		reveal.column(expr)

	@inline implicit def hideJoinColumn[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Adjoin B,
	                                    S >: LocalScope <: GlobalScope, T,
	                                    X[O] <: MappingAt[O], Y[O] <: BaseMapping[_, O]]
	                                   (expr :ColumnSQL[L J Y, S, T])(implicit reveal :MappingReveal[X, Y, MappingAt])
			:ColumnSQL[L J X, S, T] =
		reveal.back.column(expr)




	type MappingSubject[X[O] <: MappingAt[O], Y[O] <: RefinedMapping[S, O], S] =
		MappingReveal[X, Y, MappingOf[S]#Projection]

	type BaseMappingSubject[X[O] <: MappingAt[O], Y[O] <: BaseMapping[S, O], S] =
		MappingReveal[X, Y, MappingOf[S]#TypedProjection]

}
