package net.noresttherein.oldsql.schema.forms

import java.sql.{JDBCType, PreparedStatement}

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.schema.SQLWriteForm
import net.noresttherein.oldsql.schema.SQLWriteForm.{CompositeWriteForm, WriteFormSeparateLiterals}






trait ScalaWriteForms {

	@inline private[this] def f[T](implicit form :SQLWriteForm[T]) :SQLWriteForm[T] = form

	
	//implicit type class definitions
	
	implicit def tuple2WriteForm[A :SQLWriteForm, B :SQLWriteForm] :SQLWriteForm[(A, B)] =
		new Tuple2WriteForm[A, B](f[A], f[B])

	implicit def tuple3WriteForm[A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm] :SQLWriteForm[(A, B, C)] =
		new Tuple3WriteForm[A, B, C](f[A], f[B], f[C])

	implicit def tuple4WriteForm[A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm]
			:SQLWriteForm[(A, B, C, D)] =
		new Tuple4WriteForm[A, B, C, D](f[A], f[B], f[C], f[D])

	implicit def tuple5WriteForm[A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm]
			:SQLWriteForm[(A, B, C, D, E)] =
		new Tuple5WriteForm[A, B, C, D, E](f[A], f[B], f[C], f[D], f[E])

	implicit def tuple6WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F)] =
		new Tuple6WriteForm[A, B, C, D, E, F](f[A], f[B], f[C], f[D], f[E], f[F])

	implicit def tuple7WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G)] =
		new Tuple7WriteForm[A, B, C, D, E, F, G](f[A], f[B], f[C], f[D], f[E], f[F], f[G])

	implicit def tuple8WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H)] =
		new Tuple8WriteForm[A, B, C, D, E, F, G, H](f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H])

	implicit def tuple9WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I)] =
		new Tuple9WriteForm[A, B, C, D, E, F, G, H, I](f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I])

	implicit def tuple10WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J)] =
		new Tuple10WriteForm[A, B, C, D, E, F, G, H, I, J](f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J])

	implicit def tuple11WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K)] =
		new Tuple11WriteForm[A, B, C, D, E, F, G, H, I, J, K](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K]
		)

	implicit def tuple12WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L)] =
		new Tuple12WriteForm[A, B, C, D, E, F, G, H, I, J, K, L](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L]
		)

	implicit def tuple13WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
		new Tuple13WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M]
		)

	implicit def tuple14WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
		new Tuple14WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N]
		)

	implicit def tuple15WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
		new Tuple15WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O]
		)

	implicit def tuple16WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
		new Tuple16WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P]
		)

	implicit def tuple17WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm, Q :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
		new Tuple17WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q]
		)

	implicit def tuple18WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm, Q :SQLWriteForm, R :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
		new Tuple18WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R]
		)

	implicit def tuple19WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm, Q :SQLWriteForm, R :SQLWriteForm,
				S :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
		new Tuple19WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R],
			f[S]
		)

	implicit def tuple20WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm, Q :SQLWriteForm, R :SQLWriteForm,
				S :SQLWriteForm, T :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
		new Tuple20WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R],
			f[S], f[T]
		)

	implicit def tuple21WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm, Q :SQLWriteForm, R :SQLWriteForm,
				S :SQLWriteForm, T :SQLWriteForm, U :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
		new Tuple21WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R],
			f[S], f[T], f[U]
		)

	implicit def tuple22WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm, Q :SQLWriteForm, R :SQLWriteForm,
				S :SQLWriteForm, T :SQLWriteForm, U :SQLWriteForm, V :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
		new Tuple22WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R],
			f[S], f[T], f[U], f[V]
		)




	//implicit conversions from tuples of forms
	
	implicit def toTuple2WriteForm[A, B](t :(SQLWriteForm[A], SQLWriteForm[B])) = 
		new Tuple2WriteForm(t._1, t._2)
	
	implicit def toTuple3WriteForm[A, B, C](t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C])) =
		new Tuple3WriteForm(t._1, t._2, t._3)

	implicit def toTuple4WriteForm[A, B, C, D](t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D])) =
		new Tuple4WriteForm(t._1, t._2, t._3, t._4)
	
	implicit def toTuple5WriteForm[A, B, C, D, E]
	                         (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E])) =
		new Tuple5WriteForm(t._1, t._2, t._3, t._4, t._5)
	
	implicit def toTuple6WriteForm[A, B, C, D, E, F]
	                         (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                              SQLWriteForm[F])) =
		new Tuple6WriteForm(t._1, t._2, t._3, t._4, t._5, t._6)
	
	implicit def toTuple7WriteForm[A, B, C, D, E, F, G]
	                         (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                              SQLWriteForm[F], SQLWriteForm[G])) =
		new Tuple7WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7)
	
	implicit def toTuple8WriteForm[A, B, C, D, E, F, G, H]
	                         (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                              SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H])) =
		new Tuple8WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)
	
	implicit def toTuple9WriteForm[A, B, C, D, E, F, G, H, I]
	                         (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                              SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I])) =
		new Tuple9WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
		
	implicit def toTuple10WriteForm[A, B, C, D, E, F, G, H, I, J]
	             (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E], 
	                  SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I], SQLWriteForm[J])) =
		new Tuple10WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)
		
	implicit def toTuple11WriteForm[A, B, C, D, E, F, G, H, I, J, K]
	             (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                  SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I], SQLWriteForm[J], 
	                  SQLWriteForm[K])) =
		new Tuple11WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)
		
	implicit def toTuple12WriteForm[A, B, C, D, E, F, G, H, I, J, K, L]
	             (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                  SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I], SQLWriteForm[J],
	                  SQLWriteForm[K], SQLWriteForm[L])) =
		new Tuple12WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)
		
	implicit def toTuple13WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M]
	             (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                  SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I], SQLWriteForm[J],
	                  SQLWriteForm[K], SQLWriteForm[L], SQLWriteForm[M])) =
		new Tuple13WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)
		
	implicit def toTuple14WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
	             (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                  SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I], SQLWriteForm[J],
	                  SQLWriteForm[K], SQLWriteForm[L], SQLWriteForm[M], SQLWriteForm[N])) =
		new Tuple14WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)
		
	implicit def toTuple15WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
	             (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                  SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I], SQLWriteForm[J],
	                  SQLWriteForm[K], SQLWriteForm[L], SQLWriteForm[M], SQLWriteForm[N], SQLWriteForm[O])) =
		new Tuple15WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                         t._11, t._12, t._13, t._14, t._15)
		
	implicit def toTuple16WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
	             (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
		                           SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I], SQLWriteForm[J],
		                           SQLWriteForm[K], SQLWriteForm[L], SQLWriteForm[M], SQLWriteForm[N], SQLWriteForm[O],
		                           SQLWriteForm[P])) =
		new Tuple16WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                         t._11, t._12, t._13, t._14, t._15, t._16)
		
	implicit def toTuple17WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
	             (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                  SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I], SQLWriteForm[J],
	                  SQLWriteForm[K], SQLWriteForm[L], SQLWriteForm[M], SQLWriteForm[N], SQLWriteForm[O],
	                  SQLWriteForm[P], SQLWriteForm[Q])) =
		new Tuple17WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                         t._11, t._12, t._13, t._14, t._15, t._16, t._17)
		
	implicit def toTuple18WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
	             (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                  SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I], SQLWriteForm[J],
	                  SQLWriteForm[K], SQLWriteForm[L], SQLWriteForm[M], SQLWriteForm[N], SQLWriteForm[O],
	                  SQLWriteForm[P], SQLWriteForm[Q], SQLWriteForm[R])) =
		new Tuple18WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                         t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
		
	implicit def toTuple19WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
	             (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                  SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I], SQLWriteForm[J],
	                  SQLWriteForm[K], SQLWriteForm[L], SQLWriteForm[M], SQLWriteForm[N], SQLWriteForm[O],
	                  SQLWriteForm[P], SQLWriteForm[Q], SQLWriteForm[R], SQLWriteForm[S])) =
		new Tuple19WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                         t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
		
	implicit def toTuple20WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
	             (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                  SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I], SQLWriteForm[J],
	                  SQLWriteForm[K], SQLWriteForm[L], SQLWriteForm[M], SQLWriteForm[N], SQLWriteForm[O],
	                  SQLWriteForm[P], SQLWriteForm[Q], SQLWriteForm[R], SQLWriteForm[S], SQLWriteForm[T])) =
		new Tuple20WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                         t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
		
	implicit def toTuple21WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
	             (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                  SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I], SQLWriteForm[J],
	                  SQLWriteForm[K], SQLWriteForm[L], SQLWriteForm[M], SQLWriteForm[N], SQLWriteForm[O],
	                  SQLWriteForm[P], SQLWriteForm[Q], SQLWriteForm[R], SQLWriteForm[S], SQLWriteForm[T],
	                  SQLWriteForm[U])) =
		new Tuple21WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10,
	                         t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
		
	implicit def toTuple22WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
	             (t :(SQLWriteForm[A], SQLWriteForm[B], SQLWriteForm[C], SQLWriteForm[D], SQLWriteForm[E],
	                  SQLWriteForm[F], SQLWriteForm[G], SQLWriteForm[H], SQLWriteForm[I], SQLWriteForm[J],
	                  SQLWriteForm[K], SQLWriteForm[L], SQLWriteForm[M], SQLWriteForm[N], SQLWriteForm[O],
	                  SQLWriteForm[P], SQLWriteForm[Q], SQLWriteForm[R], SQLWriteForm[S], SQLWriteForm[T],
	                  SQLWriteForm[U], SQLWriteForm[V])) =
		new Tuple22WriteForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                         t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
	





	@inline private def literals[T](form :SQLWriteForm[T], value :T) :Seq[String] =
		form.columnLiterals(value)

	
	private[schema] trait AbstractTuple2WriteForm[-L, -R] 
		extends SQLWriteForm[(L, R)] with WriteFormSeparateLiterals[(L, R)] 
	{
		val _1 :SQLWriteForm[L]
		val _2 :SQLWriteForm[R]

		override val columnCount: Int = _1.columnCount + _2.columnCount
		override lazy val columnTypes :Seq[JDBCType] = _1.columnTypes ++ _2.columnTypes
		override def isUniversal: Boolean = _1.isUniversal && _2.isUniversal

		override def set(statement :PreparedStatement, position :Int, value :(L, R)) :Unit =
			if (value == null) {
				_1.setNull(statement, position)
				_2.setNull(statement, position + _1.columnCount)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + _1.columnCount, value._2)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + _1.columnCount)
		}


		override def inlineLiteral(value: (L, R)): String =
			if (value == null) inlineNullLiteral
			else _1.inlineLiteral(value._1) + ", " + _2.inlineLiteral(value._2)

		override lazy val inlineNullLiteral :String = _1.inlineNullLiteral + ", " + _2.inlineNullLiteral

		override def columnLiterals(value: (L, R)): Seq[String] =
			_1.columnLiterals(value._1) ++ _2.columnLiterals(value._2)

		override lazy val nullColumnLiterals: Seq[String] = _1.nullColumnLiterals ++ _2.nullColumnLiterals


		override def split =
			(_1.split.view.map(_.compose((_:(L, R))._1)) ++ 
				_2.split.view.map(_.compose((_:(L, R))._2))
			).to(ArraySeq)
			
	}

	private[schema] case class Tuple2WriteForm[-L, -R](_1 :SQLWriteForm[L], _2 :SQLWriteForm[R])
		extends AbstractTuple2WriteForm[L, R]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple3WriteForm[-A, -B, -C] extends CompositeWriteForm[(A, B, C)] {
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		lazy val forms = Seq(_1, _2, _3)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount

		override val columnCount: Int = offset_3 + _3.columnCount
		override lazy val columnTypes :Seq[JDBCType] = _1.columnTypes ++ _2.columnTypes ++ _3.columnTypes

		override def set(statement :PreparedStatement, position :Int, value :(A, B, C)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
		}


		override def literal(t :(A, B, C), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3)

		override def split =
			(_1.split.view.map(_.compose((_:(A, B, C))._1)) ++
				_2.split.view.map(_.compose((_:(A, B, C))._2)) ++
				_3.split.view.map(_.compose((_:(A, B, C))._3))
			).to(ArraySeq)
	}

	private[schema] case class Tuple3WriteForm[-A, -B, -C](_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C])
		extends AbstractTuple3WriteForm[A, B, C]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple4WriteForm[-A, -B, -C, -D] extends CompositeWriteForm[(A, B, C, D)] {
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		override lazy val forms = Seq(_1, _2, _3, _4)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount

		override val columnCount: Int = offset_4 + _4.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes

		override def set(statement :PreparedStatement, position :Int, value :(A, B, C, D)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
		}


		override def literal(t :(A, B, C, D), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D))._4))
			).to(ArraySeq)
	}

	private[schema] case class Tuple4WriteForm[-A, -B, -C, -D](_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D])
		extends AbstractTuple4WriteForm[A, B, C, D]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple5WriteForm[-A, -B, -C, -D, -E] extends CompositeWriteForm[(A, B, C, D, E)] {
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		override lazy val forms = Seq(_1, _2, _3, _4, _5)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount

		override val columnCount: Int = offset_5 + _5.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes

		override def set(statement :PreparedStatement, position :Int, value :(A, B, C, D, E)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
		}


		override def literal(t :(A, B, C, D, E), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E))._5))
			).to(ArraySeq)
	}

	private[schema] case class Tuple5WriteForm[-A, -B, -C, -D, -E](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E]
		) extends AbstractTuple5WriteForm[A, B, C, D, E]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple6WriteForm[-A, -B, -C, -D, -E, -F] extends CompositeWriteForm[(A, B, C, D, E, F)] {
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		override lazy val forms = Seq(_1, _2, _3, _4, _5, _6)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount

		override val columnCount: Int = offset_6 + _6.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes

		override def set(statement :PreparedStatement, position :Int, value :(A, B, C, D, E, F)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
		}


		override def literal(t :(A, B, C, D, E, F), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F))._6))
			).to(ArraySeq)
	}

	private[schema] case class Tuple6WriteForm[-A, -B, -C, -D, -E, -F](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F]
		) extends AbstractTuple6WriteForm[A, B, C, D, E, F]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple7WriteForm[-A, -B, -C, -D, -E, -F, -G] extends CompositeWriteForm[(A, B, C, D, E, F, G)] {
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		override lazy val forms = Seq(_1, _2, _3, _4, _5, _6, _7)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount

		override val columnCount: Int = offset_7 + _7.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes

		override def set(statement :PreparedStatement, position :Int, value :(A, B, C, D, E, F, G)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
		}


		override def literal(t :(A, B, C, D, E, F, G), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G))._7))
			).to(ArraySeq)
	}

	private[schema] case class Tuple7WriteForm[-A, -B, -C, -D, -E, -F, -G](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G]
		) extends AbstractTuple7WriteForm[A, B, C, D, E, F, G]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple8WriteForm[-A, -B, -C, -D, -E, -F, -G, -H]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		override lazy val forms = Seq(_1, _2, _3, _4, _5, _6, _7, _8)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount

		override val columnCount: Int = offset_8 + _8.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes

		override def set(statement :PreparedStatement, position :Int, value :(A, B, C, D, E, F, G, H)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
		}


		override def literal(t :(A, B, C, D, E, F, G, H), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H))._8))
			).to(ArraySeq)
	}

	private[schema] case class Tuple8WriteForm[-A, -B, -C, -D, -E, -F, -G, -H](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H]
		) extends AbstractTuple8WriteForm[A, B, C, D, E, F, G, H]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple9WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		override lazy val forms = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount

		override val columnCount: Int = offset_9 + _9.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes

		override def set(statement :PreparedStatement, position :Int, value :(A, B, C, D, E, F, G, H, I)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I))._9))
			).to(ArraySeq)
	}

	private[schema] case class Tuple9WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I]
		) extends AbstractTuple9WriteForm[A, B, C, D, E, F, G, H, I]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple10WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I, J)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		val _10 :SQLWriteForm[J]
		override lazy val forms = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount
		private[this] val offset_10 = offset_9 + _9.columnCount

		override val columnCount: Int = offset_10 + _10.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes ++ _9.columnTypes

		override def set(statement :PreparedStatement, position :Int, value :(A, B, C, D, E, F, G, H, I, J)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
				_10.set(statement, position + offset_10, value._10)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
			_10.setNull(statement, position + offset_10)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I, J), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral append ", "
				res append _10.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9) append ", "
				res append _10.inlineLiteral(t._10)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I, J)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9) ++
				literals(_10, t._10)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J))._9)) ++
				_10.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J))._10))
			).to(ArraySeq)
	}

	private[schema] case class Tuple10WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J]
		) extends AbstractTuple10WriteForm[A, B, C, D, E, F, G, H, I, J]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple11WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I, J, K)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		val _10 :SQLWriteForm[J]
		val _11 :SQLWriteForm[K]
		override lazy val forms = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount
		private[this] val offset_10 = offset_9 + _9.columnCount
		private[this] val offset_11 = offset_10 + _10.columnCount

		override val columnCount: Int = offset_11 + _11.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes ++ _9.columnTypes ++ _10.columnTypes

		override def set(statement :PreparedStatement, position :Int, value :(A, B, C, D, E, F, G, H, I, J, K)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
				_10.set(statement, position + offset_10, value._10)
				_11.set(statement, position + offset_11, value._11)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
			_10.setNull(statement, position + offset_10)
			_11.setNull(statement, position + offset_11)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I, J, K), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral append ", "
				res append _10.inlineNullLiteral append ", "
				res append _11.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9) append ", "
				res append _10.inlineLiteral(t._10) append ", "
				res append _11.inlineLiteral(t._11)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I, J, K)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9) ++
				literals(_10, t._10) ++ literals(_11, t._11)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K))._9)) ++
				_10.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K))._10)) ++
				_11.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K))._11))
			).to(ArraySeq)
	}

	private[schema] case class Tuple11WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K]
		) extends AbstractTuple11WriteForm[A, B, C, D, E, F, G, H, I, J, K]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple12WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		val _10 :SQLWriteForm[J]
		val _11 :SQLWriteForm[K]
		val _12 :SQLWriteForm[L]
		override lazy val forms = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount
		private[this] val offset_10 = offset_9 + _9.columnCount
		private[this] val offset_11 = offset_10 + _10.columnCount
		private[this] val offset_12 = offset_11 + _11.columnCount

		override val columnCount: Int = offset_12 + _12.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes ++ _9.columnTypes ++ _10.columnTypes ++
				_11.columnTypes

		override def set(statement :PreparedStatement, position :Int, value :(A, B, C, D, E, F, G, H, I, J, K, L)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
				_10.set(statement, position + offset_10, value._10)
				_11.set(statement, position + offset_11, value._11)
				_12.set(statement, position + offset_12, value._12)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
			_10.setNull(statement, position + offset_10)
			_11.setNull(statement, position + offset_11)
			_12.setNull(statement, position + offset_12)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I, J, K, L), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral append ", "
				res append _10.inlineNullLiteral append ", "
				res append _11.inlineNullLiteral append ", "
				res append _12.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9) append ", "
				res append _10.inlineLiteral(t._10) append ", "
				res append _11.inlineLiteral(t._11) append ", "
				res append _12.inlineLiteral(t._12)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I, J, K, L)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9) ++
				literals(_10, t._10) ++ literals(_11, t._11) ++ literals(_12, t._12)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L))._9)) ++
				_10.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L))._10)) ++
				_11.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L))._11)) ++
				_12.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L))._12))
			).to(ArraySeq)
	}

	private[schema] case class Tuple12WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L]
		) extends AbstractTuple12WriteForm[A, B, C, D, E, F, G, H, I, J, K, L]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple13WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		val _10 :SQLWriteForm[J]
		val _11 :SQLWriteForm[K]
		val _12 :SQLWriteForm[L]
		val _13 :SQLWriteForm[M]
		override lazy val forms = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount
		private[this] val offset_10 = offset_9 + _9.columnCount
		private[this] val offset_11 = offset_10 + _10.columnCount
		private[this] val offset_12 = offset_11 + _11.columnCount
		private[this] val offset_13 = offset_12 + _12.columnCount

		override val columnCount: Int = offset_13 + _13.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes ++ _9.columnTypes ++ _10.columnTypes ++
				_11.columnTypes ++ _12.columnTypes

		override def set(statement :PreparedStatement, position :Int, value :(A, B, C, D, E, F, G, H, I, J, K, L, M)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
				_10.set(statement, position + offset_10, value._10)
				_11.set(statement, position + offset_11, value._11)
				_12.set(statement, position + offset_12, value._12)
				_13.set(statement, position + offset_13, value._13)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
			_10.setNull(statement, position + offset_10)
			_11.setNull(statement, position + offset_11)
			_12.setNull(statement, position + offset_12)
			_13.setNull(statement, position + offset_13)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I, J, K, L, M), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral append ", "
				res append _10.inlineNullLiteral append ", "
				res append _11.inlineNullLiteral append ", "
				res append _12.inlineNullLiteral append ", "
				res append _13.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9) append ", "
				res append _10.inlineLiteral(t._10) append ", "
				res append _11.inlineLiteral(t._11) append ", "
				res append _12.inlineLiteral(t._12) append ", "
				res append _13.inlineLiteral(t._13)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I, J, K, L, M)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9) ++
				literals(_10, t._10) ++ literals(_11, t._11) ++ literals(_12, t._12) ++ literals(_13, t._13)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M))._9)) ++
				_10.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M))._10)) ++
				_11.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M))._11)) ++
				_12.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M))._12)) ++
				_13.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M))._13))
			).to(ArraySeq)
	}

	private[schema] case class Tuple13WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M]
		) extends AbstractTuple13WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple14WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		val _10 :SQLWriteForm[J]
		val _11 :SQLWriteForm[K]
		val _12 :SQLWriteForm[L]
		val _13 :SQLWriteForm[M]
		val _14 :SQLWriteForm[N]
		override lazy val forms = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount
		private[this] val offset_10 = offset_9 + _9.columnCount
		private[this] val offset_11 = offset_10 + _10.columnCount
		private[this] val offset_12 = offset_11 + _11.columnCount
		private[this] val offset_13 = offset_12 + _12.columnCount
		private[this] val offset_14 = offset_13 + _13.columnCount

		override val columnCount: Int = offset_14 + _14.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes ++ _9.columnTypes ++ _10.columnTypes ++
				_11.columnTypes ++ _12.columnTypes ++ _13.columnTypes

		override def set(statement :PreparedStatement, position :Int, value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
				_10.set(statement, position + offset_10, value._10)
				_11.set(statement, position + offset_11, value._11)
				_12.set(statement, position + offset_12, value._12)
				_13.set(statement, position + offset_13, value._13)
				_14.set(statement, position + offset_14, value._14)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
			_10.setNull(statement, position + offset_10)
			_11.setNull(statement, position + offset_11)
			_12.setNull(statement, position + offset_12)
			_13.setNull(statement, position + offset_13)
			_14.setNull(statement, position + offset_14)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral append ", "
				res append _10.inlineNullLiteral append ", "
				res append _11.inlineNullLiteral append ", "
				res append _12.inlineNullLiteral append ", "
				res append _13.inlineNullLiteral append ", "
				res append _14.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9) append ", "
				res append _10.inlineLiteral(t._10) append ", "
				res append _11.inlineLiteral(t._11) append ", "
				res append _12.inlineLiteral(t._12) append ", "
				res append _13.inlineLiteral(t._13) append ", "
				res append _14.inlineLiteral(t._14)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9) ++
				literals(_10, t._10) ++ literals(_11, t._11) ++ literals(_12, t._12) ++ literals(_13, t._13) ++
				literals(_14, t._14)

		override def split = 
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._9)) ++
				_10.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._10)) ++
				_11.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._11)) ++
				_12.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._12)) ++
				_13.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._13)) ++
				_14.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N))._14))
			).to(ArraySeq)
	}

	private[schema] case class Tuple14WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N]
		) extends AbstractTuple14WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple15WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		val _10 :SQLWriteForm[J]
		val _11 :SQLWriteForm[K]
		val _12 :SQLWriteForm[L]
		val _13 :SQLWriteForm[M]
		val _14 :SQLWriteForm[N]
		val _15 :SQLWriteForm[O]
		override lazy val forms = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount
		private[this] val offset_10 = offset_9 + _9.columnCount
		private[this] val offset_11 = offset_10 + _10.columnCount
		private[this] val offset_12 = offset_11 + _11.columnCount
		private[this] val offset_13 = offset_12 + _12.columnCount
		private[this] val offset_14 = offset_13 + _13.columnCount
		private[this] val offset_15 = offset_14 + _14.columnCount

		override val columnCount: Int = offset_15 + _15.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes ++ _9.columnTypes ++ _10.columnTypes ++
				_11.columnTypes ++ _12.columnTypes ++ _13.columnTypes ++ _14.columnTypes

		override def set(statement :PreparedStatement, position :Int,
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
				_10.set(statement, position + offset_10, value._10)
				_11.set(statement, position + offset_11, value._11)
				_12.set(statement, position + offset_12, value._12)
				_13.set(statement, position + offset_13, value._13)
				_14.set(statement, position + offset_14, value._14)
				_15.set(statement, position + offset_15, value._15)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
			_10.setNull(statement, position + offset_10)
			_11.setNull(statement, position + offset_11)
			_12.setNull(statement, position + offset_12)
			_13.setNull(statement, position + offset_13)
			_14.setNull(statement, position + offset_14)
			_15.setNull(statement, position + offset_15)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral append ", "
				res append _10.inlineNullLiteral append ", "
				res append _11.inlineNullLiteral append ", "
				res append _12.inlineNullLiteral append ", "
				res append _13.inlineNullLiteral append ", "
				res append _14.inlineNullLiteral append ", "
				res append _15.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9) append ", "
				res append _10.inlineLiteral(t._10) append ", "
				res append _11.inlineLiteral(t._11) append ", "
				res append _12.inlineLiteral(t._12) append ", "
				res append _13.inlineLiteral(t._13) append ", "
				res append _14.inlineLiteral(t._14) append ", "
				res append _15.inlineLiteral(t._15)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9) ++
				literals(_10, t._10) ++ literals(_11, t._11) ++ literals(_12, t._12) ++ literals(_13, t._13) ++
				literals(_14, t._14) ++ literals(_15, t._15)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._9)) ++
				_10.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._10)) ++
				_11.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._11)) ++
				_12.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._12)) ++
				_13.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._13)) ++
				_14.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._14)) ++
				_15.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))._15)) 
			).to(ArraySeq)
	}

	private[schema] case class Tuple15WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O]
		) extends AbstractTuple15WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple16WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		val _10 :SQLWriteForm[J]
		val _11 :SQLWriteForm[K]
		val _12 :SQLWriteForm[L]
		val _13 :SQLWriteForm[M]
		val _14 :SQLWriteForm[N]
		val _15 :SQLWriteForm[O]
		val _16 :SQLWriteForm[P]
		override lazy val forms = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount
		private[this] val offset_10 = offset_9 + _9.columnCount
		private[this] val offset_11 = offset_10 + _10.columnCount
		private[this] val offset_12 = offset_11 + _11.columnCount
		private[this] val offset_13 = offset_12 + _12.columnCount
		private[this] val offset_14 = offset_13 + _13.columnCount
		private[this] val offset_15 = offset_14 + _14.columnCount
		private[this] val offset_16 = offset_15 + _15.columnCount

		override val columnCount: Int = offset_16 + _16.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes ++ _9.columnTypes ++ _10.columnTypes ++
				_11.columnTypes ++ _12.columnTypes ++ _13.columnTypes ++ _14.columnTypes ++ _15.columnTypes

		override def set(statement :PreparedStatement, position :Int,
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
				_10.set(statement, position + offset_10, value._10)
				_11.set(statement, position + offset_11, value._11)
				_12.set(statement, position + offset_12, value._12)
				_13.set(statement, position + offset_13, value._13)
				_14.set(statement, position + offset_14, value._14)
				_15.set(statement, position + offset_15, value._15)
				_16.set(statement, position + offset_16, value._16)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
			_10.setNull(statement, position + offset_10)
			_11.setNull(statement, position + offset_11)
			_12.setNull(statement, position + offset_12)
			_13.setNull(statement, position + offset_13)
			_14.setNull(statement, position + offset_14)
			_15.setNull(statement, position + offset_15)
			_16.setNull(statement, position + offset_16)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral append ", "
				res append _10.inlineNullLiteral append ", "
				res append _11.inlineNullLiteral append ", "
				res append _12.inlineNullLiteral append ", "
				res append _13.inlineNullLiteral append ", "
				res append _14.inlineNullLiteral append ", "
				res append _15.inlineNullLiteral append ", "
				res append _16.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9) append ", "
				res append _10.inlineLiteral(t._10) append ", "
				res append _11.inlineLiteral(t._11) append ", "
				res append _12.inlineLiteral(t._12) append ", "
				res append _13.inlineLiteral(t._13) append ", "
				res append _14.inlineLiteral(t._14) append ", "
				res append _15.inlineLiteral(t._15) append ", "
				res append _16.inlineLiteral(t._16)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9) ++
				literals(_10, t._10) ++ literals(_11, t._11) ++ literals(_12, t._12) ++ literals(_13, t._13) ++
				literals(_14, t._14) ++ literals(_15, t._15) ++ literals(_16, t._16)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._9)) ++
				_10.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._10)) ++
				_11.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._11)) ++
				_12.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._12)) ++
				_13.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._13)) ++
				_14.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._14)) ++
				_15.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._15)) ++
				_16.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))._16)) 
			).to(ArraySeq)
	}

	private[schema] case class Tuple16WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O],
			_16 :SQLWriteForm[P]
		) extends AbstractTuple16WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple17WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		val _10 :SQLWriteForm[J]
		val _11 :SQLWriteForm[K]
		val _12 :SQLWriteForm[L]
		val _13 :SQLWriteForm[M]
		val _14 :SQLWriteForm[N]
		val _15 :SQLWriteForm[O]
		val _16 :SQLWriteForm[P]
		val _17 :SQLWriteForm[Q]
		override lazy val forms = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount
		private[this] val offset_10 = offset_9 + _9.columnCount
		private[this] val offset_11 = offset_10 + _10.columnCount
		private[this] val offset_12 = offset_11 + _11.columnCount
		private[this] val offset_13 = offset_12 + _12.columnCount
		private[this] val offset_14 = offset_13 + _13.columnCount
		private[this] val offset_15 = offset_14 + _14.columnCount
		private[this] val offset_16 = offset_15 + _15.columnCount
		private[this] val offset_17 = offset_16 + _16.columnCount

		override val columnCount: Int = offset_17 + _17.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes ++ _9.columnTypes ++ _10.columnTypes ++
				_11.columnTypes ++ _12.columnTypes ++ _13.columnTypes ++ _14.columnTypes ++ _15.columnTypes ++
				_16.columnTypes

		override def set(statement :PreparedStatement, position :Int,
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
				_10.set(statement, position + offset_10, value._10)
				_11.set(statement, position + offset_11, value._11)
				_12.set(statement, position + offset_12, value._12)
				_13.set(statement, position + offset_13, value._13)
				_14.set(statement, position + offset_14, value._14)
				_15.set(statement, position + offset_15, value._15)
				_16.set(statement, position + offset_16, value._16)
				_17.set(statement, position + offset_17, value._17)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
			_10.setNull(statement, position + offset_10)
			_11.setNull(statement, position + offset_11)
			_12.setNull(statement, position + offset_12)
			_13.setNull(statement, position + offset_13)
			_14.setNull(statement, position + offset_14)
			_15.setNull(statement, position + offset_15)
			_16.setNull(statement, position + offset_16)
			_17.setNull(statement, position + offset_17)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral append ", "
				res append _10.inlineNullLiteral append ", "
				res append _11.inlineNullLiteral append ", "
				res append _12.inlineNullLiteral append ", "
				res append _13.inlineNullLiteral append ", "
				res append _14.inlineNullLiteral append ", "
				res append _15.inlineNullLiteral append ", "
				res append _16.inlineNullLiteral append ", "
				res append _17.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9) append ", "
				res append _10.inlineLiteral(t._10) append ", "
				res append _11.inlineLiteral(t._11) append ", "
				res append _12.inlineLiteral(t._12) append ", "
				res append _13.inlineLiteral(t._13) append ", "
				res append _14.inlineLiteral(t._14) append ", "
				res append _15.inlineLiteral(t._15) append ", "
				res append _16.inlineLiteral(t._16) append ", "
				res append _17.inlineLiteral(t._17)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9) ++
				literals(_10, t._10) ++ literals(_11, t._11) ++ literals(_12, t._12) ++ literals(_13, t._13) ++
				literals(_14, t._14) ++ literals(_15, t._15) ++ literals(_16, t._16) ++ literals(_17, t._17)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._9)) ++
				_10.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._10)) ++
				_11.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._11)) ++
				_12.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._12)) ++
				_13.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._13)) ++
				_14.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._14)) ++
				_15.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._15)) ++
				_16.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._16)) ++
				_17.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))._17))
			).to(ArraySeq)
	}

	private[schema] case class Tuple17WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O],
			_16 :SQLWriteForm[P], _17 :SQLWriteForm[Q]
		) extends AbstractTuple17WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple18WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		val _10 :SQLWriteForm[J]
		val _11 :SQLWriteForm[K]
		val _12 :SQLWriteForm[L]
		val _13 :SQLWriteForm[M]
		val _14 :SQLWriteForm[N]
		val _15 :SQLWriteForm[O]
		val _16 :SQLWriteForm[P]
		val _17 :SQLWriteForm[Q]
		val _18 :SQLWriteForm[R]
		override lazy val forms = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount
		private[this] val offset_10 = offset_9 + _9.columnCount
		private[this] val offset_11 = offset_10 + _10.columnCount
		private[this] val offset_12 = offset_11 + _11.columnCount
		private[this] val offset_13 = offset_12 + _12.columnCount
		private[this] val offset_14 = offset_13 + _13.columnCount
		private[this] val offset_15 = offset_14 + _14.columnCount
		private[this] val offset_16 = offset_15 + _15.columnCount
		private[this] val offset_17 = offset_16 + _16.columnCount
		private[this] val offset_18 = offset_17 + _17.columnCount

		override val columnCount: Int = offset_18 + _18.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes ++ _9.columnTypes ++ _10.columnTypes ++
				_11.columnTypes ++ _12.columnTypes ++ _13.columnTypes ++ _14.columnTypes ++ _15.columnTypes ++
				_16.columnTypes ++ _17.columnTypes ++ _18.columnTypes

		override def set(statement :PreparedStatement, position :Int,
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
				_10.set(statement, position + offset_10, value._10)
				_11.set(statement, position + offset_11, value._11)
				_12.set(statement, position + offset_12, value._12)
				_13.set(statement, position + offset_13, value._13)
				_14.set(statement, position + offset_14, value._14)
				_15.set(statement, position + offset_15, value._15)
				_16.set(statement, position + offset_16, value._16)
				_17.set(statement, position + offset_17, value._17)
				_18.set(statement, position + offset_18, value._18)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
			_10.setNull(statement, position + offset_10)
			_11.setNull(statement, position + offset_11)
			_12.setNull(statement, position + offset_12)
			_13.setNull(statement, position + offset_13)
			_14.setNull(statement, position + offset_14)
			_15.setNull(statement, position + offset_15)
			_16.setNull(statement, position + offset_16)
			_17.setNull(statement, position + offset_17)
			_18.setNull(statement, position + offset_18)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral append ", "
				res append _10.inlineNullLiteral append ", "
				res append _11.inlineNullLiteral append ", "
				res append _12.inlineNullLiteral append ", "
				res append _13.inlineNullLiteral append ", "
				res append _14.inlineNullLiteral append ", "
				res append _15.inlineNullLiteral append ", "
				res append _16.inlineNullLiteral append ", "
				res append _17.inlineNullLiteral append ", "
				res append _18.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9) append ", "
				res append _10.inlineLiteral(t._10) append ", "
				res append _11.inlineLiteral(t._11) append ", "
				res append _12.inlineLiteral(t._12) append ", "
				res append _13.inlineLiteral(t._13) append ", "
				res append _14.inlineLiteral(t._14) append ", "
				res append _15.inlineLiteral(t._15) append ", "
				res append _16.inlineLiteral(t._16) append ", "
				res append _17.inlineLiteral(t._17) append ", "
				res append _18.inlineLiteral(t._18)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9) ++
				literals(_10, t._10) ++ literals(_11, t._11) ++ literals(_12, t._12) ++ literals(_13, t._13) ++
				literals(_14, t._14) ++ literals(_15, t._15) ++ literals(_16, t._16) ++ literals(_17, t._17) ++
				literals(_18, t._18)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._9)) ++
				_10.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._10)) ++
				_11.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._11)) ++
				_12.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._12)) ++
				_13.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._13)) ++
				_14.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._14)) ++
				_15.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._15)) ++
				_16.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._16)) ++
				_17.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._17)) ++
				_18.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))._18))
			).to(ArraySeq)
	}

	private[schema] case class Tuple18WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O],
			_16 :SQLWriteForm[P], _17 :SQLWriteForm[Q], _18 :SQLWriteForm[R]
		) extends AbstractTuple18WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple19WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		val _10 :SQLWriteForm[J]
		val _11 :SQLWriteForm[K]
		val _12 :SQLWriteForm[L]
		val _13 :SQLWriteForm[M]
		val _14 :SQLWriteForm[N]
		val _15 :SQLWriteForm[O]
		val _16 :SQLWriteForm[P]
		val _17 :SQLWriteForm[Q]
		val _18 :SQLWriteForm[R]
		val _19 :SQLWriteForm[S]
		override lazy val forms =
			Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount
		private[this] val offset_10 = offset_9 + _9.columnCount
		private[this] val offset_11 = offset_10 + _10.columnCount
		private[this] val offset_12 = offset_11 + _11.columnCount
		private[this] val offset_13 = offset_12 + _12.columnCount
		private[this] val offset_14 = offset_13 + _13.columnCount
		private[this] val offset_15 = offset_14 + _14.columnCount
		private[this] val offset_16 = offset_15 + _15.columnCount
		private[this] val offset_17 = offset_16 + _16.columnCount
		private[this] val offset_18 = offset_17 + _17.columnCount
		private[this] val offset_19 = offset_18 + _18.columnCount

		override val columnCount: Int = offset_19 + _19.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes ++ _9.columnTypes ++ _10.columnTypes ++
				_11.columnTypes ++ _12.columnTypes ++ _13.columnTypes ++ _14.columnTypes ++ _15.columnTypes ++
				_16.columnTypes ++ _17.columnTypes ++ _18.columnTypes ++ _19.columnTypes

		override def set(statement :PreparedStatement, position :Int,
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
				_10.set(statement, position + offset_10, value._10)
				_11.set(statement, position + offset_11, value._11)
				_12.set(statement, position + offset_12, value._12)
				_13.set(statement, position + offset_13, value._13)
				_14.set(statement, position + offset_14, value._14)
				_15.set(statement, position + offset_15, value._15)
				_16.set(statement, position + offset_16, value._16)
				_17.set(statement, position + offset_17, value._17)
				_18.set(statement, position + offset_18, value._18)
				_19.set(statement, position + offset_19, value._19)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
			_10.setNull(statement, position + offset_10)
			_11.setNull(statement, position + offset_11)
			_12.setNull(statement, position + offset_12)
			_13.setNull(statement, position + offset_13)
			_14.setNull(statement, position + offset_14)
			_15.setNull(statement, position + offset_15)
			_16.setNull(statement, position + offset_16)
			_17.setNull(statement, position + offset_17)
			_18.setNull(statement, position + offset_18)
			_19.setNull(statement, position + offset_19)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral append ", "
				res append _10.inlineNullLiteral append ", "
				res append _11.inlineNullLiteral append ", "
				res append _12.inlineNullLiteral append ", "
				res append _13.inlineNullLiteral append ", "
				res append _14.inlineNullLiteral append ", "
				res append _15.inlineNullLiteral append ", "
				res append _16.inlineNullLiteral append ", "
				res append _17.inlineNullLiteral append ", "
				res append _18.inlineNullLiteral append ", "
				res append _19.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9) append ", "
				res append _10.inlineLiteral(t._10) append ", "
				res append _11.inlineLiteral(t._11) append ", "
				res append _12.inlineLiteral(t._12) append ", "
				res append _13.inlineLiteral(t._13) append ", "
				res append _14.inlineLiteral(t._14) append ", "
				res append _15.inlineLiteral(t._15) append ", "
				res append _16.inlineLiteral(t._16) append ", "
				res append _17.inlineLiteral(t._17) append ", "
				res append _18.inlineLiteral(t._18) append ", "
				res append _19.inlineLiteral(t._19)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9) ++
				literals(_10, t._10) ++ literals(_11, t._11) ++ literals(_12, t._12) ++ literals(_13, t._13) ++
				literals(_14, t._14) ++ literals(_15, t._15) ++ literals(_16, t._16) ++ literals(_17, t._17) ++
				literals(_18, t._18) ++ literals(_19, t._19)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._9)) ++
				_10.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._10)) ++
				_11.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._11)) ++
				_12.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._12)) ++
				_13.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._13)) ++
				_14.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._14)) ++
				_15.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._15)) ++
				_16.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._16)) ++
				_17.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._17)) ++
				_18.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._18)) ++
				_19.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))._19)) 
			).to(ArraySeq)
	}

	private[schema] case class Tuple19WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O],
			_16 :SQLWriteForm[P], _17 :SQLWriteForm[Q], _18 :SQLWriteForm[R], _19 :SQLWriteForm[S]
		) extends AbstractTuple19WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple20WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S, -T]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		val _10 :SQLWriteForm[J]
		val _11 :SQLWriteForm[K]
		val _12 :SQLWriteForm[L]
		val _13 :SQLWriteForm[M]
		val _14 :SQLWriteForm[N]
		val _15 :SQLWriteForm[O]
		val _16 :SQLWriteForm[P]
		val _17 :SQLWriteForm[Q]
		val _18 :SQLWriteForm[R]
		val _19 :SQLWriteForm[S]
		val _20 :SQLWriteForm[T]
		override lazy val forms =
			Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount
		private[this] val offset_10 = offset_9 + _9.columnCount
		private[this] val offset_11 = offset_10 + _10.columnCount
		private[this] val offset_12 = offset_11 + _11.columnCount
		private[this] val offset_13 = offset_12 + _12.columnCount
		private[this] val offset_14 = offset_13 + _13.columnCount
		private[this] val offset_15 = offset_14 + _14.columnCount
		private[this] val offset_16 = offset_15 + _15.columnCount
		private[this] val offset_17 = offset_16 + _16.columnCount
		private[this] val offset_18 = offset_17 + _17.columnCount
		private[this] val offset_19 = offset_18 + _18.columnCount
		private[this] val offset_20 = offset_19 + _19.columnCount

		override val columnCount: Int = offset_20 + _20.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes ++ _9.columnTypes ++ _10.columnTypes ++
				_11.columnTypes ++ _12.columnTypes ++ _13.columnTypes ++ _14.columnTypes ++ _15.columnTypes ++
				_16.columnTypes ++ _17.columnTypes ++ _18.columnTypes ++ _19.columnTypes ++ _20.columnTypes

		override def set(statement :PreparedStatement, position :Int,
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
				_10.set(statement, position + offset_10, value._10)
				_11.set(statement, position + offset_11, value._11)
				_12.set(statement, position + offset_12, value._12)
				_13.set(statement, position + offset_13, value._13)
				_14.set(statement, position + offset_14, value._14)
				_15.set(statement, position + offset_15, value._15)
				_16.set(statement, position + offset_16, value._16)
				_17.set(statement, position + offset_17, value._17)
				_18.set(statement, position + offset_18, value._18)
				_19.set(statement, position + offset_19, value._19)
				_20.set(statement, position + offset_20, value._20)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
			_10.setNull(statement, position + offset_10)
			_11.setNull(statement, position + offset_11)
			_12.setNull(statement, position + offset_12)
			_13.setNull(statement, position + offset_13)
			_14.setNull(statement, position + offset_14)
			_15.setNull(statement, position + offset_15)
			_16.setNull(statement, position + offset_16)
			_17.setNull(statement, position + offset_17)
			_18.setNull(statement, position + offset_18)
			_19.setNull(statement, position + offset_19)
			_20.setNull(statement, position + offset_20)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral append ", "
				res append _10.inlineNullLiteral append ", "
				res append _11.inlineNullLiteral append ", "
				res append _12.inlineNullLiteral append ", "
				res append _13.inlineNullLiteral append ", "
				res append _14.inlineNullLiteral append ", "
				res append _15.inlineNullLiteral append ", "
				res append _16.inlineNullLiteral append ", "
				res append _17.inlineNullLiteral append ", "
				res append _18.inlineNullLiteral append ", "
				res append _19.inlineNullLiteral append ", "
				res append _20.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9) append ", "
				res append _10.inlineLiteral(t._10) append ", "
				res append _11.inlineLiteral(t._11) append ", "
				res append _12.inlineLiteral(t._12) append ", "
				res append _13.inlineLiteral(t._13) append ", "
				res append _14.inlineLiteral(t._14) append ", "
				res append _15.inlineLiteral(t._15) append ", "
				res append _16.inlineLiteral(t._16) append ", "
				res append _17.inlineLiteral(t._17) append ", "
				res append _18.inlineLiteral(t._18) append ", "
				res append _19.inlineLiteral(t._19) append ", "
				res append _20.inlineLiteral(t._20)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9) ++
				literals(_10, t._10) ++ literals(_11, t._11) ++ literals(_12, t._12) ++ literals(_13, t._13) ++
				literals(_14, t._14) ++ literals(_15, t._15) ++ literals(_16, t._16) ++ literals(_17, t._17) ++
				literals(_18, t._18) ++ literals(_19, t._19) ++ literals(_20, t._20)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._9)) ++
				_10.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._10)) ++
				_11.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._11)) ++
				_12.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._12)) ++
				_13.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._13)) ++
				_14.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._14)) ++
				_15.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._15)) ++
				_16.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._16)) ++
				_17.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._17)) ++
				_18.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._18)) ++
				_19.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._19)) ++
				_20.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))._20))
			).to(ArraySeq)
	}

	private[schema] case class Tuple20WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S, -T](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O],
			_16 :SQLWriteForm[P], _17 :SQLWriteForm[Q], _18 :SQLWriteForm[R], _19 :SQLWriteForm[S], _20 :SQLWriteForm[T]
		) extends AbstractTuple20WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple21WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S, -T, -U]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		val _10 :SQLWriteForm[J]
		val _11 :SQLWriteForm[K]
		val _12 :SQLWriteForm[L]
		val _13 :SQLWriteForm[M]
		val _14 :SQLWriteForm[N]
		val _15 :SQLWriteForm[O]
		val _16 :SQLWriteForm[P]
		val _17 :SQLWriteForm[Q]
		val _18 :SQLWriteForm[R]
		val _19 :SQLWriteForm[S]
		val _20 :SQLWriteForm[T]
		val _21 :SQLWriteForm[U]
		override lazy val forms =
			Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount
		private[this] val offset_10 = offset_9 + _9.columnCount
		private[this] val offset_11 = offset_10 + _10.columnCount
		private[this] val offset_12 = offset_11 + _11.columnCount
		private[this] val offset_13 = offset_12 + _12.columnCount
		private[this] val offset_14 = offset_13 + _13.columnCount
		private[this] val offset_15 = offset_14 + _14.columnCount
		private[this] val offset_16 = offset_15 + _15.columnCount
		private[this] val offset_17 = offset_16 + _16.columnCount
		private[this] val offset_18 = offset_17 + _17.columnCount
		private[this] val offset_19 = offset_18 + _18.columnCount
		private[this] val offset_20 = offset_19 + _19.columnCount
		private[this] val offset_21 = offset_20 + _20.columnCount

		override val columnCount: Int = offset_21 + _21.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes ++ _9.columnTypes ++ _10.columnTypes ++
				_11.columnTypes ++ _12.columnTypes ++ _13.columnTypes ++ _14.columnTypes ++ _15.columnTypes ++
				_16.columnTypes ++ _17.columnTypes ++ _18.columnTypes ++ _19.columnTypes ++ _20.columnTypes ++
				_21.columnTypes

		override def set(statement :PreparedStatement, position :Int,
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
				_10.set(statement, position + offset_10, value._10)
				_11.set(statement, position + offset_11, value._11)
				_12.set(statement, position + offset_12, value._12)
				_13.set(statement, position + offset_13, value._13)
				_14.set(statement, position + offset_14, value._14)
				_15.set(statement, position + offset_15, value._15)
				_16.set(statement, position + offset_16, value._16)
				_17.set(statement, position + offset_17, value._17)
				_18.set(statement, position + offset_18, value._18)
				_19.set(statement, position + offset_19, value._19)
				_20.set(statement, position + offset_20, value._20)
				_21.set(statement, position + offset_21, value._21)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
			_10.setNull(statement, position + offset_10)
			_11.setNull(statement, position + offset_11)
			_12.setNull(statement, position + offset_12)
			_13.setNull(statement, position + offset_13)
			_14.setNull(statement, position + offset_14)
			_15.setNull(statement, position + offset_15)
			_16.setNull(statement, position + offset_16)
			_17.setNull(statement, position + offset_17)
			_18.setNull(statement, position + offset_18)
			_19.setNull(statement, position + offset_19)
			_20.setNull(statement, position + offset_20)
			_21.setNull(statement, position + offset_21)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral append ", "
				res append _10.inlineNullLiteral append ", "
				res append _11.inlineNullLiteral append ", "
				res append _12.inlineNullLiteral append ", "
				res append _13.inlineNullLiteral append ", "
				res append _14.inlineNullLiteral append ", "
				res append _15.inlineNullLiteral append ", "
				res append _16.inlineNullLiteral append ", "
				res append _17.inlineNullLiteral append ", "
				res append _18.inlineNullLiteral append ", "
				res append _19.inlineNullLiteral append ", "
				res append _20.inlineNullLiteral append ", "
				res append _21.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9) append ", "
				res append _10.inlineLiteral(t._10) append ", "
				res append _11.inlineLiteral(t._11) append ", "
				res append _12.inlineLiteral(t._12) append ", "
				res append _13.inlineLiteral(t._13) append ", "
				res append _14.inlineLiteral(t._14) append ", "
				res append _15.inlineLiteral(t._15) append ", "
				res append _16.inlineLiteral(t._16) append ", "
				res append _17.inlineLiteral(t._17) append ", "
				res append _18.inlineLiteral(t._18) append ", "
				res append _19.inlineLiteral(t._19) append ", "
				res append _20.inlineLiteral(t._20) append ", "
				res append _21.inlineLiteral(t._21)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9) ++
				literals(_10, t._10) ++ literals(_11, t._11) ++ literals(_12, t._12) ++ literals(_13, t._13) ++
				literals(_14, t._14) ++ literals(_15, t._15) ++ literals(_16, t._16) ++ literals(_17, t._17) ++
				literals(_18, t._18) ++ literals(_19, t._19) ++ literals(_20, t._20) ++ literals(_21, t._21)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._9)) ++
				_10.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._10)) ++
				_11.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._11)) ++
				_12.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._12)) ++
				_13.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._13)) ++
				_14.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._14)) ++
				_15.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._15)) ++
				_16.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._16)) ++
				_17.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._17)) ++
				_18.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._18)) ++
				_19.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._19)) ++
				_20.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._20)) ++
				_21.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))._21)) 
			).to(ArraySeq)
	}

	private[schema] case class Tuple21WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S, -T, -U](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O],
			_16 :SQLWriteForm[P], _17 :SQLWriteForm[Q], _18 :SQLWriteForm[R], _19 :SQLWriteForm[S], _20 :SQLWriteForm[T],
			_21 :SQLWriteForm[U]
		) extends AbstractTuple21WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple22WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S, -T, -U, -V]
		extends CompositeWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]
		val _9 :SQLWriteForm[I]
		val _10 :SQLWriteForm[J]
		val _11 :SQLWriteForm[K]
		val _12 :SQLWriteForm[L]
		val _13 :SQLWriteForm[M]
		val _14 :SQLWriteForm[N]
		val _15 :SQLWriteForm[O]
		val _16 :SQLWriteForm[P]
		val _17 :SQLWriteForm[Q]
		val _18 :SQLWriteForm[R]
		val _19 :SQLWriteForm[S]
		val _20 :SQLWriteForm[T]
		val _21 :SQLWriteForm[U]
		val _22 :SQLWriteForm[V]
		override lazy val forms =
			Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22)

		private[this] val offset_2 = _1.columnCount
		private[this] val offset_3 = offset_2 + _2.columnCount
		private[this] val offset_4 = offset_3 + _3.columnCount
		private[this] val offset_5 = offset_4 + _4.columnCount
		private[this] val offset_6 = offset_5 + _5.columnCount
		private[this] val offset_7 = offset_6 + _6.columnCount
		private[this] val offset_8 = offset_7 + _7.columnCount
		private[this] val offset_9 = offset_8 + _8.columnCount
		private[this] val offset_10 = offset_9 + _9.columnCount
		private[this] val offset_11 = offset_10 + _10.columnCount
		private[this] val offset_12 = offset_11 + _11.columnCount
		private[this] val offset_13 = offset_12 + _12.columnCount
		private[this] val offset_14 = offset_13 + _13.columnCount
		private[this] val offset_15 = offset_14 + _14.columnCount
		private[this] val offset_16 = offset_15 + _15.columnCount
		private[this] val offset_17 = offset_16 + _16.columnCount
		private[this] val offset_18 = offset_17 + _17.columnCount
		private[this] val offset_19 = offset_18 + _18.columnCount
		private[this] val offset_20 = offset_19 + _19.columnCount
		private[this] val offset_21 = offset_20 + _20.columnCount
		private[this] val offset_22 = offset_21 + _21.columnCount

		override val columnCount: Int = offset_22 + _22.columnCount
		override lazy val columnTypes :Seq[JDBCType] =
			_1.columnTypes ++ _2.columnTypes ++ _3.columnTypes ++ _4.columnTypes ++ _5.columnTypes ++
				_6.columnTypes ++ _7.columnTypes ++ _8.columnTypes ++ _9.columnTypes ++ _10.columnTypes ++
				_11.columnTypes ++ _12.columnTypes ++ _13.columnTypes ++ _14.columnTypes ++ _15.columnTypes ++
				_16.columnTypes ++ _17.columnTypes ++ _18.columnTypes ++ _19.columnTypes ++ _20.columnTypes ++
				_21.columnTypes ++ _22.columnTypes

		override def set(statement :PreparedStatement, position :Int, 
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)) :Unit =
			if (value == null) {
				setNull(statement, position)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + offset_2, value._2)
				_3.set(statement, position + offset_3, value._3)
				_4.set(statement, position + offset_4, value._4)
				_5.set(statement, position + offset_5, value._5)
				_6.set(statement, position + offset_6, value._6)
				_7.set(statement, position + offset_7, value._7)
				_8.set(statement, position + offset_8, value._8)
				_9.set(statement, position + offset_9, value._9)
				_10.set(statement, position + offset_10, value._10)
				_11.set(statement, position + offset_11, value._11)
				_12.set(statement, position + offset_12, value._12)
				_13.set(statement, position + offset_13, value._13)
				_14.set(statement, position + offset_14, value._14)
				_15.set(statement, position + offset_15, value._15)
				_16.set(statement, position + offset_16, value._16)
				_17.set(statement, position + offset_17, value._17)
				_18.set(statement, position + offset_18, value._18)
				_19.set(statement, position + offset_19, value._19)
				_20.set(statement, position + offset_20, value._20)
				_21.set(statement, position + offset_21, value._21)
				_22.set(statement, position + offset_22, value._22)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + offset_2)
			_3.setNull(statement, position + offset_3)
			_4.setNull(statement, position + offset_4)
			_5.setNull(statement, position + offset_5)
			_6.setNull(statement, position + offset_6)
			_7.setNull(statement, position + offset_7)
			_8.setNull(statement, position + offset_8)
			_9.setNull(statement, position + offset_9)
			_10.setNull(statement, position + offset_10)
			_11.setNull(statement, position + offset_11)
			_12.setNull(statement, position + offset_12)
			_13.setNull(statement, position + offset_13)
			_14.setNull(statement, position + offset_14)
			_15.setNull(statement, position + offset_15)
			_16.setNull(statement, position + offset_16)
			_17.setNull(statement, position + offset_17)
			_18.setNull(statement, position + offset_18)
			_19.setNull(statement, position + offset_19)
			_20.setNull(statement, position + offset_20)
			_21.setNull(statement, position + offset_21)
			_22.setNull(statement, position + offset_22)
		}


		override def literal(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), inline :Boolean) :String = {
			val res = new StringBuilder
			if (!inline) res append "("

			if (t == null) {
				res append _1.inlineNullLiteral append ", "
				res append _2.inlineNullLiteral append ", "
				res append _3.inlineNullLiteral append ", "
				res append _4.inlineNullLiteral append ", "
				res append _5.inlineNullLiteral append ", "
				res append _6.inlineNullLiteral append ", "
				res append _7.inlineNullLiteral append ", "
				res append _8.inlineNullLiteral append ", "
				res append _9.inlineNullLiteral append ", "
				res append _10.inlineNullLiteral append ", "
				res append _11.inlineNullLiteral append ", "
				res append _12.inlineNullLiteral append ", "
				res append _13.inlineNullLiteral append ", "
				res append _14.inlineNullLiteral append ", "
				res append _15.inlineNullLiteral append ", "
				res append _16.inlineNullLiteral append ", "
				res append _17.inlineNullLiteral append ", "
				res append _18.inlineNullLiteral append ", "
				res append _19.inlineNullLiteral append ", "
				res append _20.inlineNullLiteral append ", "
				res append _21.inlineNullLiteral append ", "
				res append _22.inlineNullLiteral
			} else {
				res append _1.inlineLiteral(t._1) append ", "
				res append _2.inlineLiteral(t._2) append ", "
				res append _3.inlineLiteral(t._3) append ", "
				res append _4.inlineLiteral(t._4) append ", "
				res append _5.inlineLiteral(t._5) append ", "
				res append _6.inlineLiteral(t._6) append ", "
				res append _7.inlineLiteral(t._7) append ", "
				res append _8.inlineLiteral(t._8) append ", "
				res append _9.inlineLiteral(t._9) append ", "
				res append _10.inlineLiteral(t._10) append ", "
				res append _11.inlineLiteral(t._11) append ", "
				res append _12.inlineLiteral(t._12) append ", "
				res append _13.inlineLiteral(t._13) append ", "
				res append _14.inlineLiteral(t._14) append ", "
				res append _15.inlineLiteral(t._15) append ", "
				res append _16.inlineLiteral(t._16) append ", "
				res append _17.inlineLiteral(t._17) append ", "
				res append _18.inlineLiteral(t._18) append ", "
				res append _19.inlineLiteral(t._19) append ", "
				res append _20.inlineLiteral(t._20) append ", "
				res append _21.inlineLiteral(t._21) append ", "
				res append _22.inlineLiteral(t._22)
			}

			if (!inline) res append ")"
			res.toString
		}

		override def columnLiterals(t :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)) :Seq[String] =
			literals(_1, t._1) ++ literals(_2, t._2) ++ literals(_3, t._3) ++ literals(_4, t._4) ++ literals(_5, t._5) ++
				literals(_6, t._6) ++ literals(_7, t._7) ++ literals(_8, t._8) ++ literals(_9, t._9) ++
				literals(_10, t._10) ++ literals(_11, t._11) ++ literals(_12, t._12) ++ literals(_13, t._13) ++
				literals(_14, t._14) ++ literals(_15, t._15) ++ literals(_16, t._16) ++ literals(_17, t._17) ++
				literals(_18, t._18) ++ literals(_19, t._19) ++ literals(_20, t._20) ++ literals(_21, t._21) ++
				literals(_22, t._22)

		override def split =
			(_1.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._1)) ++
				_2.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._2)) ++
				_3.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._3)) ++
				_4.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._4)) ++
				_5.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._5)) ++
				_6.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._6)) ++
				_7.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._7)) ++
				_8.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._8)) ++
				_9.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._9)) ++
				_10.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._10)) ++
				_11.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._11)) ++
				_12.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._12)) ++
				_13.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._13)) ++
				_14.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._14)) ++
				_15.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._15)) ++
				_16.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._16)) ++
				_17.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._17)) ++
				_18.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._18)) ++
				_19.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._19)) ++
				_20.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._20)) ++
				_21.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._21)) ++
				_22.split.view.map(_.compose((_ :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))._22))
			).to(ArraySeq)
	}

	private[schema] case class Tuple22WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S, -T, -U, -V](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O],
			_16 :SQLWriteForm[P], _17 :SQLWriteForm[Q], _18 :SQLWriteForm[R], _19 :SQLWriteForm[S], _20 :SQLWriteForm[T],
			_21 :SQLWriteForm[U], _22 :SQLWriteForm[V]
		) extends AbstractTuple22WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
	{
		override def productPrefix :String = "<"
	}

	
	
}
