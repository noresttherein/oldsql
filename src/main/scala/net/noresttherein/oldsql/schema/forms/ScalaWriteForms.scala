package net.noresttherein.oldsql.schema.forms

import java.sql.PreparedStatement

import net.noresttherein.oldsql.schema.SQLWriteForm
import net.noresttherein.oldsql.schema.SQLWriteForm.NullableWriteFormLiterals






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
	







	
	private[schema] trait AbstractTuple2WriteForm[-L, -R] extends SQLWriteForm[(L, R)] {
		override def writtenColumns: Int = _1.writtenColumns + _2.writtenColumns

		val _1 :SQLWriteForm[L]
		val _2 :SQLWriteForm[R]

		override def set(statement :PreparedStatement, position :Int, value :(L, R)) :Unit =
			if (value == null) {
				_1.setNull(statement, position)
				_2.setNull(statement, position + _1.writtenColumns)
			} else {
				_1.set(statement, position, value._1)
				_2.set(statement, position + _1.writtenColumns, value._2)
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			_1.setNull(statement, position)
			_2.setNull(statement, position + _1.writtenColumns)
		}


		override def literal(value: (L, R)): String =
			if (value == null) s"(${_1.inlineNullLiteral}, ${_2.inlineNullLiteral})"
			else s"(${_1.inlineLiteral(value._1)}, ${_2.inlineLiteral(value._2)})"

		override def nullLiteral: String = s"(${_1.inlineNullLiteral}, ${_2.inlineNullLiteral})"

		override def inlineLiteral(value: (L, R)): String = _1.inlineLiteral(value._1) + ", " + _2.inlineLiteral(value._2)

		override def inlineNullLiteral: String = _1.inlineNullLiteral + ", " + _2.inlineNullLiteral

	}

	private[schema] case class Tuple2WriteForm[-L, -R](_1 :SQLWriteForm[L], _2 :SQLWriteForm[R])
		extends AbstractTuple2WriteForm[L, R]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple3WriteForm[-A, -B, -C] extends NullableWriteFormLiterals[(A, B, C)] {
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns

		override val writtenColumns: Int = offset_3 + _3.writtenColumns

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

	}

	private[schema] case class Tuple3WriteForm[-A, -B, -C](_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C])
		extends AbstractTuple3WriteForm[A, B, C]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple4WriteForm[-A, -B, -C, -D] extends NullableWriteFormLiterals[(A, B, C, D)] {
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns

		override val writtenColumns: Int = offset_4 + _4.writtenColumns

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

	}

	private[schema] case class Tuple4WriteForm[-A, -B, -C, -D](_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D])
		extends AbstractTuple4WriteForm[A, B, C, D]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple5WriteForm[-A, -B, -C, -D, -E] extends NullableWriteFormLiterals[(A, B, C, D, E)] {
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns

		override val writtenColumns: Int = offset_5 + _5.writtenColumns

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

	}

	private[schema] case class Tuple5WriteForm[-A, -B, -C, -D, -E](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E]
		) extends AbstractTuple5WriteForm[A, B, C, D, E]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple6WriteForm[-A, -B, -C, -D, -E, -F] extends NullableWriteFormLiterals[(A, B, C, D, E, F)] {
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns

		override val writtenColumns: Int = offset_6 + _6.writtenColumns

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

	}

	private[schema] case class Tuple6WriteForm[-A, -B, -C, -D, -E, -F](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F]
		) extends AbstractTuple6WriteForm[A, B, C, D, E, F]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple7WriteForm[-A, -B, -C, -D, -E, -F, -G] extends NullableWriteFormLiterals[(A, B, C, D, E, F, G)] {
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns

		override val writtenColumns: Int = offset_7 + _7.writtenColumns

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

	}

	private[schema] case class Tuple7WriteForm[-A, -B, -C, -D, -E, -F, -G](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G]
		) extends AbstractTuple7WriteForm[A, B, C, D, E, F, G]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple8WriteForm[-A, -B, -C, -D, -E, -F, -G, -H]
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H)]
	{
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]
		val _5 :SQLWriteForm[E]
		val _6 :SQLWriteForm[F]
		val _7 :SQLWriteForm[G]
		val _8 :SQLWriteForm[H]

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns

		override val writtenColumns: Int = offset_8 + _8.writtenColumns

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

	}

	private[schema] case class Tuple8WriteForm[-A, -B, -C, -D, -E, -F, -G, -H](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H]
		) extends AbstractTuple8WriteForm[A, B, C, D, E, F, G, H]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple9WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I]
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I)]
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

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns

		override val writtenColumns: Int = offset_9 + _9.writtenColumns

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

	}

	private[schema] case class Tuple9WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I]
		) extends AbstractTuple9WriteForm[A, B, C, D, E, F, G, H, I]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple10WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J]
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I, J)]
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

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns
		private[this] val offset_10 = offset_9 + _9.writtenColumns

		override val writtenColumns: Int = offset_10 + _10.writtenColumns

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

	}

	private[schema] case class Tuple10WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J]
		) extends AbstractTuple10WriteForm[A, B, C, D, E, F, G, H, I, J]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple11WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K]
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I, J, K)]
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

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns
		private[this] val offset_10 = offset_9 + _9.writtenColumns
		private[this] val offset_11 = offset_10 + _10.writtenColumns

		override val writtenColumns: Int = offset_11 + _11.writtenColumns

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
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I, J, K, L)]
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

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns
		private[this] val offset_10 = offset_9 + _9.writtenColumns
		private[this] val offset_11 = offset_10 + _10.writtenColumns
		private[this] val offset_12 = offset_11 + _11.writtenColumns

		override val writtenColumns: Int = offset_12 + _12.writtenColumns

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
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I, J, K, L, M)]
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

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns
		private[this] val offset_10 = offset_9 + _9.writtenColumns
		private[this] val offset_11 = offset_10 + _10.writtenColumns
		private[this] val offset_12 = offset_11 + _11.writtenColumns
		private[this] val offset_13 = offset_12 + _12.writtenColumns

		override val writtenColumns: Int = offset_13 + _13.writtenColumns

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
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)]
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

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns
		private[this] val offset_10 = offset_9 + _9.writtenColumns
		private[this] val offset_11 = offset_10 + _10.writtenColumns
		private[this] val offset_12 = offset_11 + _11.writtenColumns
		private[this] val offset_13 = offset_12 + _12.writtenColumns
		private[this] val offset_14 = offset_13 + _13.writtenColumns

		override val writtenColumns: Int = offset_14 + _14.writtenColumns

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
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)]
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

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns
		private[this] val offset_10 = offset_9 + _9.writtenColumns
		private[this] val offset_11 = offset_10 + _10.writtenColumns
		private[this] val offset_12 = offset_11 + _11.writtenColumns
		private[this] val offset_13 = offset_12 + _12.writtenColumns
		private[this] val offset_14 = offset_13 + _13.writtenColumns
		private[this] val offset_15 = offset_14 + _14.writtenColumns

		override val writtenColumns: Int = offset_15 + _15.writtenColumns

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
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)]
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

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns
		private[this] val offset_10 = offset_9 + _9.writtenColumns
		private[this] val offset_11 = offset_10 + _10.writtenColumns
		private[this] val offset_12 = offset_11 + _11.writtenColumns
		private[this] val offset_13 = offset_12 + _12.writtenColumns
		private[this] val offset_14 = offset_13 + _13.writtenColumns
		private[this] val offset_15 = offset_14 + _14.writtenColumns
		private[this] val offset_16 = offset_15 + _15.writtenColumns

		override val writtenColumns: Int = offset_16 + _16.writtenColumns

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
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)]
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

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns
		private[this] val offset_10 = offset_9 + _9.writtenColumns
		private[this] val offset_11 = offset_10 + _10.writtenColumns
		private[this] val offset_12 = offset_11 + _11.writtenColumns
		private[this] val offset_13 = offset_12 + _12.writtenColumns
		private[this] val offset_14 = offset_13 + _13.writtenColumns
		private[this] val offset_15 = offset_14 + _14.writtenColumns
		private[this] val offset_16 = offset_15 + _15.writtenColumns
		private[this] val offset_17 = offset_16 + _16.writtenColumns

		override val writtenColumns: Int = offset_17 + _17.writtenColumns

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
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)]
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

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns
		private[this] val offset_10 = offset_9 + _9.writtenColumns
		private[this] val offset_11 = offset_10 + _10.writtenColumns
		private[this] val offset_12 = offset_11 + _11.writtenColumns
		private[this] val offset_13 = offset_12 + _12.writtenColumns
		private[this] val offset_14 = offset_13 + _13.writtenColumns
		private[this] val offset_15 = offset_14 + _14.writtenColumns
		private[this] val offset_16 = offset_15 + _15.writtenColumns
		private[this] val offset_17 = offset_16 + _16.writtenColumns
		private[this] val offset_18 = offset_17 + _17.writtenColumns

		override val writtenColumns: Int = offset_18 + _18.writtenColumns

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
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)]
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

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns
		private[this] val offset_10 = offset_9 + _9.writtenColumns
		private[this] val offset_11 = offset_10 + _10.writtenColumns
		private[this] val offset_12 = offset_11 + _11.writtenColumns
		private[this] val offset_13 = offset_12 + _12.writtenColumns
		private[this] val offset_14 = offset_13 + _13.writtenColumns
		private[this] val offset_15 = offset_14 + _14.writtenColumns
		private[this] val offset_16 = offset_15 + _15.writtenColumns
		private[this] val offset_17 = offset_16 + _16.writtenColumns
		private[this] val offset_18 = offset_17 + _17.writtenColumns
		private[this] val offset_19 = offset_18 + _18.writtenColumns

		override val writtenColumns: Int = offset_19 + _19.writtenColumns

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
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)]
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

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns
		private[this] val offset_10 = offset_9 + _9.writtenColumns
		private[this] val offset_11 = offset_10 + _10.writtenColumns
		private[this] val offset_12 = offset_11 + _11.writtenColumns
		private[this] val offset_13 = offset_12 + _12.writtenColumns
		private[this] val offset_14 = offset_13 + _13.writtenColumns
		private[this] val offset_15 = offset_14 + _14.writtenColumns
		private[this] val offset_16 = offset_15 + _15.writtenColumns
		private[this] val offset_17 = offset_16 + _16.writtenColumns
		private[this] val offset_18 = offset_17 + _17.writtenColumns
		private[this] val offset_19 = offset_18 + _18.writtenColumns
		private[this] val offset_20 = offset_19 + _19.writtenColumns

		override val writtenColumns: Int = offset_20 + _20.writtenColumns

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
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)]
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

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns
		private[this] val offset_10 = offset_9 + _9.writtenColumns
		private[this] val offset_11 = offset_10 + _10.writtenColumns
		private[this] val offset_12 = offset_11 + _11.writtenColumns
		private[this] val offset_13 = offset_12 + _12.writtenColumns
		private[this] val offset_14 = offset_13 + _13.writtenColumns
		private[this] val offset_15 = offset_14 + _14.writtenColumns
		private[this] val offset_16 = offset_15 + _15.writtenColumns
		private[this] val offset_17 = offset_16 + _16.writtenColumns
		private[this] val offset_18 = offset_17 + _17.writtenColumns
		private[this] val offset_19 = offset_18 + _18.writtenColumns
		private[this] val offset_20 = offset_19 + _19.writtenColumns
		private[this] val offset_21 = offset_20 + _20.writtenColumns

		override val writtenColumns: Int = offset_21 + _21.writtenColumns

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
		extends NullableWriteFormLiterals[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)]
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
		
		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		private[this] val offset_5 = offset_4 + _4.writtenColumns
		private[this] val offset_6 = offset_5 + _5.writtenColumns
		private[this] val offset_7 = offset_6 + _6.writtenColumns
		private[this] val offset_8 = offset_7 + _7.writtenColumns
		private[this] val offset_9 = offset_8 + _8.writtenColumns
		private[this] val offset_10 = offset_9 + _9.writtenColumns
		private[this] val offset_11 = offset_10 + _10.writtenColumns
		private[this] val offset_12 = offset_11 + _11.writtenColumns
		private[this] val offset_13 = offset_12 + _12.writtenColumns
		private[this] val offset_14 = offset_13 + _13.writtenColumns
		private[this] val offset_15 = offset_14 + _14.writtenColumns
		private[this] val offset_16 = offset_15 + _15.writtenColumns
		private[this] val offset_17 = offset_16 + _16.writtenColumns
		private[this] val offset_18 = offset_17 + _17.writtenColumns
		private[this] val offset_19 = offset_18 + _18.writtenColumns
		private[this] val offset_20 = offset_19 + _19.writtenColumns
		private[this] val offset_21 = offset_20 + _20.writtenColumns
		private[this] val offset_22 = offset_21 + _21.writtenColumns

		override val writtenColumns: Int = offset_22 + _22.writtenColumns

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
