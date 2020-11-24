package net.noresttherein.oldsql.schema.forms

import java.sql.ResultSet

import net.noresttherein.oldsql.schema.SQLReadForm






trait ScalaReadForms {

	@inline private[this] def f[T](implicit form :SQLReadForm[T]) = form


	//implicit type class definitions
	
	implicit def tuple2ReadForm[T1 :SQLReadForm, T2 :SQLReadForm] :SQLReadForm[(T1, T2)] =
		new Tuple2ReadForm(f[T1], f[T2])

	implicit def tuple3ReadForm[T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm] :SQLReadForm[(T1, T2, T3)] =
		new Tuple3ReadForm(f[T1], f[T2], f[T3])

	implicit def tuple4ReadForm[T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm]
			:SQLReadForm[(T1, T2, T3, T4)] =
		new Tuple4ReadForm(f[T1], f[T2], f[T3], f[T4])

	implicit def tuple5ReadForm[T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm]
			:SQLReadForm[(T1, T2, T3, T4, T5)] =
		new Tuple5ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5])

	implicit def tuple6ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6)] =
		new Tuple6ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6])

	implicit def tuple7ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7)] =
		new Tuple7ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7])

	implicit def tuple8ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8)] =
		new Tuple8ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8])

	implicit def tuple9ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
		new Tuple9ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9])

	implicit def tuple10ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm, T10 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
		new Tuple10ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9], f[T10])

	implicit def tuple11ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm, T10 :SQLReadForm, T11 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] =
		new Tuple11ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9], f[T10], f[T11])

	implicit def tuple12ReadForm[
		T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
		T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm, T10 :SQLReadForm, T11 :SQLReadForm, T12 :SQLReadForm
	] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] =
		new Tuple12ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9], f[T10], f[T11], f[T12])

	implicit def tuple13ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm, T10 :SQLReadForm, T11 :SQLReadForm, T12 :SQLReadForm,
				T13 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] =
		new Tuple13ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9], f[T10], f[T11], f[T12], f[T13])

	implicit def tuple14ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm, T10 :SQLReadForm, T11 :SQLReadForm, T12 :SQLReadForm,
				T13 :SQLReadForm, T14 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] =
		new Tuple14ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9], f[T10], f[T11], f[T12],
		                    f[T13], f[T14]
		)

	implicit def tuple15ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm, T10 :SQLReadForm, T11 :SQLReadForm, T12 :SQLReadForm,
				T13 :SQLReadForm, T14 :SQLReadForm, T15 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] =
		new Tuple15ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9], f[T10], f[T11], f[T12],
		                    f[T13], f[T14], f[T15]
		)

	implicit def tuple16ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm, T10 :SQLReadForm, T11 :SQLReadForm, T12 :SQLReadForm,
				T13 :SQLReadForm, T14 :SQLReadForm, T15 :SQLReadForm, T16 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
		new Tuple16ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9], f[T10], f[T11], f[T12],
		                    f[T13], f[T14], f[T15], f[T16]
		)

	implicit def tuple17ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm, T10 :SQLReadForm, T11 :SQLReadForm, T12 :SQLReadForm,
				T13 :SQLReadForm, T14 :SQLReadForm, T15 :SQLReadForm, T16 :SQLReadForm, T17 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
		new Tuple17ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9], f[T10], f[T11], f[T12],
		                    f[T13], f[T14], f[T15], f[T16], f[T17]
		)

	implicit def tuple18ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm, T10 :SQLReadForm, T11 :SQLReadForm, T12 :SQLReadForm,
				T13 :SQLReadForm, T14 :SQLReadForm, T15 :SQLReadForm, T16 :SQLReadForm, T17 :SQLReadForm, T18 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
		new Tuple18ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9], f[T10], f[T11], f[T12],
		                    f[T13], f[T14], f[T15], f[T16], f[T17], f[T18]
		)

	implicit def tuple19ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm, T10 :SQLReadForm, T11 :SQLReadForm, T12 :SQLReadForm,
				T13 :SQLReadForm, T14 :SQLReadForm, T15 :SQLReadForm, T16 :SQLReadForm, T17 :SQLReadForm, T18 :SQLReadForm,
				T19 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
		new Tuple19ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9], f[T10], f[T11], f[T12],
		                    f[T13], f[T14], f[T15], f[T16], f[T17], f[T18], f[T19]
		)

	implicit def tuple20ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm, T10 :SQLReadForm, T11 :SQLReadForm, T12 :SQLReadForm,
				T13 :SQLReadForm, T14 :SQLReadForm, T15 :SQLReadForm, T16 :SQLReadForm, T17 :SQLReadForm, T18 :SQLReadForm,
				T19 :SQLReadForm, T20 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] =
		new Tuple20ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9], f[T10], f[T11], f[T12],
		                    f[T13], f[T14], f[T15], f[T16], f[T17], f[T18], f[T19], f[T20]
		)

	implicit def tuple21ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm, T10 :SQLReadForm, T11 :SQLReadForm, T12 :SQLReadForm,
				T13 :SQLReadForm, T14 :SQLReadForm, T15 :SQLReadForm, T16 :SQLReadForm, T17 :SQLReadForm, T18 :SQLReadForm,
				T19 :SQLReadForm, T20 :SQLReadForm, T21 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] =
		new Tuple21ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9], f[T10], f[T11], f[T12],
		                    f[T13], f[T14], f[T15], f[T16], f[T17], f[T18], f[T19], f[T20], f[T21]
		)

	implicit def tuple22ReadForm[
				T1 :SQLReadForm, T2 :SQLReadForm, T3 :SQLReadForm, T4 :SQLReadForm, T5 :SQLReadForm, T6 :SQLReadForm,
				T7 :SQLReadForm, T8 :SQLReadForm, T9 :SQLReadForm, T10 :SQLReadForm, T11 :SQLReadForm, T12 :SQLReadForm,
				T13 :SQLReadForm, T14 :SQLReadForm, T15 :SQLReadForm, T16 :SQLReadForm, T17 :SQLReadForm, T18 :SQLReadForm,
				T19 :SQLReadForm, T20 :SQLReadForm, T21 :SQLReadForm, T22 :SQLReadForm
			] :SQLReadForm[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] =
		new Tuple22ReadForm(f[T1], f[T2], f[T3], f[T4], f[T5], f[T6], f[T7], f[T8], f[T9], f[T10], f[T11], f[T12],
		                    f[T13], f[T14], f[T15], f[T16], f[T17], f[T18], f[T19], f[T20], f[T21], f[T22]
		)




	//implicit conversions from tuples of forms
	
	implicit def toTuple2ReadForm[A, B](t :(SQLReadForm[A], SQLReadForm[B])) = 
		new Tuple2ReadForm(t._1, t._2)
	
	implicit def toTuple3ReadForm[A, B, C](t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C])) =
		new Tuple3ReadForm(t._1, t._2, t._3)

	implicit def toTuple4ReadForm[A, B, C, D](t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D])) =
		new Tuple4ReadForm(t._1, t._2, t._3, t._4)
	
	implicit def toTuple5ReadForm[A, B, C, D, E]
	                         (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E])) =
		new Tuple5ReadForm(t._1, t._2, t._3, t._4, t._5)
	
	implicit def toTuple6ReadForm[A, B, C, D, E, F]
	                         (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                              SQLReadForm[F])) =
		new Tuple6ReadForm(t._1, t._2, t._3, t._4, t._5, t._6)
	
	implicit def toTuple7ReadForm[A, B, C, D, E, F, G]
	                         (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                              SQLReadForm[F], SQLReadForm[G])) =
		new Tuple7ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7)
	
	implicit def toTuple8ReadForm[A, B, C, D, E, F, G, H]
	                         (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                              SQLReadForm[F], SQLReadForm[G], SQLReadForm[H])) =
		new Tuple8ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)
	
	implicit def toTuple9ReadForm[A, B, C, D, E, F, G, H, I]
	                         (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                              SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I])) =
		new Tuple9ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
		
	implicit def toTuple10ReadForm[A, B, C, D, E, F, G, H, I, J]
	             (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E], 
	                  SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I], SQLReadForm[J])) =
		new Tuple10ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)
		
	implicit def toTuple11ReadForm[A, B, C, D, E, F, G, H, I, J, K]
	             (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                  SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I], SQLReadForm[J], 
	                  SQLReadForm[K])) =
		new Tuple11ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)
		
	implicit def toTuple12ReadForm[A, B, C, D, E, F, G, H, I, J, K, L]
	             (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                  SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I], SQLReadForm[J],
	                  SQLReadForm[K], SQLReadForm[L])) =
		new Tuple12ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)
		
	implicit def toTuple13ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M]
	             (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                  SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I], SQLReadForm[J],
	                  SQLReadForm[K], SQLReadForm[L], SQLReadForm[M])) =
		new Tuple13ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)
		
	implicit def toTuple14ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
	             (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                  SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I], SQLReadForm[J],
	                  SQLReadForm[K], SQLReadForm[L], SQLReadForm[M], SQLReadForm[N])) =
		new Tuple14ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)
		
	implicit def toTuple15ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
	             (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                  SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I], SQLReadForm[J],
	                  SQLReadForm[K], SQLReadForm[L], SQLReadForm[M], SQLReadForm[N], SQLReadForm[O])) =
		new Tuple15ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                        t._11, t._12, t._13, t._14, t._15)
		
	implicit def toTuple16ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
	             (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
		                           SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I], SQLReadForm[J],
		                           SQLReadForm[K], SQLReadForm[L], SQLReadForm[M], SQLReadForm[N], SQLReadForm[O],
		                           SQLReadForm[P])) =
		new Tuple16ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                        t._11, t._12, t._13, t._14, t._15, t._16)
		
	implicit def toTuple17ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
	             (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                  SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I], SQLReadForm[J],
	                  SQLReadForm[K], SQLReadForm[L], SQLReadForm[M], SQLReadForm[N], SQLReadForm[O],
	                  SQLReadForm[P], SQLReadForm[Q])) =
		new Tuple17ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                        t._11, t._12, t._13, t._14, t._15, t._16, t._17)
		
	implicit def toTuple18ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
	             (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                  SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I], SQLReadForm[J],
	                  SQLReadForm[K], SQLReadForm[L], SQLReadForm[M], SQLReadForm[N], SQLReadForm[O],
	                  SQLReadForm[P], SQLReadForm[Q], SQLReadForm[R])) =
		new Tuple18ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                        t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
		
	implicit def toTuple19ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
	             (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                  SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I], SQLReadForm[J],
	                  SQLReadForm[K], SQLReadForm[L], SQLReadForm[M], SQLReadForm[N], SQLReadForm[O],
	                  SQLReadForm[P], SQLReadForm[Q], SQLReadForm[R], SQLReadForm[S])) =
		new Tuple19ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                        t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
		
	implicit def toTuple20ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
	             (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                  SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I], SQLReadForm[J],
	                  SQLReadForm[K], SQLReadForm[L], SQLReadForm[M], SQLReadForm[N], SQLReadForm[O],
	                  SQLReadForm[P], SQLReadForm[Q], SQLReadForm[R], SQLReadForm[S], SQLReadForm[T])) =
		new Tuple20ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                        t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
		
	implicit def toTuple21ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
	             (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                  SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I], SQLReadForm[J],
	                  SQLReadForm[K], SQLReadForm[L], SQLReadForm[M], SQLReadForm[N], SQLReadForm[O],
	                  SQLReadForm[P], SQLReadForm[Q], SQLReadForm[R], SQLReadForm[S], SQLReadForm[T],
	                  SQLReadForm[U])) =
		new Tuple21ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10,
	                        t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
		
	implicit def toTuple22ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
	             (t :(SQLReadForm[A], SQLReadForm[B], SQLReadForm[C], SQLReadForm[D], SQLReadForm[E],
	                  SQLReadForm[F], SQLReadForm[G], SQLReadForm[H], SQLReadForm[I], SQLReadForm[J],
	                  SQLReadForm[K], SQLReadForm[L], SQLReadForm[M], SQLReadForm[N], SQLReadForm[O],
	                  SQLReadForm[P], SQLReadForm[Q], SQLReadForm[R], SQLReadForm[S], SQLReadForm[T],
	                  SQLReadForm[U], SQLReadForm[V])) =
		new Tuple22ReadForm(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, 
	                        t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)






	private[schema] trait AbstractTuple2ReadForm[L, R] extends SQLReadForm[(L, R)] {
		val _1  :SQLReadForm[L]
		val _2  :SQLReadForm[R]

		override def opt(res :ResultSet, position :Int): Option[(L, R)] = {
			val l = _1.opt(res, position)
			val r = _2.opt(res, position + _1.readColumns)
			for (v1 <- l; v2 <- r) yield (v1, v2)
		}

		override def nullValue: (L, R) = (_1.nullValue, _2.nullValue)

		override def readColumns: Int = _1.readColumns + _2.readColumns

	}

	private[schema] case class Tuple2ReadForm[L, R](_1  :SQLReadForm[L], _2 :SQLReadForm[R])
		extends AbstractTuple2ReadForm[L, R]
	{
		override def productPrefix :String = "<"
	}

	
	
	private[schema] trait AbstractTuple3ReadForm[A, B, C] extends SQLReadForm[(A, B, C)] {
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]

		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = _1.readColumns + _2.readColumns
		
		override def opt(res :ResultSet, position :Int) :Option[(A, B, C)] = for {
				a <- _1.opt(res, position)
				b <- _2.opt(res, position + offset_2)
				c <- _3.opt(res, position + offset_3)
			} yield (a, b, c)

		override def nullValue :(A, B, C) = (_1.nullValue, _2.nullValue, _3.nullValue)

		override def readColumns :Int = offset_3 + _3.readColumns
	}
	
	private[schema] case class Tuple3ReadForm[A, B, C](_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C])
		extends AbstractTuple3ReadForm[A, B, C]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple4ReadForm[A, B, C, D] extends SQLReadForm[(A, B, C, D)] {
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]

		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
		} yield (a, b, c, d)

		override def nullValue :(A, B, C, D) = (_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue)

		override def readColumns :Int = offset_4 + _4.readColumns
	}

	private[schema] case class Tuple4ReadForm[A, B, C, D](_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D])
		extends AbstractTuple4ReadForm[A, B, C, D]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple5ReadForm[A, B, C, D, E] extends SQLReadForm[(A, B, C, D, E)] {
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		
		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
		} yield (a, b, c, d, e)

		override def nullValue :(A, B, C, D, E) = (_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue)

		override def readColumns :Int = offset_5 + _5.readColumns
	}

	private[schema] case class Tuple5ReadForm[A, B, C, D, E]
			(_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E])
		extends AbstractTuple5ReadForm[A, B, C, D, E]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple6ReadForm[A, B, C, D, E, F] extends SQLReadForm[(A, B, C, D, E, F)] {
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]

		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
		} yield (a, b, c, d, e, f)

		override def nullValue :(A, B, C, D, E, F) = 
			(_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue)

		override def readColumns :Int = offset_6 + _6.readColumns
	}

	private[schema] case class Tuple6ReadForm[A, B, C, D, E, F](
			_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
			_6 :SQLReadForm[F]
		) extends AbstractTuple6ReadForm[A, B, C, D, E, F]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple7ReadForm[A, B, C, D, E, F, G] extends SQLReadForm[(A, B, C, D, E, F, G)] {
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]

		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
		} yield (a, b, c, d, e, f, g)

		override def nullValue :(A, B, C, D, E, F, G) =
			(_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue)

		override def readColumns :Int = offset_7 + _7.readColumns
	}

	private[schema] case class Tuple7ReadForm[A, B, C, D, E, F, G](
			_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
			_6 :SQLReadForm[F], _7 :SQLReadForm[G]
		) extends AbstractTuple7ReadForm[A, B, C, D, E, F, G]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple8ReadForm[A, B, C, D, E, F, G, H] extends SQLReadForm[(A, B, C, D, E, F, G, H)] {
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]

		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
		} yield (a, b, c, d, e, f, g, h)

		override def nullValue :(A, B, C, D, E, F, G, H) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue
		)

		override def readColumns :Int = offset_8 + _8.readColumns
	}

	private[schema] case class Tuple8ReadForm[A, B, C, D, E, F, G, H](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H]
		) extends AbstractTuple8ReadForm[A, B, C, D, E, F, G, H]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple9ReadForm[A, B, C, D, E, F, G, H, I] extends SQLReadForm[(A, B, C, D, E, F, G, H, I)] {
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]

		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
		} yield (a, b, c, d, e, f, g, h, i)

		override def nullValue :(A, B, C, D, E, F, G, H, I) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue
		)

		override def readColumns :Int = offset_9 + _9.readColumns
	}

	private[schema] case class Tuple9ReadForm[A, B, C, D, E, F, G, H, I](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I]
		) extends AbstractTuple9ReadForm[A, B, C, D, E, F, G, H, I]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple10ReadForm[A, B, C, D, E, F, G, H, I, J] extends SQLReadForm[(A, B, C, D, E, F, G, H, I, J)] {
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]
		val _10 :SQLReadForm[J]

		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns
		private[this] val offset_10 = offset_9 + _9.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I, J)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
			j <- _10.opt(res, position + offset_10)
		} yield (a, b, c, d, e, f, g, h, i, j)

		override def nullValue :(A, B, C, D, E, F, G, H, I, J) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue, _10.nullValue
		)

		override def readColumns :Int = offset_10 + _10.readColumns
	}

	private[schema] case class Tuple10ReadForm[A, B, C, D, E, F, G, H, I, J](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I], _10 :SQLReadForm[J]
		) extends AbstractTuple10ReadForm[A, B, C, D, E, F, G, H, I, J]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple11ReadForm[A, B, C, D, E, F, G, H, I, J, K] 
		extends SQLReadForm[(A, B, C, D, E, F, G, H, I, J, K)] 
	{
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]
		val _10 :SQLReadForm[J]
		val _11 :SQLReadForm[K]
		
		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns
		private[this] val offset_10 = offset_9 + _9.readColumns
		private[this] val offset_11 = offset_10 + _10.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I, J, K)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
			j <- _10.opt(res, position + offset_10)
			k <- _11.opt(res, position + offset_11)
		} yield (a, b, c, d, e, f, g, h, i, j, k)

		override def nullValue :(A, B, C, D, E, F, G, H, I, J, K) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue, _10.nullValue, _11.nullValue
		)

		override def readColumns :Int = offset_11 + _11.readColumns
	}

	private[schema] case class Tuple11ReadForm[A, B, C, D, E, F, G, H, I, J, K](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I], _10 :SQLReadForm[J],
				_11 :SQLReadForm[K]
		) extends AbstractTuple11ReadForm[A, B, C, D, E, F, G, H, I, J, K]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple12ReadForm[A, B, C, D, E, F, G, H, I, J, K, L] 
		extends SQLReadForm[(A, B, C, D, E, F, G, H, I, J, K, L)] 
	{
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]
		val _10 :SQLReadForm[J]
		val _11 :SQLReadForm[K]
		val _12 :SQLReadForm[L]
		
		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns
		private[this] val offset_10 = offset_9 + _9.readColumns
		private[this] val offset_11 = offset_10 + _10.readColumns
		private[this] val offset_12 = offset_11 + _11.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I, J, K, L)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
			j <- _10.opt(res, position + offset_10)
			k <- _11.opt(res, position + offset_11)
			l <- _12.opt(res, position + offset_12)
		} yield (a, b, c, d, e, f, g, h, i, j, k, l)

		override def nullValue :(A, B, C, D, E, F, G, H, I, J, K, L) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue, _10.nullValue, _11.nullValue, _12.nullValue
		)

		override def readColumns :Int = offset_12 + _12.readColumns
	}

	private[schema] case class Tuple12ReadForm[A, B, C, D, E, F, G, H, I, J, K, L](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I], _10 :SQLReadForm[J],
				_11 :SQLReadForm[K], _12 :SQLReadForm[L]
		) extends AbstractTuple12ReadForm[A, B, C, D, E, F, G, H, I, J, K, L]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple13ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M] 
		extends SQLReadForm[(A, B, C, D, E, F, G, H, I, J, K, L, M)] 
	{
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]
		val _10 :SQLReadForm[J]
		val _11 :SQLReadForm[K]
		val _12 :SQLReadForm[L]
		val _13 :SQLReadForm[M]
		
		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns
		private[this] val offset_10 = offset_9 + _9.readColumns
		private[this] val offset_11 = offset_10 + _10.readColumns
		private[this] val offset_12 = offset_11 + _11.readColumns
		private[this] val offset_13 = offset_12 + _12.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
			j <- _10.opt(res, position + offset_10)
			k <- _11.opt(res, position + offset_11)
			l <- _12.opt(res, position + offset_12)
			m <- _13.opt(res, position + offset_13)
		} yield (a, b, c, d, e, f, g, h, i, j, k, l, m)

		override def nullValue :(A, B, C, D, E, F, G, H, I, J, K, L, M) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue, _10.nullValue, _11.nullValue, _12.nullValue, _13.nullValue
		)

		override def readColumns :Int = offset_13 + _13.readColumns
	}

	private[schema] case class Tuple13ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I], _10 :SQLReadForm[J],
				_11 :SQLReadForm[K], _12 :SQLReadForm[L], _13 :SQLReadForm[M]
		) extends AbstractTuple13ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M]
	{
		override def productPrefix :String = "<"
	}

	
	
	
	private[schema] trait AbstractTuple14ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N] 
		extends SQLReadForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] 
	{
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]
		val _10 :SQLReadForm[J]
		val _11 :SQLReadForm[K]
		val _12 :SQLReadForm[L]
		val _13 :SQLReadForm[M]
		val _14 :SQLReadForm[N]
		
		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns
		private[this] val offset_10 = offset_9 + _9.readColumns
		private[this] val offset_11 = offset_10 + _10.readColumns
		private[this] val offset_12 = offset_11 + _11.readColumns
		private[this] val offset_13 = offset_12 + _12.readColumns
		private[this] val offset_14 = offset_13 + _13.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
			j <- _10.opt(res, position + offset_10)
			k <- _11.opt(res, position + offset_11)
			l <- _12.opt(res, position + offset_12)
			m <- _13.opt(res, position + offset_13)
			n <- _14.opt(res, position + offset_14)
		} yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

		override def nullValue :(A, B, C, D, E, F, G, H, I, J, K, L, M, N) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue, _10.nullValue, _11.nullValue, _12.nullValue, _13.nullValue, _14.nullValue
		)

		override def readColumns :Int = offset_14 + _14.readColumns
	}

	private[schema] case class Tuple14ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I], _10 :SQLReadForm[J],
				_11 :SQLReadForm[K], _12 :SQLReadForm[L], _13 :SQLReadForm[M], _14 :SQLReadForm[N]
		) extends AbstractTuple14ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple15ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] 
		extends SQLReadForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] 
	{
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]
		val _10 :SQLReadForm[J]
		val _11 :SQLReadForm[K]
		val _12 :SQLReadForm[L]
		val _13 :SQLReadForm[M]
		val _14 :SQLReadForm[N]
		val _15 :SQLReadForm[O]
		
		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns
		private[this] val offset_10 = offset_9 + _9.readColumns
		private[this] val offset_11 = offset_10 + _10.readColumns
		private[this] val offset_12 = offset_11 + _11.readColumns
		private[this] val offset_13 = offset_12 + _12.readColumns
		private[this] val offset_14 = offset_13 + _13.readColumns
		private[this] val offset_15 = offset_14 + _14.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
			j <- _10.opt(res, position + offset_10)
			k <- _11.opt(res, position + offset_11)
			l <- _12.opt(res, position + offset_12)
			m <- _13.opt(res, position + offset_13)
			n <- _14.opt(res, position + offset_14)
			o <- _15.opt(res, position + offset_15)
		} yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

		override def nullValue :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue, _10.nullValue, _11.nullValue, _12.nullValue, _13.nullValue, _14.nullValue,
			_15.nullValue
		)

		override def readColumns :Int = offset_15 + _15.readColumns
	}

	private[schema] case class Tuple15ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I], _10 :SQLReadForm[J],
				_11 :SQLReadForm[K], _12 :SQLReadForm[L], _13 :SQLReadForm[M], _14 :SQLReadForm[N], _15 :SQLReadForm[O]
		) extends AbstractTuple15ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple16ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] 
		extends SQLReadForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] 
	{
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]
		val _10 :SQLReadForm[J]
		val _11 :SQLReadForm[K]
		val _12 :SQLReadForm[L]
		val _13 :SQLReadForm[M]
		val _14 :SQLReadForm[N]
		val _15 :SQLReadForm[O]
		val _16 :SQLReadForm[P]
		
		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns
		private[this] val offset_10 = offset_9 + _9.readColumns
		private[this] val offset_11 = offset_10 + _10.readColumns
		private[this] val offset_12 = offset_11 + _11.readColumns
		private[this] val offset_13 = offset_12 + _12.readColumns
		private[this] val offset_14 = offset_13 + _13.readColumns
		private[this] val offset_15 = offset_14 + _14.readColumns
		private[this] val offset_16 = offset_15 + _15.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
			j <- _10.opt(res, position + offset_10)
			k <- _11.opt(res, position + offset_11)
			l <- _12.opt(res, position + offset_12)
			m <- _13.opt(res, position + offset_13)
			n <- _14.opt(res, position + offset_14)
			o <- _15.opt(res, position + offset_15)
			p <- _16.opt(res, position + offset_16)
		} yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

		override def nullValue :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue, _10.nullValue, _11.nullValue, _12.nullValue, _13.nullValue, _14.nullValue,
			_15.nullValue, _16.nullValue
		)

		override def readColumns :Int = offset_16 + _16.readColumns
	}

	private[schema] case class Tuple16ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I], _10 :SQLReadForm[J],
				_11 :SQLReadForm[K], _12 :SQLReadForm[L], _13 :SQLReadForm[M], _14 :SQLReadForm[N], _15 :SQLReadForm[O],
				_16 :SQLReadForm[P]
		) extends AbstractTuple16ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple17ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] 
		extends SQLReadForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] 
	{
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]
		val _10 :SQLReadForm[J]
		val _11 :SQLReadForm[K]
		val _12 :SQLReadForm[L]
		val _13 :SQLReadForm[M]
		val _14 :SQLReadForm[N]
		val _15 :SQLReadForm[O]
		val _16 :SQLReadForm[P]
		val _17 :SQLReadForm[Q]
		
		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns
		private[this] val offset_10 = offset_9 + _9.readColumns
		private[this] val offset_11 = offset_10 + _10.readColumns
		private[this] val offset_12 = offset_11 + _11.readColumns
		private[this] val offset_13 = offset_12 + _12.readColumns
		private[this] val offset_14 = offset_13 + _13.readColumns
		private[this] val offset_15 = offset_14 + _14.readColumns
		private[this] val offset_16 = offset_15 + _15.readColumns
		private[this] val offset_17 = offset_16 + _16.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
			j <- _10.opt(res, position + offset_10)
			k <- _11.opt(res, position + offset_11)
			l <- _12.opt(res, position + offset_12)
			m <- _13.opt(res, position + offset_13)
			n <- _14.opt(res, position + offset_14)
			o <- _15.opt(res, position + offset_15)
			p <- _16.opt(res, position + offset_16)
			q <- _17.opt(res, position + offset_17)
		} yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)

		override def nullValue :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue, _10.nullValue, _11.nullValue, _12.nullValue, _13.nullValue, _14.nullValue,
			_15.nullValue, _16.nullValue, _17.nullValue
		)

		override def readColumns :Int = offset_17 + _17.readColumns
	}

	private[schema] case class Tuple17ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I], _10 :SQLReadForm[J],
				_11 :SQLReadForm[K], _12 :SQLReadForm[L], _13 :SQLReadForm[M], _14 :SQLReadForm[N], _15 :SQLReadForm[O],
				_16 :SQLReadForm[P], _17 :SQLReadForm[Q]	                                                                       
		) extends AbstractTuple17ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
	{
		override def productPrefix :String = "<"
	}



	private[schema] trait AbstractTuple18ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] 
		extends SQLReadForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] 
	{
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]
		val _10 :SQLReadForm[J]
		val _11 :SQLReadForm[K]
		val _12 :SQLReadForm[L]
		val _13 :SQLReadForm[M]
		val _14 :SQLReadForm[N]
		val _15 :SQLReadForm[O]
		val _16 :SQLReadForm[P]
		val _17 :SQLReadForm[Q]
		val _18 :SQLReadForm[R]
		
		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns
		private[this] val offset_10 = offset_9 + _9.readColumns
		private[this] val offset_11 = offset_10 + _10.readColumns
		private[this] val offset_12 = offset_11 + _11.readColumns
		private[this] val offset_13 = offset_12 + _12.readColumns
		private[this] val offset_14 = offset_13 + _13.readColumns
		private[this] val offset_15 = offset_14 + _14.readColumns
		private[this] val offset_16 = offset_15 + _15.readColumns
		private[this] val offset_17 = offset_16 + _16.readColumns
		private[this] val offset_18 = offset_17 + _17.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
			j <- _10.opt(res, position + offset_10)
			k <- _11.opt(res, position + offset_11)
			l <- _12.opt(res, position + offset_12)
			m <- _13.opt(res, position + offset_13)
			n <- _14.opt(res, position + offset_14)
			o <- _15.opt(res, position + offset_15)
			p <- _16.opt(res, position + offset_16)
			q <- _17.opt(res, position + offset_17)
			r <- _18.opt(res, position + offset_18)
		} yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)

		override def nullValue :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue, _10.nullValue, _11.nullValue, _12.nullValue, _13.nullValue, _14.nullValue,
			_15.nullValue, _16.nullValue, _17.nullValue, _18.nullValue
		)

		override def readColumns :Int = offset_18 + _18.readColumns
	}

	private[schema] case class Tuple18ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I], _10 :SQLReadForm[J],
				_11 :SQLReadForm[K], _12 :SQLReadForm[L], _13 :SQLReadForm[M], _14 :SQLReadForm[N], _15 :SQLReadForm[O],
				_16 :SQLReadForm[P], _17 :SQLReadForm[Q], _18 :SQLReadForm[R]	                                                                       
		) extends AbstractTuple18ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
	{
		override def productPrefix :String = "<"
	}


	
	private[schema] trait AbstractTuple19ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] 
		extends SQLReadForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] 
	{
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]
		val _10 :SQLReadForm[J]
		val _11 :SQLReadForm[K]
		val _12 :SQLReadForm[L]
		val _13 :SQLReadForm[M]
		val _14 :SQLReadForm[N]
		val _15 :SQLReadForm[O]
		val _16 :SQLReadForm[P]
		val _17 :SQLReadForm[Q]
		val _18 :SQLReadForm[R]
		val _19 :SQLReadForm[S]
		
		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns
		private[this] val offset_10 = offset_9 + _9.readColumns
		private[this] val offset_11 = offset_10 + _10.readColumns
		private[this] val offset_12 = offset_11 + _11.readColumns
		private[this] val offset_13 = offset_12 + _12.readColumns
		private[this] val offset_14 = offset_13 + _13.readColumns
		private[this] val offset_15 = offset_14 + _14.readColumns
		private[this] val offset_16 = offset_15 + _15.readColumns
		private[this] val offset_17 = offset_16 + _16.readColumns
		private[this] val offset_18 = offset_17 + _17.readColumns
		private[this] val offset_19 = offset_18 + _18.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
			j <- _10.opt(res, position + offset_10)
			k <- _11.opt(res, position + offset_11)
			l <- _12.opt(res, position + offset_12)
			m <- _13.opt(res, position + offset_13)
			n <- _14.opt(res, position + offset_14)
			o <- _15.opt(res, position + offset_15)
			p <- _16.opt(res, position + offset_16)
			q <- _17.opt(res, position + offset_17)
			r <- _18.opt(res, position + offset_18)
			s <- _19.opt(res, position + offset_19)
		} yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)

		override def nullValue :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue, _10.nullValue, _11.nullValue, _12.nullValue, _13.nullValue, _14.nullValue,
			_15.nullValue, _16.nullValue, _17.nullValue, _18.nullValue, _19.nullValue
		)

		override def readColumns :Int = offset_19 + _19.readColumns
	}

	private[schema] case class Tuple19ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I], _10 :SQLReadForm[J],
				_11 :SQLReadForm[K], _12 :SQLReadForm[L], _13 :SQLReadForm[M], _14 :SQLReadForm[N], _15 :SQLReadForm[O],
				_16 :SQLReadForm[P], _17 :SQLReadForm[Q], _18 :SQLReadForm[R], _19 :SQLReadForm[S]	                                                                       
		) extends AbstractTuple19ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
	{
		override def productPrefix :String = "<"
	}


	
	private[schema] trait AbstractTuple20ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] 
		extends SQLReadForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] 
	{
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]
		val _10 :SQLReadForm[J]
		val _11 :SQLReadForm[K]
		val _12 :SQLReadForm[L]
		val _13 :SQLReadForm[M]
		val _14 :SQLReadForm[N]
		val _15 :SQLReadForm[O]
		val _16 :SQLReadForm[P]
		val _17 :SQLReadForm[Q]
		val _18 :SQLReadForm[R]
		val _19 :SQLReadForm[S]
		val _20 :SQLReadForm[T]
		
		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns
		private[this] val offset_10 = offset_9 + _9.readColumns
		private[this] val offset_11 = offset_10 + _10.readColumns
		private[this] val offset_12 = offset_11 + _11.readColumns
		private[this] val offset_13 = offset_12 + _12.readColumns
		private[this] val offset_14 = offset_13 + _13.readColumns
		private[this] val offset_15 = offset_14 + _14.readColumns
		private[this] val offset_16 = offset_15 + _15.readColumns
		private[this] val offset_17 = offset_16 + _16.readColumns
		private[this] val offset_18 = offset_17 + _17.readColumns
		private[this] val offset_19 = offset_18 + _18.readColumns
		private[this] val offset_20 = offset_19 + _19.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
			j <- _10.opt(res, position + offset_10)
			k <- _11.opt(res, position + offset_11)
			l <- _12.opt(res, position + offset_12)
			m <- _13.opt(res, position + offset_13)
			n <- _14.opt(res, position + offset_14)
			o <- _15.opt(res, position + offset_15)
			p <- _16.opt(res, position + offset_16)
			q <- _17.opt(res, position + offset_17)
			r <- _18.opt(res, position + offset_18)
			s <- _19.opt(res, position + offset_19)
			t <- _20.opt(res, position + offset_20)
		} yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)

		override def nullValue :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue, _10.nullValue, _11.nullValue, _12.nullValue, _13.nullValue, _14.nullValue,
			_15.nullValue, _16.nullValue, _17.nullValue, _18.nullValue, _19.nullValue, _20.nullValue
		)

		override def readColumns :Int = offset_20 + _20.readColumns
	}

	private[schema] case class Tuple20ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I], _10 :SQLReadForm[J],
				_11 :SQLReadForm[K], _12 :SQLReadForm[L], _13 :SQLReadForm[M], _14 :SQLReadForm[N], _15 :SQLReadForm[O],
				_16 :SQLReadForm[P], _17 :SQLReadForm[Q], _18 :SQLReadForm[R], _19 :SQLReadForm[S], _20 :SQLReadForm[T]	                                                                       
		) extends AbstractTuple20ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
	{
		override def productPrefix :String = "<"
	}


	
	private[schema] trait AbstractTuple21ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] 
		extends SQLReadForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] 
	{
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]
		val _10 :SQLReadForm[J]
		val _11 :SQLReadForm[K]
		val _12 :SQLReadForm[L]
		val _13 :SQLReadForm[M]
		val _14 :SQLReadForm[N]
		val _15 :SQLReadForm[O]
		val _16 :SQLReadForm[P]
		val _17 :SQLReadForm[Q]
		val _18 :SQLReadForm[R]
		val _19 :SQLReadForm[S]
		val _20 :SQLReadForm[T]
		val _21 :SQLReadForm[U]
		
		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns
		private[this] val offset_10 = offset_9 + _9.readColumns
		private[this] val offset_11 = offset_10 + _10.readColumns
		private[this] val offset_12 = offset_11 + _11.readColumns
		private[this] val offset_13 = offset_12 + _12.readColumns
		private[this] val offset_14 = offset_13 + _13.readColumns
		private[this] val offset_15 = offset_14 + _14.readColumns
		private[this] val offset_16 = offset_15 + _15.readColumns
		private[this] val offset_17 = offset_16 + _16.readColumns
		private[this] val offset_18 = offset_17 + _17.readColumns
		private[this] val offset_19 = offset_18 + _18.readColumns
		private[this] val offset_20 = offset_19 + _19.readColumns
		private[this] val offset_21 = offset_20 + _20.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
			j <- _10.opt(res, position + offset_10)
			k <- _11.opt(res, position + offset_11)
			l <- _12.opt(res, position + offset_12)
			m <- _13.opt(res, position + offset_13)
			n <- _14.opt(res, position + offset_14)
			o <- _15.opt(res, position + offset_15)
			p <- _16.opt(res, position + offset_16)
			q <- _17.opt(res, position + offset_17)
			r <- _18.opt(res, position + offset_18)
			s <- _19.opt(res, position + offset_19)
			t <- _20.opt(res, position + offset_20)
			u <- _21.opt(res, position + offset_21)
		} yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)

		override def nullValue :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue, _10.nullValue, _11.nullValue, _12.nullValue, _13.nullValue, _14.nullValue,
			_15.nullValue, _16.nullValue, _17.nullValue, _18.nullValue, _19.nullValue, _20.nullValue, _21.nullValue
		)

		override def readColumns :Int = offset_21 + _21.readColumns
	}

	private[schema] case class Tuple21ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I], _10 :SQLReadForm[J],
				_11 :SQLReadForm[K], _12 :SQLReadForm[L], _13 :SQLReadForm[M], _14 :SQLReadForm[N], _15 :SQLReadForm[O],
				_16 :SQLReadForm[P], _17 :SQLReadForm[Q], _18 :SQLReadForm[R], _19 :SQLReadForm[S], _20 :SQLReadForm[T],
				_21 :SQLReadForm[U]	                                                                                         
		) extends AbstractTuple21ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
	{
		override def productPrefix :String = "<"
	}


	
	private[schema] trait AbstractTuple22ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] 
		extends SQLReadForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] 
	{
		val _1 :SQLReadForm[A]
		val _2 :SQLReadForm[B]
		val _3 :SQLReadForm[C]
		val _4 :SQLReadForm[D]
		val _5 :SQLReadForm[E]
		val _6 :SQLReadForm[F]
		val _7 :SQLReadForm[G]
		val _8 :SQLReadForm[H]
		val _9 :SQLReadForm[I]
		val _10 :SQLReadForm[J]
		val _11 :SQLReadForm[K]
		val _12 :SQLReadForm[L]
		val _13 :SQLReadForm[M]
		val _14 :SQLReadForm[N]
		val _15 :SQLReadForm[O]
		val _16 :SQLReadForm[P]
		val _17 :SQLReadForm[Q]
		val _18 :SQLReadForm[R]
		val _19 :SQLReadForm[S]
		val _20 :SQLReadForm[T]
		val _21 :SQLReadForm[U]
		val _22 :SQLReadForm[V]
		
		private[this] val offset_2 = _1.readColumns
		private[this] val offset_3 = offset_2 + _2.readColumns
		private[this] val offset_4 = offset_3 + _3.readColumns
		private[this] val offset_5 = offset_4 + _4.readColumns
		private[this] val offset_6 = offset_5 + _5.readColumns
		private[this] val offset_7 = offset_6 + _6.readColumns
		private[this] val offset_8 = offset_7 + _7.readColumns
		private[this] val offset_9 = offset_8 + _8.readColumns
		private[this] val offset_10 = offset_9 + _9.readColumns
		private[this] val offset_11 = offset_10 + _10.readColumns
		private[this] val offset_12 = offset_11 + _11.readColumns
		private[this] val offset_13 = offset_12 + _12.readColumns
		private[this] val offset_14 = offset_13 + _13.readColumns
		private[this] val offset_15 = offset_14 + _14.readColumns
		private[this] val offset_16 = offset_15 + _15.readColumns
		private[this] val offset_17 = offset_16 + _16.readColumns
		private[this] val offset_18 = offset_17 + _17.readColumns
		private[this] val offset_19 = offset_18 + _18.readColumns
		private[this] val offset_20 = offset_19 + _19.readColumns
		private[this] val offset_21 = offset_20 + _20.readColumns
		private[this] val offset_22 = offset_21 + _21.readColumns

		override def opt(res :ResultSet, position :Int) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = for {
			a <- _1.opt(res, position)
			b <- _2.opt(res, position + offset_2)
			c <- _3.opt(res, position + offset_3)
			d <- _4.opt(res, position + offset_4)
			e <- _5.opt(res, position + offset_5)
			f <- _6.opt(res, position + offset_6)
			g <- _7.opt(res, position + offset_7)
			h <- _8.opt(res, position + offset_8)
			i <- _9.opt(res, position + offset_9)
			j <- _10.opt(res, position + offset_10)
			k <- _11.opt(res, position + offset_11)
			l <- _12.opt(res, position + offset_12)
			m <- _13.opt(res, position + offset_13)
			n <- _14.opt(res, position + offset_14)
			o <- _15.opt(res, position + offset_15)
			p <- _16.opt(res, position + offset_16)
			q <- _17.opt(res, position + offset_17)
			r <- _18.opt(res, position + offset_18)
			s <- _19.opt(res, position + offset_19)
			t <- _20.opt(res, position + offset_20)
			u <- _21.opt(res, position + offset_21)
			v <- _22.opt(res, position + offset_22)
		} yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)

		override def nullValue :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = (
			_1.nullValue, _2.nullValue, _3.nullValue, _4.nullValue, _5.nullValue, _6.nullValue, _7.nullValue, 
			_8.nullValue, _9.nullValue, _10.nullValue, _11.nullValue, _12.nullValue, _13.nullValue, _14.nullValue,
			_15.nullValue, _16.nullValue, _17.nullValue, _18.nullValue, _19.nullValue, _20.nullValue, _21.nullValue,
			_22.nullValue
		)

		override def readColumns :Int = offset_22 + _22.readColumns
	}

	private[schema] case class Tuple22ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](
				_1 :SQLReadForm[A], _2 :SQLReadForm[B], _3 :SQLReadForm[C], _4 :SQLReadForm[D], _5 :SQLReadForm[E],
				_6 :SQLReadForm[F], _7 :SQLReadForm[G], _8 :SQLReadForm[H], _9 :SQLReadForm[I], _10 :SQLReadForm[J],
				_11 :SQLReadForm[K], _12 :SQLReadForm[L], _13 :SQLReadForm[M], _14 :SQLReadForm[N], _15 :SQLReadForm[O],
				_16 :SQLReadForm[P], _17 :SQLReadForm[Q], _18 :SQLReadForm[R], _19 :SQLReadForm[S], _20 :SQLReadForm[T],
				_21 :SQLReadForm[U], _22 :SQLReadForm[V]	                                                                                         
		) extends AbstractTuple22ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
	{
		override def productPrefix :String = "<"
	}





	
}
