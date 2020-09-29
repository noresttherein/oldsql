package net.noresttherein.oldsql.schema

import java.sql.ResultSet






trait ScalaReadForms






object ScalaReadForms {

	@inline private[this] def f[T](implicit form :SQLReadForm[T]) = form



	implicit def OptionReadForm[T :SQLReadForm] :SQLReadForm[Option[T]] =
		new OptionReadForm[T] { override val form = SQLReadForm[T] }
	//risks becoming preferred SQLReadForm[Option[T]] for any T
	def SomeReadForm[T :SQLReadForm] :SQLReadForm[Some[T]] =
		SQLReadForm[T].nullMap(Some.apply)



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









	private[schema] trait OptionReadForm[T] extends SQLReadForm[Option[T]] {
		protected def form :SQLReadForm[T]
		override def readColumns :Int = form.readColumns

		override def apply(position :Int)(res :ResultSet) :Option[T] = form.opt(position)(res)
		override def opt(position :Int)(res :ResultSet) :Option[Option[T]] = Some(form.opt(position)(res))
		override def nullValue :Option[T] = None

		override def equals(that :Any) :Boolean = that match  {
			case self :AnyRef if self eq this => true
			case opt :OptionReadForm[_] if opt canEqual this => opt.form == form
			case _ => false
		}

		override def hashCode :Int = form.hashCode

		override def toString :String = "Option[" + form + "]>"
	}



	private[schema] trait AbstractTuple2ReadForm[L, R] extends SQLReadForm[(L, R)] {
		val _1  :SQLReadForm[L]
		val _2  :SQLReadForm[R]

		override def opt(position: Int)(res: ResultSet): Option[(L, R)] = {
			val l = _1.opt(position)(res)
			val r = _2.opt(position + _1.readColumns)(res)
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
		
		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C)] = for {
				a <- _1.opt(position)(res)
				b <- _2.opt(position + offset_2)(res)
				c <- _3.opt(position + offset_3)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I, J)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
			j <- _10.opt(position + offset_10)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I, J, K)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
			j <- _10.opt(position + offset_10)(res)
			k <- _11.opt(position + offset_11)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I, J, K, L)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
			j <- _10.opt(position + offset_10)(res)
			k <- _11.opt(position + offset_11)(res)
			l <- _12.opt(position + offset_12)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
			j <- _10.opt(position + offset_10)(res)
			k <- _11.opt(position + offset_11)(res)
			l <- _12.opt(position + offset_12)(res)
			m <- _13.opt(position + offset_13)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
			j <- _10.opt(position + offset_10)(res)
			k <- _11.opt(position + offset_11)(res)
			l <- _12.opt(position + offset_12)(res)
			m <- _13.opt(position + offset_13)(res)
			n <- _14.opt(position + offset_14)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
			j <- _10.opt(position + offset_10)(res)
			k <- _11.opt(position + offset_11)(res)
			l <- _12.opt(position + offset_12)(res)
			m <- _13.opt(position + offset_13)(res)
			n <- _14.opt(position + offset_14)(res)
			o <- _15.opt(position + offset_15)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
			j <- _10.opt(position + offset_10)(res)
			k <- _11.opt(position + offset_11)(res)
			l <- _12.opt(position + offset_12)(res)
			m <- _13.opt(position + offset_13)(res)
			n <- _14.opt(position + offset_14)(res)
			o <- _15.opt(position + offset_15)(res)
			p <- _16.opt(position + offset_16)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
			j <- _10.opt(position + offset_10)(res)
			k <- _11.opt(position + offset_11)(res)
			l <- _12.opt(position + offset_12)(res)
			m <- _13.opt(position + offset_13)(res)
			n <- _14.opt(position + offset_14)(res)
			o <- _15.opt(position + offset_15)(res)
			p <- _16.opt(position + offset_16)(res)
			q <- _17.opt(position + offset_17)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
			j <- _10.opt(position + offset_10)(res)
			k <- _11.opt(position + offset_11)(res)
			l <- _12.opt(position + offset_12)(res)
			m <- _13.opt(position + offset_13)(res)
			n <- _14.opt(position + offset_14)(res)
			o <- _15.opt(position + offset_15)(res)
			p <- _16.opt(position + offset_16)(res)
			q <- _17.opt(position + offset_17)(res)
			r <- _18.opt(position + offset_18)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
			j <- _10.opt(position + offset_10)(res)
			k <- _11.opt(position + offset_11)(res)
			l <- _12.opt(position + offset_12)(res)
			m <- _13.opt(position + offset_13)(res)
			n <- _14.opt(position + offset_14)(res)
			o <- _15.opt(position + offset_15)(res)
			p <- _16.opt(position + offset_16)(res)
			q <- _17.opt(position + offset_17)(res)
			r <- _18.opt(position + offset_18)(res)
			s <- _19.opt(position + offset_19)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
			j <- _10.opt(position + offset_10)(res)
			k <- _11.opt(position + offset_11)(res)
			l <- _12.opt(position + offset_12)(res)
			m <- _13.opt(position + offset_13)(res)
			n <- _14.opt(position + offset_14)(res)
			o <- _15.opt(position + offset_15)(res)
			p <- _16.opt(position + offset_16)(res)
			q <- _17.opt(position + offset_17)(res)
			r <- _18.opt(position + offset_18)(res)
			s <- _19.opt(position + offset_19)(res)
			t <- _20.opt(position + offset_20)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
			j <- _10.opt(position + offset_10)(res)
			k <- _11.opt(position + offset_11)(res)
			l <- _12.opt(position + offset_12)(res)
			m <- _13.opt(position + offset_13)(res)
			n <- _14.opt(position + offset_14)(res)
			o <- _15.opt(position + offset_15)(res)
			p <- _16.opt(position + offset_16)(res)
			q <- _17.opt(position + offset_17)(res)
			r <- _18.opt(position + offset_18)(res)
			s <- _19.opt(position + offset_19)(res)
			t <- _20.opt(position + offset_20)(res)
			u <- _21.opt(position + offset_21)(res)
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

		override def opt(position :Int)(res :ResultSet) :Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = for {
			a <- _1.opt(position)(res)
			b <- _2.opt(position + offset_2)(res)
			c <- _3.opt(position + offset_3)(res)
			d <- _4.opt(position + offset_4)(res)
			e <- _5.opt(position + offset_5)(res)
			f <- _6.opt(position + offset_6)(res)
			g <- _7.opt(position + offset_7)(res)
			h <- _8.opt(position + offset_8)(res)
			i <- _9.opt(position + offset_9)(res)
			j <- _10.opt(position + offset_10)(res)
			k <- _11.opt(position + offset_11)(res)
			l <- _12.opt(position + offset_12)(res)
			m <- _13.opt(position + offset_13)(res)
			n <- _14.opt(position + offset_14)(res)
			o <- _15.opt(position + offset_15)(res)
			p <- _16.opt(position + offset_16)(res)
			q <- _17.opt(position + offset_17)(res)
			r <- _18.opt(position + offset_18)(res)
			s <- _19.opt(position + offset_19)(res)
			t <- _20.opt(position + offset_20)(res)
			u <- _21.opt(position + offset_21)(res)
			v <- _22.opt(position + offset_22)(res)
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
