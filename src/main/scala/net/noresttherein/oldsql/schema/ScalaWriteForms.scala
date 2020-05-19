package net.noresttherein.oldsql.schema

import java.sql.PreparedStatement

import net.noresttherein.oldsql.schema.ScalaWriteForms._



trait ScalaWriteForms {
	@inline private[this] def f[T](implicit form :SQLWriteForm[T]) :SQLWriteForm[T] = form



	implicit def OptionWriteForm[T :SQLWriteForm] :SQLWriteForm[Option[T]] =
		new OptionWriteForm[T] { val form :SQLWriteForm[T] = SQLWriteForm[T] }

	implicit def SomeWriteForm[T :SQLWriteForm] :SQLWriteForm[Some[T]] =
		SQLWriteForm[T].unmap(_.get)



	implicit def Tuple2WriteForm[A :SQLWriteForm, B :SQLWriteForm] :SQLWriteForm[(A, B)] =
		new Tuple2WriteForm[A, B](f[A], f[B])

	implicit def Tuple3WriteForm[A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm] :SQLWriteForm[(A, B, C)] =
		new Tuple3WriteForm[A, B, C](f[A], f[B], f[C])

	implicit def Tuple4WriteForm[A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm]
			:SQLWriteForm[(A, B, C, D)] =
		new Tuple4WriteForm[A, B, C, D](f[A], f[B], f[C], f[D])

	implicit def Tuple5WriteForm[A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm]
			:SQLWriteForm[(A, B, C, D, E)] =
		new Tuple5WriteForm[A, B, C, D, E](f[A], f[B], f[C], f[D], f[E])

	implicit def Tuple6WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F)] =
		new Tuple6WriteForm[A, B, C, D, E, F](f[A], f[B], f[C], f[D], f[E], f[F])

	implicit def Tuple7WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G)] =
		new Tuple7WriteForm[A, B, C, D, E, F, G](f[A], f[B], f[C], f[D], f[E], f[F], f[G])

	implicit def Tuple8WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H)] =
		new Tuple8WriteForm[A, B, C, D, E, F, G, H](f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H])

	implicit def Tuple9WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I)] =
		new Tuple9WriteForm[A, B, C, D, E, F, G, H, I](f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I])

	implicit def Tuple10WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J)] =
		new Tuple10WriteForm[A, B, C, D, E, F, G, H, I, J](f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J])

	implicit def Tuple11WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K)] =
		new Tuple11WriteForm[A, B, C, D, E, F, G, H, I, J, K](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K]
		)

	implicit def Tuple12WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L)] =
		new Tuple12WriteForm[A, B, C, D, E, F, G, H, I, J, K, L](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L]
		)

	implicit def Tuple13WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
		new Tuple13WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M]
		)

	implicit def Tuple14WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
		new Tuple14WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N]
		)

	implicit def Tuple15WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
		new Tuple15WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O]
		)

	implicit def Tuple16WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
		new Tuple16WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P]
		)

	implicit def Tuple17WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm, Q :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
		new Tuple17WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q]
		)

	implicit def Tuple18WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm, Q :SQLWriteForm, R :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
		new Tuple18WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R]
		)

	implicit def Tuple19WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm, Q :SQLWriteForm, R :SQLWriteForm,
				S :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
		new Tuple19WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R],
			f[S]
		)

	implicit def Tuple20WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm, Q :SQLWriteForm, R :SQLWriteForm,
				S :SQLWriteForm, T :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
		new Tuple20WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R],
			f[S], f[T]
		)

	implicit def Tuple21WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm, Q :SQLWriteForm, R :SQLWriteForm,
				S :SQLWriteForm, T :SQLWriteForm, U :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
		new Tuple21WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R],
			f[S], f[T], f[U]
		)

	implicit def Tuple22WriteForm[
				A :SQLWriteForm, B :SQLWriteForm, C :SQLWriteForm, D :SQLWriteForm, E :SQLWriteForm, F :SQLWriteForm,
				G :SQLWriteForm, H :SQLWriteForm, I :SQLWriteForm, J :SQLWriteForm, K :SQLWriteForm, L :SQLWriteForm,
				M :SQLWriteForm, N :SQLWriteForm, O :SQLWriteForm, P :SQLWriteForm, Q :SQLWriteForm, R :SQLWriteForm,
				S :SQLWriteForm, T :SQLWriteForm, U :SQLWriteForm, V :SQLWriteForm
			] :SQLWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
		new Tuple22WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R],
			f[S], f[T], f[U], f[V]
		)



}









private[schema] object ScalaWriteForms {



	trait NullableLiteralWriteForm[-T >: Null] extends SQLWriteForm[T] {
		override def literal(value :T) :String = literal(value, false)
		override def nullLiteral :String = literal(null, false)
		override def inlineLiteral(value :T) :String = literal(value, true)
		override def inlineNullLiteral :String = literal(null, true)
		override def nullLiteral(inline :Boolean) :String = literal(null, false)
	}



	trait OptionWriteForm[-T] extends SQLWriteForm[Option[T]] {
		protected def form :SQLWriteForm[T]

		override def writtenColumns :Int = form.writtenColumns

		override def set(position :Int)(statement :PreparedStatement, value :Option[T]) :Unit =
			form.setOpt(position)(statement, value)

		override def setNull(position :Int)(statement :PreparedStatement) :Unit =
			form.setNull(position)(statement)

		override def literal(value :Option[T]) :String = value match {
			case Some(x) => form.literal(x)
			case _ => form.nullLiteral
		}
		override def nullLiteral :String = form.nullLiteral

		override def inlineLiteral(value :Option[T]) :String = value match {
			case Some(x) => form.inlineLiteral(x)
			case _ => form.inlineNullLiteral
		}
		override def inlineNullLiteral :String = form.inlineNullLiteral

		override def equals(that :Any) :Boolean = that match {
			case opt :OptionWriteForm[_] => (this eq opt) || opt.canEqual(this) && opt.form == form
			case _ => false
		}

		override def hashCode :Int = form.hashCode

		override def toString :String = "Option[" + form + "]"
	}



	trait AbstractTuple2WriteForm[-L, -R] extends SQLWriteForm[(L, R)] {
		override def writtenColumns: Int = _1.writtenColumns + _2.writtenColumns

		val _1 :SQLWriteForm[L]
		val _2 :SQLWriteForm[R]

		override def set(position :Int)(statement :PreparedStatement, value :(L, R)) :Unit =
			if (value == null) {
				_1.setNull(position)(statement)
				_2.setNull(position + _1.writtenColumns)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + _1.writtenColumns)(statement, value._2)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + _1.writtenColumns)(statement)
		}


		override def literal(value: (L, R)): String =
			if (value == null) s"(${_1.inlineNullLiteral}, ${_2.inlineNullLiteral})"
			else s"(${_1.inlineLiteral(value._1)}, ${_2.inlineLiteral(value._2)})"

		override def nullLiteral: String = s"(${_1.inlineNullLiteral}, ${_2.inlineNullLiteral})"

		override def inlineLiteral(value: (L, R)): String = _1.inlineLiteral(value._1) + ", " + _2.inlineLiteral(value._2)

		override def inlineNullLiteral: String = _1.inlineNullLiteral + ", " + _2.inlineNullLiteral

	}

	case class Tuple2WriteForm[-L, -R](_1 :SQLWriteForm[L], _2 :SQLWriteForm[R])
		extends AbstractTuple2WriteForm[L, R]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple3WriteForm[-A, -B, -C] extends NullableLiteralWriteForm[(A, B, C)] {
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns

		override val writtenColumns: Int = offset_3 + _3.writtenColumns

		override def set(position :Int)(statement :PreparedStatement, value :(A, B, C)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
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

	case class Tuple3WriteForm[-A, -B, -C](_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C])
		extends AbstractTuple3WriteForm[A, B, C]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple4WriteForm[-A, -B, -C, -D] extends NullableLiteralWriteForm[(A, B, C, D)] {
		val _1 :SQLWriteForm[A]
		val _2 :SQLWriteForm[B]
		val _3 :SQLWriteForm[C]
		val _4 :SQLWriteForm[D]

		private[this] val offset_2 = _1.writtenColumns
		private[this] val offset_3 = offset_2 + _2.writtenColumns
		private[this] val offset_4 = offset_3 + _3.writtenColumns
		
		override val writtenColumns: Int = offset_4 + _4.writtenColumns

		override def set(position :Int)(statement :PreparedStatement, value :(A, B, C, D)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
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

	case class Tuple4WriteForm[-A, -B, -C, -D](_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D])
		extends AbstractTuple4WriteForm[A, B, C, D]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple5WriteForm[-A, -B, -C, -D, -E] extends NullableLiteralWriteForm[(A, B, C, D, E)] {
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

		override def set(position :Int)(statement :PreparedStatement, value :(A, B, C, D, E)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
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

	case class Tuple5WriteForm[-A, -B, -C, -D, -E](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E]
		) extends AbstractTuple5WriteForm[A, B, C, D, E]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple6WriteForm[-A, -B, -C, -D, -E, -F] extends NullableLiteralWriteForm[(A, B, C, D, E, F)] {
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

		override def set(position :Int)(statement :PreparedStatement, value :(A, B, C, D, E, F)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
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

	case class Tuple6WriteForm[-A, -B, -C, -D, -E, -F](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F]
		) extends AbstractTuple6WriteForm[A, B, C, D, E, F]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple7WriteForm[-A, -B, -C, -D, -E, -F, -G] extends NullableLiteralWriteForm[(A, B, C, D, E, F, G)] {
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

		override def set(position :Int)(statement :PreparedStatement, value :(A, B, C, D, E, F, G)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
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

	case class Tuple7WriteForm[-A, -B, -C, -D, -E, -F, -G](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G]
		) extends AbstractTuple7WriteForm[A, B, C, D, E, F, G]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple8WriteForm[-A, -B, -C, -D, -E, -F, -G, -H] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H)] 
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

		override def set(position :Int)(statement :PreparedStatement, value :(A, B, C, D, E, F, G, H)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
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

	case class Tuple8WriteForm[-A, -B, -C, -D, -E, -F, -G, -H](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H]
		) extends AbstractTuple8WriteForm[A, B, C, D, E, F, G, H]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple9WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I)] 
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

		override def set(position :Int)(statement :PreparedStatement, value :(A, B, C, D, E, F, G, H, I)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
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

	case class Tuple9WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I]
		) extends AbstractTuple9WriteForm[A, B, C, D, E, F, G, H, I]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple10WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I, J)] 
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

		override def set(position :Int)(statement :PreparedStatement, value :(A, B, C, D, E, F, G, H, I, J)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
				_10.set(position + offset_10)(statement, value._10)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
			_10.setNull(position + offset_10)(statement)
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

	case class Tuple10WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J]
		) extends AbstractTuple10WriteForm[A, B, C, D, E, F, G, H, I, J]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple11WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I, J, K)] 
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

		override def set(position :Int)(statement :PreparedStatement, value :(A, B, C, D, E, F, G, H, I, J, K)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
				_10.set(position + offset_10)(statement, value._10)
				_11.set(position + offset_11)(statement, value._11)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
			_10.setNull(position + offset_10)(statement)
			_11.setNull(position + offset_11)(statement)
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

	case class Tuple11WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K]	                                                                       
		) extends AbstractTuple11WriteForm[A, B, C, D, E, F, G, H, I, J, K]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple12WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L)] 
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

		override def set(position :Int)(statement :PreparedStatement, value :(A, B, C, D, E, F, G, H, I, J, K, L)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
				_10.set(position + offset_10)(statement, value._10)
				_11.set(position + offset_11)(statement, value._11)
				_12.set(position + offset_12)(statement, value._12)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
			_10.setNull(position + offset_10)(statement)
			_11.setNull(position + offset_11)(statement)
			_12.setNull(position + offset_12)(statement)
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

	case class Tuple12WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L]	                                                                       
		) extends AbstractTuple12WriteForm[A, B, C, D, E, F, G, H, I, J, K, L]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple13WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M)] 
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

		override def set(position :Int)(statement :PreparedStatement, value :(A, B, C, D, E, F, G, H, I, J, K, L, M)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
				_10.set(position + offset_10)(statement, value._10)
				_11.set(position + offset_11)(statement, value._11)
				_12.set(position + offset_12)(statement, value._12)
				_13.set(position + offset_13)(statement, value._13)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
			_10.setNull(position + offset_10)(statement)
			_11.setNull(position + offset_11)(statement)
			_12.setNull(position + offset_12)(statement)
			_13.setNull(position + offset_13)(statement)
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

	case class Tuple13WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M] 	                                                                       
		) extends AbstractTuple13WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple14WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] 
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

		override def set(position :Int)(statement :PreparedStatement, value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
				_10.set(position + offset_10)(statement, value._10)
				_11.set(position + offset_11)(statement, value._11)
				_12.set(position + offset_12)(statement, value._12)
				_13.set(position + offset_13)(statement, value._13)
				_14.set(position + offset_14)(statement, value._14)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
			_10.setNull(position + offset_10)(statement)
			_11.setNull(position + offset_11)(statement)
			_12.setNull(position + offset_12)(statement)
			_13.setNull(position + offset_13)(statement)
			_14.setNull(position + offset_14)(statement)
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

	case class Tuple14WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N] 	                                                                       
		) extends AbstractTuple14WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple15WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] 
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

		override def set(position :Int)(
		                 statement :PreparedStatement, 
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
				_10.set(position + offset_10)(statement, value._10)
				_11.set(position + offset_11)(statement, value._11)
				_12.set(position + offset_12)(statement, value._12)
				_13.set(position + offset_13)(statement, value._13)
				_14.set(position + offset_14)(statement, value._14)
				_15.set(position + offset_15)(statement, value._15)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
			_10.setNull(position + offset_10)(statement)
			_11.setNull(position + offset_11)(statement)
			_12.setNull(position + offset_12)(statement)
			_13.setNull(position + offset_13)(statement)
			_14.setNull(position + offset_14)(statement)
			_15.setNull(position + offset_15)(statement)
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

	case class Tuple15WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O] 	                                                                       
		) extends AbstractTuple15WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple16WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] 
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

		override def set(position :Int)(
		                 statement :PreparedStatement, 
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
				_10.set(position + offset_10)(statement, value._10)
				_11.set(position + offset_11)(statement, value._11)
				_12.set(position + offset_12)(statement, value._12)
				_13.set(position + offset_13)(statement, value._13)
				_14.set(position + offset_14)(statement, value._14)
				_15.set(position + offset_15)(statement, value._15)
				_16.set(position + offset_16)(statement, value._16)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
			_10.setNull(position + offset_10)(statement)
			_11.setNull(position + offset_11)(statement)
			_12.setNull(position + offset_12)(statement)
			_13.setNull(position + offset_13)(statement)
			_14.setNull(position + offset_14)(statement)
			_15.setNull(position + offset_15)(statement)
			_16.setNull(position + offset_16)(statement)
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

	case class Tuple16WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O],
			_16 :SQLWriteForm[P]		                                                                                           
		) extends AbstractTuple16WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple17WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] 
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

		override def set(position :Int)(
		                 statement :PreparedStatement, 
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
				_10.set(position + offset_10)(statement, value._10)
				_11.set(position + offset_11)(statement, value._11)
				_12.set(position + offset_12)(statement, value._12)
				_13.set(position + offset_13)(statement, value._13)
				_14.set(position + offset_14)(statement, value._14)
				_15.set(position + offset_15)(statement, value._15)
				_16.set(position + offset_16)(statement, value._16)
				_17.set(position + offset_17)(statement, value._17)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
			_10.setNull(position + offset_10)(statement)
			_11.setNull(position + offset_11)(statement)
			_12.setNull(position + offset_12)(statement)
			_13.setNull(position + offset_13)(statement)
			_14.setNull(position + offset_14)(statement)
			_15.setNull(position + offset_15)(statement)
			_16.setNull(position + offset_16)(statement)
			_17.setNull(position + offset_17)(statement)
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

	case class Tuple17WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O],
			_16 :SQLWriteForm[P], _17 :SQLWriteForm[Q]		                                                                                           
		) extends AbstractTuple17WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple18WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] 
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

		override def set(position :Int)(
		                 statement :PreparedStatement, 
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
				_10.set(position + offset_10)(statement, value._10)
				_11.set(position + offset_11)(statement, value._11)
				_12.set(position + offset_12)(statement, value._12)
				_13.set(position + offset_13)(statement, value._13)
				_14.set(position + offset_14)(statement, value._14)
				_15.set(position + offset_15)(statement, value._15)
				_16.set(position + offset_16)(statement, value._16)
				_17.set(position + offset_17)(statement, value._17)
				_18.set(position + offset_18)(statement, value._18)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
			_10.setNull(position + offset_10)(statement)
			_11.setNull(position + offset_11)(statement)
			_12.setNull(position + offset_12)(statement)
			_13.setNull(position + offset_13)(statement)
			_14.setNull(position + offset_14)(statement)
			_15.setNull(position + offset_15)(statement)
			_16.setNull(position + offset_16)(statement)
			_17.setNull(position + offset_17)(statement)
			_18.setNull(position + offset_18)(statement)
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

	case class Tuple18WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O],
			_16 :SQLWriteForm[P], _17 :SQLWriteForm[Q], _18 :SQLWriteForm[R]		                                                                                           
		) extends AbstractTuple18WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple19WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] 
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

		override def set(position :Int)(
		                 statement :PreparedStatement, 
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
				_10.set(position + offset_10)(statement, value._10)
				_11.set(position + offset_11)(statement, value._11)
				_12.set(position + offset_12)(statement, value._12)
				_13.set(position + offset_13)(statement, value._13)
				_14.set(position + offset_14)(statement, value._14)
				_15.set(position + offset_15)(statement, value._15)
				_16.set(position + offset_16)(statement, value._16)
				_17.set(position + offset_17)(statement, value._17)
				_18.set(position + offset_18)(statement, value._18)
				_19.set(position + offset_19)(statement, value._19)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
			_10.setNull(position + offset_10)(statement)
			_11.setNull(position + offset_11)(statement)
			_12.setNull(position + offset_12)(statement)
			_13.setNull(position + offset_13)(statement)
			_14.setNull(position + offset_14)(statement)
			_15.setNull(position + offset_15)(statement)
			_16.setNull(position + offset_16)(statement)
			_17.setNull(position + offset_17)(statement)
			_18.setNull(position + offset_18)(statement)
			_19.setNull(position + offset_19)(statement)
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

	case class Tuple19WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O],
			_16 :SQLWriteForm[P], _17 :SQLWriteForm[Q], _18 :SQLWriteForm[R], _19 :SQLWriteForm[S]		                                                                                           
		) extends AbstractTuple19WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple20WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S, -T] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] 
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

		override def set(position :Int)(
		                 statement :PreparedStatement, 
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
				_10.set(position + offset_10)(statement, value._10)
				_11.set(position + offset_11)(statement, value._11)
				_12.set(position + offset_12)(statement, value._12)
				_13.set(position + offset_13)(statement, value._13)
				_14.set(position + offset_14)(statement, value._14)
				_15.set(position + offset_15)(statement, value._15)
				_16.set(position + offset_16)(statement, value._16)
				_17.set(position + offset_17)(statement, value._17)
				_18.set(position + offset_18)(statement, value._18)
				_19.set(position + offset_19)(statement, value._19)
				_20.set(position + offset_20)(statement, value._20)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
			_10.setNull(position + offset_10)(statement)
			_11.setNull(position + offset_11)(statement)
			_12.setNull(position + offset_12)(statement)
			_13.setNull(position + offset_13)(statement)
			_14.setNull(position + offset_14)(statement)
			_15.setNull(position + offset_15)(statement)
			_16.setNull(position + offset_16)(statement)
			_17.setNull(position + offset_17)(statement)
			_18.setNull(position + offset_18)(statement)
			_19.setNull(position + offset_19)(statement)
			_20.setNull(position + offset_20)(statement)
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

	case class Tuple20WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S, -T](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O],
			_16 :SQLWriteForm[P], _17 :SQLWriteForm[Q], _18 :SQLWriteForm[R], _19 :SQLWriteForm[S], _20 :SQLWriteForm[T]		                                                                                           
		) extends AbstractTuple20WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple21WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S, -T, -U] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] 
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

		override def set(position :Int)(
		                 statement :PreparedStatement, 
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
				_10.set(position + offset_10)(statement, value._10)
				_11.set(position + offset_11)(statement, value._11)
				_12.set(position + offset_12)(statement, value._12)
				_13.set(position + offset_13)(statement, value._13)
				_14.set(position + offset_14)(statement, value._14)
				_15.set(position + offset_15)(statement, value._15)
				_16.set(position + offset_16)(statement, value._16)
				_17.set(position + offset_17)(statement, value._17)
				_18.set(position + offset_18)(statement, value._18)
				_19.set(position + offset_19)(statement, value._19)
				_20.set(position + offset_20)(statement, value._20)
				_21.set(position + offset_21)(statement, value._21)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
			_10.setNull(position + offset_10)(statement)
			_11.setNull(position + offset_11)(statement)
			_12.setNull(position + offset_12)(statement)
			_13.setNull(position + offset_13)(statement)
			_14.setNull(position + offset_14)(statement)
			_15.setNull(position + offset_15)(statement)
			_16.setNull(position + offset_16)(statement)
			_17.setNull(position + offset_17)(statement)
			_18.setNull(position + offset_18)(statement)
			_19.setNull(position + offset_19)(statement)
			_20.setNull(position + offset_20)(statement)
			_21.setNull(position + offset_21)(statement)
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

	case class Tuple21WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S, -T, -U](
			_1 :SQLWriteForm[A], _2 :SQLWriteForm[B], _3 :SQLWriteForm[C], _4 :SQLWriteForm[D], _5 :SQLWriteForm[E],
			_6 :SQLWriteForm[F], _7 :SQLWriteForm[G], _8 :SQLWriteForm[H], _9 :SQLWriteForm[I], _10 :SQLWriteForm[J],
			_11 :SQLWriteForm[K], _12 :SQLWriteForm[L], _13 :SQLWriteForm[M], _14 :SQLWriteForm[N], _15 :SQLWriteForm[O],
			_16 :SQLWriteForm[P], _17 :SQLWriteForm[Q], _18 :SQLWriteForm[R], _19 :SQLWriteForm[S], _20 :SQLWriteForm[T],
			_21 :SQLWriteForm[U]	                                                                                                               
		) extends AbstractTuple21WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
	{
		override def productPrefix :String = "<"
	}



	trait AbstractTuple22WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S, -T, -U, -V] 
		extends NullableLiteralWriteForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] 
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

		override def set(position :Int)(
		                 statement :PreparedStatement, 
		                 value :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)) :Unit =
			if (value == null) {
				setNull(position)(statement)
			} else {
				_1.set(position)(statement, value._1)
				_2.set(position + offset_2)(statement, value._2)
				_3.set(position + offset_3)(statement, value._3)
				_4.set(position + offset_4)(statement, value._4)
				_5.set(position + offset_5)(statement, value._5)
				_6.set(position + offset_6)(statement, value._6)
				_7.set(position + offset_7)(statement, value._7)
				_8.set(position + offset_8)(statement, value._8)
				_9.set(position + offset_9)(statement, value._9)
				_10.set(position + offset_10)(statement, value._10)
				_11.set(position + offset_11)(statement, value._11)
				_12.set(position + offset_12)(statement, value._12)
				_13.set(position + offset_13)(statement, value._13)
				_14.set(position + offset_14)(statement, value._14)
				_15.set(position + offset_15)(statement, value._15)
				_16.set(position + offset_16)(statement, value._16)
				_17.set(position + offset_17)(statement, value._17)
				_18.set(position + offset_18)(statement, value._18)
				_19.set(position + offset_19)(statement, value._19)
				_20.set(position + offset_20)(statement, value._20)
				_21.set(position + offset_21)(statement, value._21)
				_22.set(position + offset_22)(statement, value._22)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + offset_2)(statement)
			_3.setNull(position + offset_3)(statement)
			_4.setNull(position + offset_4)(statement)
			_5.setNull(position + offset_5)(statement)
			_6.setNull(position + offset_6)(statement)
			_7.setNull(position + offset_7)(statement)
			_8.setNull(position + offset_8)(statement)
			_9.setNull(position + offset_9)(statement)
			_10.setNull(position + offset_10)(statement)
			_11.setNull(position + offset_11)(statement)
			_12.setNull(position + offset_12)(statement)
			_13.setNull(position + offset_13)(statement)
			_14.setNull(position + offset_14)(statement)
			_15.setNull(position + offset_15)(statement)
			_16.setNull(position + offset_16)(statement)
			_17.setNull(position + offset_17)(statement)
			_18.setNull(position + offset_18)(statement)
			_19.setNull(position + offset_19)(statement)
			_20.setNull(position + offset_20)(statement)
			_21.setNull(position + offset_21)(statement)
			_22.setNull(position + offset_22)(statement)
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

	case class Tuple22WriteForm[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q, -R, -S, -T, -U, -V](
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
