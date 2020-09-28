package net.noresttherein.oldsql.schema

import java.sql.{PreparedStatement, ResultSet}

import net.noresttherein.oldsql.schema.ScalaForms._
import net.noresttherein.oldsql.schema.ScalaReadForms._
import net.noresttherein.oldsql.schema.ScalaWriteForms._
import net.noresttherein.oldsql.schema.SQLForm.EmptyForm






/** Trait mixed in by all forms solely to bring into the implicit search scope declarations from its companion object. */
trait ScalaForms






/** Implicit `SQLForm` definitions for some standard scala container types such as `Option` and tuples.
  * All these forms are in the implicit search scope for all form classes, so, prefer to rely on implicit resolution
  * by using `SQLForm[Xxx]` to access a form for the type `Xxx` instead of explicit references to declarations
  * in this object and others.
  */
object ScalaForms {

	@inline private[this] final def f[T](implicit form :SQLForm[T]) = form



	implicit case object UnitForm extends EmptyForm[Unit](()) {
		override def toString = "Unit"
	}

	implicit case object NothingForm extends EmptyForm[Nothing](
		throw new UnsupportedOperationException("ScalaForms.NothingForm")
	) {
		override def set(position :Int)(statement :PreparedStatement, value :Nothing) :Nothing =
			setNull(position)(statement)

		override def setNull(position :Int)(statement :PreparedStatement) :Nothing =
			throw new UnsupportedOperationException("ScalaForms.NothingForm")

		override def toString = "Nothing"
	}

	implicit case object NoneForm extends EmptyForm[Option[Nothing]](None) {
		override def toString = "None"
	}



	implicit def OptionForm[T :SQLForm] :SQLForm[Option[T]] = new OptionForm[T]

	implicit def SomeForm[T :SQLForm] :SQLForm[Some[T]] = SQLForm[T].nullBimap(Some.apply)(_.get)



	implicit def tuple2Form[A :SQLForm, B :SQLForm] :SQLForm[(A, B)] =
		new Tuple2Form[A, B](SQLForm[A], SQLForm[B])

	implicit def tuple3Form[A :SQLForm, B :SQLForm, C :SQLForm] :SQLForm[(A, B, C)] =
		new Tuple3Form[A, B, C](f[A], f[B], f[C])

	implicit def tuple4Form[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm]
			:SQLForm[(A, B, C, D)] =
		new Tuple4Form[A, B, C, D](f[A], f[B], f[C], f[D])

	implicit def tuple5Form[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm]
			:SQLForm[(A, B, C, D, E)] =
		new Tuple5Form[A, B, C, D, E](f[A], f[B], f[C], f[D], f[E])

	implicit def tuple6Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm
			] :SQLForm[(A, B, C, D, E, F)] =
		new Tuple6Form[A, B, C, D, E, F](f[A], f[B], f[C], f[D], f[E], f[F])

	implicit def tuple7Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G)] =
		new Tuple7Form[A, B, C, D, E, F, G](f[A], f[B], f[C], f[D], f[E], f[F], f[G])

	implicit def tuple8Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H)] =
		new Tuple8Form[A, B, C, D, E, F, G, H](f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H])

	implicit def tuple9Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I)] =
		new Tuple9Form[A, B, C, D, E, F, G, H, I](f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I])

	implicit def tuple10Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm, J :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I, J)] =
		new Tuple10Form[A, B, C, D, E, F, G, H, I, J](f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J])

	implicit def tuple11Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm, J :SQLForm, K :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I, J, K)] =
		new Tuple11Form[A, B, C, D, E, F, G, H, I, J, K](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K]
		)

	implicit def tuple12Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm, J :SQLForm, K :SQLForm, L :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L)] =
		new Tuple12Form[A, B, C, D, E, F, G, H, I, J, K, L](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L]
		)

	implicit def tuple13Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm, J :SQLForm, K :SQLForm, L :SQLForm,
				M :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
		new Tuple13Form[A, B, C, D, E, F, G, H, I, J, K, L, M](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M]
		)

	implicit def tuple14Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm, J :SQLForm, K :SQLForm, L :SQLForm,
				M :SQLForm, N :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
		new Tuple14Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N]
		)

	implicit def tuple15Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm, J :SQLForm, K :SQLForm, L :SQLForm,
				M :SQLForm, N :SQLForm, O :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
		new Tuple15Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O]
		)

	implicit def tuple16Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm, J :SQLForm, K :SQLForm, L :SQLForm,
				M :SQLForm, N :SQLForm, O :SQLForm, P :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
		new Tuple16Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P]
		)

	implicit def tuple17Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm, J :SQLForm, K :SQLForm, L :SQLForm,
				M :SQLForm, N :SQLForm, O :SQLForm, P :SQLForm, Q :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
		new Tuple17Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q]
		)

	implicit def tuple18Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm, J :SQLForm, K :SQLForm, L :SQLForm,
				M :SQLForm, N :SQLForm, O :SQLForm, P :SQLForm, Q :SQLForm, R :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
		new Tuple18Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R]
		)

	implicit def tuple19Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm, J :SQLForm, K :SQLForm, L :SQLForm,
				M :SQLForm, N :SQLForm, O :SQLForm, P :SQLForm, Q :SQLForm, R :SQLForm,
				S :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
		new Tuple19Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R],
			f[S]
		)

	implicit def tuple20Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm, J :SQLForm, K :SQLForm, L :SQLForm,
				M :SQLForm, N :SQLForm, O :SQLForm, P :SQLForm, Q :SQLForm, R :SQLForm,
				S :SQLForm, T :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
		new Tuple20Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R],
			f[S], f[T]
		)

	implicit def tuple21Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm, J :SQLForm, K :SQLForm, L :SQLForm,
				M :SQLForm, N :SQLForm, O :SQLForm, P :SQLForm, Q :SQLForm, R :SQLForm,
				S :SQLForm, T :SQLForm, U :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
		new Tuple21Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R],
			f[S], f[T], f[U]
		)

	implicit def tuple22Form[
				A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm,
				G :SQLForm, H :SQLForm, I :SQLForm, J :SQLForm, K :SQLForm, L :SQLForm,
				M :SQLForm, N :SQLForm, O :SQLForm, P :SQLForm, Q :SQLForm, R :SQLForm,
				S :SQLForm, T :SQLForm, U :SQLForm, V :SQLForm
			] :SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
		new Tuple22Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](
			f[A], f[B], f[C], f[D], f[E], f[F], f[G], f[H], f[I], f[J], f[K], f[L], f[M], f[N], f[O], f[P], f[Q], f[R],
			f[S], f[T], f[U], f[V]
		)









	private[schema] class OptionForm[T](implicit protected val form :SQLForm[T])
		extends OptionWriteForm[T] with OptionReadForm[T] with SQLForm[Option[T]]
	{
		override def toString = "Option[" + form + "]"
	}



	private[schema] case class Tuple2Form[L, R](_1 :SQLForm[L], _2 :SQLForm[R])
		extends AbstractTuple2ReadForm[L, R] with AbstractTuple2WriteForm[L, R] with SQLForm[(L, R)]
	{
		override def toString = s"(${_1},${_2})"
	}

	private[schema] case class Tuple3Form[A, B, C](_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C])
		extends AbstractTuple3ReadForm[A, B, C] with AbstractTuple3WriteForm[A, B, C] with SQLForm[(A, B, C)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple4Form[A, B, C, D](_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D])
		extends AbstractTuple4ReadForm[A, B, C, D] with AbstractTuple4WriteForm[A, B, C, D] with SQLForm[(A, B, C, D)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple5Form[A, B, C, D, E](
            _1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E]
        ) extends AbstractTuple5ReadForm[A, B, C, D, E] with AbstractTuple5WriteForm[A, B, C, D, E]
		     with SQLForm[(A, B, C, D, E)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple6Form[A, B, C, D, E, F](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F]
		) extends AbstractTuple6ReadForm[A, B, C, D, E, F] with AbstractTuple6WriteForm[A, B, C, D, E, F] 
		     with SQLForm[(A, B, C, D, E, F)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple7Form[A, B, C, D, E, F, G](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G]
		) extends AbstractTuple7ReadForm[A, B, C, D, E, F, G] with AbstractTuple7WriteForm[A, B, C, D, E, F, G] 
		     with SQLForm[(A, B, C, D, E, F, G)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple8Form[A, B, C, D, E, F, G, H](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H]
		) extends AbstractTuple8ReadForm[A, B, C, D, E, F, G, H] with AbstractTuple8WriteForm[A, B, C, D, E, F, G, H] 
		     with SQLForm[(A, B, C, D, E, F, G, H)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple9Form[A, B, C, D, E, F, G, H, I](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I]
		) extends AbstractTuple9ReadForm[A, B, C, D, E, F, G, H, I] with AbstractTuple9WriteForm[A, B, C, D, E, F, G, H, I] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple10Form[A, B, C, D, E, F, G, H, I, J](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I], _10 :SQLForm[J]
		) extends AbstractTuple10ReadForm[A, B, C, D, E, F, G, H, I, J] 
		     with AbstractTuple10WriteForm[A, B, C, D, E, F, G, H, I, J] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I, J)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple11Form[A, B, C, D, E, F, G, H, I, J, K](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I], _10 :SQLForm[J], _11 :SQLForm[K]
		) extends AbstractTuple11ReadForm[A, B, C, D, E, F, G, H, I, J, K] 
		     with AbstractTuple11WriteForm[A, B, C, D, E, F, G, H, I, J, K] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I, J, K)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple12Form[A, B, C, D, E, F, G, H, I, J, K, L](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I], _10 :SQLForm[J], _11 :SQLForm[K], _12 :SQLForm[L]
		) extends AbstractTuple12ReadForm[A, B, C, D, E, F, G, H, I, J, K, L] 
		     with AbstractTuple12WriteForm[A, B, C, D, E, F, G, H, I, J, K, L] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple13Form[A, B, C, D, E, F, G, H, I, J, K, L, M](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I], _10 :SQLForm[J], _11 :SQLForm[K], _12 :SQLForm[L],
			_13 :SQLForm[M]
		) extends AbstractTuple13ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M] 
		     with AbstractTuple13WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple14Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I], _10 :SQLForm[J], _11 :SQLForm[K], _12 :SQLForm[L],
			_13 :SQLForm[M], _14 :SQLForm[N]
		) extends AbstractTuple14ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N] 
		     with AbstractTuple14WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple15Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I], _10 :SQLForm[J], _11 :SQLForm[K], _12 :SQLForm[L],
			_13 :SQLForm[M], _14 :SQLForm[N], _15 :SQLForm[O]
		) extends AbstractTuple15ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] 
		     with AbstractTuple15WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple16Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I], _10 :SQLForm[J], _11 :SQLForm[K], _12 :SQLForm[L],
			_13 :SQLForm[M], _14 :SQLForm[N], _15 :SQLForm[O], _16 :SQLForm[P]
		) extends AbstractTuple16ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] 
		     with AbstractTuple16WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple17Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I], _10 :SQLForm[J], _11 :SQLForm[K], _12 :SQLForm[L],
			_13 :SQLForm[M], _14 :SQLForm[N], _15 :SQLForm[O], _16 :SQLForm[P], _17 :SQLForm[Q]
		) extends AbstractTuple17ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] 
		     with AbstractTuple17WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple18Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I], _10 :SQLForm[J], _11 :SQLForm[K], _12 :SQLForm[L],
			_13 :SQLForm[M], _14 :SQLForm[N], _15 :SQLForm[O], _16 :SQLForm[P], _17 :SQLForm[Q], _18 :SQLForm[R]
		) extends AbstractTuple18ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] 
		     with AbstractTuple18WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple19Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I], _10 :SQLForm[J], _11 :SQLForm[K], _12 :SQLForm[L],
			_13 :SQLForm[M], _14 :SQLForm[N], _15 :SQLForm[O], _16 :SQLForm[P], _17 :SQLForm[Q], _18 :SQLForm[R],
			_19 :SQLForm[S]	                                                                               
		) extends AbstractTuple19ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] 
		     with AbstractTuple19WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple20Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I], _10 :SQLForm[J], _11 :SQLForm[K], _12 :SQLForm[L],
			_13 :SQLForm[M], _14 :SQLForm[N], _15 :SQLForm[O], _16 :SQLForm[P], _17 :SQLForm[Q], _18 :SQLForm[R],
			_19 :SQLForm[S], _20 :SQLForm[T]	                                                                               
		) extends AbstractTuple20ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] 
		     with AbstractTuple20WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple21Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I], _10 :SQLForm[J], _11 :SQLForm[K], _12 :SQLForm[L],
			_13 :SQLForm[M], _14 :SQLForm[N], _15 :SQLForm[O], _16 :SQLForm[P], _17 :SQLForm[Q], _18 :SQLForm[R],
			_19 :SQLForm[S], _20 :SQLForm[T], _21 :SQLForm[U]	                                                                               
		) extends AbstractTuple21ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] 
		     with AbstractTuple21WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)]
	{
		override def productPrefix = ""
	}

	private[schema] case class Tuple22Form[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](
			_1 :SQLForm[A], _2 :SQLForm[B], _3 :SQLForm[C], _4 :SQLForm[D], _5 :SQLForm[E], _6 :SQLForm[F], 
			_7 :SQLForm[G], _8 :SQLForm[H], _9 :SQLForm[I], _10 :SQLForm[J], _11 :SQLForm[K], _12 :SQLForm[L],
			_13 :SQLForm[M], _14 :SQLForm[N], _15 :SQLForm[O], _16 :SQLForm[P], _17 :SQLForm[Q], _18 :SQLForm[R],
			_19 :SQLForm[S], _20 :SQLForm[T], _21 :SQLForm[U], _22 :SQLForm[V]	                                                                               
		) extends AbstractTuple22ReadForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] 
		     with AbstractTuple22WriteForm[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] 
		     with SQLForm[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)]
	{
		override def productPrefix = ""
	}


	
}
