package net.noresttherein.oldsql.schema

import java.sql.ResultSet

import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.{FallbackColumnReadForm, FallbackReadForm, MappedSQLReadForm, Tuple2ReadForm}
import net.noresttherein.oldsql.slang._






trait SQLReadForm[+T] {
	def apply(position :Int)(res :ResultSet) :T = opt(position)(res) getOrElse nullValue

	def opt(position :Int)(res :ResultSet) :Option[T]

	def nullValue :T

	def readColumns :Int


	def nullMap[X :NullValue](fun :T => X) :SQLReadForm[X] = map(fun, NullValue.Null[X])

	def map[X](fun :T => X, nullValue :X) :SQLReadForm[X] = MappedSQLReadForm((t :T) => Some(fun(t)), nullValue)(this)

	def map[X](fun :T => X) :SQLReadForm[X] = MappedSQLReadForm((t :T) => Some(fun(t)), fun(nullValue))(this)



	def flatMap[X :NullValue](fun :T => Option[X]) :SQLReadForm[X] = flatMap(fun, NullValue.Null[X])

	def flatMap[X](fun :T => Option[X], nullValue :X) :SQLReadForm[X] = MappedSQLReadForm(fun, nullValue)(this)

	def asOpt :SQLReadForm[Option[T]] = SQLReadForm.OptionReadType(this)



	def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] =
		if (readColumns != fallback.readColumns && fallback.readColumns != 0)
			throw new IllegalArgumentException(
				s"$this orElse $fallback: different number of read columns ($readColumns vs ${fallback.readColumns})."
			)
		else
			new FallbackReadForm(this, fallback)



	def *[O](other :SQLReadForm[O]) :SQLReadForm[(T, O)] = new Tuple2ReadForm()(this, other)

	def &&[O>:T](write :SQLWriteForm[O]) :SQLForm[O] = SQLForm.combine[O](this, write)

	def compatible(other :SQLReadForm[_]) :Boolean = this == other



	override def toString :String = this.innerClassName
}






trait ColumnReadForm[+T] extends SQLReadForm[T] with BaseColumnForm {
	override def readColumns = 1

	def apply(column :String)(res :ResultSet) :T = apply(res.findColumn(column))(res)

	override def opt(position :Int)(res :ResultSet) :Option[T] = Option(apply(position)(res)).filterNot(_ => res.wasNull)

	def opt(column :String)(res :ResultSet) :Option[T] = Option(apply(column)(res)).filterNot(_ => res.wasNull)


	override def nullMap[X :NullValue](fun :T => X) :ColumnReadForm[X] =
		map(fun, NullValue.Null[X])

	override def map[X](fun :T => X) :ColumnReadForm[X] =
		MappedSQLReadForm.column((t :T) => Some(fun(t)), fun(this.nullValue))(this)

	override def map[X](fun :T => X, nullValue :X) :ColumnReadForm[X] =
		MappedSQLReadForm.column((t :T) => Some(fun(t)), nullValue)(this)

	override def flatMap[X :NullValue](fun :T => Option[X]) :ColumnReadForm[X] =
		flatMap(fun, NullValue.Null[X])

	override def flatMap[X](fun :T => Option[X], nullValue :X) :ColumnReadForm[X] =
		MappedSQLReadForm.column(fun, nullValue)(this)



	override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] = fallback match {
		case atom :ColumnReadForm[S] => this orElse atom
		case _ => super.orElse(fallback)
	}

	def orElse[S >: T](fallback :ColumnReadForm[S]) :ColumnReadForm[S] =
		if (fallback.sqlType != sqlType)
			throw new IllegalArgumentException(s"$this orElse $fallback: different sqlType ($sqlType vs ${fallback.sqlType}).")
		else
			new FallbackColumnReadForm[S](this, fallback)



	override def compatible(other: SQLReadForm[_]): Boolean = other match {
		case a :ColumnReadForm[_] => a.sqlType == sqlType
		case _ => false
	}

}





object SQLReadForm {
	def apply[T :SQLReadForm] :SQLReadForm[T] = implicitly[SQLReadForm[T]]

	def column[T :ColumnReadForm] :ColumnReadForm[T] = implicitly[ColumnReadForm[T]]



	def const[T :NullValue](value :Option[T]) :SQLReadForm[T] = new ConstReadForm(value)

	def const[T](value :Option[T], nullValue :T) :SQLReadForm[T] = new ConstReadForm(value)(NullValue(nullValue))

	def const[T](value :T) :SQLReadForm[T] = new ConstReadForm(Some(value))(NullValue(value))

	def eval[T :NullValue](value : =>Option[T]) :SQLReadForm[T] = new EvalReadForm(value)

	def eval[T](value: =>Option[T], nullValue :T) :SQLReadForm[T] = new EvalReadForm(value)(NullValue(nullValue))

	def eval[T](value: =>T) :SQLReadForm[T] = new EvalReadForm(Some(value))(NullValue(value))



	def seq[T](forms :Seq[SQLReadForm[T]]) :SQLReadForm[Seq[T]] = new SeqReadFormImpl[T](forms)



	implicit def fromImplicitForm[T :SQLForm] :SQLReadForm[T] = SQLForm[T]

	implicit def OptionReadType[T :SQLReadForm] :SQLReadForm[Option[T]] =
		SQLReadForm[T].map(Option(_), None)

	implicit def SomeType[T :SQLReadForm] :SQLReadForm[Some[T]] =
		SQLReadForm[T].map(Some(_))



	case class ConstReadForm[+T :NullValue](value :Option[T]) extends SQLReadForm[T] {
		override def opt(position: Int)(res: ResultSet): Option[T] = value

		override def readColumns: Int = 0

		override def nullValue: T = NullValue.Null[T]

		override def toString :String = "<" + value
	}



	private class EvalReadForm[+T :NullValue](value: =>Option[T]) extends SQLReadForm[T] {
		override def opt(position: Int)(res: ResultSet): Option[T] = value

		override def readColumns: Int = 0

		override def nullValue: T = NullValue.Null[T]

		override def toString :String = "<=?"
	}



	class MappedSQLReadForm[+T, S](val map :S=>Option[T], nullExpr : =>T)(implicit val source :SQLReadForm[S])
		extends SQLReadForm[T]
	{
		override def nullValue :T = nullExpr

		override def opt(position: Int)(res: ResultSet): Option[T] =
			source.opt(position)(res).flatMap(map)

		override def readColumns: Int = source.readColumns

		override def toString = s"<=$source"
	}



	object MappedSQLReadForm {
		def apply[T :NullValue, S :SQLReadForm](map :S=>Option[T]) :MappedSQLReadForm[T, S] =
			apply(map, NullValue.Null[T])

		def apply[T, S :SQLReadForm](map :S=>Option[T], nullValue : =>T) :MappedSQLReadForm[T, S] =
			implicitly[SQLReadForm[S]] match {
				case a :ColumnReadForm[_] =>
					column(map, nullValue)(a.asInstanceOf[ColumnReadForm[S]])
				case _ =>
					new MappedSQLReadForm[T, S](map, nullValue)
			}


		def column[T :NullValue, S :ColumnReadForm](map :S=>Option[T]) :MappedSQLReadForm[T, S] with ColumnReadForm[T] =
			column(map, NullValue.Null[T])

		def column[T, S :ColumnReadForm](map :S=>Option[T], nullValue : =>T) :MappedSQLReadForm[T, S] with ColumnReadForm[T] =
			new MappedSQLReadForm[T, S](map, nullValue) with ColumnReadForm[T] {
				override val source = implicitly[ColumnReadForm[S]]
				override def sqlType: Int = source.sqlType
				override def readColumns :Int = 1

				override def opt(position: Int)(res: ResultSet): Option[T] =
					super[MappedSQLReadForm].opt(position)(res)

			}
	}



	private[schema] class FallbackReadForm[T](first :SQLReadForm[T], second :SQLReadForm[T]) extends SQLReadForm[T] {
		override def opt(position :Int)(res :ResultSet) :Option[T] =
			first.opt(position)(res) orElse second.opt(position)(res)

		override def nullValue :T = first.nullValue

		override def readColumns :Int = first.readColumns

		override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] =
			if (readColumns != fallback.readColumns && fallback.readColumns != 0)
				throw new IllegalArgumentException(
					s"($this) orElse $fallback: different number of read columns ($readColumns vs ${fallback.readColumns})."
				)
			else
				new FallbackReadForm(first, second orElse fallback)
	}

	private[schema] class FallbackColumnReadForm[T](first :ColumnReadForm[T], second :ColumnReadForm[T])
		extends FallbackReadForm[T](first, second) with ColumnReadForm[T]
	{
		override val sqlType :Int = first.sqlType

		override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] = fallback match {
			case atom :ColumnReadForm[S] => orElse(atom)
			case _ => super.orElse(fallback)
		}

		override def orElse[S >: T](fallback :ColumnReadForm[S]) :ColumnReadForm[S] =
			if (sqlType != fallback.sqlType)
				throw new IllegalArgumentException(
					s"($this) orElse $fallback: different sqlType ($sqlType vs ${fallback.sqlType})."
				)
			else new FallbackColumnReadForm(first, second orElse fallback)
	}






	trait CompositeReadForm[+T] extends SQLReadForm[T] {
		protected def forms :Seq[SQLReadForm[_]]

		override def readColumns: Int = (0 /: forms)(_ + _.readColumns)
	}



	trait SeqReadForm[+T] extends SQLReadForm[Seq[T]] with CompositeReadForm[Seq[T]] {
		protected def forms :Seq[SQLReadForm[T]]

		override def opt(position: Int)(res: ResultSet): Option[Seq[T]] = {
			var i = position
			val result = List.newBuilder[T]
			var formCount = 0; var resultCount = 0
			for (form <- forms) {
				form.opt(i)(res) foreach { t => resultCount += 1; result += t  }
				i += form.readColumns
				formCount += 1
			}
			if (resultCount != formCount) None
			else Some(result.result)
		}


		override def apply(position: Int)(res: ResultSet): Seq[T] = {
			var i = position
			val result = List.newBuilder[T]
			for (form <- forms) {
				form.opt(i)(res) foreach { result += _  }
				i += form.readColumns
			}
			result.result
		}



		override def nullValue: Seq[T] = forms.map(_.nullValue)

		override def toString :String = forms.mkString("<Seq(",",",")")
	}

	private case class SeqReadFormImpl[+T](forms :Seq[SQLReadForm[T]]) extends SeqReadForm[T] {
		override val readColumns = super.readColumns
		override def toString = super.toString
	}



	trait AbstractTuple2ReadForm[L, R] extends SQLReadForm[(L, R)] {
		val _1  :SQLReadForm[L]
		val _2  :SQLReadForm[R]

		override def opt(position: Int)(res: ResultSet): Option[(L, R)] = {
			val l = _1.opt(position)(res)
			val r = _2.opt(position + _1.readColumns)(res)
			for (v1<-l; v2<-r) yield (v1, v2)
		}

		override def nullValue: (L, R) = (_1.nullValue, _2.nullValue)

		override def readColumns: Int = _1.readColumns + _2.readColumns

		override def toString = s"<(${_1},${_2})"
	}

	class Tuple2ReadForm[L, R](implicit val _1  :SQLReadForm[L], val _2 :SQLReadForm[R]) extends AbstractTuple2ReadForm[L, R]







/*
		trait AbstractHListReadForm[+T >:Null <:HList] extends SQLReadForm[T] { self =>
			def ::[H](form :SQLReadForm[H]) :HListReadForm[H, T] = new HListReadFormImpl(form, this)
		}


		trait HListReadForm[+H, +T<:HList] extends AbstractHListReadForm[H::T] {
			val head :SQLReadForm[H]
			val tail :SQLReadForm[T]

			override def opt(position: Int)(res: ResultSet): Option[H::T] =
				for (h<-head.opt(position)(res); t<-tail.opt(position+head.readColumns)(res))
					yield h::t

			override def readColumns: Int = head.readColumns + tail.readColumns

			override def opt(res: PositionedResult): Option[H::T] = {
				val (first, rest) = (head.opt(res), tail.opt(res))
				for (h<-first; t<-rest) yield h::t
			}

			override def nullValue: H::T = head.nullValue::tail.nullValue

			override def toString = s"^$head::" + tail.toString
		}

		case class HListReadFormImpl[+H, +T<:HList](head :SQLReadForm[H], tail :SQLReadForm[T]) extends HListReadForm[H, T] {

			override def readColumns: Int = head.readColumns + tail.readColumns

			override def toString = super.toString
		}
	*/

}

