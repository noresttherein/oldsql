package com.hcore.ogre.sql

import java.sql.ResultSet

import com.hcore.ogre.mapping.PositionedResultView
import com.hcore.ogre.morsels.Names
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.sql.SQLForm.NullValue
import com.hcore.ogre.sql.SQLReadForm.{Tuple2ReadForm, MappedSQLReadForm}
import shapeless.{::, HList}

import scala.slick.jdbc.{PositionedParameters, PositionedResult}


//implicits
import extensions._

trait SQLReadForm[+T] {
	def apply(position :Int)(res :ResultSet) :T = opt(position)(res) getOrElse nullValue
	def apply(res :PositionedResult) :T = opt(res) getOrElse nullValue

	def opt(position :Int)(res :ResultSet) :Option[T]
	def opt(res :PositionedResult) :Option[T]

	def nullValue :T
//	def columnCount :Int
	def readColumns :Int


	def nullMap[X :NullValue](fun :T=>X) :SQLReadForm[X] = map(fun, NullValue.Null[X])

	def map[X](fun :T=>X, nullValue :X) :SQLReadForm[X] = MappedSQLReadForm((t:T)=>Some(fun(t)), nullValue)(this)

	def map[X](fun :T=>X) :SQLReadForm[X] = MappedSQLReadForm((t :T)=>Some(fun(t)), fun(nullValue))(this)



	def flatMap[X :NullValue](fun :T=>Option[X]) :SQLReadForm[X] = flatMap(fun, NullValue.Null[X])

	def flatMap[X](fun :T=>Option[X], nullValue :X) :SQLReadForm[X] = MappedSQLReadForm(fun, nullValue)(this)

	def asOpt :SQLReadForm[Option[T]] = SQLReadForm.OptionReadType(this)




	def *[O](other :SQLReadForm[O]) :SQLReadForm[(T, O)] = new Tuple2ReadForm()(this, other)

	def &&[O>:T](write :SQLWriteForm[O]) :SQLForm[O] = SQLForm.combine[O](this, write)

	def compatible(other :SQLReadForm[_]) :Boolean = this == other

	override def toString = Names.unqualifiedClassName(this)
}







trait AtomicReadForm[+T] extends SQLReadForm[T] with BaseAtomicForm {
	//	def sqlType :Int
	def readColumns = 1

	def apply(column :String)(res :ResultSet) :T

	def opt(position :Int)(res :ResultSet) :Option[T] = Option(apply(position)(res)).filterNot(_ => res.wasNull)
	def opt(column :String)(res :ResultSet) :Option[T] = Option(apply(column)(res)).filterNot(_ => res.wasNull)
	def opt(res :PositionedResult) :Option[T] = Option(apply(res)).filterNot(_ => res.wasNull)

	//	override def columnCount = 1

	override def nullMap[X :NullValue](fun :T=>X) :AtomicReadForm[X] =
		map(fun, NullValue.Null[X])

	override def map[X](fun :T=>X) :AtomicReadForm[X] =
		MappedSQLReadForm.atom((t :T) => Some(fun(t)), fun(this.nullValue))(this)

	override def map[X](fun :T=>X, nullValue :X) :AtomicReadForm[X] =
		MappedSQLReadForm.atom((t:T)=>Some(fun(t)), nullValue)(this)

	override def flatMap[X :NullValue](fun :T=>Option[X]) :AtomicReadForm[X] =
		flatMap(fun, NullValue.Null[X])

	override def flatMap[X](fun :T=>Option[X], nullValue :X) :AtomicReadForm[X] =
		MappedSQLReadForm.atom(fun, nullValue)(this)

	override def compatible(other: SQLReadForm[_]): Boolean = other match {
		case a:AtomicReadForm[_] => a.sqlType==sqlType
		case _ => false
	}

}





object SQLReadForm {
	def apply[T :SQLReadForm] :SQLReadForm[T] = implicitly[SQLReadForm[T]]

	def atom[T :AtomicReadForm] :AtomicReadForm[T] = implicitly[AtomicReadForm[T]]

	def const[T :NullValue](value :Option[T]) :SQLReadForm[T] = ConstReadForm(value)

	def const[T](value :Option[T], nullValue :T) :SQLReadForm[T] = ConstReadForm(value)(NullValue(nullValue))

	def const[T](value :T) :SQLReadForm[T] = ConstReadForm(Some(value))(NullValue(value))

	def seq[T](forms :Seq[SQLReadForm[T]]) :SQLReadForm[Seq[T]] = new SeqReadFormImpl[T](forms)


	implicit def fromImplicitForm[T :SQLForm] :SQLReadForm[T] = SQLForm[T]

	implicit def OptionReadType[T :SQLReadForm] :SQLReadForm[Option[T]] =
		SQLReadForm[T].map(Option(_), None)

	implicit def SomeType[T :SQLReadForm] :SQLReadForm[Some[T]] =
		SQLReadForm[T].map(Some(_))



	case class ConstReadForm[+T :NullValue](value :Option[T]) extends SQLReadForm[T] {
		override def opt(position: Int)(res: ResultSet): Option[T] = value

		override def opt(res: PositionedResult): Option[T] = value

		override def readColumns: Int = 0

		override def nullValue: T = NullValue.Null[T]
	}

	trait CompositeReadForm[+T] extends SQLReadForm[T] {
		protected def forms :Seq[SQLReadForm[_]]

		override def readColumns: Int = (0 /: forms)(_ + _.readColumns)
	}


	trait SeqReadForm[+T] extends SQLReadForm[Seq[T]] with CompositeReadForm[Seq[T]] {
		protected def forms :Seq[SQLReadForm[T]]

		override def opt(position: Int)(res: ResultSet): Option[Seq[T]] =
			opt(PositionedResultView(res, position))

		override def opt(res: PositionedResult): Option[Seq[T]] =
			forms.map(_.opt(res)).collect { case Some(x) => x }.providing(_.size==forms.size)


		override def apply(position: Int)(res: ResultSet): Seq[T] =
			apply(PositionedResultView(res, position))

		override def apply(res: PositionedResult): Seq[T] =
			forms.map(_(res))


		override def nullValue: Seq[T] = forms.map(_.nullValue)

		override def toString = forms.mkString("^Seq(",",",")")
	}

	private case class SeqReadFormImpl[+T](forms :Seq[SQLReadForm[T]]) extends SeqReadForm[T] {
		override val readColumns = super.readColumns
		override def toString = super.toString
	}


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


	trait AbstractTuple2ReadForm[L, R] extends SQLReadForm[(L, R)] {
		val _1  :SQLReadForm[L]
		val _2  :SQLReadForm[R]

		override def opt(position: Int)(res: ResultSet): Option[(L, R)] = {
			val l = _1.opt(position)(res)
			val r = _2.opt(position + _1.readColumns)(res)
			for (v1<-l; v2<-r) yield (v1, v2)
		}

		override def opt(res: PositionedResult): Option[(L, R)] = {
			val l = _1.opt(res)
			val r = _2.opt(res)
			for (v1<-l; v2<-r) yield (v1, v2)
		}

		override def nullValue: (L, R) = (_1.nullValue, _2.nullValue)

		override def readColumns: Int = _1.readColumns + _2.readColumns

		override def toString = s"^(${_1},${_2})"
	}

	class Tuple2ReadForm[L, R](implicit val _1  :SQLReadForm[L], val _2 :SQLReadForm[R]) extends AbstractTuple2ReadForm[L, R]



	class MappedSQLReadForm[+T, S](val map :S=>Option[T], nullExpr : =>T)(implicit val source :SQLReadForm[S])
		extends SQLReadForm[T]
	{
		override def nullValue = nullExpr
		override def opt(position: Int)(res: ResultSet): Option[T] =
			source.opt(position)(res).flatMap(map)

		override def opt(res: PositionedResult): Option[T] =
			source.opt(res).flatMap(map)

		override def readColumns: Int = source.readColumns

		override def toString = s"<=$source"
	}



	object MappedSQLReadForm {
		def apply[T :NullValue, S :SQLReadForm](map :S=>Option[T]) :MappedSQLReadForm[T, S] =
			apply(map, NullValue.Null[T])

		def apply[T, S :SQLReadForm](map :S=>Option[T], nullValue : =>T) :MappedSQLReadForm[T, S] =
			implicitly[SQLReadForm[S]] match {
				case a :AtomicReadForm[_] =>
					atom(map, nullValue)(a.asInstanceOf[AtomicReadForm[S]])
				case _ =>
					new MappedSQLReadForm[T, S](map, nullValue)
			}


		def atom[T :NullValue, S :AtomicReadForm](map :S=>Option[T]) :MappedSQLReadForm[T, S] with AtomicReadForm[T] =
			atom(map, NullValue.Null[T])

		def atom[T, S :AtomicReadForm](map :S=>Option[T], nullValue : =>T) :MappedSQLReadForm[T, S] with AtomicReadForm[T] =
			new MappedSQLReadForm[T, S](map, nullValue) with AtomicReadForm[T] {
				override val source = implicitly[AtomicReadForm[S]]
				override def sqlType: Int = source.sqlType
				override def readColumns :Int = 1

				override def apply(column: String)(res: ResultSet): T =
					source.opt(column)(res).flatMap(map) getOrElse nullValue

				override def opt(position: Int)(res: ResultSet): Option[T] =
					super[MappedSQLReadForm].opt(position)(res)

				override def opt(res: PositionedResult): Option[T] =
					super[MappedSQLReadForm].opt(res)
			}
	}

}

