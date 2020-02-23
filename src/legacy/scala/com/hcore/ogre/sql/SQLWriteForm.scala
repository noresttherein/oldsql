package com.hcore.ogre.sql

import com.hcore.ogre.morsels.Names
import com.hcore.ogre.sql.SQLWriteForm.{Tuple2WriteForm, MappedSQLWriteForm}
import shapeless.{::, HList}

import scala.slick.jdbc.PositionedParameters




//implicits
import com.hcore.ogre.slang.options.extensions._

trait SQLWriteForm[-T] {
	def opt(params :PositionedParameters, value :Option[T]) :Unit = value match {
		case Some(v) => apply(params, v)
		case _ => nullParam(params)
	}

	def apply(params :PositionedParameters, value :T) :Unit
	def nullParam(params :PositionedParameters) :Unit

	def literal(value :T) :String
	def nullLiteral :String

	def inlineLiteral(value :T) :String
	def inlineNullLiteral :String

	def literal(value :T, inline :Boolean) :String =
		if (inline) inlineLiteral(value)
		else literal(value)

	def nullLiteral(inline :Boolean) :String =
		if (inline) inlineNullLiteral
		else nullLiteral


//	def columnCount :Int
	def writtenColumns :Int

	def imap[X](fun :X=>T) :SQLWriteForm[X] = MappedSQLWriteForm((x:X)=>Some(fun(x)))(this)

	def iflatMap[X](fun :X=>Option[T]) :SQLWriteForm[X]  = MappedSQLWriteForm(fun)(this)

	def asOpt :SQLWriteForm[Option[T]] = SQLWriteForm.OptionWriteType(this)

	def *[O](other :SQLWriteForm[O]) :SQLWriteForm[(T, O)] = new Tuple2WriteForm()(this, other)



	def &&[O<:T](read :SQLReadForm[O]) :SQLForm[O] = SQLForm.combine(read, this)

	def compatible(other :SQLWriteForm[_]) :Boolean = this == other

	override def toString = Names.unqualifiedClassName(this)
}









trait AtomicWriteForm[-T] extends SQLWriteForm[T] with BaseAtomicForm {
	//	def sqlType :Int
	def writtenColumns = 1

	override def nullParam(params :PositionedParameters) :Unit = params.setNull(sqlType)

	override def literal(value: T): String = if (value==null) "null" else value.toString
	override def nullLiteral: String = "null"

	override def inlineLiteral(value: T): String = literal(value)
	override def inlineNullLiteral: String = nullLiteral

	override def imap[X](fun :X=>T) :AtomicWriteForm[X] = MappedSQLWriteForm.atom((x:X)=>Some(fun(x)))(this)

	override def iflatMap[X](fun :X=>Option[T]) :AtomicWriteForm[X]  = MappedSQLWriteForm.atom(fun)(this)

	override def compatible(other: SQLWriteForm[_]): Boolean = other match {
		case a:AtomicWriteForm[_] => a.sqlType == sqlType
		case _ => false
	}
}







object SQLWriteForm {
	def apply[T :SQLWriteForm] :SQLWriteForm[T] = implicitly[SQLWriteForm[T]]

	def atom[T :AtomicWriteForm] :AtomicWriteForm[T] = implicitly[AtomicWriteForm[T]]

	def const[T :SQLWriteForm](value :T) :SQLWriteForm[Any] = ConstWriteForm(SQLWriteForm[T], value)

	def chain[T](forms :Seq[SQLWriteForm[T]]) :SQLWriteForm[T] = forms match {
		case Seq() => empty
		case Seq(form) => form
		case _ => WriteFormChain(forms)
	}

	def seq[T](items :Seq[SQLWriteForm[T]]) :SQLWriteForm[Seq[T]] = new SeqWriteFormImpl[T](items)

	val empty :SQLWriteForm[Any] = new EmptyWriteForm[Any] {}

	def empty(columns :Int) :SQLWriteForm[Any] = new EmptyWriteForm[Any] {
		override val writtenColumns = columns
	}

	
	

	implicit def fromImplicitForm[T :SQLForm] :SQLWriteForm[T] = SQLForm[T]

	implicit def OptionWriteType[T :SQLWriteForm] :SQLWriteForm[Option[T]] =
		SQLWriteForm[T].iflatMap(identity[Option[T]] _)

	implicit def SomeType[T :SQLWriteForm] :SQLWriteForm[Some[T]] =
		SQLWriteForm[T].imap(_.get)



	
	


	trait EmptyWriteForm[-T] extends SQLWriteForm[T] {
		override def apply(params: PositionedParameters, value: T): Unit = ()

		override def nullParam(params: PositionedParameters): Unit = ()

		override def inlineLiteral(value: T): String = ""

		override def inlineNullLiteral: String = ""

		override def literal(value: T): String = ""

		override def nullLiteral: String = ""

		override def writtenColumns: Int = 0

		override def toString = "EMPTY"
	}

	
	
	case class ConstWriteForm[T](form :SQLWriteForm[T], value :T) extends SQLWriteForm[Any] {

		override def apply(params: PositionedParameters, ignored: Any): Unit =
			form(params, value)

		override def nullParam(params: PositionedParameters): Unit =
			form(params, value)

		override def literal(ignored: Any): String = form.literal(value)

		override def nullLiteral: String = form.literal(value)

		override def inlineLiteral(ignored: Any): String =
			form.inlineLiteral(value)

		override def inlineNullLiteral: String =
			form.inlineLiteral(value)


		override def writtenColumns: Int = form.writtenColumns

		override def toString = s"$form=v$value"
	}




	trait MappedSQLWriteForm[-T, S]
		extends SQLWriteForm[T]
	{
		val source :SQLWriteForm[S]
		val unmap :T=>Option[S]

		override def apply(params: PositionedParameters, value: T): Unit =
			unmap(value).mapOrElse(source.apply(params, _), source.nullParam(params))

		override def nullParam(params: PositionedParameters): Unit = source.nullParam(params)

		override def literal(value: T): String = unmap(value).mapOrElse(source.literal, source.nullLiteral)

		override def nullLiteral: String = source.nullLiteral

		override def inlineLiteral(value: T): String =
			unmap(value).mapOrElse(source.inlineLiteral, source.inlineNullLiteral)

		override def inlineNullLiteral: String = source.inlineNullLiteral

		override def writtenColumns = source.writtenColumns

		override def toString = s"$source=>"
	}



	object MappedSQLWriteForm {
		def apply[T, S :SQLWriteForm](map :T=>Option[S]) :MappedSQLWriteForm[T, S] =
			implicitly[SQLWriteForm[S]] match {
				case a :AtomicWriteForm[_] =>
					atom(map)(a.asInstanceOf[AtomicWriteForm[S]])
				case f =>
					new MappedSQLWriteForm[T, S] {
						val source = f
						val unmap = map
					}
			}

		def atom[T, S :AtomicWriteForm](map :T=>Option[S]) :MappedSQLWriteForm[T, S] with AtomicWriteForm[T] =
			new MappedSQLWriteForm[T, S] with AtomicWriteForm[T] {
				val source = implicitly[AtomicWriteForm[S]]
				val unmap = map
				override def sqlType: Int = source.sqlType
				override def writtenColumns = 1
			}
	}



	trait CompositeWriteForm[-T] extends SQLWriteForm[T] {
		protected def forms :Seq[SQLWriteForm[_]]
		def writtenColumns = (0 /: forms)(_ + _.writtenColumns)
	}
	

	trait ProxySQLWriteForm[-T] extends SQLWriteForm[T] {
		protected def form :SQLWriteForm[T]

		override def apply(params: PositionedParameters, value: T): Unit = form(params, value)

		override def nullParam(params: PositionedParameters): Unit = form.nullParam(params)

		override def literal(value: T): String = form.literal(value)

		override def inlineLiteral(value: T): String = form.inlineLiteral(value)

		override def nullLiteral: String = form.nullLiteral

		override def inlineNullLiteral: String = form.inlineNullLiteral


		override def writtenColumns: Int = form.writtenColumns

		override def toString = "~"+form
	}

	

	
	

	case class WriteFormChain[-T](forms :Seq[SQLWriteForm[T]]) extends SQLWriteForm[T] with CompositeWriteForm[T] {

		override def apply(params: PositionedParameters, value: T): Unit =
			forms.foreach(_(params, value))

		override def nullParam(params: PositionedParameters): Unit =
			forms.foreach(_.nullParam(params))

		override def literal(value: T): String =
			forms.map(_.literal(value)).mkString("(", ", ", ")")


		override def inlineLiteral(value: T): String =
			forms.map(_.inlineLiteral(value)).mkString("", ", " ,"")

		override def nullLiteral: String = forms.map(_.nullLiteral).mkString("(", ", ", ")")

		override def inlineNullLiteral: String =
			forms.map(_.inlineNullLiteral).mkString("", ", ", "")

		override val writtenColumns = super.writtenColumns

		override def toString = forms.mkString("v(","&",")")
	}


	
	
	
	
	trait SeqWriteForm[-T] extends SQLWriteForm[Seq[T]] with CompositeWriteForm[Seq[T]] {
		protected def forms :Seq[SQLWriteForm[T]]

		override def apply(params: PositionedParameters, value: Seq[T]): Unit =
			if (value.size!=forms.size)
				throw new IllegalArgumentException(s"can't set parameters $value: expected ${forms.size} values ($forms)")
			else for ((item, v) <- forms.zip(value)) item.asInstanceOf[SQLWriteForm[Any]](params, v)

		override def literal(value: Seq[T]): String =
			if (value.size!=forms.size)
				throw new IllegalArgumentException(s"can't set parameters $value: expected ${forms.size} values ($forms)")
			else (forms zip value).map { case (item, v) => item.asInstanceOf[SQLWriteForm[Any]].literal(v) }.mkString("(", ",", ")")


		override def inlineLiteral(value: Seq[T]): String =
			if (value.size!=forms.size)
				throw new IllegalArgumentException(s"can't set parameters $value: expected ${forms.size} values ($forms)")
			else (forms zip value).map { case (item, v) => item.asInstanceOf[SQLWriteForm[Any]].inlineLiteral(v) }.mkString("", ", ", "")

		override def nullParam(params: PositionedParameters): Unit =
			forms.foreach(_.nullParam(params))

		override def nullLiteral: String =
			forms.map(_ => "null").mkString("(", ", ", ")")


		override def inlineNullLiteral: String =
			forms.map(_ => "null").mkString("", ", ", "")

		override def toString = forms.mkString("vSeq(",",",")")
	}


	private case class SeqWriteFormImpl[-T](forms :Seq[SQLWriteForm[T]]) extends SeqWriteForm[T] {
		override val writtenColumns = super.writtenColumns
		override def toString = super.toString
	}
	

	trait AbstractHListWriteForm[-T >:Null <:HList] extends SQLWriteForm[T] { self =>
		protected[sql] def elementsLiteral(sb :StringBuilder, value :T) :StringBuilder

		def ::[H](form :SQLWriteForm[H]) :HListWriteForm[H, T] = new HListWriteFormImpl(form, this)
	}



	trait HListWriteForm[-H, -T>:Null <:HList] extends AbstractHListWriteForm[H::T] {
		val head :SQLWriteForm[H]
		val tail :AbstractHListWriteForm[T]

		override def apply(params: PositionedParameters, value: H::T): Unit = {
			head(params, value.head)
			tail(params, value.tail)
		}

		override def literal(value: ::[H, T]): String =
			if (value==null)
				(tail.elementsLiteral(new StringBuilder("(")++= head.nullLiteral, null) += ')').toString
			else
				(tail.elementsLiteral(new StringBuilder("(") ++= head.literal(value.head), value.tail) += ')').toString

		override def nullLiteral: String = literal(null)


		override def inlineLiteral(value: ::[H, T]): String =
			if (value==null)
				tail.elementsLiteral(new StringBuilder(head.nullLiteral), null).toString
			else
				tail.elementsLiteral(new StringBuilder(head.literal(value.head)), value.tail).toString

		override def inlineNullLiteral: String = inlineLiteral(null)

		override protected[sql] def elementsLiteral(sb: StringBuilder, value: H::T): StringBuilder =
			if (value==null)
				tail.elementsLiteral(sb ++= ", " ++= head.nullLiteral, null)
			else tail.elementsLiteral(sb ++= ", " ++= head.literal(value.head), value.tail)

		override def writtenColumns: Int = head.writtenColumns + tail.writtenColumns

		override def nullParam(params: PositionedParameters): Unit = {
			head.nullParam(params)
			tail.nullParam(params)
		}

		override def toString = s"v$head::" + tail.toString
	}


	case class HListWriteFormImpl[-H, -T >:Null <:HList](head :SQLWriteForm[H], tail :AbstractHListWriteForm[T]) extends HListWriteForm[H, T] {
		override val writtenColumns: Int = head.writtenColumns + tail.writtenColumns
		override def toString = super.toString
	}


	trait AbstractTuple2WriteForm[-L, -R] extends SQLWriteForm[(L, R)] {
		val _1 :SQLWriteForm[L]
		val _2 :SQLWriteForm[R]

		override def apply(params: PositionedParameters, value: (L, R)): Unit = {
			_1(params, value._1)
			_2(params, value._2)
		}

		override def nullParam(params: PositionedParameters): Unit = {
			_1.nullParam(params)
			_2.nullParam(params)
		}

		override def literal(value: (L, R)): String = s"(${_1.literal(value._1)}, ${_2.literal(value._2)})"

		override def nullLiteral: String = s"(${_1.nullLiteral}, ${_2.nullLiteral})"


		override def inlineLiteral(value: (L, R)): String = _1.inlineLiteral(value._1) + ", " + _2.inlineLiteral(value._2)

		override def inlineNullLiteral: String = _1.inlineNullLiteral + ", " + _2.inlineNullLiteral

		override def writtenColumns: Int = _1.writtenColumns + _2.writtenColumns

		override def toString = s"v(${_1},${_2})"
	}

	class Tuple2WriteForm[-L, -R](implicit val _1 :SQLWriteForm[L], val _2 :SQLWriteForm[R]) extends AbstractTuple2WriteForm[L, R]
	
	
}



