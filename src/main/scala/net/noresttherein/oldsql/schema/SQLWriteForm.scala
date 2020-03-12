package net.noresttherein.oldsql.schema

import java.sql.PreparedStatement

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLWriteForm.{MappedSQLWriteForm, Tuple2WriteForm}
import net.noresttherein.oldsql.slang._

import scala.collection.immutable.Seq




trait SQLWriteForm[-T] {

	def set(position :Int)(statement :PreparedStatement, value :T) :Unit

	def setNull(position :Int)(statement :PreparedStatement) :Unit



//	def literal(value :Option[T]) :String = value match {
//		case Some(x) => literal(x)
//		case _ => nullLiteral
//	}

	def literal(value :T) :String

	def nullLiteral :String


//	def inlineLiteral(value :Option[T]) :String = value match {
//		case Some(x) => inlineLiteral(x)
//		case _ => inlineNullLiteral
//	}

	def inlineLiteral(value :T) :String

	def inlineNullLiteral :String


//	def literal(value :Option[T], inline :Boolean) :String =
//		if (inline) inlineLiteral(value)
//		else literal(value)

	def literal(value :T, inline :Boolean) :String =
		if (inline) inlineLiteral(value)
		else literal(value)

	def nullLiteral(inline :Boolean) :String =
		if (inline) inlineNullLiteral
		else nullLiteral


	def writtenColumns :Int



	def unmap[X](fun :X => T) :SQLWriteForm[X] = MappedSQLWriteForm((x :X) => Some(fun(x)))(this)

	def flatUnmap[X](fun :X => Option[T]) :SQLWriteForm[X]  = MappedSQLWriteForm(fun)(this)



//	def getOrElse[S](other :)

	def asOpt :SQLWriteForm[Option[T]] = SQLWriteForm.OptionWriteForm(this)

	def *[O](other :SQLWriteForm[O]) :SQLWriteForm[(T, O)] = new Tuple2WriteForm()(this, other)



	def &&[O<:T](read :SQLReadForm[O]) :SQLForm[O] = SQLForm.combine(read, this)

	def compatible(other :SQLWriteForm[_]) :Boolean = this == other

	override def toString :String = this.innerClassName
}






trait ColumnWriteForm[-T] extends SQLWriteForm[T] with BaseColumnForm {
	override def writtenColumns = 1

	override def setNull(position :Int)(statement :PreparedStatement) :Unit =
		statement.setNull(position, sqlType)

	override def literal(value: T): String = if (value == null) "null" else value.toString
	override def nullLiteral: String = "null"

	override def inlineLiteral(value: T): String = literal(value)
	override def inlineNullLiteral: String = nullLiteral



	override def unmap[X](fun :X => T) :ColumnWriteForm[X] =
		MappedSQLWriteForm.column((x :X) => Some(fun(x)))(this)

	override def flatUnmap[X](fun :X => Option[T]) :ColumnWriteForm[X]  =
		MappedSQLWriteForm.column(fun)(this)



	override def compatible(other: SQLWriteForm[_]): Boolean = other match {
		case a :ColumnWriteForm[_] => a.sqlType == sqlType
		case _ => false
	}

}






object SQLWriteForm {
	def apply[T :SQLWriteForm] :SQLWriteForm[T] = implicitly[SQLWriteForm[T]]

	def column[T :ColumnWriteForm] :ColumnWriteForm[T] = implicitly[ColumnWriteForm[T]]



	def const[T :SQLWriteForm](value :T) :SQLWriteForm[Any] = ConstWriteForm(value, SQLWriteForm[T])

	def eval[T :SQLWriteForm](value: =>Option[T], orElse :T) :SQLWriteForm[Any] =
		new EvalWriteForm[T](value)(SQLWriteForm[T], NullValue(orElse))

	def eval[T :SQLWriteForm :NullValue](value: =>Option[T]) :SQLWriteForm[Any] =
		new EvalWriteForm[T](value)

	def eval[T :SQLWriteForm](value: =>T) :SQLWriteForm[Any] =
		new EvalWriteForm[T](Some(value))(SQLWriteForm[T], NullValue(value))



	def Lazy[T](form: => SQLWriteForm[T]) :SQLWriteForm[T] =
		new LazyWriteForm[T] {
			override protected[this] var init = () => form
		}



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




	implicit def OptionWriteForm[T :SQLWriteForm] :SQLWriteForm[Option[T]] =
		SQLWriteForm[T].flatUnmap(identity[Option[T]])

	implicit def SomeWriteForm[T :SQLWriteForm] :SQLWriteForm[Some[T]] =
		SQLWriteForm[T].unmap(_.get)

	implicit def ChainWriteForm[T <: Chain, H](implicit t :SQLWriteForm[T], h :SQLWriteForm[H]) :SQLWriteForm[T ~ H] =
		new ChainWriteForm(t, h)

	implicit val EmptyChainWriteForm :SQLWriteForm[@~] = empty






	trait EmptyWriteForm[-T] extends SQLWriteForm[T] {
		override def set(position :Int)(statement :PreparedStatement, value :T) :Unit = ()
		override def setNull(position :Int)(statement :PreparedStatement) :Unit = ()
		override def inlineLiteral(value: T): String = ""
		override def inlineNullLiteral: String = ""
		override def literal(value: T): String = ""
		override def nullLiteral: String = ""
		override def writtenColumns: Int = 0
		override def toString = "EMPTY"
	}



	case class ConstWriteForm[T](value :T, form :SQLWriteForm[T]) extends SQLWriteForm[Any] {

		override def set(position :Int)(statement :PreparedStatement, ignore :Any) :Unit =
			form.set(position)(statement, value)

		override def setNull(position :Int)(statement :PreparedStatement) :Unit =
			form.set(position)(statement, value)

		override def literal(ignored: Any): String = form.literal(value)

		override def nullLiteral: String = form.literal(value)

		override def inlineLiteral(ignored: Any): String =
			form.inlineLiteral(value)

		override def inlineNullLiteral: String =
			form.inlineLiteral(value)


		override def writtenColumns: Int = form.writtenColumns

		override def toString = s"$form=$value>"
	}



	private class EvalWriteForm[T](value: =>Option[T])(implicit form :SQLWriteForm[T], orElse :NullValue[T])
		extends SQLWriteForm[Any]
	{

		override def set(position :Int)(statement :PreparedStatement, ignore :Any) :Unit =
			setNull(position)(statement)

		@inline final override def setNull(position :Int)(statement :PreparedStatement) :Unit = value match {
			case Some(x) => form.set(position)(statement, x)
			case _ => form.set(position)(statement, orElse.value) //form.setNull(position)(statement)
		}

		override def literal(ignored: Any): String = nullLiteral

		@inline final override def nullLiteral: String = value match {
			case Some(x) => form.literal(x)
			case _ => form.literal(orElse.value) //form.nullLiteral
		}
		override def inlineLiteral(ignored: Any): String = inlineNullLiteral

		@inline final override def inlineNullLiteral: String = value match {
			case Some(x) => form.inlineLiteral(x)
			case _ => form.inlineNullLiteral
		}


		override def writtenColumns: Int = form.writtenColumns

		override def toString = s"$form=?>"
	}



	trait MappedSQLWriteForm[-T, S] extends SQLWriteForm[T] {
		val source :SQLWriteForm[S]
		val unmap :T => Option[S]

		override def set(position :Int)(statement :PreparedStatement, value :T) :Unit = unmap(value) match {
			case Some(s) => source.set(position)(statement, s)
			case _ => source.setNull(position)(statement)
		}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = source.setNull(position)(statement)

		override def literal(value: T): String = unmap(value).mapOrElse(source.literal, source.nullLiteral)

		override def nullLiteral: String = source.nullLiteral

		override def inlineLiteral(value: T): String =
			unmap(value).mapOrElse(source.inlineLiteral, source.inlineNullLiteral)

		override def inlineNullLiteral: String = source.inlineNullLiteral

		override def writtenColumns :Int = source.writtenColumns

		override def toString = s"$source=>"
	}



	object MappedSQLWriteForm {
		def apply[T, S :SQLWriteForm](map :T=>Option[S]) :MappedSQLWriteForm[T, S] =
			implicitly[SQLWriteForm[S]] match {
				case a :ColumnWriteForm[_] =>
					column(map)(a.asInstanceOf[ColumnWriteForm[S]])
				case f =>
					new MappedSQLWriteForm[T, S] {
						val source = f
						val unmap = map
					}
			}

		def column[T, S :ColumnWriteForm](map :T=>Option[S]) :MappedSQLWriteForm[T, S] with ColumnWriteForm[T] =
			new MappedSQLWriteForm[T, S] with ColumnWriteForm[T] {
				val source = implicitly[ColumnWriteForm[S]]
				val unmap = map
				override def sqlType: Int = source.sqlType
				override def writtenColumns = 1
			}
	}



	trait CompositeWriteForm[-T] extends SQLWriteForm[T] {
		protected def forms :Seq[SQLWriteForm[_]]

		def writtenColumns :Int = (0 /: forms)(_ + _.writtenColumns)

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			var i = position
			forms foreach { form => form.setNull(i)(statement); i += form.writtenColumns }
		}
	}






	trait ProxyWriteForm[-T] extends SQLWriteForm[T] {
		protected def form :SQLWriteForm[T]

		override def set(position :Int)(statement :PreparedStatement, value :T) :Unit =
			form.set(position)(statement, value)

		override def setNull(position :Int)(statement :PreparedStatement) :Unit =
			form.setNull(position)(statement)

		override def literal(value: T): String = form.literal(value)
		override def inlineLiteral(value: T): String = form.inlineLiteral(value)
		override def nullLiteral: String = form.nullLiteral
		override def inlineNullLiteral: String = form.inlineNullLiteral



		override def writtenColumns: Int = form.writtenColumns

		override def toString :String = "~"+form
	}



	private[schema] trait LazyWriteForm[-T] extends ProxyWriteForm[T] {
		protected[this] var init: () => SQLWriteForm[T]
		@volatile
		protected[this] var initialized :SQLWriteForm[T] = _
		protected[this] var fastAccess :SQLWriteForm[T] = _

		override def form :SQLWriteForm[T] = {
			if (fastAccess == null) {
				val f = initialized
				val cons = init
				if (f != null)
					fastAccess = f
				else if (cons == null)
					fastAccess = initialized
				else {
					fastAccess = cons()
					initialized = fastAccess
					init = null
				}
			}
			fastAccess
		}

		override def unmap[X](fun :X => T) :SQLWriteForm[X] =
			if (fastAccess == null && initialized == null) Lazy(form.unmap(fun))
			else form.unmap(fun)

		override def flatUnmap[X](fun :X => Option[T]) :SQLWriteForm[X] =
			if (fastAccess == null && initialized == null) Lazy(form.flatUnmap(fun))
			else form.flatUnmap(fun)

		override def asOpt :SQLWriteForm[Option[T]] =
			if (fastAccess == null && initialized == null) Lazy(form.asOpt)
			else form.asOpt

		override def *[O](other :SQLWriteForm[O]) :SQLWriteForm[(T, O)] =
			if (fastAccess == null && initialized == null) Lazy(form * other)
			else form * other

		override def &&[O <: T](read :SQLReadForm[O]) :SQLForm[O] =
			if (fastAccess == null && initialized == null) super.&&(read)
			else form && read

		override def toString :String =
			if (fastAccess == null && initialized == null) "Lazy>"
			else form.toString

	}





	private case class WriteFormChain[-T](forms :Seq[SQLWriteForm[T]]) extends SQLWriteForm[T] with CompositeWriteForm[T] {

		override def set(position :Int)(statement :PreparedStatement, value :T) :Unit = {
			var i = position
			forms foreach { form => form.set(i)(statement, value); i += form.writtenColumns }
		}

		override def literal(value: T): String =
			forms.map(_.literal(value)).mkString("(", ", ", ")")

		override def inlineLiteral(value: T): String =
			forms.map(_.inlineLiteral(value)).mkString("", ", " ,"")

		override def nullLiteral: String = forms.map(_.nullLiteral).mkString("(", ", ", ")")

		override def inlineNullLiteral: String =
			forms.map(_.inlineNullLiteral).mkString("", ", ", "")

		override val writtenColumns :Int = super.writtenColumns

		override def toString :String = forms.mkString("(", "&", ")>")
	}






	private[schema] trait SeqWriteForm[-T] extends SQLWriteForm[Seq[T]] with CompositeWriteForm[Seq[T]] {
		protected def forms :Seq[SQLWriteForm[T]]

		override def set(position :Int)(statement :PreparedStatement, value :Seq[T]) :Unit = {
			val iter = value.iterator
			var i = position
			forms foreach { form => form.set(i)(statement, iter.next()); i += form.writtenColumns }
		}

		override def literal(value: Seq[T]): String =
			if (value.size!=forms.size)
				throw new IllegalArgumentException(s"can't set parameters $value: expected ${forms.size} values ($forms)")
			else
				(forms zip value).map {
					case (item, v) => item.asInstanceOf[SQLWriteForm[Any]].literal(v)
				}.mkString("(", ",", ")")


		override def inlineLiteral(value: Seq[T]): String =
			if (value.size!=forms.size)
				throw new IllegalArgumentException(s"can't set parameters $value: expected ${forms.size} values ($forms)")
			else
				(forms zip value).map {
					case (item, v) => item.asInstanceOf[SQLWriteForm[Any]].inlineLiteral(v)
				}.mkString("", ", ", "")


		override def nullLiteral: String =
			forms.map(_ => "null").mkString("(", ", ", ")")


		override def inlineNullLiteral: String =
			forms.map(_ => "null").mkString("", ", ", "")

		override def toString :String = forms.mkString("Seq(",",",")>")
	}

	private case class SeqWriteFormImpl[-T](forms :Seq[SQLWriteForm[T]]) extends SeqWriteForm[T] {
		override val writtenColumns :Int = super.writtenColumns
		override def toString :String = super.toString
	}





	private[schema] trait AbstractTuple2WriteForm[-L, -R] extends SQLWriteForm[(L, R)] {
		val _1 :SQLWriteForm[L]
		val _2 :SQLWriteForm[R]

		override def set(position :Int)(statement :PreparedStatement, value :(L, R)) :Unit = {
			_1.set(position)(statement, value._1)
			_2.set(position + _1.writtenColumns)(statement, value._2)
		}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			_1.setNull(position)(statement)
			_2.setNull(position + _1.writtenColumns)(statement)
		}

		override def literal(value: (L, R)): String = s"(${_1.literal(value._1)}, ${_2.literal(value._2)})"

		override def nullLiteral: String = s"(${_1.nullLiteral}, ${_2.nullLiteral})"


		override def inlineLiteral(value: (L, R)): String = _1.inlineLiteral(value._1) + ", " + _2.inlineLiteral(value._2)

		override def inlineNullLiteral: String = _1.inlineNullLiteral + ", " + _2.inlineNullLiteral

		override def writtenColumns: Int = _1.writtenColumns + _2.writtenColumns

		override def toString = s"v(${_1},${_2})"
	}



	private[schema] class Tuple2WriteForm[-L, -R](implicit val _1 :SQLWriteForm[L], val _2 :SQLWriteForm[R]) extends AbstractTuple2WriteForm[L, R]



	private[schema] case class ChainWriteForm[-T <: Chain, -H](tail :SQLWriteForm[T], head :SQLWriteForm[H])
		extends SQLWriteForm[T ~ H]
	{
		override val writtenColumns :Int = tail.writtenColumns + head.writtenColumns


		override def set(position :Int)(statement :PreparedStatement, value :T ~ H) :Unit =
			if (value == null)
				setNull(position)(statement)
			else {
				tail.set(position)(statement, value.tail)
				head.set(position + tail.writtenColumns)(statement, value.head)
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			tail.setNull(position)(statement)
			head.setNull(position + tail.writtenColumns)(statement)
		}


		override def literal(value :T ~ H, inline :Boolean) :String = {
			def rec(chain :Chain, form :SQLWriteForm[_], res :StringBuilder = new StringBuilder) :StringBuilder =
				(chain, form) match {
					case (t ~ h, f :ChainWriteForm[_, _]) =>
						rec(t, f.tail, res) ++= ", "
						if (f.head.isInstanceOf[ChainWriteForm[_, _]])
							res ++= "(" ++= f.head.asInstanceOf[SQLWriteForm[Any]].literal(h) ++= ")"
						else
							res ++= f.head.asInstanceOf[SQLWriteForm[Any]].literal(h)
					case (null, f :ChainWriteForm[_, _]) =>
						rec(null, f.tail, res) ++= ", "
					case _ =>
						res
				}
			if (inline)
				rec(value, this).toString
			else
	            (rec(value, this, new StringBuilder("(")) ++= ")").toString
		}

		override def nullLiteral(inline :Boolean) :String = literal(null, inline)
		override def literal(value :T ~ H) :String = literal(value, false)
		override def inlineLiteral(value :T ~ H) :String = literal(value, true)
		override def nullLiteral :String = literal(null, false)
		override def inlineNullLiteral :String = literal(null, true)



		override def toString :String =  {
			def rec(form :SQLWriteForm[_], res :StringBuilder = new StringBuilder) :StringBuilder = form match {
				case chain :ChainWriteForm[_, _] =>
					rec(chain.tail, res) ++= "~"
					chain.head match {
						case _ :ChainWriteForm[_, _] => rec(chain.head, res ++= "(") ++= ")"
						case _ => res append chain.head
					}
				case  _ => res ++= "@~"
			}
			rec(this).toString
		}
	}



}



