package net.noresttherein.oldsql.schema

import java.sql.{PreparedStatement, ResultSet}

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.ColumnReadForm.{FlatMappedColumnReadForm, LazyColumnReadForm, MappedColumnReadForm}
import net.noresttherein.oldsql.schema.ColumnWriteForm.LazyColumnWriteForm
import net.noresttherein.oldsql.schema.SQLForm.{CombinedForm, FlatMappedSQLForm, JDBCSQLType, LazyForm, MappedSQLForm, NullableForm, NullValue, OptionForm, Tuple2Form}
import net.noresttherein.oldsql.schema.SQLReadForm.{AbstractTuple2ReadForm, ChainReadForm, FlatMappedSQLReadForm, LazyReadForm, MappedSQLReadForm, OptionMappedSQLReadForm, SeqReadForm}
import net.noresttherein.oldsql.schema.SQLWriteForm.{AbstractTuple2WriteForm, ChainWriteForm, EmptyWriteForm, FlatMappedSQLWriteForm, LazyWriteForm, MappedSQLWriteForm, SeqWriteForm}
import net.noresttherein.oldsql.slang._

import scala.collection.immutable.Seq






trait SQLForm[T] extends SQLReadForm[T] with SQLWriteForm[T] {
	def apply(position :Int)(res :ResultSet) :T

	def opt(position :Int)(res :ResultSet) :Option[T]

	def literal(value :T) :String

	def nullValue :T
	//	def columnCount :Int


	def bimap[X :NullValue](map :T => X)(unmap :X => T) :SQLForm[X] =
		SQLForm.map[T, X](map, unmap)(this, NullValue[X])

	def bimap[X](map :T => X, nullValue :X)(unmap :X => T) :SQLForm[X] =
		bimap(map)(unmap)(NullValue(nullValue))

	def bimapNull[X](map :T => X)(unmap :X => T) :SQLForm[X] =
		bimap(map)(unmap)(nulls.map(map))

	def bimapRef[X >: Null](map :T => X)(unmap :X => T) :SQLForm[X] =
		bimap(map)(unmap)

	def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
		SQLForm.flatMap(map, unmap)(this, NullValue[X])

	def biflatMap[X](map :T => Option[X], nullValue :X)(unmap :X => Option[T]) :SQLForm[X] =
		biflatMap(map)(unmap)(NullValue(nullValue))

	def biflatMapRef[X >: Null](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
		biflatMap(map)(unmap)

	override def asOpt :SQLForm[Option[T]] = SQLForm.OptionForm(this)


	def *[O](other :SQLForm[O]) :SQLForm[(T, O)] = new Tuple2Form()(this, other)

	override def toString :String = this.innerClassName

	def compatible(other :SQLForm[_]) :Boolean = this == other
}





trait BaseColumnForm {
	/** The JDBC code for the underlying column type, as defined by constants in `java.sql.Types`. */
	def sqlType :JDBCSQLType
}



trait ColumnForm[T] extends SQLForm[T] with ColumnReadForm[T] with ColumnWriteForm[T] {

	override def bimap[X :NullValue](map :T => X)(unmap :X => T) :ColumnForm[X] =
		ColumnForm.map[T, X](map, unmap)(this, NullValue[X])

	override def bimap[X](map :T => X, nullValue :X)(unmap :X => T) :ColumnForm[X] =
		bimap(map)(unmap)(NullValue(nullValue))

	override def bimapNull[X](map :T => X)(unmap :X => T) :ColumnForm[X] =
		bimap(map)(unmap)(nulls.map(map))

	override def bimapRef[X >: Null](map :T => X)(unmap :X => T) :ColumnForm[X] =
		bimap(map)(unmap)

	override def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :ColumnForm[X] =
		ColumnForm.flatMap(map, unmap)(this, NullValue[X])

	override def biflatMap[X](map :T => Option[X], nullValue :X)(unmap :X => Option[T]) :ColumnForm[X] =
		biflatMap(map)(unmap)(NullValue(nullValue))

	override def biflatMapRef[X >: Null](map :T => Option[X])(unmap :X => Option[T]) :ColumnForm[X] =
		biflatMap(map)(unmap)

	override def asOpt :ColumnForm[Option[T]] = ColumnForm.OptionColumnForm(this)



	override def compatible(other: SQLForm[_]): Boolean = other match {
		case a :ColumnForm[_] => a.sqlType == sqlType
		case _ => false
	}


}






object ColumnForm {
	@inline def apply[X :ColumnForm] :ColumnForm[X] = implicitly[ColumnForm[X]]

	def combine[T](read :ColumnReadForm[T], write :ColumnWriteForm[T]) :ColumnForm[T] =
		if (read.sqlType != write.sqlType)
			throw new IllegalArgumentException(
				s"Can't combine column forms $read and $write with different underlying sql types: ${read.sqlType} != ${write.sqlType}."
			)
		else
			new CombinedForm[T](read, write) with ColumnForm[T] {
				override val sqlType = (read :SQLReadForm[T]).asInstanceOf[ColumnReadForm[T]].sqlType

				override def read(position :Int)(res :ResultSet) =
					(read :SQLReadForm[T]).asInstanceOf[ColumnReadForm[T]].friendRead(position)(res)
			}



	def Lazy[T](delayed: => ColumnForm[T]) :ColumnForm[T] =
		new LazyForm[T](() => delayed) with LazyColumnReadForm[T] with LazyColumnWriteForm[T] with ColumnForm[T] {
			override def form :ColumnForm[T] = super[LazyForm].form.asInstanceOf[ColumnForm[T]]

			override def bimap[X :NullValue](map :T => X)(unmap :X => T) =
				if (isInitialized) form.bimap[X](map)(unmap)
				else Lazy(form.bimap[X](map)(unmap))

			override def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) =
				if (isInitialized) form.biflatMap(map)(unmap)
				else Lazy(form.biflatMap[X](map)(unmap))
		}



	def map[S, T](map :S => T, unmap :T => S)(implicit source :ColumnForm[S], nulls :NullValue[T] = null) :ColumnForm[T] =
		new MappedSQLForm[S, T](map, unmap) with MappedColumnReadForm[S, T] with ColumnForm[T]

	def flatMap[S :ColumnForm, T :NullValue](map :S => Option[T], unmap :T => Option[S]) :ColumnForm[T] =
		new FlatMappedSQLForm[S, T](map, unmap) with FlatMappedColumnReadForm[S, T] with ColumnForm[T]



	implicit def OptionColumnForm[T :ColumnForm] :ColumnForm[Option[T]] =
		new OptionForm[T] with ColumnForm[Option[T]] {
			override val sqlType = implicitly[ColumnForm[T]].sqlType

			override def read(position :Int)(res :ResultSet) = ColumnForm[T].opt(position)(res)
		}

	implicit def SomeColumnForm[T :ColumnForm] :ColumnForm[Some[T]] =
		ColumnForm[T].bimapNull(Some.apply)(_.get)






	trait NullableColumnForm[T >: Null] extends ColumnForm[T] with NullableForm[T] {
		override def apply(position :Int)(res :ResultSet) :T = read(position)(res)
	}




}







object SQLForm extends JDBCTypes {

	def combine[T](read :SQLReadForm[T], write :SQLWriteForm[T]) :SQLForm[T] =
		new CombinedForm[T](read, write)

	@inline def combine[T](read :ColumnReadForm[T], write :ColumnWriteForm[T]) :ColumnForm[T] =
		ColumnForm.combine(read, write)



	def Lazy[T](init: => SQLForm[T]) :SQLForm[T] = new LazyForm[T](() => init)



	def flatMap[S :SQLForm, T :NullValue](map :S => Option[T], unmap :T => Option[S]) :SQLForm[T] = SQLForm[S] match {
		case t :ColumnForm[_] =>
			ColumnForm.flatMap(map, unmap)(t.asInstanceOf[ColumnForm[S]], NullValue[T])
		case _ =>
			new FlatMappedSQLForm[S, T](map, unmap)
	}

	def map[S, T](map :S => T, unmap :T => S)(implicit source :SQLForm[S], nulls :NullValue[T] = null) :SQLForm[T] =
		SQLForm[S] match {
			case t :ColumnForm[_] =>
				ColumnForm.map(map, unmap)(
					t.asInstanceOf[ColumnForm[S]], if (nulls == null) source.nulls.map(map) else nulls
				)
			case _ =>
				new MappedSQLForm[S, T](map, unmap)
		}



	implicit def OptionForm[T :SQLForm] :SQLForm[Option[T]] = new OptionForm[T]

	implicit def SomeForm[T :SQLForm] :SQLForm[Some[T]] = SQLForm[T].bimapNull(Some.apply)(_.get)



	implicit def Tuple2Form[T1 :SQLForm, T2 :SQLForm] :SQLForm[(T1, T2)] = new Tuple2Form[T1, T2]

	implicit def ChainForm[T <: Chain, H](implicit t :SQLForm[T], h :SQLForm[H]) :SQLForm[T ~ H] =
		new ChainForm(t, h)

	implicit val EmptyChainForm :SQLForm[@~] = new EmptyForm[@~](@~) {
		override def toString = "@~"
	}




	trait NullValue[+T] {
		def value :T
		def map[U](f :T => U) :NullValue[U]
	}

	object NullValue {
		@inline def value[T :NullValue] :T = implicitly[NullValue[T]].value

		@inline def apply[T :NullValue] :NullValue[T] = implicitly[NullValue[T]]


		def apply[T](sqlNull :T) :NullValue[T] = new NullValue[T] {
			override def value = sqlNull
			override def map[U](f :T => U) :NullValue[U] = NullValue(f(sqlNull))
			override def toString :String = "Null(" + value + ")"
		}


		def byName[T](eval: =>T) :NullValue[T] = new NullValue[T] {
			override def value :T = eval
			override def map[U](f :T => U) = byName(f(eval))
			override def toString = "Null(?)"
		}

		final val NotNull :NullValue[Nothing] = new NullValue[Nothing] {
			override def value = throw new NullPointerException("This type does not allow null values.")
			override def map[U](f :Nothing => U) :NullValue[U] = this
			override def toString = "NotNull"
		}

		implicit final val None :NullValue[Option[Nothing]] = NullValue(scala.None)
		implicit final val Null :NullValue[Null] = NullValue(null)
		implicit final val Int = NullValue(0)
		implicit final val Long = NullValue(0L)
		implicit final val Short = NullValue(0.toShort)
		implicit final val Byte = NullValue(0.toByte)
		implicit final val Boolean = NullValue[Boolean](false)
		implicit final val Char = NullValue(0.toChar)
		implicit final val Float = NullValue(0.0f)
		implicit final val Double = NullValue(0.0)
		implicit final val Unit = NullValue(())

	}






	trait NullableForm[T >: Null] extends SQLForm[T] {
		override def nullValue :Null = null
		override def nulls :NullValue[T] = NullValue.Null
		override def nullLiteral :String = "null"
		override def inlineNullLiteral :String = "null"
	}



	trait NonLiteralForm[T] extends SQLForm[T] {
		override def literal(value: T): String = throw new UnsupportedOperationException(getClass.getName+".literal")
		override def nullLiteral :String = throw new UnsupportedOperationException(getClass.getName+".nullLiteral")
		override def inlineLiteral(value: T): String = throw new UnsupportedOperationException(getClass.getName+".inlineLiteral")
		override def inlineNullLiteral :String = throw new UnsupportedOperationException(getClass.getName+".inlineNullLiteral")
	}



	abstract class AbstractEmptyForm[T] extends SQLForm[T] with EmptyWriteForm[T] {
		override def apply(position: Int)(res: ResultSet): T = nullValue

		def apply(column :String)(res :ResultSet) :T = nullValue

		override def opt(position :Int)(rs :ResultSet) :Option[T] = None

		override def readColumns = 0
	}


	class EmptyForm[T](nullExpr : =>T) extends AbstractEmptyForm[T] with NonLiteralForm[T] {
		def nullValue :T = nullExpr
	}

	object EmptyForm {
		def apply[T](nullExpr : =>T) :EmptyForm[T] = new EmptyForm[T](nullExpr)
		def unapply[T](form :SQLForm[T]) :Boolean = form.isInstanceOf[EmptyForm[_]]
	}






	implicit case object UnitForm extends EmptyForm[Unit](()) {
		override def toString = "UNIT"
	}

	case object NothingForm extends EmptyForm[Nothing](throw new UnsupportedOperationException("SQLType.NothingType")) {
		override def toString = "NOTHING"
	}

	case object NoneForm extends EmptyForm[Option[Nothing]](None) {
		override def toString = "NONE"
	}



	class UnknownForm[T] extends EmptyForm[T](throw new UnsupportedOperationException("SQLType.UnknownType")) {
		override def toString = "UNKNOWN"
	}

	object Unknown {
		def apply[T]() :UnknownForm[T] = unknown.asInstanceOf[UnknownForm[T]]
		def apply[T](form :SQLForm[T]) :Boolean = form.isInstanceOf[UnknownForm[_]]

		def unapply[T](form :SQLForm[T]) :Boolean = apply(form)

		private val unknown = new UnknownForm[Any]
	}






	private[schema] case class CombinedForm[T](read :SQLReadForm[T], write :SQLWriteForm[T]) extends SQLForm[T] {

		override def set(position :Int)(statement :PreparedStatement, value :T) :Unit =
			write.set(position)(statement, value)

		override def setNull(position :Int)(statement :PreparedStatement) :Unit =
			write.setNull(position)(statement)

		override def opt(position: Int)(res: ResultSet): Option[T] = read.opt(position)(res)

		override def nullValue: T = read.nullValue

		override def literal(value: T): String = write.literal(value)
		override def nullLiteral: String = write.nullLiteral

		override def inlineLiteral(value: T): String = write.inlineLiteral(value)
		override def inlineNullLiteral: String = write.inlineNullLiteral

		override def writtenColumns: Int = write.writtenColumns

		override def readColumns: Int = read.readColumns

		override def toString = s"($read & $write)"
	}






	private[schema] class FlatMappedSQLForm[S, T](map :S => Option[T], val unmap :T => Option[S])
	                                             (implicit override val source :SQLForm[S], nulls :NullValue[T])
		extends FlatMappedSQLReadForm[S, T](map) with FlatMappedSQLWriteForm[S, T] with SQLForm[T]
	{
		override def bimap[X :NullValue](map :T => X)(unmap :X => T) :SQLForm[X] =
			source.biflatMap(this.map(_).map(map))(unmap andThen this.unmap)

		override def toString = s"<=$source=>"
	}



	private[schema] class MappedSQLForm[S, T](map :S => T, val unmap :T => S)
	                                         (implicit override val source :SQLForm[S], nulls :NullValue[T])
		extends MappedSQLReadForm[S, T](map) with MappedSQLWriteForm[S, T] with SQLForm[T]
	{

		override def bimap[X :NullValue](map :T => X)(unmap :X => T) :SQLForm[X] =
			source.bimap(this.map andThen map)(unmap andThen this.unmap)

		override def toString :String = "<=" + source + "=>"
	}



	private[schema] class LazyForm[T](delayed: () => SQLForm[T])
		extends LazyReadForm[T](delayed) with LazyWriteForm[T] with SQLForm[T]
	{
		protected[this] override var init :() => SQLWriteForm[T] = delayed

		override protected def form :SQLForm[T] = {
			val read = super[LazyReadForm].form.asInstanceOf[SQLForm[T]]
			if (fastAccess == null) {
				fastAccess = read
				if (initialized == null)
					initialized = read
				init = null
			}
			read
		}

		override def isInitialized :Boolean = super[LazyReadForm].isInitialized

		override def bimap[X :NullValue](map :T => X)(unmap :X => T) :SQLForm[X] =
			if (isInitialized) form.bimap[X](map)(unmap)
			else Lazy(form.bimap[X](map)(unmap))


		override def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
			if (isInitialized) form.biflatMap(map)(unmap)
			else Lazy(form.biflatMap(map)(unmap))

		override def *[O](other :SQLForm[O]) :SQLForm[(T, O)] =
			if (isInitialized) form * other
			else Lazy(form * other)

		override def toString :String = if (isInitialized) form.toString else "<Lazy>"
	}







	private[schema] class OptionForm[T](implicit form :SQLForm[T]) extends SQLForm[Option[T]] {
		override def readColumns :Int = form.readColumns
		override def writtenColumns :Int = form.writtenColumns


		override def set(position :Int)(statement :PreparedStatement, value :Option[T]) :Unit = value match {
			case Some(x) => form.set(position)(statement, x)
			case _ => form.setNull(position)(statement)
		}
		override def setNull(position :Int)(statement :PreparedStatement) :Unit = form.setNull(position)(statement)

		override def opt(position :Int)(res :ResultSet) :Option[Option[T]] = form.opt(position)(res).map(Option.apply)
		override def apply(position :Int)(res :ResultSet) :Option[T] = form.opt(position)(res)

		override def literal(value :Option[T]) :String = value match {
			case Some(x) => form.literal(x)
			case _ => form.nullLiteral
		}
		override def inlineLiteral(value :Option[T]) :String = value match {
			case Some(x) => form.inlineLiteral(x)
			case _ => form.inlineNullLiteral
		}

		override def nullValue :Option[T] = None

		override def nullLiteral :String = form.nullLiteral
		override def inlineNullLiteral :String = form.inlineNullLiteral

		private def some :SQLForm[T] = form

		override def equals(that :Any) :Boolean = that match {
			case opt :OptionForm[_] => opt.some == form
			case _ => false
		}

		override def hashCode :Int = form.hashCode

		override def toString :String = "Option[" + form + "]"
	}



	private class Tuple2Form[L, R](implicit val _1 :SQLForm[L], implicit val _2 :SQLForm[R])
		extends AbstractTuple2ReadForm[L, R] with AbstractTuple2WriteForm[L, R] with SQLForm[(L, R)]
	{
		override def equals(that :Any) :Boolean = that match {
			case t :Tuple2Form[_, _] => (t eq this) || t._1 == _1 && t._2 == _2
			case _ => false
		}

		override def hashCode :Int = (_1, _2).hashCode

		override def toString = s"(${_1},${_2})"
	}



	private case class SeqForm[T](forms :Seq[SQLForm[T]]) extends SQLForm[Seq[T]] with SeqWriteForm[T] with SeqReadForm[T] {
		override def toString :String = forms.mkString("Seq(",",",")")
	}



	private class ChainForm[T <: Chain, H](override val tail :SQLForm[T], override val head :SQLForm[H])
		extends ChainWriteForm(tail, head) with ChainReadForm[T, H] with SQLForm[T ~ H]
	{
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ChainForm[_, _]]

		override def toString :String = super[ChainWriteForm].toString
	}



/*
	import shapeless.::



	implicit case object HNilForm extends AbstractEmptyForm[HNil] with AbstractHListWriteForm[HNil] {
		def ::[X](f :SQLForm[X]) :HListForm[X, HNil] = new HListForm(f, this)

		override def nullValue: HNil = HNil
		override def nullLiteral: String = "()"
		override def literal(value :HNil) = "()"

		override def inlineLiteral(value: HNil): String = ""
		override def inlineNullLiteral: String = ""

		override protected[sql] def elementsLiteral(sb: StringBuilder, value :HNil): StringBuilder = sb
		override def toString = "HNIL"
	}



	case class HListForm[H, T>:Null<:HList](head :SQLForm[H], tail :SQLForm[T] with AbstractHListWriteForm[T])
		extends SQLForm[H::T] with HListReadForm[H, T] with HListWriteForm[H, T]
	{
		override def toString = s"$head::" + tail.toString
	}
*/




}


