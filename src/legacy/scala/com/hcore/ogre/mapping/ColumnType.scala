package com.hcore.ogre.mapping

import com.hcore.ogre.sql.{AtomicForm, SQLForm}

import scala.slick.jdbc.{GetResult, PositionedParameters, PositionedResult, SetParameter}
import scala.util.Try


trait ColumnType[T] {
	def sqlForm :SQLForm[T]
	
	def Setter :SetParameter[T]
	def Getter :GetResult[T]

	def apply(res :PositionedResult) = Getter(res)
	def apply(value :T, params :PositionedParameters) = Setter(value, params)

	def nullValue :T

	def isDefined(res :PositionedResult) = {
		val r = res.rs getObject res.currentPos
		res.rs.wasNull
	}
}


object ColumnType {
	@deprecated("use SQLForm.NullValue", "now")
	class NullValue[+T](val value :T)

	@deprecated("use SQLForm.NullValue", "now")
	object NullValue {
		def apply[T :NullValue] :NullValue[T] = implicitly[NullValue[T]]

		def Null[T :NullValue] = implicitly[NullValue[T]].value

		implicit def nullable[T>:Null] :NullValue[T] = new NullValue[T](null)
		implicit val int = new NullValue(0)
		implicit val long = new NullValue(0L)
		implicit val short = new NullValue[Short](0)
		implicit val byte = new NullValue[Byte](0)
		implicit val boolean = new NullValue[Boolean](false)
		implicit val char = new NullValue[Char](0)
		implicit val float = new NullValue[Float](0)
		implicit val double = new NullValue[Double](0)
		implicit val unit = new NullValue(())

	}

	class SQLColumnType[T](implicit val sqlForm :AtomicForm[T]) extends ColumnType[T] {
		override def Setter: SetParameter[T] = SetParameter((t, params) => sqlForm(params, t))

		override def Getter: GetResult[T] = GetResult(sqlForm(_:PositionedResult))

		override def nullValue: T = sqlForm.nullValue
	}
	implicit def SQLColumnType[T :AtomicForm] :ColumnType[T] = new SQLColumnType[T]
//	abstract class UniqueValue[T] {
//		def apply(ctx :MappingContext) :Option[T]
//	}
//
//	object UniqueValue {
//		class NumberValue[T :Forgerable] extends UniqueValue[T] {
//			override def apply(ctx: MappingContext): Option[T] = ctx.forger.next[T]
//		}
//
//	}


	def apply[T :ColumnType] :ColumnType[T] = implicitly[ColumnType[T]]


	def mapped[S :NullValue, T :ColumnType](map :T=>S)(unmap :S=>T) = new MappedType(map, unmap)

	def nullSafe[S >:Null, T >:Null :ColumnType](map :T=>S)(unmap :S=>T) =
		mapped[S, T](Option(_).map(map).orNull)(Option(_).map(unmap).orNull)(NullValue[S], ColumnType[T])

	def safe[S >:Null, T >:Null :ColumnType](map :T=>S)(unmap :S=>T) =
		mapped[S, T](
			//			t => Try(map(t)).toOption.orNull)(
			//	        s => Try(unmap(s)).toOption.orNull)(NullValue[S], ColumnType[T])
			t => Try(map(t)).recover {
				case e :Exception => System.err.println(s"failed converting db value $t: $e. Returning null."); null
			}.toOption.orNull)(
		        s => Try(unmap(s)).recover {
			        case e :Exception => System.err.println(s"failed converting $s to db value: $e. Returning null."); null
		        }.toOption.orNull)(NullValue[S], ColumnType[T])



	def apply[S :NullValue, T :ColumnType](map :T=>S)(unmap :S=>T) = new MappedType(map, unmap)


//	implicit def slickType[T :SetParameter :GetResult :NullValue] :ColumnType[T] =
//		new SlickType(implicitly[SetParameter[T]], implicitly[GetResult[T]])


//	def apply[T :NullValue](set :(T, PositionedParameters) => Unit, get :PositionedResult=>T) :ColumnType[T] =
//		new SlickType(SetParameter[T](set), GetResult[T](get))


	class MappedType[S :NullValue, T :ColumnType](val map :T=>S, val unmap :S=>T) extends ColumnType[S] {

		val baseType = implicitly[ColumnType[T]]

		val sqlForm = baseType.sqlForm.as(map, NullValue.Null[S])(unmap)

		implicit lazy val Setter :SetParameter[S] =
			SetParameter[S]{ case (value, params) => baseType.Setter(this.unmap(value), params) }

		implicit lazy val Getter :GetResult[S] = baseType.Getter.andThen(this.map)

		override def nullValue: S = NullValue.Null[S]
	}

//	class SlickType[T :NullValue](val Setter :SetParameter[T], val Getter :GetResult[T]) extends ColumnType[T] {
//		val nullValue = NullValue.Null[T]
//	}

	trait TypeSupport[T] extends ColumnType[T] {
		val Getter = GetResult[T](get)
		val Setter = SetParameter[T](set)

		def get(res :PositionedResult) :T
		def set(value :T, params :PositionedParameters) :Unit

	}

}

