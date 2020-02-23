package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping

import java.lang.reflect.{Method, InvocationHandler}
import java.sql.ResultSet

import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.ColumnMapping.ColumnOption
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.ColumnType.NullValue
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.Mapping._
import com.adpilot.cortb.clientapi.util.OptionOps

import scala.reflect.ClassTag
import scala.slick.jdbc.{GetResult, PositionedParameters, PositionedResult, SetParameter}


import ColumnMapping._
import ColumnOption._
import OptionOps._

import scala.reflect._
import scala.util.Try

trait ColumnType[T] {

	def Setter :SetParameter[T]
	def Getter :GetResult[T]
	
	def nullValue :T
}


object ColumnType {
	class NullValue[+T](val value :T)

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


	implicit def slickType[T :SetParameter :GetResult :NullValue] :ColumnType[T] =
		new SlickType(implicitly[SetParameter[T]], implicitly[GetResult[T]])


	def apply[T :NullValue](set :(T, PositionedParameters) => Unit, get :PositionedResult=>T) :ColumnType[T] =
		new SlickType(SetParameter[T](set), GetResult[T](get))


	class MappedType[S :NullValue, T :ColumnType](val map :T=>S, val unmap :S=>T) extends ColumnType[S] {
		val baseType = implicitly[ColumnType[T]]

		implicit lazy val Setter :SetParameter[S] =
			SetParameter[S]{ case (value, params) => baseType.Setter(this.unmap(value), params) }

		implicit lazy val Getter :GetResult[S] = baseType.Getter.andThen(this.map)
		
		override def nullValue: S = NullValue.Null[S]
	}

	class SlickType[T :NullValue](val Setter :SetParameter[T], val Getter :GetResult[T]) extends ColumnType[T] {
		val nullValue = NullValue.Null[T]
	}

	trait TypeSupport[T] extends ColumnType[T] {
		val Getter = GetResult[T](get)
		val Setter = SetParameter[T](set)

		def get(res :PositionedResult) :T
		def set(value :T, params :PositionedParameters) :Unit

	}
}


trait ColumnMapping[E, T] extends ComponentMapping[E, T] with SetParameter[T] {
	type Component[X] <: ColumnMapping[T, X]
	type Column[X] <: ColumnMapping[T, X] with Component[X]

	def selectHeader(prefix :String) :String =
		OptionalSelect.test(this).map(_ => "null") orElse prefixOption(prefix).map(_+"."+name) getOrElse name

	def name :String

	def options :Seq[ColumnOption[T]]


	def param(value :T) :QueryParam = QueryParam(value)(this)

	def paramFrom(entity :E) :QueryParam = param(value(entity))

	def nullValue :T = columnType.nullValue

	def columnType :ColumnType[T]
	
	
	final def components :Seq[Nothing] = Seq()
	final def nestedComponents :Seq[Nothing] = Seq()

	final def columns :Seq[Column[T]] = Seq(self)
	def querable :Seq[Column[T]] = emptyIf(NoQuery)
	def selectable :Seq[Column[T]] = emptyIf(NoSelect)
	def updatable :Seq[Column[T]] = emptyIf(NoUpdate)
	def insertable :Seq[Column[T]] = emptyIf(NoInsert)
	def generated :Seq[Column[T]] = columns.providing(enabled(AutoGen)) getOrElse Seq()

	def enabled[O[X] <:ColumnOption[X] :ColumnOptionType] :Boolean = option[O].isDefined
	def disabled[O[X] <:ColumnOption[X] :ColumnOptionType] :Boolean = option[O].isEmpty

	def option[O[X] <:ColumnOption[X] :ColumnOptionType] :Option[O[T]] = implicitly[ColumnOptionType[O]].test(this)


	private def emptyIf[O[X]  <:ColumnOption[X] :ColumnOptionType] =
		columns.unless(enabled(ColumnOptionType[O])) getOrElse Seq()


	lazy val InsertParameters = this.unless(enabled[NoInsert]) getOrElse SetParameters.NoOp
	lazy val UpdateParameters = this.unless(enabled[NoUpdate]) getOrElse SetParameters.NoOp

	def self :Column[T]


	def apply(v1: T, v2: PositionedParameters): Unit = columnType.Setter(v1, v2)

	final override def apply(res :PositionedResult) :T =
		if (selectable.nonEmpty)
			columnType.Getter(res)
		else
			option(OptionalSelect).map(_.defaultValue) getOrElse {
				throw new UnsupportedOperationException(s"attempted to read a non-selectable column $this from PositionedResult")
			}


	final override def apply(values: ColumnValues): T =
		values(this) orElse option(OptionalSelect).map(_.defaultValue) getOrElse {
			throw new UnsupportedOperationException(s"attempted to read a non-selectable column $this from $values")
		}

	override def valuesFor(component: Component[_]) = identity

	override def toString =
		if (options.nonEmpty)
			options.mkString(name+"[", ",", "]")
		else name
}



object ColumnMapping {
	type SelfColumn[T] = ColumnMapping[T, T]

	def apply[E, T :ColumnType](name :String, pick :E=>T, options :ColumnOption[T]*) :ColumnMapping[E, T] =
		new ColumnComponent(name, pick, options)

	def apply[T :ColumnType](name :String, /*opt0 :ColumnOption, */opts :ColumnOption[T]*) :ColumnMapping[T, T] =
		apply[T,T](name, x=>x, /*opt0 +: */opts :_*)




	class BaseColumn[T](val name :String, val options :Seq[ColumnOption[T]])(implicit val columnType :ColumnType[T])
		extends ColumnMapping[T, T]
	{
		type Component[X] = ColumnMapping[T, X]
		type Column[X] = ColumnMapping[T, X]

		override val selectable = super.selectable
		override val querable = super.querable
		override val updatable = super.updatable
		override val insertable = super.insertable
		override val generated = super.generated
		
		def value(entity: T): T = entity
		def self: Column[T] = this
	}


	class ColumnComponent[E, T](val name :String, pick :E=>T, val options :Seq[ColumnOption[T]])(implicit val columnType :ColumnType[T])
		extends ColumnMapping[E, T]
	{
		type Component[X] = ColumnMapping[T, X]
		type Column[X] = ColumnMapping[T, X]

		override val selectable = super.selectable
		override val querable = super.querable
		override val updatable = super.updatable
		override val insertable = super.insertable
		override val generated = super.generated

		override def value(entity: E): T = pick(entity)

		lazy val self = new BaseColumn[T](name, options)
	}


	trait ColumnAdapter[E, T, C<:ColumnMapping[_, T]] extends ColumnMapping[E, T] with MappingDecorator[T, C]


	trait ColumnDecorator[E, T, C<:ColumnMapping[E, T]] extends ColumnAdapter[E, T, C] {
		override def value(entity: E): T = adaptedMapping.value(entity)
	}



	trait AbstractColumnAdapter[E, T, C<:ColumnMapping[_, T]] extends ColumnAdapter[E, T, C] {

		def name = adaptedMapping.name
		def options = adaptedMapping.options

		override lazy val InsertParameters: SetParameter[T] = adaptedMapping.InsertParameters
		override lazy val UpdateParameters: SetParameter[T] = adaptedMapping.UpdateParameters

		override def columnType: ColumnType[T] = adaptedMapping.columnType
	}

	trait AbstractColumnDecorator[E, T, C<:ColumnMapping[E, T]]
		extends AbstractColumnAdapter[E, T, C] with ColumnDecorator[E, T, C]



	trait DirectColumnAdapter[E, T, C<:ColumnMapping[_, T]]
		extends AbstractColumnAdapter[E, T, C]
	{
		type Component[X] = adaptedMapping.Column[X]
		type Column[X] = adaptedMapping.Column[X]

		override def querable = adaptedMapping.querable
		override def selectable = adaptedMapping.selectable
		override def updatable = adaptedMapping.updatable
		override def insertable = adaptedMapping.insertable
		override def generated = adaptedMapping.generated

		override def self: Column[T] = adaptedMapping.self
	}

	trait DirectColumnDecorator[E, T, C<:ColumnMapping[E, T]]
		extends DirectColumnAdapter[E, T, C] with ColumnDecorator[E, T, C]



	abstract class AbstractEmbeddedColumn[E, T, C<:ColumnMapping[_, T]](
			val adaptedMapping :C, pick: E => T, override val name: String, override val options: Seq[ColumnOption[T]])
		extends AbstractColumnAdapter[E, T, C]
	{
		override lazy val InsertParameters = this.unless(enabled[NoInsert]) getOrElse SetParameters.NoOp
		override lazy val UpdateParameters = this.unless(enabled[NoUpdate]) getOrElse SetParameters.NoOp


		def value(entity: E): T = pick(entity)
	}



	class EmbeddedColumn[E, C, T](col :ColumnMapping[C, T], pick :E=>T, name :Option[String], options :Seq[ColumnOption[T]])
		extends AbstractEmbeddedColumn[E, T, ColumnMapping[C, T]](col, pick, name getOrElse col.name, options) with DirectColumnAdapter[E, T, ColumnMapping[C, T]]
	{
		def this(column :ColumnMapping[C, T], pick :E=>T, name :Option[String]) = this(column, pick, name, column.options)
		def this(column :ColumnMapping[C, T], pick :E=>T) = this(column, pick, None, column.options)
//
	}





	trait ColumnOption[+T]


	object ColumnOption {
		trait NoSelect[+T] extends ColumnFlag[T]
		trait NoInsert[+T] extends ColumnFlag[T]
		trait NoUpdate[+T] extends ColumnFlag[T]
		trait NoQuery[+T] extends ColumnFlag[T]
		trait ReadOnly[+T] extends NoInsert[T] with NoUpdate[T]
		trait AutoGen[+T] extends ReadOnly[T]

		case class OptionalSelect[T](defaultValue :T) extends ColumnOption[T]

		implicit object NoSelect extends ColumnFlagType[NoSelect] with NoSelect[Nothing]
		implicit object NoInsert extends ColumnFlagType[NoInsert] with NoInsert[Nothing]
		implicit object NoUpdate extends ColumnFlagType[NoUpdate] with NoUpdate[Nothing]
		implicit object NoQuery extends ColumnFlagType[NoQuery] with NoQuery[Nothing]
		implicit object ReadOnly extends ColumnFlagType[ReadOnly] with ReadOnly[Nothing]
		implicit object AutoGen extends ColumnFlagType[AutoGen] with AutoGen[Nothing]

		implicit object OptionalSelect extends ColumnOptionType[OptionalSelect] {
			override def test[T](option: ColumnOption[T]): Option[OptionalSelect[T]] = option match {
				case o:OptionalSelect[_] => Some(o.asInstanceOf[OptionalSelect[T]])
				case _ => None
			}


			def apply[T :NullValue] :OptionalSelect[T] = new OptionalSelect(NullValue.Null[T])
		}


		/** Parameterless column option which is completely type agnostic */
		trait ColumnFlag[+T] extends ColumnOption[T] {

			override lazy val toString = {
				val name = getClass.getName
				val offset = name.lastIndexOf('.')
				val inner = name.lastIndexOf('$')
				name.substring((inner max offset) +1)
			}
			
			override def equals(that :Any) = that match {
				case o:ColumnFlag[_] => (this eq o) || o.getClass==this.getClass
				case _ => false
			}
		}



		trait ColumnOptionType[O[T]<:ColumnOption[T]] {
			def test[T](option :ColumnOption[T]) :Option[O[T]]
			def test[T](options :Seq[ColumnOption[T]]) :Option[O[T]] = options.map(test(_)).collectFirst{ case Some(x) => x }
			def test[T](column :ColumnMapping[_, T]) :Option[O[T]] = test(column.options)

			def enabled(option :ColumnOption[_]) :Boolean = test(option).isDefined
			def enabled(options :Seq[ColumnOption[_]]) :Boolean = options.exists(enabled)
			def enabled(column :ColumnMapping[_, _]) :Boolean = enabled(column.options)

			def disabled(option :ColumnOption[_]) = test(option).isEmpty
			def disabled(options :Seq[ColumnOption[_]]) :Boolean = options.forall(disabled)
			def disabled(column :ColumnMapping[_, _]) :Boolean = disabled(column.options)

		}

		class ColumnFlagType[O[T]<:ColumnOption[T]](implicit tag :ClassTag[O[_]]) extends ColumnOptionType[O]  {
			this :O[Nothing] =>

//			def this()(implicit tag :ClassTag[O[_]]) = this(this)(tag)

			override def test[T](option: ColumnOption[T]): Option[O[T]] = option match {
				case tag(o) => Some(o.asInstanceOf[O[T]])
				case _ => None
			}

			def unapply[T](column :ColumnMapping[_, T]) = enabled(column)

			def apply[T]() :O[T] = this.asInstanceOf[O[T]]
//			def apply[T]() :O[T] = instance.asInstanceOf[O[T]]

//			private[this] val instance = cons

//			override val toString = {
//				val name = tag.runtimeClass.getName
//				name.substring(name.lastIndexOf('.'))
//			}
		}


		object ColumnOptionType {
			def apply[O[X] <: ColumnOption[X]](implicit opt :ColumnOptionType[O]) :ColumnOptionType[O] = implicitly[ColumnOptionType[O]]

			def fromClassTag[O[X]<:ColumnOption[X]](implicit tag :ClassTag[O[_]]) :ColumnOptionType[O] =
				new ColumnOptionType[O] {
					override def test[T](option: ColumnOption[T]): Option[O[T]] = option match {
						case tag(o) => Some(o.asInstanceOf[O[T]])
						case _ => None
					}
				}
		}


	}




}

