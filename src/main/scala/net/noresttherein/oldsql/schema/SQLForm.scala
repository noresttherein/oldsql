package net.noresttherein.oldsql.schema

import java.sql.{PreparedStatement, ResultSet}

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.ColumnReadForm.{FlatMappedColumnReadForm, LazyColumnReadForm, MappedColumnReadForm}
import net.noresttherein.oldsql.schema.ColumnWriteForm.LazyColumnWriteForm
import net.noresttherein.oldsql.schema.SQLForm.{CombinedForm, FlatMappedSQLForm, JDBCSQLType, LazyForm, MappedSQLForm, NullableForm, NullValue, OptionForm, Tuple2Form}
import net.noresttherein.oldsql.schema.SQLReadForm.{AbstractTuple2ReadForm, ChainReadForm, FlatMappedSQLReadForm, LazyReadForm, MappedSQLReadForm, SeqReadForm}
import net.noresttherein.oldsql.schema.SQLWriteForm.{AbstractTuple2WriteForm, ChainWriteForm, EmptyWriteForm, FlatMappedSQLWriteForm, LazyWriteForm, MappedSQLWriteForm, SeqWriteForm}
import net.noresttherein.oldsql.slang._

import scala.collection.immutable.Seq






/** Encapsulates the logic of reading a value of type `T` from (possibly several columns of) a `ResultSet` as well
  * as setting SQL statement parameters based on the values of `T`. It is a combination of `SQLReadForm`
  * and `SQLWriteForm` which define most of the available functions apart from a handful of additional bidirectional
  * mapping methods for adapting it to other value types. Basic and typical implementations define the read and write
  * forms of `T` symmetrically, with the exact same column list read and written, but it is not strictly required.
  * This is a lower level API than [[net.noresttherein.oldsql.schema.Mapping Mapping]] as it allows no possibility
  * of customization which columns of a table are included and carries no information about them apart of their
  * relative order in the `ResultSet`/statement parameter list.
  *
  * @see [[net.noresttherein.oldsql.schema.ColumnForm]]
  */
trait SQLForm[T] extends SQLReadForm[T] with SQLWriteForm[T] {

	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values. If the underlying
	  * column(s) is null, implicitly provided 'null' value for type `X` is returned directly from `opt`/`apply`
	  * reading methods without mapping the 'null' value of this type.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`. Will never be called
	  *            for `null` values unless this form returns `Some(null)` from `opt` in a non-standard practice.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement parameters.
	  */
	def bimap[X :NullValue](map :T => X)(unmap :X => T) :SQLForm[X] =
		SQLForm.map[T, X](map, unmap)(this, NullValue[X])

	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values. If the underlying
	  * column(s) is null, the `nullValue` provided  here is returned directly from `opt`/`apply`
	  * reading methods without mapping the 'null' value of this type.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`. Will never be called
	  *            for `null` values unless this form returns `Some(null)` from `opt` in a non-standard practice.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement parameters.
	  */
	def bimap[X](map :T => X, nullValue :X)(unmap :X => T) :SQLForm[X] =
		bimap(map)(unmap)(NullValue(nullValue))

	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values.
	  * The `nullValue` of the new form is the result of mapping this instance's `nulls` with the given function,
	  * meaning it must handle `null` (or its counterpart for `T`) without throwing an exception.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement parameters.
	  */
	def bimapNull[X](map :T => X)(unmap :X => T) :SQLForm[X] =
		bimap(map)(unmap)(nulls.map(map))



	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values. If the underlying
	  * column(s) is null or `map` returns `None`, implicitly provided 'null' value for type `X` is returned directly
	  * from `opt`/`apply` reading methods without mapping the 'null' value of this type. Similarly, if `unmap` returns
	  * `None`, the new form will call `setNull` on this instance instead of `set`.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`. Will never be called
	  *            for `null` values unless this form returns `Some(null)` from `opt` in a non-standard practice.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement parameters.
	  */
	def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
		SQLForm.flatMap(map, unmap)(this, NullValue[X])

	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values. If the underlying
	  * column(s) is null, or `map` returns `None`, the `nullValue` provided  here is returned directly
	  * from `opt`/`apply` reading methods without mapping the 'null' value of this type. Similarly, if `unmap`
	  * returns `None`, the form will call on this instance `setNull` instead of `set`.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`. Will never be called
	  *            for `null` values unless this form returns `Some(null)` from `opt` in a non-standard practice.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement parameters.
	  */
	def biflatMap[X](map :T => Option[X], nullValue :X)(unmap :X => Option[T]) :SQLForm[X] =
		biflatMap(map)(unmap)(NullValue(nullValue))

	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values.
	  * The `nullValue` of the new form is the result of mapping this instance's `nulls` with the given function,
	  * meaning it must handle `null` (or its counterpart for `T`) without throwing an exception. If `map` returns
	  * `None` for `this.nullValue`, a `NoSuchElementException` will be thrown; this can happen both from this method,
	  * and when `nullValue` for the created form is accessed, depending on how this form handles null values.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement parameters.
	  */
	def biflatMapNull[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
		biflatMap(map)(unmap)(nulls.flatMap(map))


	/** Lifts this form to represent `Option[T]`. The created form maps all values returned by this form using
	  * `Option(...)`. This means that `null` values (actual JVM nulls, not the somewhat arbitrary value provided
	  * by this form) are mapped to `Some(None)`, while a returned `None` indicates that this form returned `None`.
	  * Basically this means that the returned form uses this form's `opt` method as its `apply` implementation.
	  */
	override def asOpt :SQLForm[Option[T]] = SQLForm.OptionForm(this)


	def *[O](other :SQLForm[O]) :SQLForm[(T, O)] = new Tuple2Form()(this, other)



	def compatible(other :SQLForm[_]) :Boolean = this == other

	override def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def toString :String = this.innerClassName
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




	/** A type class providing a 'null' value for type `T`. This is the value which should be used by a form when
	  * the underlying column(s) is null. This may be any default value, not only `null` or 'zero';
	  * for example, a collection type can return an empty collection.
	  */
	trait NullValue[+T] {
		/** Value to which null columns in the `ResultSet` are mapped.
		  * @throws NullPointerException if the particular form for type `T` prohibits null values.
		  */
		def value :T

		/** Adapt this null value to some other type `U`. In most cases, this will simply apply the function to
		  * the wrapped value, but special instances may propagate themselves instead.
		  */
		def map[U](f :T => U) :NullValue[U]

		def flatMap[U](f :T => Option[U]) :NullValue[U] =
			map(tnull => f(tnull) getOrElse {
				throw new NoSuchElementException("No corresponding null value for " + tnull + " of " + this)
			})

	}

	/** Factory of `NullValue[T]` type class providing values representing `null` in the underlying column(s).
	  * Provides implicit values for reference types and all built-in value types (using their default 'zero' values).
	  */
	object NullValue {
		/** Summon the 'null' value for `T` from an implicit `NullValue[T]`. */
		@inline def value[T :NullValue] :T = implicitly[NullValue[T]].value

		/** Summon an implicit `NullValue[T]`. */
		@inline def apply[T :NullValue] :NullValue[T] = implicitly[NullValue[T]]


		/** Create a new instance wrapping the given value of `T` as the 'null' value. */
		def apply[T](sqlNull :T) :NullValue[T] = new NullValue[T] {
			override def value = sqlNull
			override def map[U](f :T => U) :NullValue[U] = NullValue(f(sqlNull))
			override def toString :String = "Null(" + value + ")"
		}


		/** Create a new instance which evaluates the given expression each time its `value` method is called.
		  * While returning different values for different calls is strongly discouraged, this allows to provide
		  * an expression which throws an exception or references a not initialized as of yet value.
		  */
		def byName[T](eval: =>T) :NullValue[T] = new NullValue[T] {
			override def value :T = eval
			override def map[U](f :T => U) = byName(f(eval))
			override def toString = "Null(?)"
		}

		/** A `NullValue` instance which always throws a `NullPointerException`. Used with forms for types which
		  * don't accept null values or simply wish to forbid them.
		  */
		final val NotNull :NullValue[Nothing] = new NullValue[Nothing] {
			override def value = throw new NullPointerException("This type does not allow null values.")
			override def map[U](f :Nothing => U) :NullValue[U] = this
			override def toString = "NotNull"
		}

		/** Scala `None` as the null value for `Option[T]`. */
		implicit final val None :NullValue[Option[Nothing]] = NullValue(scala.None)
		/** `null` itself as the value used by nullable types `T >: scala.Null`. */
		implicit final val Null :NullValue[Null] = NullValue(null)
		/** Sets zero as the 'null' value. */
		implicit final val Int = NullValue(0)
		/** Sets zero as the 'null' value. */
		implicit final val Long = NullValue(0L)
		/** Sets zero as the 'null' value. */
		implicit final val Short = NullValue(0.toShort)
		/** Sets zero as the 'null' value. */
		implicit final val Byte = NullValue(0.toByte)
		/** Sets `false` as the 'null' value. */
		implicit final val Boolean = NullValue[Boolean](false)
		/** Sets zero as the 'null' value. */
		implicit final val Char = NullValue(0.toChar)
		/** Sets zero as the 'null' value. */
		implicit final val Float = NullValue(0.0f)
		/** Sets zero as the 'null' value. */
		implicit final val Double = NullValue(0.0)
		/** Unit itself as its 'null' value. */
		implicit final val Unit = NullValue(())

	}





	/** A convenience mixin trait for forms of reference types using `null` as their `nullValue`.
	  * Implements also the null literal methods to return "null". Note that the latter will be likely inappropriate
	  * for multi-column types!
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue]]
	  */
	trait NullableForm[T >: Null] extends SQLForm[T] {
		override def nullValue :Null = null
		override def nulls :NullValue[T] = NullValue.Null
		override def nullLiteral :String = "null"
		override def inlineNullLiteral :String = "null"
	}



	/** A convenience mixin trait for forms of types which don't have SQL literal representations. These typically
	  * include `Blob` and similar large data types. All literal-related methods of `SQLWriteForm`
	  * throw an `UnsupportedOperationException`.
	  */
	trait NonLiteralForm[T] extends SQLForm[T] {
		override def literal(value: T): String = throw new UnsupportedOperationException(getClass.getName+".literal")
		override def nullLiteral :String = throw new UnsupportedOperationException(getClass.getName+".nullLiteral")
		override def inlineLiteral(value: T): String = throw new UnsupportedOperationException(getClass.getName+".inlineLiteral")
		override def inlineNullLiteral :String = throw new UnsupportedOperationException(getClass.getName+".inlineNullLiteral")
	}


	/** A base class for forms which do not read or write any columns. Read methods always return `nullValue`,
	  * implementation of which is left to the subclass.
	  */
	abstract class AbstractEmptyForm[T] extends SQLForm[T] with EmptyWriteForm[T] {
		override def apply(position: Int)(res: ResultSet): T = nullValue

		def apply(column :String)(res :ResultSet) :T = nullValue

		override def opt(position :Int)(rs :ResultSet) :Option[T] = None

		override def readColumns = 0
	}



	/** A form which does not read or write any columns but always returns the result of evaluating `nullExpr`
	  * from `apply`, `opt` and `nullValue` methods.
	  */
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






	private[schema] class CombinedForm[T](read :SQLReadForm[T], write :SQLWriteForm[T]) extends SQLForm[T] {
		protected def r :SQLReadForm[T] = read
		protected def w :SQLWriteForm[T] = write

		override def writtenColumns: Int = write.writtenColumns
		override def readColumns: Int = read.readColumns

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


		override def equals(that :Any) :Boolean = that match {
			case combine :CombinedForm[_] =>
				(this eq combine) || (combine canEqual this) && combine.r == r && combine.w == w
			case _ => false
		}

		override def hashCode :Int = read.hashCode * 31 + write.hashCode

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


		override def set(position :Int)(statement :PreparedStatement, value :Option[T]) :Unit =
			form.setOpt(position)(statement, value)

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


