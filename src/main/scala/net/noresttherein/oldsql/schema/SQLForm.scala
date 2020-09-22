package net.noresttherein.oldsql.schema

import java.sql.{PreparedStatement, ResultSet}

import net.noresttherein.oldsql.collection.{Chain, ChainMap, IndexedChain, LabeledChain, Record}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.ChainMap.&~
import net.noresttherein.oldsql.collection.IndexedChain.{:~, |~}
import net.noresttherein.oldsql.collection.Record.|#
import net.noresttherein.oldsql.schema.SQLForm.{ChainForm, ChainMapForm, FlatMappedSQLForm, MappedSQLForm, NullValue}
import net.noresttherein.oldsql.schema.SQLReadForm.{ChainIndexReadForm, ChainReadForm, FlatMappedSQLReadForm, LazyReadForm, MappedSQLReadForm, SeqReadForm}
import net.noresttherein.oldsql.schema.SQLWriteForm.{ChainWriteForm, EmptyWriteForm, EvalOrNullWriteForm, FlatMappedSQLWriteForm, GenericChainWriteForm, LazyWriteForm, MappedSQLWriteForm, NonLiteralWriteForm, SeqWriteForm}
import net.noresttherein.oldsql.slang._
import scala.collection.immutable.Seq
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.LabeledChain.>~
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.ScalaForms.Tuple2Form
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.slang






/** Base trait combining companion traits to objects containing implicit form declarations.
  * Extended by both read and write forms, it brings those implicits into the search scope for all form types.
  */
trait SQLForms extends JDBCTypes with ScalaForms with JavaForms with Serializable {

	def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def toString :String = this.innerClassName

}






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
  */ //todo: specialization!
trait SQLForm[T] extends SQLReadForm[T] with SQLWriteForm[T] {

	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values. If the underlying
	  * column(s) is null, implicitly provided 'null' value for type `X` is returned directly from `opt`/`apply`
	  * reading methods without mapping the 'null' value of this type.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`. Will never be called
	  *            for `null` values unless this form returns `Some(null)` from `opt` in a non-standard practice.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement parameters.
	  */
	def bimap[X :NullValue](map :T => X)(unmap :X => T) :SQLForm[X] =
		new MappedSQLForm[T, X](map, unmap)(this, NullValue[X] match {
			case null => nulls.map(map)
			case nulls => nulls
		})



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
	  * The `nullValue` of the new form is the result of mapping this instance's `nulls` with the given function.
	  * Any exceptions thrown in that case will be propagated when the `nullValue` for the new form is called.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement parameters.
	  */
	def nullBimap[X](map :T => X)(unmap :X => T) :SQLForm[X] =
		bimap(map)(unmap)(nulls.map(map))



	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values. If the underlying
	  * column(s) is null or `map` returns `None`, implicitly provided 'null' value for type `X` is returned directly
	  * from `opt`/`apply` reading methods without mapping the 'null' value of this type. Similarly, if `unmap` returns
	  * `None`, the new form will call `setNull` on this instance instead of `set`.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`. Will never be called
	  *            for `null` values unless this form returns `Some(null)` from `opt` in a non-standard practice.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement parameters.
	  */ //todo: rename optMap
	def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
		new FlatMappedSQLForm[T, X](map, unmap)(this, NullValue[X] match {
			case null => nulls.flatMap(map)
			case nulls => nulls
		})


	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values. If the underlying
	  * column(s) is null, or `map` returns `None`, the `nullValue` provided  here is returned directly
	  * from `opt`/`apply` reading methods without mapping the 'null' value of this type. Similarly, if `unmap`
	  * returns `None`, the form will call on this instance `setNull` instead of `set`.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`. Will never be called
	  *            for `null` values unless this form returns `Some(null)` from `opt` in a non-standard practice.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement
	  *              parameters.
	  */
	def biflatMap[X](map :T => Option[X], nullValue :X)(unmap :X => Option[T]) :SQLForm[X] =
		biflatMap(map)(unmap)(NullValue(nullValue))

	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values.
	  * The `nullValue` of the new form is the result of mapping this instance's `nulls` with the given function,
	  * meaning it must handle `null` (or its counterpart for `T`) without throwing an exception. If `map` returns
	  * `None` for `this.nullValue`, a `NoSuchElementException` will be thrown when `nullValue` for the created form
	  * is accessed. Similarly, any exceptions thrown by the `map` function will be propagated.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement
	  *              parameters.
	  */ //todo: rename optMap ?
	def nullBiflatMap[X](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
		biflatMap(map)(unmap)(nulls.flatMap(map))



	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values with the given
	  * extractor pair. If the underlying column(s) is null or `map` returns `None`, implicitly provided 'null' value
	  * for type `X` is returned directly from `opt`/`apply` reading methods without mapping the 'null' value
	  * of this type. Similarly, if `unmap` returns `None`, the new form will call `setNull` on this instance
	  * instead of `set`.
	  * @param map an `Extractor` mapping the result read from the `ResultSet` to the new type `X`. Will never be called
	  *            for `null` values unless this form returns `Some(null)` from `opt` in a non-standard practice.
	  * @param unmap an `Extractor` mapping values of `X` for passing them to this form before setting the statement
	  *              parameters.
	  */
	def as[X :NullValue](map :T =?> X)(unmap :X =?> T) :SQLForm[X] = (map, unmap) match {
		case (_ :IdentityExtractor[_], _:IdentityExtractor[_]) => this.asInstanceOf[SQLForm[X]]
		case (Extractor.Requisite(there), Extractor.Requisite(back)) => bimap(there)(back)
		case _ => biflatMap(map.optional)(unmap.optional)
	}

	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values with the given
	  * extractor pair. If the underlying column(s) is null, or `map` returns `None`, the `nullValue` provided here
	  * is returned directly from `opt`/`apply` reading methods without mapping the 'null' value of this type.
	  * Similarly, if `unmap` returns `None`, the form will call on this instance `setNull` instead of `set`.
	  * @param map an `Extractor` mapping the result read from the `ResultSet` to the new type `X`. Will never be called
	  *            for `null` values unless this form returns `Some(null)` from `opt` in a non-standard practice.
	  * @param unmap an `Extractor` mapping values of `X` for passing them to this form before setting the statement
	  *              parameters.
	  */
	def as[X](map :T =?> X, nullValue :X)(unmap :X =?> T) :SQLForm[X] =
		as(map)(unmap)(NullValue(nullValue))

	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values with the given
	  * extractor pair. The `nullValue` of the new form is the result of mapping this instance's `nulls` with the `map`
	  * extractor, meaning it must handle `null` (or its counterpart for `T`) without throwing an exception.
	  * If `map` returns `None` for `this.nullValue`, a `NoSuchElementException` will be thrown when `nullValue`
	  * of the created form is accessed. Similarly, any exceptions thrown by the `map` extractor will be propagated.
	  * @param map an `Extractor` mapping the result read from the `ResultSet` to the new type `X`.
	  * @param unmap an `Extractor` mapping values of `X` for passing them to this form before setting the statement
	  *              parameters.
	  */
	def nullAs[X](map :T =?> X)(unmap :X =?> T) :SQLForm[X] =
		as(map)(unmap)(nulls.extract(map))


	/** Lifts this form to represent `Option[T]`. The created form maps all values returned by this form using
	  * `Option(...)`. This means that `null` values (actual JVM nulls, not the somewhat arbitrary value provided
	  * by this form) are mapped to `Some(None)`, while a returned `None` indicates that this form returned `None`.
	  * Basically this means that the returned form uses this form's `opt` method as its `apply` implementation.
	  */
	override def toOpt :SQLForm[Option[T]] = ScalaForms.OptionForm(this)


	def *[O](other :SQLForm[O]) :SQLForm[(T, O)] = Tuple2Form(this, other)



	def compatible(other :SQLForm[_]) :Boolean = this == other

}






sealed trait SQLFormLevel2Implicits {
	implicit def ChainForm[I <: Chain, L](implicit i :SQLForm[I], l :SQLForm[L]) :SQLForm[I ~ L] =
		new ChainForm(i, l)
}

sealed trait SQLFormLevel1Implicits extends SQLFormLevel2Implicits {
	implicit def ChainMapForm[I <: ChainMap :SQLForm, K <: Singleton :ValueOf, V :SQLForm] :SQLForm[I &~ (K, V)] =
		new ChainMapForm(SQLForm[I], valueOf[K], SQLForm[V])
}






object SQLForm extends SQLFormLevel1Implicits {

	/** Summon an implicit instance of `SQLForm[T]`. */
	def apply[T :SQLForm] :SQLForm[T] = implicitly[SQLForm[T]]



	/** Creates an `SQLForm` delegating all calls to the implicitly provided read and write forms. */
	def combine[T](implicit read :SQLReadForm[T], write :SQLWriteForm[T]) :SQLForm[T] =
		new CombinedForm[T](read, write)



	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
	  * and returns `None`/`nulls.value` when reading.
	  */
	def nulls[T](implicit write :SQLWriteForm[T], nulls :NullValue[T]) :SQLForm[T] =
		combine(SQLReadForm.nulls[T](write.writtenColumns), SQLWriteForm.none[T])

	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
	  * and returns `None` when reading. All calls to `apply` will result in a `NullPointerException`.
	  */
	def none[T](implicit write :SQLWriteForm[T]) :SQLForm[T] =
		combine(SQLReadForm.none[T](write.writtenColumns), SQLWriteForm.none[T])



	/** Creates a dummy form which always returns and writes the same value. */
	def opt[T](value :Option[T])(implicit write :SQLWriteForm[T], nulls :NullValue[T]) :SQLForm[T] =
		combine(SQLReadForm.opt(value, write.writtenColumns), SQLWriteForm.opt(value))

	/** Creates a dummy form which always returns and writes the same value. */
	def const[T](value :T)(implicit write :SQLWriteForm[T]) :SQLForm[T] =
		combine(SQLReadForm.const(value, write.writtenColumns), SQLWriteForm.const(value))



	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression.
	  */
	def evalopt[T](value: => Option[T])(implicit write :SQLWriteForm[T], nulls :NullValue[T]) :SQLForm[T] =
		combine(SQLReadForm.evalopt(value, write.writtenColumns), new EvalOrNullWriteForm[T](value)(write, nulls))

	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression.
	  */
	def eval[T](value: => T)(implicit write :SQLWriteForm[T]) :SQLForm[T] =
		combine(SQLReadForm.eval(value, write.writtenColumns), SQLWriteForm.eval(value))



	/** Creates a proxy form which will delegate all methods to another form, returned by the given by-name argument.
	  * The expression is not evaluated until the form is actually needed. All mapping methods map this instance
	  * if the backing form expression has not been evaluated, and defer to the backing form it has been computed
	  * (essentially shedding the lazy proxy layer). The expression may be evaluated more than once if several
	  * threads trigger the its initialization, but the created form is thread safe.
	  */
	def Lazy[T](init: => SQLForm[T]) :SQLForm[T] = new LazyForm[T](() => init)



	def flatMap[S, T](map :S => Option[T])(unmap :T => Option[S])
	                 (implicit source :SQLForm[S], nulls :NullValue[T] = null) :SQLForm[T] =
		source.biflatMap(map)(unmap)

	def map[S, T](map :S => T)(unmap :T => S)(implicit source :SQLForm[S], nulls :NullValue[T] = null) :SQLForm[T] =
		source.bimap(map)(unmap)

	def apply[S, T](map :S =?> T)(unmap :T =?> S)(implicit source :SQLForm[S], nulls :NullValue[T] = null) :SQLForm[T] =
		source.as(map)(unmap)






	/** An empty chain form for the `Chain` terminator `@~`, which doesn't write or read any columns. */
	implicit val EmptyChainForm :SQLForm[@~] = new EmptyForm[@~](@~) {
		override def equals(that :Any) :Boolean = that.getClass == getClass
		override def hashCode :Int = getClass.hashCode
		override def toString = "@~"
	}

	implicit def IndexedChainFrom[I <: IndexedChain :SQLForm, K :ValueOf, V :SQLForm] :SQLForm[I |~ (K :~ V)] =
		new IndexedChainForm(SQLForm[I], valueOf[K], SQLForm[V])

	implicit def LabeledChainForm[I <: LabeledChain :SQLForm, K <: Label :ValueOf, V :SQLForm] :SQLForm[I >~ (K :~ V)] =
		new LabeledChainForm(SQLForm[I], valueOf[K], SQLForm[V])

	implicit def RecordForm[I <: Record :SQLForm, K <: Label :ValueOf, V :SQLForm] :SQLForm[I |# (K, V)] =
		new RecordForm(SQLForm[I], valueOf[K], SQLForm[V])






	implicit class ChainFormConstructor[I <: Chain](private val self :SQLForm[I]) extends AnyVal {
		def ~[L](implicit next :SQLForm[L]) :SQLForm[I ~ L] = ChainForm(self, next)
	}






	/** A type class providing a 'null' value for type `T`. This is the value which should be used by a form when
	  * the underlying column(s) is null. This may be any default value, not only `null` or 'zero';
	  * for example, a collection type can return an empty collection. It can also be used to enforce
	  * not-null semantics by throwing an exception as in `NullValue.NotNull`.
	  */
	trait NullValue[+T] extends Serializable {

		/** Value to which null columns in the `ResultSet` are mapped.
		  * @throws NullPointerException if the particular form for type `T` prohibits null values.
		  */
		def value :T

		/** Returns the `value` inside a `Some` unless it would throw an exception, in which case `None` is returned. */
		def toOption :Option[T] = Option(value)

		/** Adapt this null value to some other type `U`. In most cases, this will simply apply the function to
		  * the wrapped value, but special instances may propagate themselves instead. If the function throws
		  * an exception for the `value`, it will be swallowed and a `NullValue` instance which reevaluates
		  * `map(this.value)` at each access will be returned.
		  */
		def map[U](f :T => U) :NullValue[U]

		/** Adapt this null value to some other type `U`. In most cases, this will simply apply the function to
		  * the wrapped value, but special instances may propagate themselves instead. If the function throws
		  * an exception for the `value`, it will be swallowed and a `NullValue` instance which reevaluates
		  * `map(this.value)` at each access will be returned. Returning `None` by the function has the same
		  * effect as throwing a `NoSuchElementException`, which will be throw by the returned `NullValue` instead
		  * of letting it out of this method.
		  */
		def flatMap[U](f :T => Option[U]) :NullValue[U] =
			map(tnull => f(tnull) getOrElse {
			 	throw new NoSuchElementException("No corresponding null value for " + tnull + " of " + this)
			})

		/** Adapt this null value to some other type `U`. In most cases, this will simply apply the function to
		  * the wrapped value, but special instances may propagate themselves instead. If the function throws
		  * an exception for the `value`, it will be swallowed and a `NullValue` instance which reevaluates
		  * `map(this.value)` at each access will be returned. Returning `None` by the function has the same
		  * effect as throwing a `NoSuchElementException`, which will be throw by the returned `NullValue` instead
		  * of letting it out of this method.
		  */
		def extract[U](f :T =?> U) :NullValue[U] = f match {
			case _ :EmptyExtractor[_, _] => NullValue.NotNull
			case _ :OptionalExtractor[_, _] => flatMap(f.optional)
			case _ :IdentityExtractor[_] => this.asInstanceOf[NullValue[U]]
			case const :ConstantExtractor[_, U @unchecked] => NullValue(const.constant)
			case _ :RequisiteExtractor[_, _] => map(f.requisite.get)
			case _ => flatMap(f.optional)
		}

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
		def apply[T](sqlNull :T) :NullValue[T] = new ArbitraryNullValue(sqlNull)



		/** Create a new instance which evaluates the given expression each time its `value` method is called.
		  * While returning different values for different calls is strongly discouraged, this allows to provide
		  * an expression which throws an exception or references a not initialized as of yet value.
		  */
		def eval[T](whenNull: =>T) :NullValue[T] = new NullValue[T] {
			override def value :T = whenNull
			override def map[U](f :T => U) = eval(f(whenNull))
			override def toString = "Null(?)"
		}

		/** A `NullValue` instance which always throws a `NullPointerException`. Used with forms for types which
		  * don't accept null values or simply wish to forbid them.
		  */
		final val NotNull :NullValue[Nothing] = new NotNull
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



		private class ArbitraryNullValue[T](override val value :T) extends NullValue[T] {

			override def map[U](f :T => U) :NullValue[U] = try {
				NullValue(f(value))
			} catch {
				case _ :Exception => NullValue.eval(f(value))
			}

			override def equals(that :Any) :Boolean = that match {
				case nulls :ArbitraryNullValue[_] => (nulls eq this) || nulls.value == value
				case _ => false
			}

			override def hashCode :Int = if (value == null) 0 else value.hashCode

			override def toString :String = "Null(" + value + ")"
		}



		private class NotNull extends NullValue[Nothing] {
			override def value = throw new NullPointerException("This type does not allow null values.")
			override def toOption = scala.None
			override def map[U](f :Nothing => U) :NullValue[U] = this

			override def equals(that :Any) :Boolean = that.isInstanceOf[NotNull]
			override def hashCode :Int = getClass.hashCode

			override def toString = "NotNull"
		}

	}





	/** A convenience mixin trait for forms of reference types using `null` as their `nullValue`.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue]]
	  */
	trait NullableForm[T >: Null] extends SQLForm[T] {
		override def nullValue :Null = null
		override def nulls :NullValue[T] = NullValue.Null
	}



	/** A convenience mixin trait for forms of types which don't have SQL literal representations. These typically
	  * include `Blob` and similar large data types. All literal-related methods of `SQLWriteForm`
	  * throw an `UnsupportedOperationException`.
	  */
	trait NonLiteralForm[T] extends SQLForm[T] with NonLiteralWriteForm[T]



	/** A Convenience base `SQLReadForm[T]` class which implements `nullValue` based on an implicit `NullValue[T]`
	  * (overriding also `nulls` in the process). */
	abstract class AbstractForm[T](implicit override val nulls :NullValue[T])
		extends SQLForm[T]
	{
		override def nullValue :T = nulls.value
	}


	/** A convenience base `SQLForm` class relying on implicit `NullValue[T]` as well as `ClassTag[T]`
	  * for its `toString` implementation.
	  */
	abstract class ReifiedForm[T](implicit override val nulls :NullValue[T], clazz :ClassTag[T])
		extends SQLForm[T]
	{
		override def nullValue :T = nulls.value

		def runtimeClass :Class[_] = clazz.runtimeClass

		override def equals(that :Any) :Boolean = that match {
			case same :ReifiedForm[_] => (same eq this) || (same canEqual this) && same.runtimeClass == runtimeClass
			case _ => false
		}

		override def hashCode :Int = clazz.runtimeClass.hashCode

		override def toString :String = slang.innerClassName(clazz.runtimeClass)
	}


	/** A base class for forms which do not read or write any columns. Read methods always return `nullValue`,
	  * implementation of which is left to the subclass.
	  */
	abstract class AbstractEmptyForm[T] extends SQLForm[T] with EmptyWriteForm[T] {
		override def apply(position: Int)(res: ResultSet): T = nullValue

		override def opt(position :Int)(rs :ResultSet) :Option[T] = None

		override def readColumns = 0

		override def toString = "EMPTY"
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






	class UnknownForm[T] extends EmptyForm[T](throw new UnsupportedOperationException("SQLForm.UnknownForm")) {
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
		override def nulls :NullValue[T] = read.nulls

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

		override def toString = s"($read<>$write)"
	}






	class FlatMappedSQLForm[S, T](map :S => Option[T], val unmap :T => Option[S])
	                             (implicit override val source :SQLForm[S], nulls :NullValue[T])
		extends FlatMappedSQLReadForm[S, T](map) with FlatMappedSQLWriteForm[S, T] with SQLForm[T]
	{
		override def bimap[X :NullValue](map :T => X)(unmap :X => T) :SQLForm[X] =
			source.biflatMap(this.map(_).map(map))(unmap andThen this.unmap)

		override def toString = s"<=$source=>"
	}



	class MappedSQLForm[S, T](map :S => T, val unmap :T => S)
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
			form.bimap(map)(unmap)

		override def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
			form.biflatMap(map)(unmap)

		override def *[O](other :SQLForm[O]) :SQLForm[(T, O)] = form * other

		override def toString :String = if (isInitialized) form.toString else "<Lazy>"
	}






	private case class SeqForm[T](forms :Seq[SQLForm[T]]) extends SQLForm[Seq[T]] with SeqWriteForm[T] with SeqReadForm[T] {
		override def toString :String = forms.mkString("Seq(",",",")")
	}



	private[schema] class ChainForm[I <: Chain, L](override val init :SQLForm[I], override val last :SQLForm[L])
		extends ChainWriteForm(init, last) with ChainReadForm[I, L] with SQLForm[I ~ L]
	{
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ChainForm[_, _]]

		override def toString :String = super[ChainWriteForm].toString
	}


	private class IndexedChainForm[I <: IndexedChain, K <: IndexedChain.Key, V]
	                              (override val init :SQLForm[I], override val key :K, override val value :SQLForm[V])
		extends GenericChainWriteForm[|~, I, K :~ V, V](init, value.unmap(_.value), value, "|~")
		   with ChainIndexReadForm[|~, :~, IndexedChain.Key, I, K, V] with SQLForm[I |~ (K :~ V)]
	{
		protected[this] override def cons(init :I, value :V) :I |~ (K :~ V) = init |~ (key :~ value)
	}

	private class LabeledChainForm[I <: LabeledChain, K <: LabeledChain.Key, V]
	                              (override val init :SQLForm[I], override val key :K, override val value :SQLForm[V])
		extends GenericChainWriteForm[>~, I, K :~ V, V](init, value.unmap(_.value), value, ">~")
		   with ChainIndexReadForm[>~, :~, LabeledChain.Key, I, K, V] with SQLForm[I >~ (K :~ V)]
	{
		protected[this] override def cons(init :I, value :V) :I >~ (K :~ V) = init >~ (key :~ value)
	}


	private[schema] class ChainMapForm[I <: ChainMap, K <: ChainMap.Key, V]
	                          (override val init :SQLForm[I], override val key :K, override val value :SQLForm[V])
		extends GenericChainWriteForm[&~, I, (K, V), V](init, value.unmap(_._2), value, "&~")
		   with ChainIndexReadForm[&~, Tuple2, ChainMap.Key, I, K, V] with SQLForm[I &~ (K, V)]
	{
		override protected[this] def cons(init :I, value :V) :I &~ (K, V) = init &~ (key -> value)
	}


	private class RecordForm[I <: Record, K <: Record.Key, V]
	                        (override val init :SQLForm[I], override val key :K, override val value :SQLForm[V])
		extends GenericChainWriteForm[|#, I, (K, V), V](init, value.unmap(_._2), value, "|#")
		   with ChainIndexReadForm[|#, Tuple2, Record.Key, I, K, V] with SQLForm[I |# (K, V)]
	{
		override protected[this] def cons(init :I, value :V) :I |# (K, V) = init |# key -> value
	}



}


