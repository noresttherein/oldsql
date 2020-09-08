package net.noresttherein.oldsql.schema

import java.sql.{ResultSet, SQLException}

import net.noresttherein.oldsql.collection.{Chain, ChainMap, IndexedChain, LabeledChain, Record}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.ChainMap.&~
import net.noresttherein.oldsql.collection.IndexedChain.{:~, |~}
import net.noresttherein.oldsql.collection.Record.|#
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.{AbstractChainIndexReadForm, BaseChainReadForm, FallbackReadForm, FlatMappedSQLReadForm, MappedSQLReadForm}
import scala.annotation.tailrec
import scala.collection.immutable.Seq

import net.noresttherein.oldsql.collection.LabeledChain.>~
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label

//implicits
import net.noresttherein.oldsql.slang._




/** Encapsulates the logic of reading and building the value of `T` from a number of consecutive columns
  * in a `ResultSet`. Both the construction process and, in particular, the number of read columns should be fixed,
  * providing a pure contract. This makes it a lower-level counterpart of [[net.noresttherein.oldsql.schema.Mapping]],
  * which can be represented by possibly many forms, depending on the column list included in the selection.
  * All implementations must be thread safe.
  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm]]
  * @see [[net.noresttherein.oldsql.schema.SQLForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnReadForm]]
  */
trait SQLReadForm[+T] extends SQLForms {

	/** Reads the column values from columns `&lt;position..position + this.readColumns)` of the passed `ResultSet`
	  * and creates an instance of `T`. The default implementation delegates to `opt` and fallbacks to `nullValue`
	  * if no value is available, but the exact handling is completely up to the implementation. Note that `null` column
	  * values may happen even on not-null database columns in outer joins.
	  * @throws NoSuchElementException if the value cannot be assembled, typically because the indicated columns are null;
	  *                                this is different in intent from the `NullPointerException` case
	  *                                as it is used primarily for multi-column forms, when the missing values are
	  *                                a likely the result of an outer join or subclass columns in a
	  *                                table per class hierarchy mapping.
	  * @throws NullPointerException if the read column is `null` and type `T` does not define a value corresponding to `null`.
	  * @throws SQLException if any of the columns cannot be read, either due to connection error or it being closed.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#opt opt]]
	  */
	def apply(position :Int)(res :ResultSet) :T = opt(position)(res) match {
		case Some(x) => x
		case _ => nullValue
	}

	/** Attempts to read the column values from columns `&lt;position..position + this.readColumns` of the passed
	  * `ResultSet` and create an instance of `T`. If the values are unavailable (required columns carry `null` values),
	  * `None` is returned. It is the recommended practice to have the returned option reflect only the availability
	  * of the input values and not their validity. It is allowed for the form to return `Some(null)`
	  * (as long as `T >: Null`), but this results in propagation of `null` values to any forms derived from it
	  * and to the application. As such, it is discouraged, with the exception of `ColumnForm`s, as the validity of
	  * `null` values can be explicitly switched on for columns using the `Nullable` buff (and otherwise affected
	  * by other buffs). The `ColumnMapping` trait explicitly checks for `null` values throwing a `NullPointerException`
	  * if they are not permitted. While not strictly required, the form should
	  * throw an exception if the values do not conform to expected constraints or the assembly process fails
	  * for any other reason. Similarly, all thrown `SQLException`s are propagated.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]]
	  */
	def opt(position :Int)(res :ResultSet) :Option[T]

	/** The value a `null` column (or all `null` columns) should be mapped to. It is used in particular by `apply`
	  * when the value is unavailable, for example as a result of an outer join. Extending classes are allowed
	  * to throw an exception here (either a `NoSuchElementException` or a `NullPointerException`) if a concept
	  * of null does not exist for `T` or `null`s are not acceptable values. It is however completely acceptable
	  * to provide any particular value of `T` as a part of the mapping process (for example, a form for `Option[T]`
	  * will return `None`).
	  */
	def nullValue :T

	/** Null representation for type `T`. Wrapped value is used by the form when the mapped column(s) is null.
	  * It should be consistent with `this.nullValue` and the default implementation simply delegates to it at each call.
	  * This wrapper can be implicitly passed as a type class, even if the form does not support `null` values
	  * (i.e. `nullValue` throws an exception), which would not be possible by simply using `nullValue`.
	  */
	implicit def nulls :NullValue[T] = new DerivedNullValue

	private class DerivedNullValue extends NullValue[T] {
		private def outer :SQLReadForm[T] = SQLReadForm.this

		override def value :T = nullValue
		override def map[U](f :T => U) = NullValue.eval(f(nullValue))

		override def equals(that :Any) :Boolean = that match {
			case formNull :SQLReadForm[_]#DerivedNullValue => formNull.outer eq SQLReadForm.this
			case _ => false
		}

		override def hashCode :Int = System.identityHashCode(SQLReadForm.this)

		override def toString = s"$outer.nullValue"
	}



	/** Number of columns read by this form. This must be a constant as it is typically is used to calculate offsets
	  * for various forms once per `ResultSet` rather than per row. Naturally, there is no requirement for actual
	  * reading of all columns if the form can determine based on some indicator (such as a `null` primary key) that
	  * the value cannot be assembled from the given column set for the row.
	  */
	def readColumns :Int


	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. This method variant
	  * depends on implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] to provide the value of `X`
	  * to be used when `null` value(s) are read from the `ResultSet`. This guarantees that the given function will
	  * not be called for `null` arguments unless this form returns `Some(null)` from its `opt` method in a non-standard
	  * practice and allows handling of `null` values also when `T` is a value type without a natural `null` value.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#nullMap]]
	  */
	def map[X :NullValue](fun :T => X) :SQLReadForm[X] = NullValue[X] match {
		case null => new MappedSQLReadForm(fun)(this, nulls.map(fun))
		case nulls => new MappedSQLReadForm(fun)(this, nulls)
	}

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. If the underlying columns
	  * carry null values, passed `nullValue` is used instead of mapping. This guarantees that the given function will
	  * not be called for `null` arguments unless this form returns `Some(null)` from its `opt` method in a non-standard
	  * practice and allows handling of `null` values also when `T` is a value type without a natural `null` value.
	  * Note that `this.nullValue` is never called, directly or indirectly, by the created form.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#nullMap]]
	  */
	def map[X](fun :T => X, nullValue :X) :SQLReadForm[X] = map(fun)(NullValue(nullValue))

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. Note that the given
	  * function may be called for `null` arguments, even if the underlying columns have a ''not null'' constraint
	  * in case of outer join queries.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#map]]
	  */
	def nullMap[X](fun :T => X) :SQLReadForm[X] = map(fun)(nulls.map(fun))




	/** Attempts to map the value of `T` read by this form to type `X` in order to produce a form for `X`.
	  * Unlike `map`, not all values of `T` may have an associated counterpart in `X`, in which case the given function
	  * returns `None` and the new form defaults to its `nullValue`. This method variant relies on implicit
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] to provide the value of `X` which should be used
	  * when the underlying columns carry null values or a value of `X` cannot be assembled due to anticipated reasons.
	  * If the value cannot be assembled due to unforeseen circumstances or data errors, the form should
	  * throw an exception. The function is guaranteed not to be called for `null` arguments, with the created form
	  * using the implicit null value instead, unless this form returns `Some(null)` from its `opt` method
	  * in a non-standard practice. Note that `this.nullValue` is never called, directly or indirectly,
	  * by the created form.
	  */
	def flatMap[X :NullValue](fun :T => Option[X]) :SQLReadForm[X] =
		new FlatMappedSQLReadForm(fun)(this, NullValue[X] match {
			case null => nulls.flatMap(fun)
			case nulls => nulls
		})

	/** Attempts to map the value of `T` read by this form to type `X` in order to produce a form for `X`.
	  * Unlike `map`, not all values of `T` may have an associated counterpart in `X`, in which case the given function
	  * returns `None` and the new form defaults to the given `nullValue`.  The function is guaranteed not to be called
	  * for `null` arguments, with the created form using `nullValue` instead, unless this form returns `Some(null)`
	  * from its `opt` method in a non-standard practice. Note that `this.nullValue` is never called,
	  * directly or indirectly, by the created form.
	  */
	def flatMap[X](fun :T => Option[X], nullValue :X) :SQLReadForm[X] = flatMap(fun)(NullValue(nullValue))

	/** Attempts to map the value of `T` read by this form to type `X` in order to produce a form for `X`.
	  * Unlike `map`, not all values of `T` may have an associated counterpart in `X`, in which case the given function
	  * returns `None` and the new form defaults to the given `nullValue`. The `nullValue` for the new form
	  * is determined by applying the given function to the `nullValue` of this form. If the function returns `None`,
	  * a `NoSuchElementException` is thrown when the new form's `nullValue` method is called. If the function throws
	  * a `NullPointerException` or `NoSuchElementException` for the null value, the same exception will be thrown
	  * from the `nullValue` method.
	  */
	def nullFlatMap[X](fun :T => Option[X]) :SQLReadForm[X] = flatMap(fun)(nulls.flatMap(fun))



	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. The returned form will
	  * use the implicitly available `nullValue` except for when `fun` is an identity extractor, in which case
	  * this instance will be returned. This will investigate the type of the extractor and either delegate to `map`
	  * or `flatMap` or return a specific form in corner cases.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#flatMap]]
	  */
	def to[X :NullValue](fun :T =?> X) :SQLReadForm[X] = fun match {
		case _ :EmptyExtractor[_, _] => SQLReadForm.nulls(readColumns)
		case _ :OptionalExtractor[_, _] => flatMap(fun.optional)
		case _ :IdentityExtractor[_] => this.asInstanceOf[SQLReadForm[X]]
		case const :ConstantExtractor[_, X @unchecked] => SQLReadForm.const(const.constant, readColumns)
		case req :RequisiteExtractor[T @unchecked, X @unchecked] => map(req.getter)
		case _ => flatMap(fun.optional)
	}

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. The returned form will
	  * use the given `nullValue` except for when `fun` is an identity extractor, in which case this instance
	  * will be returned. This will investigate the type of the extractor and either delegate to `map` or `flatMap` or
	  * return a specific form in corner cases.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#flatMap]]
	  */
	def to[X](f :T =?> X, nullValue :X) :SQLReadForm[X] = to(f)(NullValue(nullValue))

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`.
	  * This will call `nullMap` or `nullFlatMap` based on whether the extract is a `RequisiteExtractor`.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#nullMap]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#nullFlatMap]]
	  */
	def nullTo[X](f :T =?> X) :SQLReadForm[X] = to(f)(nulls.extract(f))



	/** Lifts this form to represent `Option[T]`. The created form maps all values returned by this form using
	  * `Option(...)`. This means that `null` values (actual JVM nulls, not the somewhat arbitrary value provided
	  * by this form) are mapped to `Some(None)`, while a returned `None` indicates that this form returned `None`.
	  * Basically this means that the returned form uses this form's `opt` method as its `apply` implementation.
	  */
	def toOpt :SQLReadForm[Option[T]] = SQLReadForm.OptionReadForm(this)



	/** Chains a default form to fallback to if this form was unable to produce a value by its `opt` method.
	  * The fallback form must use the same number of columns as this form.
	  * @throws IllegalArgumentException if this.readColumns != fallback.readColumns
	  * @return a form for `S` which defines its `opt` method as `this.opt(...) orElse fallback.opt(...)`.
	  */
	def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] =
		if (readColumns != fallback.readColumns && fallback.readColumns != 0)
			throw new IllegalArgumentException(
				s"$this orElse $fallback: different number of read columns ($readColumns vs ${fallback.readColumns})."
			)
		else
			new FallbackReadForm(this, fallback)



	/** Combines this form with another form, which columns are expected to directly follow the columns for this
	  * form in the result set, to create a form producing pairs of values.
	  */
	def *[O](other :SQLReadForm[O]) :SQLReadForm[(T, O)] = SQLReadForm.Tuple2ReadForm(this, other)

	/** Combines this form with a `SQLWriteForm` to create a read/write `SQLForm[O]`. */
	def <>[O >: T](write :SQLWriteForm[O]) :SQLForm[O] = SQLForm.combine[O](this, write)



	def compatible(other :SQLReadForm[_]) :Boolean = this == other

}






sealed trait SQLReadFormLevel2Implicits {
	/** Provides an implicit form for the heterogeneous list (`Chain`) `I ~ L` as long as implicit forms for both
	  * `I` and `L` are available. */
	implicit def ChainReadForm[I <: Chain, L](implicit i :SQLReadForm[I], l :SQLReadForm[L]) :SQLReadForm[I ~ L] =
		new BaseChainReadForm[I, L](i, l)
}



sealed trait SQLReadFormLevel1Implicits extends SQLReadFormLevel2Implicits {
	/** Provides an implicit form for the heterogeneous map indexed by literal types (`ChainMap`) `I &~ L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def ChainMapReadForm[I <: ChainMap :SQLReadForm, K <: Singleton :ValueOf, V :SQLReadForm]
			:SQLReadForm[I &~ (K, V)] =
		new AbstractChainIndexReadForm[&~, Tuple2, Singleton, I, K, V] {
			protected[this] override def cons(init :I, value :V) = init &~ (key -> value)
			protected override def symbol = "&~"
		}
}






object SQLReadForm extends ScalaReadForms with SQLReadFormLevel1Implicits {

	/** Summons an implicit `SQLReadForm[T].` */
	def apply[T :SQLReadForm] :SQLReadForm[T] = implicitly[SQLReadForm[T]]



	/** Creates a new `SQLReadForm` using the given function to read the values from the result set.
	  * The function is responsible for handling null columns itself.
	  * @param columns the number of columns read by the form.
	  * @param read a function taking an SQL `ResultSet`, a start column position, and reads the following
	  *             `columns` columns to create a value of `T`.
	  */
	def apply[T :NullValue](columns :Int)(read :(ResultSet, Int) => T) :SQLReadForm[T] =
		new AbstractReadForm[T] {
			override def apply(position :Int)(res :ResultSet) = read(res, position)
			override def opt(position :Int)(res :ResultSet) = Option(read(res, position))

			override val readColumns = columns

			override def toString = "CustomReadForm@" + System.identityHashCode(this)
		}

	/** Creates a new `SQLReadForm` using the given function to read an optional value from the result set.
	  * The function is responsible for handling null columns itself.
	  * @param columns the number of columns read by the form.
	  * @param read a function taking an SQL `ResultSet`, a start column position, and reads the following
	  *             `columns` columns to create a value of `T`.
	  */
	def opt[T :NullValue](columns :Int)(read :(ResultSet, Int) => Option[T]) :SQLReadForm[T] =
		new AbstractReadForm[T] {
			override def opt(position :Int)(res :ResultSet) = read(res, position)

			override val readColumns = columns
			override def toString = "CustomReadForm@" + System.identityHashCode(this)
		}




	/** Creates a dummy form of zero column size which produces no value on every read.
	  * The `opt` method will always return `None`, while `apply` will return the implicitly available `NullValue[T]`.
	  */
	def nulls[T :NullValue] :SQLReadForm[T] = new NullReadForm[T]

	/** Creates a dummy form of the given column size which produces no value on every read.
	  * The `opt` method will always return `None`, while `apply` will return the implicitly available `NullValue[T]`.
	  */
	def nulls[T :NullValue](readColumns :Int) :SQLReadForm[T] = new NullReadForm[T](readColumns)

	/** Creates a dummy form of zero column size which produces no value on every read.
	  * The `opt` method will always return `None`, while `apply` (and `nullValue`) will throw a `NullPointerException`.
	  */
	def none[T] = new NullReadForm[T]()(NullValue.NotNull)

	/** Creates a dummy form of the given column column size which produces no value on every read.
	  * The `opt` method will always return `None`, while `apply` (and `nullValue`) will throw a `NullPointerException`.
	  */
	def none[T](readColumns :Int) = new NullReadForm[T](readColumns)(NullValue.NotNull)



	/** Creates a form reading the given number of columns and always returning the provided value from its `opt` method.
	  * If `value` is `None`, the implicit `NullValue[T]` is used by the form's `apply` method to stand in.
	  */
	def opt[T :NullValue](value :Option[T], readColumns :Int = 0) :SQLReadForm[T] =
		new ConstReadForm(value, readColumns)

	/** Creates a form reading zero columns and always returning the provided value from its `opt` method.
	  * If `value` is `None`, the `apply` method will return the provided `nullValue` instead.
	  */
	def opt[T](value :Option[T], nullValue :T) :SQLReadForm[T] =
		new ConstReadForm(value)(NullValue(nullValue))

	/** Creates a form reading the given number columns and always returning `Some(value)` from its `opt` method.
	  * The `nullValue` of the created form, while largely irrelevant, is likewise defined as `value`.
	  * Note that if `value` is `null`, it will be treated as a valid return value, rather than 'no value'
	  * by the caller and may result in `NullPointerException` exceptions higher up the call stack.
	  */
	def const[T](value :T, readColumns :Int = 0) :SQLReadForm[T] =
		new ConstReadForm(Some(value), readColumns)(NullValue(value))

	/** Creates a form reading the given number of columns and always returning from its `opt` method the value obtained
	  * by reevaluating the given expression. An implicitly provided null value is used by its `nullValue` method,
	  * to which the `apply` method delegates when the former yields `None`. The expression must be thread safe.
	  */
	def evalopt[T :NullValue](value : => Option[T], readColumns :Int = 0) :SQLReadForm[T] =
		new EvalReadForm(value, readColumns)

	/** Creates a form reading zero columns and always returning from its `opt` method the value obtained
	  * by reevaluating the given expression. An explicitly provided null value is used by its `nullValue` method,
	  * to which the `apply` method delegates when the former yields `None`. The expression must be thread safe.
	  */
	def eval[T](value: => Option[T], nullValue :T) :SQLReadForm[T] = new EvalReadForm(value)(NullValue(nullValue))

	/** Creates a form reading the given number of columns and always returning from its `apply` method the value
	  * obtained by reevaluating the given expression. The result of `opt` is defined as `Some(value)`.
	  * The expression must be thread safe.
	  */
	def eval[T](value: => T, readColumns :Int = 0) :SQLReadForm[T] =
		new EvalReadForm(Some(value), readColumns)(NullValue.eval(value))


	/** A form always throwing the given exception. This functions the same as `eval`, but can more clearly define intent. */
	def error(value: => Nothing, readColumns :Int = 0) :SQLReadForm[Nothing] = eval(value, readColumns)

	/** A  form which throws an `UnsupportedOperationException` with the given message with every read attempt. */
	def unsupported(message :String, readColumns :Int = 0) :SQLReadForm[Nothing] =
		error(throw new UnsupportedOperationException(message), readColumns)



	/** Creates a proxy form which will delegate all methods to another form, returned by the given by-name argument.
	  * The expression is not evaluated until the form is actually needed. All mapping methods map this instance
	  * if the backing form expression has not been evaluated, and defer to the backing form it has been computed
	  * (essentially shedding the lazy proxy layer). The expression may be evaluated more than once if several
	  * threads trigger the its initialization, but the created form is thread safe.
	  */
	def Lazy[T](form : => SQLReadForm[T]) :SQLReadForm[T] = new LazyReadForm(() => form)



	/** Maps the result of reading `S` with an implicit form to `T` using the specified `Extractor`. */
	def apply[S, T](map :S =?> T)(implicit source :SQLReadForm[S], nulls :NullValue[T] = null) :SQLReadForm[T] =
		source.to(map)(if (nulls == null) source.nulls.extract(map) else nulls)

	/** Maps the result of reading `S` with an implicit form to `T` using the specified `Extractor`. */
	def apply[S :SQLReadForm, T](map :S =?> T, nullValue: => T) :SQLReadForm[T] =
		apply(map)(SQLReadForm[S], NullValue.eval(nullValue))

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def map[S, T](map :S => T)(implicit source :SQLReadForm[S], nulls :NullValue[T] = null) :SQLReadForm[T] =
		source.map(map)(if (nulls == null) source.nulls.map(map) else nulls)

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def map[S :SQLReadForm, T](map :S => T, nullValue: => T) :SQLReadForm[T] =
		this.map(map)(SQLReadForm[S], NullValue.eval(nullValue))

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def flatMap[S, T](map :S => Option[T])(implicit source :SQLReadForm[S], nulls :NullValue[T] = null) :SQLReadForm[T] =
		source.flatMap(map)(if (nulls == null) source.nulls.flatMap(map) else nulls)

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def flatMap[S :SQLReadForm, T](map :S => Option[T], nullValue: => T) :SQLReadForm[T] =
		flatMap(map)(SQLReadForm[S], NullValue.eval(nullValue))



	def seq[T](forms :Seq[SQLReadForm[T]]) :SQLReadForm[Seq[T]] = new SeqReadFormImpl[T](forms)






	/** An implicit value for the empty chain `@~`, which reads zero columns and simply returns `@~`. */
	implicit val EmptyChainReadForm :SQLReadForm[@~] = SQLForm.EmptyChainForm

	/** Provides an implicit form for the heterogeneous map indexed by types (`IndexedChain`) `I |~ L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def IndexedChainReadFrom[I <: IndexedChain :SQLReadForm, K <: IndexedChain.Key :ValueOf, V :SQLReadForm]
			:SQLReadForm[I |~ (K :~ V)] =
		new AbstractChainIndexReadForm[|~, :~, IndexedChain.Key, I, K, V] {
			protected[this] override def cons(init :I, value :V) = init |~ key :~ value
			protected override def symbol = "|~"
		}


	/** Provides an implicit form for the heterogeneous map indexed by string literal types (`LabeledChain`) `I >~ L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def LabeledChainReadFrom[I <: LabeledChain :SQLReadForm, K <: Label :ValueOf, V :SQLReadForm]
			:SQLReadForm[I >~ (K :~ V)] =
		new AbstractChainIndexReadForm[>~, :~, Label, I, K, V] {
			protected[this] override def cons(init :I, value :V) = init >~ key :~ value
			protected override def symbol = ">~"
		}

	/** Provides an implicit form for the heterogeneous map indexed by string literals (`Record`) `I |# L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def RecordReadForm[I <: Record :SQLReadForm, K <: String with Singleton :ValueOf, V :SQLReadForm]
			:SQLReadForm[I |# (K, V)] =
		new AbstractChainIndexReadForm[|#, Tuple2, Label, I, K, V] {
			protected[this] override def cons(init :I, value :V) = init |# (key -> value)
			protected override def symbol = "|#"
		}






	/** Implements `nullValue` to throw a `NoSuchElementException`. */
	trait NotNullReadForm[T] extends SQLReadForm[T] {
		override def nulls :NullValue[T] = NullValue.NotNull
		override def nullValue :T = throw new NoSuchElementException("No null value allowed for " + this)
	}

	/** A Convenience base `SQLReadForm[T]` class which implements `nullValue` based on an implicit `NullValue[T]`
	  * (overriding also `nulls` in the process). */
	abstract class AbstractReadForm[+T](implicit override val nulls :NullValue[T])
		extends SQLReadForm[T]
	{
		override def nullValue :T = nulls.value
	}




	private[schema] trait EmptyReadForm[+T] extends SQLReadForm[T] {
		override def readColumns = 0
		override def apply(position :Int)(res :ResultSet) :T = nullValue
		override def opt(position :Int)(res :ResultSet) :Option[T] = None
	}


	private[schema] class NullReadForm[+T :NullValue](columns :Int = 0)
		extends AbstractReadForm[T] with EmptyReadForm[T]
	{
		override def readColumns :Int = columns

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case nulls :NullReadForm[_] if nulls.canEqual(this) =>
				nulls.readColumns == readColumns && nulls.nulls == this.nulls
			case _ => false
		}

		override def hashCode :Int = nulls.hashCode

		override def toString = "NULL>"
	}






	private[schema] class ConstReadForm[+T :NullValue](value :Option[T], columns :Int = 0)
		extends AbstractReadForm[T]
	{
		private[this] val result = value getOrElse nullValue

		override def readColumns :Int = columns

		protected def get :Option[T] = value

		override def apply(position :Int)(res :ResultSet) :T = result

		override def opt(position: Int)(res: ResultSet): Option[T] = value

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case const :ConstReadForm[_] if const canEqual this =>
				const.get == value && const.readColumns == readColumns && const.nulls == nulls
			case _ => false
		}

		override def hashCode :Int = value.hashCode

		override def toString :String = String.valueOf(value) + ">"
	}



	private[schema] class EvalReadForm[+T :NullValue](value: => Option[T], columns :Int = 0)
		extends AbstractReadForm[T]
	{
		override def readColumns :Int = columns

		override def opt(position: Int)(res: ResultSet): Option[T] = value

		override def toString :String = "?=>"
	}






	class FlatMappedSQLReadForm[S, +T](protected[this] final val map :S => Option[T])
	                                  (implicit protected[this] val source :SQLReadForm[S],
	                                   final override val nulls :NullValue[T])
		extends AbstractReadForm[T]
	{
		override def readColumns :Int = source.readColumns

		override def opt(position :Int)(res :ResultSet) :Option[T] = source.opt(position)(res).flatMap(map)

		override def map[X :NullValue](fun :T => X) :SQLReadForm[X] = source.flatMap(map.apply(_).map(fun))

		override def flatMap[X :NullValue](fun :T => Option[X]) :SQLReadForm[X] =
			source.flatMap(map.apply(_).flatMap(fun))

		override def toString :String = source.toString + "=>"

	}



	class MappedSQLReadForm[S, +T](protected[this] final val map :S => T)
	                                              (implicit protected[this] val source :SQLReadForm[S],
	                                               implicit final override val nulls :NullValue[T])
		extends AbstractReadForm[T]
	{
		override def readColumns :Int = source.readColumns

		override def opt(position :Int)(res :ResultSet) :Option[T] = source.opt(position)(res).map(map)

		override def map[X :NullValue](fun :T => X) :SQLReadForm[X] = source.map(map andThen fun)

		override def flatMap[X :NullValue](fun :T => Option[X]) :SQLReadForm[X] = source.flatMap(map andThen fun)

		override def toString :String = source.toString + "=>"
	}






	private[schema] class FallbackReadForm[+T](overrides :SQLReadForm[T], fallback :SQLReadForm[T]) extends SQLReadForm[T] {
		protected def first :SQLReadForm[T] = overrides
		protected def second :SQLReadForm[T] = fallback

		override def opt(position :Int)(res :ResultSet) :Option[T] =
			first.opt(position)(res) orElse second.opt(position)(res)

		override def nullValue :T = first.nullValue

		override def nulls :NullValue[T] = first.nulls

		override def readColumns :Int = first.readColumns

		override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] =
			if (readColumns != fallback.readColumns && fallback.readColumns != 0)
				throw new IllegalArgumentException(
					s"($this) orElse $fallback: different number of read columns ($readColumns vs ${fallback.readColumns})."
				)
			else
				new FallbackReadForm(first, second orElse fallback)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case fallback :FallbackReadForm[_] if fallback canEqual this =>
				first == fallback.first && second == fallback.second
			case _ => false
		}

		override def hashCode :Int = overrides.hashCode * 31 + fallback.hashCode

		override def toString :String = {
			@tailrec def rec(fallback :SQLReadForm[_], res :StringBuilder) :String = fallback match {
				case next :FallbackReadForm[_] => rec(next.second, res append first append " orElse ")
				case _ => (res append ')').toString
			}
			rec(this, new StringBuilder append '(')
		}
	}






	trait ProxyReadForm[+T] extends SQLReadForm[T] {
		protected def form :SQLReadForm[T]

		override def opt(position :Int)(res :ResultSet) :Option[T] = form.opt(position)(res)
		override def apply(position :Int)(res :ResultSet) :T = form(position)(res)

		override def map[X :NullValue](fun :T => X) :SQLReadForm[X] = form.map(fun)
		override def flatMap[X :NullValue](fun :T => Option[X]) :SQLReadForm[X] = form.flatMap(fun)
		override def toOpt :SQLReadForm[Option[T]] = form.toOpt


		override def nulls :NullValue[T] = form.nulls
		override def nullValue :T = form.nullValue
		override def readColumns :Int = form.readColumns

		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case ref :AnyRef if ref eq this => true
			case proxy :ProxyReadForm[_] if proxy.canEqual(this) && canEqual(proxy) => form == proxy.form
			case _ => false
		}

		override def hashCode :Int = form.hashCode

		override def toString :String = "~" + form
	}




	private[schema] class LazyReadForm[+T](private[this] var init :() => SQLReadForm[T]) extends ProxyReadForm[T] {
		@volatile
		private[this] var initialized :SQLReadForm[T] = _
		private[this] var fastAccess :SQLReadForm[T] = _

		def isInitialized :Boolean = fastAccess != null || initialized != null

		protected override def form :SQLReadForm[T] = {
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

		//better to risk too early evaluation and remove the decorator overhead
		override def map[X :NullValue](fun :T => X) :SQLReadForm[X] = form.map(fun)

		override def flatMap[X :NullValue](fun :T => Option[X]) :SQLReadForm[X] = form.flatMap(fun)

		override def toOpt :SQLReadForm[Option[T]] = form.toOpt

		override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] = form orElse fallback

		override def *[O](other :SQLReadForm[O]) :SQLReadForm[(T, O)] = form * other

		override def <>[O >: T](write :SQLWriteForm[O]) :SQLForm[O] = form <> write


		override def canEqual(that :Any) :Boolean =
			(that.asInstanceOf[AnyRef] eq this) || that.getClass == getClass && isInitialized

		override def toString :String =
			if (fastAccess == null && initialized == null) "Lazy>"
			else "Lazy(" + form + ")"
	}





	trait CompositeReadForm[+T] extends SQLReadForm[T] {
		protected def forms :Seq[SQLReadForm[_]]

		override def readColumns: Int = (0 /: forms)(_ + _.readColumns)
	}



	private[schema] trait SeqReadForm[+T] extends SQLReadForm[Seq[T]] with CompositeReadForm[Seq[T]] {
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
			else Some(result.result())
		}


		override def apply(position: Int)(res: ResultSet): Seq[T] = {
			var i = position
			val result = List.newBuilder[T]
			for (form <- forms) {
				form.opt(i)(res) foreach { result += _  }
				i += form.readColumns
			}
			result.result()
		}

		override def nullValue: Seq[T] = forms.map(_.nullValue)



		override def equals(that :Any) :Boolean = that match {
			case seq :SeqReadForm[_] =>
				(seq eq this) || seq.canEqual(this) && forms == seq.forms
			case _ => false
		}

		override def hashCode() :Int = forms.hashCode

		override def toString :String = forms.mkString("Seq(",",",")>")
	}



	private class SeqReadFormImpl[+T](val forms :Seq[SQLReadForm[T]]) extends SeqReadForm[T] {
		override val readColumns = super.readColumns
		override def toString = super.toString
	}





	private[schema] trait ChainReadForm[+I <: Chain, +L] extends SQLReadForm[I ~ L] {
		protected val init :SQLReadForm[I]
		protected val last :SQLReadForm[L]

		override val readColumns :Int = init.readColumns + last.readColumns

		override def opt(position :Int)(res :ResultSet) :Option[I ~ L] =
			for (t <- init.opt(position)(res); h <- last.opt(position + init.readColumns)(res)) yield t ~ h

		private[this] val nullChain :I ~ L = try {
			init.nullValue ~ last.nullValue
		} catch { case _ :NullPointerException => null }

		override def nullValue = if (nullChain == null) init.nullValue ~ last.nullValue else nullChain



		override def equals(that :Any) :Boolean = that match {
			case chain :ChainReadForm[_, _] =>
				(chain eq this) || (chain canEqual this) && chain.last == last && chain.init == init
			case _ => false
		}

		override def hashCode :Int = init.hashCode * 31 + last.hashCode

		override def toString :String = last match { //consider: could be more efficient
			case _ :ChainReadForm[_, _] => init.toString + "~(" + last + ")"
			case _ => init.toString + "~" + last
		}
	}
	
	
	
	private[schema] class BaseChainReadForm[+I <: Chain, +L]
	                      (protected override val init :SQLReadForm[I], protected override val last :SQLReadForm[L])
		extends ChainReadForm[I, L]



	private[schema] trait ChainIndexReadForm[C[+A <: I, +B <: E[K, V]] <: A ~ B,
	                                         E[+A <: U, +B], U, I <: Chain, K <: U, V]
		extends SQLReadForm[I C E[K, V]]
	{
		protected val init :SQLReadForm[I]
		protected val value :SQLReadForm[V]
		protected def key :K

		override val readColumns :Int = init.readColumns + value.readColumns

		protected[this] def cons(init :I, value :V) :I C E[K, V]

		override def opt(position :Int)(res :ResultSet) :Option[I C E[K, V]] =
			for (i <- init.opt(position)(res); v <- value.opt(position + init.readColumns)(res))
				yield cons(i, v)

		private[this] val nullChain = try {
			cons(init.nullValue, value.nullValue)
		} catch { case _ :NullPointerException => null.asInstanceOf[I C E[K, V]] }

		override def nullValue =
			if (nullChain == null) cons(init.nullValue, value.nullValue) else nullChain


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case index :ChainIndexReadForm[_, _, _, _, _, _] if index canEqual this =>
				index.key == key && index.value == value && index.init == init
			case _ => false
		}

		override def hashCode :Int = (init.hashCode * 31 + key.hashCode) * 31 + value.hashCode

		protected def symbol :String

		//consider: could be more efficient
		override def toString :String = init.toString + symbol + "(" + key + "->" + value + ")"
	}
	
	
	
	private[schema] abstract class AbstractChainIndexReadForm
	                               [C[+A <: I, +B <: E[K, V]] <: A ~ B, E[+A <: U, +B], U,
	                                I <: Chain, K <: U, V]
	                               (implicit protected override val init :SQLReadForm[I], 
	                                         protected override val value :SQLReadForm[V], keyValue :ValueOf[K])
		extends ChainIndexReadForm[C, E, U, I, K, V]
	{
		final override def key :K = keyValue.value
	}




}

