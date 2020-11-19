package net.noresttherein.oldsql.schema

import java.sql.{ResultSet, SQLException}

import scala.annotation.implicitNotFound
import scala.collection.immutable.Seq

import net.noresttherein.oldsql.morsels.ColumnBasedFactory
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.FallbackSQLReadForm
import net.noresttherein.oldsql.schema.forms.{SQLForms, SuperSQLForm}







/** Encapsulates the logic of reading and building the value of `T` from a number of consecutive columns
  * in a `ResultSet`. Both the construction process and, in particular, the number of read columns should be fixed,
  * providing a pure contract. This makes it a lower-level counterpart of [[net.noresttherein.oldsql.schema.Mapping]],
  * which can be represented by possibly many forms, depending on the column list included in the selection.
  * All implementations must be thread safe.
  *
  * While not always strictly followed and not required from custom implementations, default implicit read forms
  * are generally characterised by:
  *   1. handling gracefully `null` values returned by other forms (for composed/mapped forms);
  *   1. using [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]] as their null value,
  *      throwing a `NullPointerException` when a null is read from a column ''by the''
  *      [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]] ''method''
  *      ([[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] can still be used to recognize a null column
  *      if the application chooses to handle `null` values by itself);
  *   1. composite forms will map the [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]
  *      of their component(s) if no implicit is present;
  *   1. Inclusion of data from read columns in thrown exceptions for easier debugging, which makes them potentially
  *      unsuitable in security conscious applications or highly optimised code which chooses to catch exceptions
  *      thrown by forms rather than manually discover if read values are not null);
  *   1. making a best-effort attempt to implement sensible `equals` where it is possible.
  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm]]
  * @see [[net.noresttherein.oldsql.schema.SQLForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnReadForm]]
  */
@implicitNotFound("I do not know how to read values of type ${T} from a ResultSet: missing implicit SQLReadForm[${T}].")
trait SQLReadForm[+T] extends SuperSQLForm {

	/** Reads the column values from columns `<position..position + this.readColumns)` of the passed `ResultSet`
	  * and creates an instance of `T`. The default implementation delegates to `opt` and fallbacks to `nullValue`
	  * if no value is available, but the exact handling is completely up to the implementation. Note that `null` column
	  * values may happen even on not-null database columns in outer joins.
	  * @throws NoSuchElementException if the value cannot be assembled, typically because the indicated columns are null;
	  *                                this is different in intent from the `NullPointerException` case
	  *                                as it is used primarily for multi-column forms, when the missing values are
	  *                                likely the result of an outer join or subclass columns in a
	  *                                table per class hierarchy mapping.
	  * @throws NullPointerException if the read column is `null` and type `T` does not define a representation of `null`.
	  * @throws SQLException if any of the columns cannot be read, either due to connection error or it being closed.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]]
	  */
	def apply(res :ResultSet, position :Int) :T = opt(res, position) match {
		case Some(x) => x
		case _ => try {
			nullValue
		} catch {
			case e :NullPointerException =>
				throw (new NullPointerException(errorMessage(position, res)).initCause(e))
			case e :NoSuchElementException =>
				throw (new NoSuchElementException(errorMessage(position, res)).initCause(e))
		}
	}

	/** Attempts to read the column values from columns `<position..position + this.readColumns` of the passed
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
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]]
	  */
	def opt(res :ResultSet, position :Int) :Option[T]

	/** The value a `null` column (or all `null` columns) should be mapped to. It is used in particular by `apply`
	  * when the value is unavailable, for example as a result of an outer join. Extending classes are allowed
	  * to throw an exception here (either a `NoSuchElementException` or a `NullPointerException`) if a concept
	  * of null does not exist for `T` or `null`s are not acceptable values. It is however completely acceptable
	  * to provide any particular value of `T` as a part of the mapping process (for example, a form for `Option[T]`
	  * will return `None`).
	  */
	def nullValue :T

	/** Null representation for type `T`. It is the value returned whenever a `null` is read from a database column
	  * (or is reported with [[java.sql.ResultSet.wasNull wasNull]]).
	  * It should be consistent with `this.nullValue` and the default implementation simply delegates to it at each call.
	  * This wrapper can be implicitly passed as a type class, even if the form does not support `null` values
	  * (i.e. `nullValue` throws an exception), which would not be possible by simply using `nullValue`.
	  */
	def nulls :NullValue[T] = new DerivedNullValue

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


	/** A hook for subclasses to provide an error message to use for a rethrown `NullPointerException` caught
	  * from [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]]. Default implementations lists
	  * columns read by [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]] with their values, which lead
	  * to the exception `caught` being thrown.
	  */
	protected def errorMessage(position :Int, res :ResultSet) :String = {
		val columns = Iterator.iterate(position)(_ + 1).take(readColumns).map {
			i => res.getMetaData.getColumnName(i) + "=" + res.getObject(i)
		}
		columns.mkString(this.toString + " does not allow null values. Read ", ", ", ".")
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
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullMap]]
	  */
	def map[X :NullValue](fun :T => X) :SQLReadForm[X] = SQLReadForm.map(fun)(this, NullValue[X])

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. If the underlying columns
	  * carry null values, passed `nullValue` is used instead of mapping. This guarantees that the given function will
	  * not be called for `null` arguments unless this form returns `Some(null)` from its `opt` method in a non-standard
	  * practice and allows handling of `null` values also when `T` is a value type without a natural `null` value.
	  * Note that `this.nullValue` is never called, directly or indirectly, by the created form.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullMap]]
	  */
	def map[X](fun :T => X, nullValue :X) :SQLReadForm[X] = map(fun)(NullValue(nullValue))

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. Note that the given
	  * function may be called for `null` arguments, even if the underlying columns have a ''not null'' constraint
	  * in case of outer join queries.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
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
		SQLReadForm.flatMap(fun)(this, NullValue[X])

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
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.flatMap]]
	  */
	def to[X :NullValue](fun :T =?> X) :SQLReadForm[X] = SQLReadForm(fun)(this, NullValue[X])

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. The returned form will
	  * use the given `nullValue` except for when `fun` is an identity extractor, in which case this instance
	  * will be returned. This will investigate the type of the extractor and either delegate to `map` or `flatMap` or
	  * return a specific form in corner cases.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.flatMap]]
	  */
	def to[X](f :T =?> X, nullValue :X) :SQLReadForm[X] = to(f)(NullValue(nullValue))

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`.
	  * This will call `nullMap` or `nullFlatMap` based on whether the extract is a `RequisiteExtractor`.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullMap]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullFlatMap]]
	  */
	def nullTo[X](f :T =?> X) :SQLReadForm[X] = to(f)(nulls.extract(f))



	/** Lifts this form to represent `Option[T]`. The created form maps all values returned by this form using
	  * `Option(...)`. This means that `null` values (actual JVM nulls, not the somewhat arbitrary value provided
	  * by this form) are mapped to `Some(None)`, while a returned `None` indicates that this form returned `None`.
	  * Basically this means that the returned form uses this form's `opt` method as its `apply` implementation.
	  */
	def toOpt :SQLReadForm[Option[T]] = SQLForms.OptionReadForm(this)


	/** Creates a read form which will apply this form `repeat` number of times, reading identical consecutive columns
	  * (or repeated sequences of same columns) and returning all 'non-null' (that is all, for which
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] was not empty) values as a sequence.
	  */
	def *(repeat :Int) :SQLReadForm[Seq[T]] = SQLReadForm.seq(repeat)(this)

	/** Combines this form with another form, which columns are expected to directly follow the columns for this
	  * form in the result set, to create a form producing pairs of values.
	  */
	def *[O](other :SQLReadForm[O]) :SQLReadForm[(T, O)] = forms.ScalaForms.tuple2ReadForm(this, other)

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
			new FallbackSQLReadForm(this, fallback)

	/** Combines this form with a `SQLWriteForm` to create a read/write `SQLForm[O]`. */
	def <>[O >: T](write :SQLWriteForm[O]) :SQLForm[O] = SQLForm.join[O](write, this)



	def compatible(other :SQLReadForm[_]) :Boolean = this == other

}






object SQLReadForm {

	/** Summons an implicit `SQLReadForm[T].` */
	def apply[T :SQLReadForm] :SQLReadForm[T] = implicitly[SQLReadForm[T]]



	/** Creates a new `SQLReadForm` using the given function to read the values from the result set.
	  * The function is responsible for handling null columns itself.
	  * @param columns the number of columns read by the form.
	  * @param read a function taking an SQL `ResultSet`, a start column position, and reads the following
	  *             `columns` columns to create a value of `T`.
	  */
	def apply[T :NullValue](columns :Int)(read :(ResultSet, Int) => T) :SQLReadForm[T] =
		SQLReadForm[T](columns, null)(read)

	/** Creates a new `SQLReadForm` using the given function to read the values from the result set.
	  * The function is responsible for handling null columns itself.
	  * @param name the name of the form returned by its `toString` method.
	  * @param columns the number of columns read by the form.
	  * @param read a function taking an SQL `ResultSet`, a start column position, and reads the following
	  *             `columns` columns to create a value of `T`.
	  */
	def apply[T :NullValue](columns :Int, name :String)(read :(ResultSet, Int) => T) :SQLReadForm[T] =
		new AbstractSQLReadForm[T] {
			override def apply(res :ResultSet, position :Int) = read(res, position)
			override def opt(res :ResultSet, position :Int) = Option(read(res, position))

			override val readColumns = columns
			override val toString = if (name != null) name else "SQLReadForm@" + hashCode
		}

	/** Creates a new `SQLReadForm` using the given function to read an optional value from the result set.
	  * @param columns the number of columns read by the form.
	  * @param name the name of the form returned from its `toString` method.
	  * @param read a function taking an SQL `ResultSet`, a start column position, and reads the following
	  *             `columns` columns to create a value of `T`.
	  */
	def opt[T :NullValue](columns :Int, name :String = null)(read :(ResultSet, Int) => Option[T]) :SQLReadForm[T] =
		new AbstractSQLReadForm[T] {
			override def opt(res :ResultSet, position :Int) = read(res, position)

			override val readColumns = columns
			override val toString = if (name != null) name else "SQLReadForm@" + hashCode
		}



	/** Creates a form reading the given number of columns and always returning from its `apply` method the value
	  * obtained by reevaluating the given expression. The result of `opt` is defined as `Some(value)`.
	  * The expression must be thread safe.
	  */
	def eval[T](expr: => T, readColumns :Int = 0, name :String = null) :SQLReadForm[T] =
		evalopt(Some(expr), readColumns, name)(NullValue.eval(expr))

	/** Creates a form reading the given number of columns and always returning from its `opt` method the value obtained
	  * by reevaluating the given expression. An implicitly provided null value is used by its `nullValue` method,
	  * to which the `apply` method delegates when the former yields `None`. The expression must be thread safe.
	  */
	def evalopt[T :NullValue](expr : => Option[T], readColumns :Int = 0, name :String = null) :SQLReadForm[T] =
		new EvalSQLReadForm(expr, readColumns, if (name != null) name else "=_>")



	/** Creates a form reading the given number columns and always returning `Some(value)` from its `opt` method.
	  * The `nullValue` of the created form, while largely irrelevant, is likewise defined as `value`.
	  * Note that if `value` is `null`, it will be treated as a valid return value, rather than 'no value'
	  * by the caller and may result in `NullPointerException` exceptions higher up the call stack.
	  */
	def const[T](value :T, readColumns :Int = 0, name :String = null) :SQLReadForm[T] =
		new ConstSQLReadForm[T](
			Some(value), readColumns, if (name != null) name else "='" + value + "'>"
		)(NullValue(value))

	/** Creates a form reading the given number of columns and always returning the provided value from its `opt` method.
	  * If `value` is `None`, the implicit `NullValue[T]` is used by the form's `apply` method to stand in.
	  */
	def constopt[T :NullValue](value :Option[T], readColumns :Int = 0, name :String = null) :SQLReadForm[T] =
		new ConstSQLReadForm[T](value, readColumns, if (name != null) name else "=" + value + ">")



	/** A form always throwing the given exception instead of producing a value. Note that this applies also to
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method.
	  * See [[net.noresttherein.oldsql.schema.SQLReadForm.none none]] and
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nulls nulls]] if you wish the form to simply return `None`.
	  * This functions the same way as `eval`, but can more clearly define intent.
	  */
	def error(value: => Nothing, readColumns :Int = 0, name :String = "ERROR>") :SQLReadForm[Nothing] =
		eval(value, readColumns, name)

//	/** A form always throwing the given exception instead of producing a value. This doesn't apply to
//	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method, which will simply return `None`.
//	  * This functions the same way as `eval`, but can more clearly define intent.
//	  */
//	def error(value: => Nothing) :SQLReadForm[Nothing] = eval(value)

	/** A  form which throws an `UnsupportedOperationException` with the given message with every read attempt.
	  * This is a simple shorthand for [[net.noresttherein.oldsql.schema.SQLReadForm.error error]].
	  */
	def unsupported(readColumns :Int, name :String = "UNSUPPORTED>")(message :String) :SQLReadForm[Nothing] =
		error(throw new UnsupportedOperationException(message), readColumns, name)

	/** A  form which throws an `UnsupportedOperationException` with the given message with every read attempt.
	  * This is a simple shorthand for [[net.noresttherein.oldsql.schema.SQLReadForm.error error]].
	  */
	def unsupported(message :String) :SQLReadForm[Nothing] =
		error(throw new UnsupportedOperationException(message))



	/** Creates a dummy form of zero column size which produces no value on every read.
	  * The `opt` method will always return `None`, while `apply` will return the value from type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]. If no implicit `NullValue[T]` is available,
	  * it will default to [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]],
	  * which throws `NullPointerException`.
	  */
	def nulls[T :NullValue] :SQLReadForm[T] = nulls(0)

	/** Creates a dummy form of the given column size which produces no value on every read.
	  * The `opt` method will always return `None`, while `apply` will return the value from type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]. If no implicit `NullValue[T]` is available,
	  * it will default to [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]],
	  * which throws `NullPointerException`.
	  * @param readColumns the span of this form in columns required for proper positioning among other forms.
	  *                    The values of the columns are never actually read.
	  * @param name the name for the form, which will be used in its `toString` method.
	  */
	def nulls[T :NullValue](readColumns :Int, name :String = "NULL>") :SQLReadForm[T] =
		new NullSQLReadForm[T](readColumns, name)

	/** Creates a dummy form of the given column size which produces no value on every read.
      * The `opt` method will always return `None`, while `apply` will always throw `NullPointerException`.
	  * This is equivalent of [[net.noresttherein.oldsql.schema.SQLWriteForm.nulls SQLWriteForm.nulls]]`(readColumns, name)(NullValue.NotNull)`
	  * @param readColumns the span of this form in columns required for proper positioning among other forms.
	  *                    The values of the columns are never actually read.
	  * @param name the name for the form, which will be used in its `toString` method.
	  */
	def none[T](readColumns :Int, name :String = "NULL>") :SQLReadForm[T] = nulls(readColumns, name)(NullValue.NotNull)

	/** A read form of zero columns which produces no value on every read. The `opt` method will always return `None`,
	  * while `apply` (and `nullValue`) will throw a `NullPointerException`. This is the same as
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nulls SQLReadForm.nulls]]`(`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]]`)`.
	  */
	val empty :SQLReadForm[Nothing] = empty("EMPTY>")

	/** A read form of zero columns which produces no value on every read. The `opt` method will always return `None`,
	  * while `apply` (and `nullValue`) will throw a `NullPointerException`. This is the same as
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nulls SQLReadForm.nulls]]`(0, name)(`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]]`)`.
	  * @param name the name for the form, which will be used in its `toString` method.
	  */
	def empty(name :String) :SQLReadForm[Nothing] = new NullSQLReadForm[Nothing](0, name)(NullValue.NotNull)



	/** Maps the result of reading `S` with an implicit form to `T` using the specified `Extractor`. */
	def apply[S, T](extract :S =?> T)(implicit source :SQLReadForm[S], nulls :Maybe[NullValue[T]]) :SQLReadForm[T] =
		extract match {
			case _ :EmptyExtractor[_, _] =>
				SQLReadForm.nulls(source.readColumns)(nulls.opt getOrElse NullValue.NotNull)
			case _ :OptionalExtractor[_, _] => flatMap(extract.optional)
			case _ :IdentityExtractor[_] => source.asInstanceOf[SQLReadForm[T]]
			case const :ConstantExtractor[_, T @unchecked] => SQLReadForm.const(const.constant, source.readColumns)
			case req :RequisiteExtractor[S @unchecked, T @unchecked] => map(req.getter)
			case _ => flatMap(extract.optional)
		}

	/** Maps the result of reading `S` with an implicit form to `T` using the specified `Extractor`. */
	def apply[S, T](name :String)(extract :S =?> T)
	               (implicit source :SQLReadForm[S], nulls :Maybe[NullValue[T]]) :SQLReadForm[T] =
		extract match {
			case _ :EmptyExtractor[_, _] =>
				SQLReadForm.nulls(source.readColumns, name)(nulls.opt getOrElse NullValue.NotNull)
			case _ :OptionalExtractor[_, _] => flatMap(name)(extract.optional)
//			case _ :IdentityExtractor[_] => source.asInstanceOf[SQLReadForm[T]]
			case const :ConstantExtractor[_, T @unchecked] => SQLReadForm.const(const.constant, source.readColumns, name)
			case req :RequisiteExtractor[S @unchecked, T @unchecked] => map(name)(req.getter)
			case _ => flatMap(name)(extract.optional)
		}

	/** Maps the result of reading `S` with an implicit form to `T` using the specified `Extractor`. */
	def apply[S :SQLReadForm, T](map :S =?> T, nullValue: => T) :SQLReadForm[T] =
		apply(map)(SQLReadForm[S], NullValue.eval(nullValue))



	/** Maps the result of reading `S` with an implicit form to `T`. */
	def map[S, T](map :S => T)(implicit source :SQLReadForm[S], nulls :Maybe[NullValue[T]]) :SQLReadForm[T] =  {
		val nullValue = nulls.opt getOrElse source.nulls.map(map)
		new MappedSQLReadForm(map)(source, nullValue)
	}

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def map[S, T](name :String)(map :S => T)(implicit source :SQLReadForm[S], nulls :Maybe[NullValue[T]]) :SQLReadForm[T] =  {
		val nullValue = nulls.opt getOrElse source.nulls.map(map)
		new MappedSQLReadForm(map, name)(source, nullValue)
	}

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def map[S :SQLReadForm, T](map :S => T, nullValue: => T) :SQLReadForm[T] =
		SQLReadForm.map(map)(SQLReadForm[S], NullValue.eval(nullValue))

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def map[S :SQLReadForm, T](name :String, map :S => T, nullValue: => T) :SQLReadForm[T] =
		SQLReadForm.map(name)(map)(SQLReadForm[S], NullValue.eval(nullValue))



	/** Maps the result of reading `S` with an implicit form to `T`. */
	def flatMap[S, T](map :S => Option[T])(implicit source :SQLReadForm[S], nulls :Maybe[NullValue[T]]) :SQLReadForm[T] = {
		val nullValue = nulls.opt getOrElse source.nulls.flatMap(map)
		new FlatMappedSQLReadForm(map)(source, nullValue)
	}

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def flatMap[S, T](name :String)(map :S => Option[T])
	                 (implicit source :SQLReadForm[S], nulls :Maybe[NullValue[T]]) :SQLReadForm[T] =
	{
		val nullValue = nulls.opt getOrElse source.nulls.flatMap(map)
		new FlatMappedSQLReadForm(map, name)(source, nullValue)
	}

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def flatMap[S :SQLReadForm, T](map :S => Option[T], nullValue: => T) :SQLReadForm[T] =
		flatMap(map)(SQLReadForm[S], NullValue.eval(nullValue))

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def flatMap[S :SQLReadForm, T](name :String, map :S => Option[T], nullValue: => T) :SQLReadForm[T] =
		flatMap(name)(map)(SQLReadForm[S], NullValue.eval(nullValue))



	/** A proxy form which will delegate all calls to the form returned by the given expression. The expression
	  * will be evaluated only if/when needed, but must be thread safe and may be executed more than once
	  * if several threads trigger the initialization at the same time, but the returned form is thread safe.
	  * The proxy is shallow: all methods returning another form are delegated to the backing form, evaluating them.
	  * If you wish them to remain lazy, invoke them directly on the backing form in the evaluation expression instead.
	  * This is useful when the initialization expression cannot be successfully evaluated at this time,
	  * but the form must be passed by-reference to some method. In other cases a `lazy val` or
	  * [[net.noresttherein.oldsql.morsels.Lazy Lazy]] wrapper are preferable, as they do not incur the penalty
	  * of checking for initialization and delegation at every call.
	  */
	def delayed[T](form : => SQLReadForm[T]) :SQLReadForm[T] = new LazySQLReadForm(() => form)



	/** A form reading a sequence of columns of the same type, or a sequence of columns repeated several times.
	  * The number of elements in the read sequence will equal the number of times that method
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] of the form for the individual element type `T`
	  * would return a non-empty result.
	  * @param repeats the number of time that `SQLReadForm[T]` should read from the `ResultSet`, each time
	  *                increasing the reading position by its
	  *                [[net.noresttherein.oldsql.schema.SQLReadForm.readColumns readColumns]].
	  */
	def seq[T :SQLReadForm](repeats :Int) :SQLReadForm[Seq[T]] =
		if (repeats == 0) const(Seq())
		else new SeqSQLReadForm[T](repeats)

	/** A form applying all forms in the given sequence one after another, each starting at the position following the
	  * last column read by the preceding form, and collecting the results. Produced sequence will always have
	  * the same number of elements as there are forms. If one of the form's
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] method fails to produce a value, the form's
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]] will take its place. Only if that method
	  * throws an exception, the created form's `opt` method will return `None`.
	  */
	def seq[T](items :Seq[SQLReadForm[T]]) :SQLReadForm[Seq[T]] = new SQLReadFormSeq(items)






	/** Implements `nullValue` as throwing a `NoSuchElementException`. */
	trait NotNullReadForm[T] extends SQLReadForm[T] {
		override def nulls :NullValue[T] = NullValue.NotNull
		override def nullValue :T = throw new NoSuchElementException("No null value allowed for " + this)
	}


	/** A convenience base `SQLReadForm[T]` class which implements `nullValue` based on an implicit `NullValue[T]`
	  * (overriding also `nulls` in the process). */
	abstract class AbstractSQLReadForm[+T](implicit override val nulls :NullValue[T])
		extends SQLReadForm[T]
	{
		override def nullValue :T = nulls.value
	}






	trait EmptyReadForm[+T] extends SQLReadForm[T] {
		override def readColumns = 0

		override def opt(res :ResultSet, position :Int) :Option[T] = None
		override def apply(res :ResultSet, position :Int) :T = nullValue

		protected override def errorMessage(position :Int, res :ResultSet) :String = {
			val columns = Iterator.iterate(position)(_ + 1).take(readColumns).map {
				i => res.getMetaData.getColumnName(i)
			}
			columns.mkString(this.toString + " does not allow null values for (", ", ", ").")
		}
	}



	trait ProxyReadForm[+T] extends SQLReadForm[T] {
		protected def form :SQLReadForm[T]

		override def opt(res :ResultSet, position :Int) :Option[T] = form.opt(res, position)
		override def apply(res :ResultSet, position :Int) :T = form(res, position)

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



	/** Base type for factories of some types `M[X]` and `S[X] <: M[X]`, which take as arguments
	  * `SQLReadForm` and `ColumnReadForm` instances (or some higher type parameterized with these form types),
	  * respectively. See [[net.noresttherein.oldsql.morsels.ColumnBasedFactory ColumnBasedFactory]]
	  * for more information about this framework type.
	  */
	type ReadFormFunction[A[_], M[_], S[X] <: M[X]] = ColumnBasedFactory[A, A, SQLReadForm, ColumnReadForm, M, S]






	private[schema] class NullSQLReadForm[+T :NullValue](columns :Int = 0, override val toString :String = "NULL>")
		extends AbstractSQLReadForm[T] with EmptyReadForm[T]
	{
		override def readColumns :Int = columns

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case nulls :NullSQLReadForm[_] if nulls.canEqual(this) =>
				nulls.readColumns == readColumns && nulls.nulls == this.nulls
			case _ => false
		}

		override def hashCode :Int = nulls.hashCode
	}






	private[schema] class ConstSQLReadForm[+T :NullValue](value :Option[T], columns :Int, name :String = null)
		extends AbstractSQLReadForm[T]
	{
		override def readColumns :Int = columns

		protected def get :Option[T] = value

		override def opt(res :ResultSet, position :Int): Option[T] = value

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case const :ConstSQLReadForm[_] if const canEqual this =>
				const.get == value && const.readColumns == readColumns && const.nulls == nulls
			case _ => false
		}

		override def hashCode :Int = value.hashCode

		override val toString = if (name != null) name else "=" + value + ">"
	}



	private[schema] class EvalSQLReadForm[+T :NullValue]
	                                     (value: => Option[T], columns :Int = 0, override val toString :String = "=_>")
		extends AbstractSQLReadForm[T]
	{
		override def readColumns :Int = columns

		override def opt(res :ResultSet, position :Int): Option[T] = value
	}






	private[schema] class FlatMappedSQLReadForm[S, +T](protected[this] final val map :S => Option[T], name :String)
	                                                  (implicit protected[this] val source :SQLReadForm[S],
	                                                   final override val nulls :NullValue[T])
		extends AbstractSQLReadForm[T]
	{	//map/flatMap not overriden to preserve potentially custom name.
		def this(map :S => Option[T])(implicit source :SQLReadForm[S], nulls :NullValue[T]) =
			this(map, source.toString + "=>")

		override def readColumns :Int = source.readColumns

		override def opt(res :ResultSet, position :Int) :Option[T] = source.opt(res, position).flatMap(map)

		override def toString :String = if (name != null) name else source.toString + "=>"
	}



	private[schema] class MappedSQLReadForm[S, +T](protected[this] final val map :S => T, name :String)
	                                              (implicit protected[this] val source :SQLReadForm[S],
	                                               implicit final override val nulls :NullValue[T])
		extends AbstractSQLReadForm[T]
	{	//map/flatMap not overriden to preserve potentially custom name.
		def this(map :S => T)(implicit source :SQLReadForm[S], nulls :NullValue[T]) =
			this(map, source.toString + "=>")

		override def readColumns :Int = source.readColumns

		override def opt(res :ResultSet, position :Int) :Option[T] = source.opt(res, position).map(map)

		override def toString :String = if (name != null) name else source.toString + "=>"
	}






	private[schema] class FallbackSQLReadForm[+T](overrides :SQLReadForm[T], fallback :SQLReadForm[T])
		extends SQLReadForm[T]
	{
		protected def first :SQLReadForm[T] = overrides
		protected def second :SQLReadForm[T] = fallback

		override def opt(res :ResultSet, position :Int) :Option[T] =
			first.opt(res, position) orElse second.opt(res, position)

		override def nullValue :T = overrides.nullValue

		override def nulls :NullValue[T] = overrides.nulls

		override def readColumns :Int = overrides.readColumns

		override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] =
			if (readColumns != fallback.readColumns && fallback.readColumns != 0)
				throw new IllegalArgumentException(
					s"($this) orElse $fallback: different number of read columns ($readColumns vs ${fallback.readColumns})."
				)
			else
				new FallbackSQLReadForm(first, second orElse fallback)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case fallback :FallbackSQLReadForm[_] if fallback canEqual this =>
				first == fallback.first && second == fallback.second
			case _ => false
		}

		override def hashCode :Int = overrides.hashCode * 31 + fallback.hashCode

		override val toString = overrides.toString + " orElse " + fallback
	}






	private[schema] class LazySQLReadForm[+T](private[this] var init :() => SQLReadForm[T]) extends ProxyReadForm[T] {
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
		override def *(repeat :Int) :SQLReadForm[Seq[T]] = form * repeat
		override def *[O](other :SQLReadForm[O]) :SQLReadForm[(T, O)] = form * other
		override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] = form orElse fallback
		override def <>[O >: T](write :SQLWriteForm[O]) :SQLForm[O] = form <> write

		override def canEqual(that :Any) :Boolean =
			(that.asInstanceOf[AnyRef] eq this) || that.getClass == getClass && isInitialized

		override def toString :String =
			if (fastAccess == null && initialized == null) "Lazy>"
			else "Lazy(" + form + ")"
	}






	private[schema] trait SeqReadForm[+T] extends SQLReadForm[Seq[T]] {
		protected def form :SQLReadForm[T]
		protected def repeats :Int

		override def readColumns :Int = form.readColumns * repeats

		override def opt(res :ResultSet, position :Int): Option[Seq[T]] = Some(apply(res, position))

		override def apply(res :ResultSet, position :Int): Seq[T] = {
			val f = form
			var i = position; val jump = form.readColumns
			val result = List.newBuilder[T]
			var countdown = repeats
			while (repeats > 0) f.opt(res, i) match {
				case Some(x) => result += x; countdown -= 1; i += jump
				case _ => countdown -= 1; i += jump
			}
			result.result()
		}

		override def nullValue: Seq[T] = Seq()

		override def toString :String = "(" + repeats.toString + "*" + form + ")"
	}



	private case class SeqSQLReadForm[+T](form :SQLReadForm[T], repeats :Int) extends SeqReadForm[T] {
		def this(repeats :Int)(implicit form :SQLReadForm[T]) = this(form, repeats)

		override val readColumns = super.readColumns
		override val toString = super.toString
	}



	private[schema] trait ReadFormSeq[+T] extends SQLReadForm[Seq[T]] {
		protected val forms :Seq[SQLReadForm[T]]

		override def readColumns :Int = (0 /: forms)(_ + _.readColumns)

		override def opt(res :ResultSet, position :Int) :Option[Seq[T]] = {
			var i = position + readColumns
			(forms :\ Option(List.empty[T])) { (form, acc) =>
				acc flatMap { tail =>
					i -= form.readColumns
					form.opt(res, i) match {
						case Some(head) => Some(head::tail)
						case _ => try {
							Some(form.nullValue::tail)
						} catch {
							case e :Exception => None
						}
					}
				}
			}
		}

		override def apply(res :ResultSet, position :Int) = {
			var i = position
			forms.map { f => val elem = f(res, i); i += f.readColumns; elem }
		}

		override def nullValue = forms.map(_.nullValue)

		override def toString :String = forms.mkString("::")
	}



	private case class SQLReadFormSeq[+T](forms :Seq[SQLReadForm[T]]) extends ReadFormSeq[T] {
		override val readColumns = super.readColumns
		private val nullValueCache = try { super.nullValue } catch { case e :Exception => null }
		override def nullValue :Seq[T] = if (nullValueCache != null) nullValueCache else super.nullValue
		override val toString = super.toString
	}


}


