package net.noresttherein.oldsql.schema

import java.sql.{CallableStatement, ResultSet, SQLException, Types}

import scala.annotation.implicitNotFound
import scala.collection.immutable.Seq

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.SpecializingFactory
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.pixies.CallableStatementOutParams
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull
import net.noresttherein.oldsql.schema.SQLReadForm.{FallbackSQLReadForm, NotNullSQLReadForm}
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
	  *
	  * Composite forms (implementations delegating to more than one component forms) are encouraged to override
	  * this method, in addition to [[net.noresttherein.oldsql.schema.SQLReadForm.opt(res:ResultSet,position:Int)* opt]],
	  * by delegating to `apply` method of the component forms. This results in different handling of `null` values
	  * (and, in general, column values for which a component form returns
	  * [[net.noresttherein.oldsql.collection.Opt.Lack Lack]]): `opt` method has the 'all or nothing' semantics,
	  * which fails to produce a result unless all partial results can be assembled by the component forms,
	  * while `apply` will substitute missing values with the corresponding form's
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]] (and propagate any exception thrown
	  * in particular by `nullValue` method). It is impossible to emulate this behaviour with `opt` and `nullValue`
	  * alone on the composite form. This behaviour is not mandated however and, as in most cases, for the form
	  * implementation to decide.
	  * @throws NoSuchElementException if the value cannot be assembled, typically because the indicated columns are null;
	  *                                this is different in intent from the `NullPointerException` case
	  *                                as it is used primarily for multi-column forms, when the missing values are
	  *                                likely the result of an outer join or subclass columns in a
	  *                                table per class hierarchy mapping. For example, A table entity form
	  *                                might throw this exception if the primary key is null.
	  * @throws NullPointerException if the read column is `null` and type `T` does not define a representation of `null`.
	  * @throws SQLException if any of the columns cannot be read, either due to connection error or it being closed.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]]
	  */
	def apply(res :ResultSet, position :Int) :T = opt(res, position) match {
		case Got(x) => x
		case _ => try {
			nullValue
		} catch {
			case e :NullPointerException =>
				throw new NullPointerException(errorMessage(position, res)).initCause(e)
			case e :NoSuchElementException =>
				throw new NoSuchElementException(errorMessage(position, res)).initCause(e)
			case e :ClassCastException =>
				throw new ClassCastException(errorMessage(position, res)).initCause(e)
		}
	}

	/** Attempts to read the column values from columns `<position..position + this.readColumns` of the passed
	  * `ResultSet` and create an instance of `T`. If the values are unavailable (required columns carry `null` values),
	  * `None` is returned. It is the recommended practice to have the returned option reflect only the availability
	  * of the input values and not their validity. It is allowed for the form to return `Got(null)`
	  * (as long as `T >: Null`), but this results in propagation of `null` values to any forms derived from it
	  * and to the application. As such, it is discouraged, with the exception of `ColumnForm`s, as the validity of
	  * `null` values can be explicitly switched on for columns using the `Nullable` buff (and otherwise affected
	  * by other buffs). The `ColumnMapping` trait explicitly checks for `null` values throwing a `NullPointerException`
	  * if they are not permitted. A form is allowed to throw an exception if the values do not conform
	  * to expected constraints or the assembly process fails for any other reason.
	  * Similarly, all thrown `SQLException`s are propagated.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]]
	  */
	def opt(res :ResultSet, position :Int) :Opt[T]

	/** Attempts to read the values of ''out'' parameters of the given `CallableStatement` and create an instance
	  * of `Opt[T]`. The index of the first parameter to read is `position`; all succeeding parameters until
	  * (not including) `position + this.readColumns` should also be ''out'' parameters reserved for this form.
	  * Default implementation wraps the statement in a `ResultSet` adapter and delegates
	  * to [[net.noresttherein.oldsql.schema.SQLReadForm.opt(res :ResultSet, position :Int) opt]]`(res, position)`.
	  * @param call a statement invoking a stored procedure with ''out'' parameters.
	  * @param position the index of the first parameter to read, which must be a previously registered ''out'' parameter.
	  * @throws NoSuchElementException if the value cannot be assembled, typically because the indicated columns are null;
	  *                                this is different in intent from the `NullPointerException` case
	  *                                as it is used primarily for multi-column forms, when the missing values are
	  *                                likely the result of an outer join or subclass columns in a
	  *                                table per class hierarchy mapping. For example, A table entity form
	  *                                might throw this exception if the primary key is null.
	  * @throws NullPointerException if the read column is `null` and type `T` does not define a representation of `null`.
	  * @throws SQLException if any of the columns cannot be read, either due to connection error or it being closed.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]]
	  */
	def opt(call :CallableStatement, position :Int) :Opt[T] = opt(new CallableStatementOutParams(call), position)

	/** Attempts to read the values of ''out'' parameters of the given `CallableStatement` and create an instance of `T`.
	  * The index of the first parameter to read is `position`; all succeeding parameters until (not including)
	  * `position + this.readColumns` should also be ''out'' parameters reserved for this form.
	  * Default implementation wraps the statement in a `ResultSet` adapter and delegates
	  * to [[net.noresttherein.oldsql.schema.SQLReadForm.apply(res :ResultSet, position :Int) apply]]`(res, position)`.
	  * @param call a statement invoking a stored procedure with ''out'' parameters.
	  * @param position the index of the first parameter to read, which must be a previously registered ''out'' parameter.
	  * @throws NoSuchElementException if the value cannot be assembled, typically because the indicated columns are null;
	  *                                this is different in intent from the `NullPointerException` case
	  *                                as it is used primarily for multi-column forms, when the missing values are
	  *                                likely the result of an outer join or subclass columns in a
	  *                                table per class hierarchy mapping. For example, A table entity form
	  *                                might throw this exception if the primary key is null.
	  * @throws NullPointerException if the read column is `null` and type `T` does not define a representation of `null`.
	  * @throws SQLException if any of the columns cannot be read, either due to connection error or it being closed.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]]
	  */
	def apply(call :CallableStatement, position :Int) :T = apply(new CallableStatementOutParams(call), position)


	def register(call :CallableStatement, position :Int) :Unit = {
		var i = position + readColumns - 1
		while (i >= position) {
			call.registerOutParameter(position, Types.OTHER); i -= 1
		}
	}


	/** Number of columns read by this form. This must be a constant as it is typically is used to calculate offsets
	  * for various forms once per `ResultSet` rather than per row. Naturally, there is no requirement for actual
	  * reading of all columns if the form can determine based on some indicator (such as a `null` primary key) that
	  * the value cannot be assembled from the given column set for the row.
	  */
	def readColumns :Int


	/** The value a `null` column (or all `null` columns) should be mapped to. It is used in particular by `apply`
	  * when the value is unavailable, for example as a result of an outer join. Extending classes are allowed
	  * to throw an exception here (either a `NoSuchElementException` or a `NullPointerException`) if a concept
	  * of null does not exist for `T` or `null`s are not acceptable values. It is however completely acceptable
	  * to provide any particular value of `T` as a part of the mapping process (for example, a form for `Option[T]`
	  * will return `None`).
	  */
	def nullValue :T

	protected def guardedNullValue(position :Int, res :ResultSet) :T = try {
			nullValue
		} catch {
			case e :NullPointerException => throw new NullPointerException(errorMessage(position, res)).initCause(e)
			case e :NoSuchElementException => throw new NoSuchElementException(errorMessage(position, res)).initCause(e)
			case e :ClassCastException => throw new ClassCastException(errorMessage(position, res)).initCause(e)
		}

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

	/** An adapter or modification of this form which ensures that `null` values will never be returned by
	  * `apply` or `opt`. If such an event would occur in this form, the returned form will throw
	  * a `NullPointerException`.
	  */
	def notNull :SQLReadForm[T] = new NotNullSQLReadForm[T]()(this)

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
	def <>[O >: T](write :SQLWriteForm[O]) :SQLForm[O] = SQLForm.combine[O](write, this)



	def compatible(other :SQLReadForm[_]) :Boolean = this == other

}






object SQLReadForm {

	/** Summons an implicit `SQLReadForm[T].` */
	@inline def apply[T :SQLReadForm] :SQLReadForm[T] = implicitly[SQLReadForm[T]]



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
	  * @param reader a function taking an SQL `ResultSet`, a start column position, and reads the following
	  *               `columns` columns to create a value of `T`.
	  */
	def apply[T :NullValue](columns :Int, name :String)(reader :(ResultSet, Int) => T) :SQLReadForm[T] =
		new CustomSQLReadForm[T](columns, name)(reader)

	/** Creates a new `SQLReadForm` using the given function to read an optional value from the result set.
	  * @param columns the number of columns read by the form.
	  * @param name the name of the form returned from its `toString` method.
	  * @param reader a function taking an SQL `ResultSet`, a start column position, and reads the following
	  *               `columns` columns to create a value of `T`.
	  */
	def opt[T :NullValue](columns :Int, name :String = null)(reader :(ResultSet, Int) => Opt[T]) :SQLReadForm[T] =
		new CustomOptSQLReadForm[T](columns, name)(reader)



	/** Creates a form reading the given number of columns and always returning from its `apply` method the value
	  * obtained by reevaluating the given expression. The result of `opt` is defined as `Some(value)`.
	  * The expression must be thread safe.
	  */
	def eval[T](expr: => T, readColumns :Int = 0, name :String = null) :SQLReadForm[T] =
		evalopt(Got(expr), readColumns, name)(NullValue.eval(expr))

	/** Creates a form reading the given number of columns and always returning from its `opt` method the value obtained
	  * by reevaluating the given expression. An implicitly provided null value is used by its `nullValue` method,
	  * to which the `apply` method delegates when the former yields `None`. The expression must be thread safe.
	  */
	def evalopt[T :NullValue](expr: => Opt[T], readColumns :Int = 0, name :String = null) :SQLReadForm[T] =
		new EvalSQLReadForm(expr, readColumns, if (name != null) name else "=_>")



	/** Creates a form reading the given number columns and always returning `Some(value)` from its `opt` method.
	  * The `nullValue` of the created form, while largely irrelevant, is likewise defined as `value`.
	  * Note that if `value` is `null`, it will be treated as a valid return value, rather than 'no value'
	  * by the caller and may result in `NullPointerException` exceptions higher up the call stack.
	  */
	def const[T](value :T, readColumns :Int = 0, name :String = null) :SQLReadForm[T] =
		new ConstSQLReadForm[T](
			Got(value), readColumns, if (name != null) name else "='" + value + "'>"
		)(NullValue(value))

	/** Creates a form reading the given number of columns and always returning the provided value from its `opt` method.
	  * If `value` is `Lack`, the implicit `NullValue[T]` is used by the form's `apply` method to stand in.
	  */
	def constopt[T :NullValue](value :Opt[T], readColumns :Int = 0, name :String = null) :SQLReadForm[T] =
		new ConstSQLReadForm[T](value, readColumns, if (name != null) name else "=" + value + ">")



	/** A form always throwing the given exception instead of producing a value. Note that this applies also to
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method.
	  * See [[net.noresttherein.oldsql.schema.SQLReadForm.none none]] and
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.defaults defaults]] if you wish the form to simply return `None`.
	  * This functions the same way as `eval`, but can more clearly define intent.
	  */
	def error(raise: => Nothing) :SQLReadForm[Nothing] = error(0, "ERROR>")(raise)

	/** A form always throwing the given exception instead of producing a value. Note that this applies also to
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method.
	  * See [[net.noresttherein.oldsql.schema.SQLReadForm.none none]] and
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.defaults defaults]] if you wish the form to simply return `None`.
	  * This functions the same way as `eval`, but can more clearly define intent.
	  */
	def error(readColumns :Int, name :String)(raise: => Nothing) :SQLReadForm[Nothing] =
		eval(raise, readColumns, name)

//	/** A form always throwing the given exception instead of producing a value. This doesn't apply to
//	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method, which will simply return `None`.
//	  * This functions the same way as `eval`, but can more clearly define intent.
//	  */
//	def error(value: => Nothing) :SQLReadForm[Nothing] = eval(value)

	/** A  form which throws an `UnsupportedOperationException` with the given message with every read attempt.
	  * This is a simple shorthand for [[net.noresttherein.oldsql.schema.SQLReadForm.error error]].
	  */
	def unsupported(readColumns :Int, name :String = "UNSUPPORTED>")(message :String) :SQLReadForm[Nothing] =
		error(readColumns, name)(throw new UnsupportedOperationException(message))

	/** A  form which throws an `UnsupportedOperationException` with the given message with every read attempt.
	  * This is a simple shorthand for [[net.noresttherein.oldsql.schema.SQLReadForm.error error]].
	  */
	def unsupported(message :String) :SQLReadForm[Nothing] =
		error(throw new UnsupportedOperationException(message))



	/** Creates a dummy form of zero column size which produces no value on every read.
	  * The `opt` method will always return `None`, while `apply` will return the value from type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]. If no implicit `NullValue[T]` is available,
	  * it will default to [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]],
	  * which throws a `NullPointerException`.
	  */
	def defaults[T :NullValue] :SQLReadForm[T] = defaults(0)

	/** Creates a dummy form of the given column size which produces no value on every read.
	  * The `opt` method will always return `None`, while `apply` will return the value from type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]. If no implicit `NullValue[T]` is available,
	  * it will default to [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]],
	  * which throws `NullPointerException`.
	  * @param readColumns the span of this form in columns required for proper positioning among other forms.
	  *                    The values of the columns are never actually read.
	  * @param name the name for the form, which will be used in its `toString` method.
	  */
	def defaults[T :NullValue](readColumns :Int, name :String = null) :SQLReadForm[T] =
		new NullValueSQLReadForm[T](readColumns, name)

	/** Creates a dummy form of the given column size which produces no value on every read.
      * The `opt` method will always return `None`, while `apply` will always throw `NullPointerException`.
	  * This is equivalent of [[net.noresttherein.oldsql.schema.SQLWriteForm.defaults SQLWriteForm.defaults]]`(readColumns, name)(NullValue.NotNull)`
	  * @param readColumns the span of this form in columns required for proper positioning among other forms.
	  *                    The values of the columns are never actually read.
	  * @param name the name for the form, which will be used in its `toString` method.
	  */
	def none[T](readColumns :Int, name :String = null) :SQLReadForm[T] =
		defaults(readColumns, name)(NullValue.NotNull)

	/** Creates a dummy form of the given column size which produces no value by its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] method and `null`
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]] method.
	  * @param readColumns the span of this form in columns required for proper positioning among other forms.
	  *                    The values of the columns are never actually read.
	  * @param name the name for the form, which will be used in its `toString` method.
	  */
	def nulls(readColumns :Int, name :String = null) :SQLReadForm[Null] =
		new NullValueSQLReadForm[Null](readColumns)(NullValue.Null)

	/** A read form of zero columns which produces no value on every read. The `opt` method will always return `None`,
	  * while `apply` (and `nullValue`) will throw a `NullPointerException`. This is the same as
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.defaults SQLReadForm.defaults]]`(`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]]`)`.
	  */
	val empty :SQLReadForm[Nothing] = empty("EMPTY>")

	/** A read form of zero columns which produces no value on every read. The `opt` method will always return `None`,
	  * while `apply` (and `nullValue`) will throw a `NullPointerException`. This is the same as
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.defaults SQLReadForm.defaults]]`(0, name)(`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]]`)`.
	  * @param name the name for the form, which will be used in its `toString` method.
	  */
	def empty(name :String) :SQLReadForm[Nothing] = new NullValueSQLReadForm[Nothing](0, name)(NullValue.NotNull)



	/** Maps the result of reading `S` with an implicit form to `T` using the specified `Extractor`. */
	def apply[S, T](extract :S =?> T)(implicit source :SQLReadForm[S], nulls :Maybe[NullValue[T]]) :SQLReadForm[T] =
		extract match {
			case _ :EmptyExtractor[_, _] =>
				SQLReadForm.defaults(source.readColumns)(nulls.opt getOrElse NullValue.NotNull)
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
				SQLReadForm.defaults(source.readColumns, name)(nulls.opt getOrElse NullValue.NotNull)
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
	def map[S, T](f :S => T)(implicit source :SQLReadForm[S], nulls :Maybe[NullValue[T]]) :SQLReadForm[T] =  {
		val nullValue = nulls.opt getOrElse source.nulls.map(f)
		new MappedSQLReadForm(f)(source, nullValue)
	}

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def map[S, T](name :String)(f :S => T)(implicit source :SQLReadForm[S], nulls :Maybe[NullValue[T]]) :SQLReadForm[T] =  {
		val nullValue = nulls.opt getOrElse source.nulls.map(f)
		new MappedSQLReadForm(f, name)(source, nullValue)
	}

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def map[S :SQLReadForm, T](f :S => T, nullValue: => T) :SQLReadForm[T] =
		SQLReadForm.map(f)(SQLReadForm[S], NullValue.eval(nullValue))

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def map[S :SQLReadForm, T](name :String, f :S => T, nullValue: => T) :SQLReadForm[T] =
		SQLReadForm.map(name)(f)(SQLReadForm[S], NullValue.eval(nullValue))



	/** Maps the result of reading `S` with an implicit form to `T`. */
	def flatMap[S, T](f :S => Option[T])(implicit source :SQLReadForm[S], nulls :Maybe[NullValue[T]]) :SQLReadForm[T] = {
		val nullValue = nulls.opt getOrElse source.nulls.flatMap(f)
		new FlatMappedSQLReadForm(f)(source, nullValue)
	}

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def flatMap[S, T](name :String)(f :S => Option[T])
	                 (implicit source :SQLReadForm[S], nulls :Maybe[NullValue[T]]) :SQLReadForm[T] =
	{
		val nullValue = nulls.opt getOrElse source.nulls.flatMap(f)
		new FlatMappedSQLReadForm(f, name)(source, nullValue)
	}

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def flatMap[S :SQLReadForm, T](f :S => Option[T], nullValue: => T) :SQLReadForm[T] =
		flatMap(f)(SQLReadForm[S], NullValue.eval(nullValue))

	/** Maps the result of reading `S` with an implicit form to `T`. */
	def flatMap[S :SQLReadForm, T](name :String, f :S => Option[T], nullValue: => T) :SQLReadForm[T] =
		flatMap(name)(f)(SQLReadForm[S], NullValue.eval(nullValue))



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






	/** A minimal mix-in trait for `SQLWriteForm` implementations which use their
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.defaults defaults]] type class for
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]]. Note that this reverses the standard
	  * order of delegation: `nulls` is not abstract and its default implementation directs to `nullValue`,
	  * leading to infinite recursion unless overriden.
	  */
	trait ReadFormNullValue[+T] extends SQLReadForm[T] {
		override def nullValue :T = nulls.value
	}


	/** Implements `nullValue` as throwing a `NoSuchElementException` and sets `nulls` to `NullValue.NotNull`. */
	trait NotNullReadForm[+T] extends SQLReadForm[T] {
		override def nulls :NullValue[T] = NullValue.NotNull
		override def nullValue :T = throw new NoSuchElementException("Null value not allowed for " + this + ".")
		override def notNull :this.type = this
	}

	trait ReadFormNullGuard[+T] extends NotNullReadForm[T] {
		override val nulls :NullValue[T] = NullValue.NotNull

//		abstract override def apply(res :ResultSet, position :Int) :T = {
//			val t = super.apply(res, position)
//			if (t == null) throw new NullPointerException("Cannot return null from " + this + ".")
//			else t
//		}

		abstract override def opt(res :ResultSet, position :Int) :Opt[T] = super.opt(res, position) match {
			case Got(null) => throw new NullPointerException("Cannot return null from " + this + ".")
			case t => t
		}
	}


	/** Implements `nullValue` as `null` and sets `nulls` to `NullValue.Null`. */
	trait NullableReadForm[+T >: Null] extends SQLReadForm[T] {
		override def nulls :NullValue[T] = NullValue.Null
		override def nullValue :T = null
	}



	/** A convenience base `SQLReadForm[T]` class which implements `nullValue` based on an implicit `NullValue[T]`
	  * (overriding also `nulls` in the process). */
	abstract class AbstractSQLReadForm[+T](implicit override val nulls :NullValue[T])
		extends SQLReadForm[T] with ReadFormNullValue[T]



	trait CompositeReadForm[+T] extends SQLReadForm[T] {
		protected def forms :Seq[SQLReadForm[Any]]

		override def register(call :CallableStatement, position :Int) :Unit = {
			var i = position
			forms foreach { form => form.register(call, position); i += form.readColumns }
		}

		override def readColumns :Int = (0 /: forms)(_ + _.readColumns)
	}



	trait EmptyReadForm[+T] extends SQLReadForm[T] {
		override def readColumns = 0

		override def opt(res :ResultSet, position :Int) :Opt[T] = Lack
		override def apply(res :ResultSet, position :Int) :T = nullValue
		override def register(call :CallableStatement, position :Int) :Unit = ()

		protected override def errorMessage(position :Int, res :ResultSet) :String = {
			val columns = Iterator.iterate(position)(_ + 1).take(readColumns).map {
				i => res.getMetaData.getColumnName(i)
			}
			columns.mkString(this.toString + " does not allow null values for (", ", ", ").")
		}
	}



	trait ReadFormAdapter[+T] extends SQLReadForm[T] {
		protected def form :SQLReadForm[Any]

		override def register(call :CallableStatement, position :Int) :Unit = form.register(call, position)
		override def readColumns :Int = form.readColumns
	}


	trait ProxyReadForm[+T] extends SQLReadForm[T] {
		protected def form :SQLReadForm[T]

		override def opt(res :ResultSet, position :Int) :Opt[T] = form.opt(res, position)
		override def apply(res :ResultSet, position :Int) :T = form(res, position)

		override def nulls :NullValue[T] = form.nulls
		override def nullValue :T = form.nullValue
		override def readColumns :Int = form.readColumns

		override def notNull :SQLReadForm[T] = form.notNull

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
	  * respectively. See [[net.noresttherein.oldsql.morsels.SpecializingFactory SpecializingFactory]]
	  * for more information about this framework type.
	  */
	type ReadFormBasedFactory[A[_], M[_], S[X] <: M[X]] = SpecializingFactory[A, A, SQLReadForm, ColumnReadForm, M, S]






	private[schema] class NullValueSQLReadForm[+T :NullValue](columns :Int = 0, name :String = null)
		extends AbstractSQLReadForm[T] with EmptyReadForm[T]
	{
		override def readColumns :Int = columns

		override def notNull :SQLReadForm[T] = try {
			if (nullValue == null) error(columns, toString + ".notNull") {
				throw new NullPointerException("Cannot return null from " + this + ".notNull.")
			} else this
		} catch { case _ :Exception => this }

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case nulls :NullValueSQLReadForm[_] if nulls.canEqual(this) =>
				nulls.readColumns == readColumns && nulls.nulls == this.nulls
			case _ => false
		}

		override def hashCode :Int = nulls.hashCode

		override val toString =
			if (name != null) name
			else if (columns == 1) "NULL>"
			else columns.toString + "*NULL>"
	}


	private[schema] class NotNullSQLReadForm[+T](implicit protected override val form :SQLReadForm[T])
		extends ProxyReadForm[T] with ReadFormNullGuard[T]
	{
		override def apply(res :ResultSet, position :Int) :T = {
			val t = super.apply(res, position)
			if (t == null) throw new NullPointerException("Cannot return null from " + this + ".")
			else t
		}

		override val toString = form.toString + ".notNull"
	}



	private[schema] class CustomSQLReadForm[+T :NullValue](columns :Int, name :String = null)(read :(ResultSet, Int) => T)
		extends AbstractSQLReadForm[T]
	{
		override def apply(res :ResultSet, position :Int) :T = read(res, position)
		override def opt(res :ResultSet, position :Int) :Opt[T] = Opt(read(res, position))

		override def notNull :SQLReadForm[T] =
			new CustomSQLReadForm[T](columns, toString + ".notNull")(read)(NotNull) with NotNullReadForm[T] {
				override val nulls = NotNull
				override def apply(res :ResultSet, position :Int) :T = {
					val t = read(res, position)
					if (t == null) throw new NullPointerException("Cannot return null from " + this + ".")
					else t
				}
			}

		override def readColumns = columns
		override val toString = if (name != null) name else "SQLReadForm@" + hashCode
	}


	private[schema] class CustomOptSQLReadForm[+T :NullValue](columns :Int, name :String = null)
	                                                         (read :(ResultSet, Int) => Opt[T])
		extends AbstractSQLReadForm[T]
	{
		override def opt(res :ResultSet, position :Int) :Opt[T] = read(res, position)

		override def notNull :SQLReadForm[T] =
			new CustomOptSQLReadForm[T](columns, toString + ".notNull")(read)(NotNull) with ReadFormNullGuard[T]

		override def readColumns = columns
		override val toString = if (name != null) name else "SQLReadForm@" + hashCode
	}


	private[schema] class ConstSQLReadForm[+T :NullValue](value :Opt[T], columns :Int, name :String = null)
		extends AbstractSQLReadForm[T]
	{
		override def readColumns :Int = columns

		protected def get :Opt[T] = value

		override def opt(res :ResultSet, position :Int): Opt[T] = value

		override def notNull :SQLReadForm[T] = value match {
			case Got(null) => error(columns, toString + ".notNull") {
				throw new NullPointerException("Cannot return a null value from " + this + ".opt.")
			}
			case _ if nulls == NullValue.NotNull => this
			case _ => new ConstSQLReadForm[T](value, columns, name)(NullValue.NotNull)
		}

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
	                                     (value: => Opt[T], columns :Int = 0, override val toString :String = "=_>")
		extends AbstractSQLReadForm[T]
	{
		override def readColumns :Int = columns

		override def opt(res :ResultSet, position :Int): Opt[T] = value

		override def notNull :SQLReadForm[T] =
			new EvalSQLReadForm[T](value, columns, toString + ".notNull") with ReadFormNullGuard[T]
	}



	private[schema] class FlatMappedSQLReadForm[S, +T](protected[this] final val map :S => Option[T], name :String)
	                                                  (implicit protected[this] val source :SQLReadForm[S],
	                                                   implicit override val nulls :NullValue[T])
		extends AbstractSQLReadForm[T]
	{	//map/flatMap not overriden to preserve potentially custom name.
		def this(map :S => Option[T])(implicit source :SQLReadForm[S], nulls :NullValue[T]) =
			this(map, source.toString + "=>")

		override def readColumns :Int = source.readColumns

		override def opt(res :ResultSet, position :Int) :Opt[T] = source.opt(res, position) match {
			case Got(s) => map(s)
			case _ => Lack
		}

		override def notNull :SQLReadForm[T] =
			new FlatMappedSQLReadForm(map, toString + ".notNull")(source, NullValue.NotNull) with ReadFormNullGuard[T]

		override def toString :String = if (name != null) name else source.toString + "=>"
	}


	private[schema] class MappedSQLReadForm[S, +T](protected[this] final val map :S => T, name :String)
	                                              (implicit protected[this] val source :SQLReadForm[S],
	                                               implicit override val nulls :NullValue[T])
		extends AbstractSQLReadForm[T]
	{	//map/flatMap not overriden to preserve potentially custom name.
		def this(map :S => T)(implicit source :SQLReadForm[S], nulls :NullValue[T]) =
			this(map, source.toString + "=>")

		override def readColumns :Int = source.readColumns

		override def opt(res :ResultSet, position :Int) :Opt[T] = source.opt(res, position).map(map)

		override def notNull :SQLReadForm[T] =
			new MappedSQLReadForm[S, T](map, toString + ".notNull")(source, NullValue.NotNull) with ReadFormNullGuard[T]

		override def toString :String = if (name != null) name else source.toString + "=>"
	}



	private[schema] class FallbackSQLReadForm[+T](overrides :SQLReadForm[T], fallback :SQLReadForm[T])
		extends SQLReadForm[T]
	{
		protected def first :SQLReadForm[T] = overrides
		protected def second :SQLReadForm[T] = fallback

		override def opt(res :ResultSet, position :Int) :Opt[T] =
			first.opt(res, position) orElse second.opt(res, position)

		override def nullValue :T = overrides.nullValue

		override def nulls :NullValue[T] = overrides.nulls

		override def register(call :CallableStatement, position :Int) :Unit =
			overrides.register(call, position)

		override def readColumns :Int = overrides.readColumns

		override def notNull :SQLReadForm[T] = overrides.notNull orElse fallback.notNull

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
		override def notNull :SQLReadForm[T] = form.notNull
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

		override def opt(res :ResultSet, position :Int): Opt[Seq[T]] = Opt(apply(res, position))

		override def apply(res :ResultSet, position :Int): Seq[T] = {
			val f = form
			var i = position; val jump = form.readColumns
			val result = List.newBuilder[T]
			var countdown = repeats
			while (repeats > 0) f.opt(res, i) match {
				case Got(x) => result += x; countdown -= 1; i += jump
				case _ => countdown -= 1; i += jump
			}
			result.result()
		}

		override def nullValue: Seq[T] = Seq()

		override def toString :String = "(" + repeats.toString + "*" + form + ")"
	}


	private case class SeqSQLReadForm[+T](form :SQLReadForm[T], repeats :Int) extends SeqReadForm[T] {
		def this(repeats :Int)(implicit form :SQLReadForm[T]) = this(form, repeats)

		override def notNull :SQLReadForm[Seq[T]] = new SeqSQLReadForm[T](form.notNull, repeats)

		override val readColumns = super.readColumns
		override val toString = super.toString
	}



	private[schema] trait ReadFormSeq[+T] extends CompositeReadForm[Seq[T]] {
		protected override val forms :Seq[SQLReadForm[T]]

		override def opt(res :ResultSet, position :Int) :Opt[Seq[T]] = {
			var i = position + readColumns
			(forms :\ Option(List.empty[T])) { (form, acc) =>
				acc flatMap { tail =>
					i -= form.readColumns
					form.opt(res, i) match {
						case Got(head) => Some(head::tail)
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

		override def notNull :SQLReadForm[Seq[T]] = new SQLReadFormSeq[T](forms.map(_.notNull))
		override val toString = super.toString
	}

}


