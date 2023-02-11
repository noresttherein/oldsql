package net.noresttherein.oldsql.schema

import java.lang.Double.doubleToLongBits
import java.sql.{CallableStatement, JDBCType, ResultSet, SQLException, Types}

import scala.annotation.implicitNotFound
import scala.collection.immutable.Seq
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{rethrow, NullValueException, OldSQLException}
import net.noresttherein.oldsql.morsels.SpecializingFactory
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.pixies.{CallableStatementOutParams, RearrangedIndexing}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull
import net.noresttherein.oldsql.schema.SQLReadForm.{FallbackSQLReadForm, IgnoringReadForm, NotNullReadFormProxy, ReadFormAdapter, ReadFormProxy}
import net.noresttherein.oldsql.schema.forms.{CaseSQLReadForm, CustomOptSQLReadForm, CustomSQLReadForm, DerivedMappedSQLReadForm, DerivedOptMappedSQLReadForm, LazySQLReadForm, MappedSQLReadForm, NotNullCustomReadForm, OffsetReadForm, OptMappedSQLReadForm, ReorderedReadForm, RepeatedSQLReadForm, SQLReadFormSeq, UnspecifiedForm}
import net.noresttherein.oldsql.schema.forms.UnspecifiedForm.{BaseFormAdapter, UnspecifiedFormAdapter, UnspecifiedNamedForm}
import net.noresttherein.oldsql.slang.{classNameMethods, localNameOf}
import net.noresttherein.oldsql.sql.RowShape







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
trait SQLReadForm[+T] extends UnspecifiedForm {

	/** Number of columns read by this form. This must be a constant as it is typically is used to calculate offsets
	  * for various forms once per `ResultSet` rather than per row. Naturally, there is no requirement for actual
	  * reading of all columns if the form can determine based on some indicator (such as a `null` primary key) that
	  * the value cannot be assembled from the given column set for the row.
	  */
	override def columnCount :Int

	/** A form is not universal if the values it returns do not depend on the values in the `ResultSet`
	  * (for example, in a [[net.noresttherein.oldsql.schema.SQLReadForm.const const]] form). For multi column forms,
	  * it is sufficient that at least some of the columns are ignored in favour of external data.
	  */
	override def isUniversal = true


	/** Reads the column values from columns `<position..position + this.columnCount)` of the passed `ResultSet`
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
		case _ => guardedNullValue(res, position)
	}

	/** Attempts to read the column values from columns `<position..position + this.columnCount` of the passed
	  * `ResultSet` and create an instance of `T`. If the values are unavailable (required columns carry `null` values),
	  * `Lack` is returned. It is the recommended practice to have the returned option reflect only the availability
	  * of the input values and not their validity. It is allowed for the form to return `Got(null)`
	  * (as long as `T >: Null`), but this results in propagation of `null` values to any forms derived from it
	  * and to the application. As such, it is discouraged, with the exception of `ColumnForm`s, as the validity of
	  * `null` values can be explicitly switched on for columns using the `Nullable` buff (and otherwise affected
	  * by other buffs). The `TypedColumn` trait explicitly checks for `null` values throwing a `NullPointerException`
	  * if they are not permitted. A form is allowed to throw an exception if the values do not conform
	  * to expected constraints or the assembly process fails for any other reason.
	  * Similarly, all thrown `SQLException`s are propagated.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]]
	  */
	def opt(res :ResultSet, position :Int) :Opt[T]

	/** Attempts to read the values of ''out'' parameters of the given `CallableStatement` and create an instance
	  * of `Opt[T]`. The index of the first parameter to read is `position`; all succeeding parameters until
	  * (not including) `position + this.columnCount` should also be ''out'' parameters reserved for this form.
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
	  * `position + this.columnCount` should also be ''out'' parameters reserved for this form.
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


	/** Registers on `call` ''out'' parameters for all columns in this form using
	  * the [[java.sql.CallableStatement.registerOutParameter registerOutParameter]] method.
	  * @param call     a `CallableStatement` with JDBC parameters representing consecutive columns of this form.
	  * @param position the index of the first ''out'' parameter.
	  */
	def register(call :CallableStatement, position :Int) :Unit = {
		var i = position + columnCount - 1
		while (i >= position) {
			call.registerOutParameter(position, Types.OTHER); i -= 1
		}
	}

	/** The value a `null` column (or all `null` columns) should be mapped to. It is used in particular by `apply`
	  * when the value is unavailable, for example as a result of an outer join. Extending classes are allowed
	  * to throw an exception here (either a `NoSuchElementException` or a `NullPointerException`) if a concept
	  * of null does not exist for `T` or `null`s are not acceptable values. It is however completely acceptable
	  * to provide any particular value of `T` as a part of the mapping process (for example, a form for `Option[T]`
	  * will return `None`).
	  */
	//Consider: we might want to differentiate from 'default value' (functional) and true 'null' value
	// (i.e, something that will map to all nulls), because LabeledColumnSQL currently uses form.nulls.nullValue
	// to generate null columns when aligning column sets of two expressions.
	def nullValue :T

	/** Returns `this.`[[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]], catching any
	  * [[net.noresttherein.oldsql.exceptions.OldSQLException OldSQLException]], [[NullPointerException]],
	  * [[NoSuchElementException]] and [[ClassCastException]] and rethrowing a new instance of the same class
	  * with information about the `position`-th column in `res`, as formatted
	  * by method [[net.noresttherein.oldsql.schema.SQLReadForm.errorMessage errorMessage]].
	  */
	protected def guardedNullValue(res :ResultSet, position :Int) :T = try {
			nullValue
		} catch {
			case e :OldSQLException =>
				throw substituteException(res, position, e) { (msg, e) => e.addInfo(msg) }
			case e :NullPointerException =>
				throw substituteException(res, position, e)(new NullPointerException(_).initCause(_))
			case e :NoSuchElementException =>
				throw substituteException(res, position, e)(new NoSuchElementException(_).initCause(_))
			case e :ClassCastException =>
				throw substituteException(res, position, e)(new ClassCastException(_).initCause(_))
		}

	private def substituteException[E <: Throwable](res :ResultSet, position :Int, cause :E)
	                                               (exception :(String, E) => Throwable) :Throwable =
		try { exception(errorMessage(res, position), cause) } catch {
			case e :Exception =>
				val res = exception(this.toString + " does not allow null values.", cause)
				res.addSuppressed(e)
				res
		}

	/** A hook for subclasses to provide an error message to use for a rethrown `NullPointerException` caught
	  * from [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]]. Default implementations lists
	  * columns read by [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]] with their values, which lead
	  * to the exception `caught` being thrown.
	  */
	protected def errorMessage(res :ResultSet, position :Int) :String = {
		val columns = Iterator.iterate(position)(_ + 1).take(columnCount).map {
			i => res.getMetaData.getColumnName(i) + "=" + res.getObject(i)
		}
		columns.mkString(this.toString + " does not allow null values. Read ", ", ", ".")
	}

	/** Null representation for type `T`. It is the value returned whenever a `null` is read from a database column
	  * (or is reported with [[java.sql.ResultSet.wasNull wasNull]]).
	  * It should be consistent with `this.nullValue` and the default implementation simply delegates to it at each call.
	  * This wrapper can be implicitly passed as a type class, even if the form does not support `null` values
	  * (i.e. `nullValue` throws an exception), which would not be possible by simply using `nullValue`.
	  */
	def nulls :NullValue[T] = new FormNullValue

	private class FormNullValue extends NullValue[T] {
		private def outer :SQLReadForm[T] = SQLReadForm.this
		override def value :T = nullValue
		override def value(msg :String) = rethrow { nullValue } (msg)
		override def map[U](f :T => U) = NullValue.eval(f(nullValue))

		override def equals(that :Any) :Boolean = that match {
			case formNull :SQLReadForm[_]#FormNullValue => formNull.outer eq SQLReadForm.this
			case _ => false
		}
		override def hashCode :Int = System.identityHashCode(SQLReadForm.this)

		override def toString = s"$outer.nullValue"
	}


	/** A mixin trait for anonymous inner proxy classes to this form, which should remain comparable to this instance.
	  * Method [[net.noresttherein.oldsql.schema.SQLReadForm.comparable comparable]] of this class checks
	  * if its argument is a `ComparableReadForm` of this instance or vice versa; if this check succeeds,
	  * the forms are automatically comparable without further comparisons.
	  */
	protected[schema] trait ComparableReadForm extends SQLReadForm[T] {
		private[SQLReadForm] def parent :SQLReadForm[T] = SQLReadForm.this
//		override def columnCount :Int = SQLReadForm.this.columnCount
//		override def columnTypes :Seq[JDBCType] = SQLReadForm.this.columnTypes
		override def comparable(other :SQLReadForm[_]) :Boolean =
			super.comparable(other) || SQLReadForm.this.comparable(other)
	}



	//todo: in Scala3, named mapping (with equality)
	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. This method variant
	  * depends on implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] to provide the value of `X`
	  * to be used when `null` value(s) are read from the `ResultSet`. This guarantees that the given function will
	  * not be called for `null` arguments unless this form returns `Some(null)` from its `opt` method in a non-standard
	  * practice and allows handling of `null` values also when `T` is a value type without a natural `null` value.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullMap]]
	  */
	def map[X :NullValue](f :T => X) :SQLReadForm[X] = SQLReadForm.map(f)(this, NullValue[X])

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. If the underlying columns
	  * carry null values, passed `nullValue` is used instead of mapping. This guarantees that the given function will
	  * not be called for `null` arguments unless this form returns `Some(null)` from its `opt` method in a non-standard
	  * practice and allows handling of `null` values also when `T` is a value type without a natural `null` value.
	  * Note that `this.nullValue` is never called, directly or indirectly, by the created form.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullMap]]
	  */
	def map[X](f :T => X, nullValue :X) :SQLReadForm[X] = map(f)(NullValue(nullValue))

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. Note that the given
	  * function may be called for `null` arguments, even if the underlying columns have a ''not null'' constraint
	  * in case of outer join queries.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  */
	def nullMap[X](f :T => X) :SQLReadForm[X] = map(f)(nulls.map(f))


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
	def optMap[X :NullValue](f :T => Option[X]) :SQLReadForm[X] = SQLReadForm.optMap(f)(this, NullValue[X])

	/** Attempts to map the value of `T` read by this form to type `X` in order to produce a form for `X`.
	  * Unlike `map`, not all values of `T` may have an associated counterpart in `X`, in which case the given function
	  * returns `None` and the new form defaults to the given `nullValue`.  The function is guaranteed not to be called
	  * for `null` arguments, with the created form using `nullValue` instead, unless this form returns `Some(null)`
	  * from its `opt` method in a non-standard practice. Note that `this.nullValue` is never called,
	  * directly or indirectly, by the created form.
	  */
	def optMap[X](f :T => Option[X], nullValue :X) :SQLReadForm[X] = optMap(f)(NullValue(nullValue))

	/** Attempts to map the value of `T` read by this form to type `X` in order to produce a form for `X`.
	  * Unlike `map`, not all values of `T` may have an associated counterpart in `X`, in which case the given function
	  * returns `None` and the new form defaults to the given `nullValue`. The `nullValue` for the new form
	  * is determined by applying the given function to the `nullValue` of this form. If the function returns `None`,
	  * a `NoSuchElementException` is thrown when the new form's `nullValue` method is called. If the function throws
	  * a `NullPointerException` or `NoSuchElementException` for the null value, the same exception will be thrown
	  * from the `nullValue` method.
	  */
	def nullOptMap[X](f :T => Option[X]) :SQLReadForm[X] = optMap(f)(nulls.optMap(f))



	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. The returned form will
	  * use the implicitly available `nullValue` except for when `fun` is an identity extractor, in which case
	  * this instance will be returned. This will investigate the type of the extractor and either delegate to `map`
	  * or `flatMap` or return a specific form in corner cases.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.optMap]]
	  */
	def to[X :NullValue](f :T =?> X) :SQLReadForm[X] = SQLReadForm(f)(this, NullValue[X])

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. The returned form will
	  * use the given `nullValue` except for when `fun` is an identity extractor, in which case this instance
	  * will be returned. This will investigate the type of the extractor and either delegate to `map` or `flatMap` or
	  * return a specific form in corner cases.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.optMap]]
	  */
	def to[X](f :T =?> X, nullValue :X) :SQLReadForm[X] = to(f)(NullValue(nullValue))

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`.
	  * This will call `nullMap` or `nullFlatMap` based on whether the extract is a `RequisiteExtractor`.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullMap]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullOptMap]]
	  */
	def nullTo[X](f :T =?> X) :SQLReadForm[X] = to(f)(nulls.andThen(f))

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. The returned form will
	  * use the implicitly available `nullValue` except for when `fun` is an identity extractor, in which case
	  * this instance will be returned. This will investigate the type of the extractor and either delegate to `map`
	  * or `flatMap` or return a specific form in corner cases.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.optMap]]
	  */
	def andThen[X :NullValue](f :T =?> X) :SQLReadForm[X] = to(f)

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. The returned form will
	  * use the given `nullValue` except for when `fun` is an identity extractor, in which case this instance
	  * will be returned. This will investigate the type of the extractor and either delegate to `map` or `flatMap` or
	  * return a specific form in corner cases.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.optMap]]
	  */
	def andThen[X](f :T =?> X, nullValue :X) :SQLReadForm[X] = to(f)(NullValue(nullValue))

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`.
	  * This will call `nullMap` or `nullFlatMap` based on whether the extract is a `RequisiteExtractor`.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullMap]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullOptMap]]
	  */
	def andThenNull[X](f :T =?> X) :SQLReadForm[X] = to(f)(nulls.andThen(f))



	/** Lifts this form to represent `Option[T]`. The created form maps all values returned by this form using
	  * `Option(...)`. This means that `null` values (actual JVM nulls, not the somewhat arbitrary value provided
	  * by this form) are mapped to `Got(None)`, while a returned `Lack` indicates that this form's `opt` method
	  * returned `Lack`. Basically this means that the returned form uses this form's `opt` method
	  * as its `apply` implementation.
	  */
	def toOpt :SQLReadForm[Option[T]] = { implicit val self = this; SQLReadForm[Option[T]] }

	/** An adapter or modification of this form which ensures that `null` values will never be returned by
	  * `apply` or `opt`. If such an event would occur in this form, the returned form will throw
	  * a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  */
	override def notNull :SQLReadForm[T] = new BaseFormAdapter(this) with NotNullReadFormProxy[T]

	/** An adapter to this form which reads the columns out of order.
	  * A call to `proxyForm.`[[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]]`(resultSet, start)` will
	  * delegate to `opt(wrapperResultSet, 1)` on this form, where `wrapperResultSet` is a wrapper over
	  * the original `resultSet` whose `n`-th column maps to column at index `permutation(n - 1) + start`
	  * in the adapted `ResultSet` (`+1` coming from the fact that column numbering in a `ResultSet` starts with `1`,
	  * rather than `0`). Note that this means that in order to calculate at what position the `n-th` column
	  * of a `ResultSet` passed to the adapter will appear to this form, one has to apply the inverse permutation
	  * `inverse`, such that `inverse(permutation(i)) == i`. In other words, the `n`-th column of an argument `ResultSet`
	  * will be moved to position `inverse(n - start) + 1`
	  * @param permutation A permutation vector of length equal to
	  *                    `this.`[[net.noresttherein.oldsql.schema.SQLReadForm.columnCount columnCount]],
	  *                    where every value from range `[0, this.columnCount)` appears exactly once.
	  */
	def reorder(permutation :IndexedSeq[Int]) :SQLReadForm[T] = ReorderedReadForm.shuffle(this, permutation)

	/** An adapter to this form which reads columns out of order, possibly ignoring some additional columns
	  * in the [[java.sql.ResultSet ResultSet]]. If the index translation
	  * is not a [[net.noresttherein.oldsql.pixies.RearrangedIndexing.isInjection injection]], then `null`
	  * will be returned for columns missing in the underlying `ResultSet`.
	  * @param order a mapping translating the indices of the argument `ResultSet` to the indices in the order
	  *              expected by this form and back.
	  *              Its [[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]] must equal
	  *              this form's [[net.noresttherein.oldsql.schema.SQLReadForm.columnCount columnCount]].
	  */
	def reorder(order :RearrangedIndexing) :SQLReadForm[T] = ReorderedReadForm(this, order)

	/** An adapter to this form which adds `shift` to the read column indices before delegating to this form.
	  * This method breaks implicit expectations for a form and the created instance will almost certainly result
	  * in unexpected behaviour if exposed to the outside. For this reason the reference to it should always
	  * be contained within a class which ensures that all its uses are valid.
	  */
	def >>(shift :Int) :SQLReadForm[T] =
		if (shift == 0) this
		else
			new BaseFormAdapter(this) with ReadFormProxy[T] with OffsetReadForm[T] {
				override val offset = shift
			}

	/** An adapter to this form which subtracts `columns` from the read column indices before delegating to this form. */
	def <<(shift :Int) :SQLReadForm[T] = this >> -shift


	/** Creates a read form which will apply this form `repeat` number of times, reading identical consecutive columns
	  * (or repeated sequences of same columns) and returning all 'non-null' (that is all, for which
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] was not empty) values as a sequence.
	  */
	def *(repeat :Int) :SQLReadForm[Seq[T]] = SQLReadForm.seq(repeat)(this)

	/** Combines this form with another form, which columns are expected to directly follow the columns for this
	  * form in the result set, to create a form producing pairs of values.
	  */
	def *[O](other :SQLReadForm[O]) :SQLReadForm[(T, O)] = {
		implicit val first = this; implicit val second = other
		SQLReadForm[(T, O)]
	}

	/** Chains a default form to fallback to if this form was unable to produce a value by its `opt` method.
	  * The fallback form must use the same number of columns as this form.
	  * @throws IllegalArgumentException if this.columnCount != fallback.columnCount
	  * @return a form for `S` which defines its `opt` method as `this.opt(...) orElse fallback.opt(...)`.
	  */
	def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] =
		if (columnCount != fallback.columnCount && fallback.columnCount != 0)
			throw new IllegalArgumentException(
				s"$this orElse $fallback: different number of read columns ($columnCount vs ${fallback.columnCount})."
			)
		else
			new FallbackSQLReadForm(this, fallback)

	/** Creates a conditional form, which uses this form to read first a discriminator value `K`, and then reads
	  * the returned value using the form under that key in the argument map. If no such form exists, the form will
	  * default to
	  * [[net.noresttherein.oldsql.collection.Opt.Lack Lack]]/[[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]]
	  * (for which the 'first' form in the collection is used). If the map however defines a default value,
	  * it will be used instead of the null value.
	  */
	def when[K >: T, X](cases :Map[K, SQLReadForm[X]]) :SQLReadForm[X] =
		if (cases.isEmpty)
			throw new IllegalArgumentException("No cases specified for a conditional form.")
		else
			new CaseSQLReadForm[K, X](this, cases)

	/** Creates a conditional form, which uses this form to read first a discriminator value `K`, and then reads
	  * the returned value using the form associated with that value in the passed list. If no such form exists,
	  * the form will default to
	  * [[net.noresttherein.oldsql.collection.Opt.Lack Lack]]/[[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]]
	  * (for which the \ first form on the collection is used).
	  */
	def when[K >: T, X](cases :(K, SQLReadForm[X])*) :SQLReadForm[X] =
		if (cases.isEmpty)
			throw new IllegalArgumentException("No cases specified for a conditional form.")
		else
			new CaseSQLReadForm[K, X](this, Map.from(cases))(cases.head._2.nulls)



	//todo: enforce columnCount == columnCount and review usages to ensure it is observed
	/** Combines this form with a `SQLWriteForm` to create a read/write `SQLForm[O]`. */
	def <>[O >: T](write :SQLWriteForm[O]) :SQLForm[O] = SQLForm.combine[O](this, write)

	//todo: review all implementations for proper equals (some are missing)

	/** Checks if this form and the other form consist of columns comparable on the database level.
	  * This is defined as having the same number
	  * of [[net.noresttherein.oldsql.schema.SQLReadForm.columnCount columnCount]] and corresponding columns
	  * having the same [[java.sql.SQLType SQLType]]. Forms being comparable says nothing about the relation between
	  * Scala type arguments of the forms (i.e, the types/classes of read objects).
	  * This condition is strictly weaker than form equality, with two equal forms always being comparable.
	  * In particular, an `SQLReadForm` can (and should, if only technically possible) be comparable
	  * with a full [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] of the same columns,
	  * while they would rarely compare equal.
	  *
	  * Note that due to technical limitations, this method may return false negatives.
	  *
	  * The default implementation first compares both forms for equality (which implies comparability) and,
	  * failing that, if the other form is one of
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.IgnoringReadForm IgnoringReadForm]],
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.ReadFormAdapter ReadFormAdapter]],
	  * result of [[net.noresttherein.oldsql.schema.SQLReadForm.orElse orElse]] and, if so, delegates to check
	  * to `other.comparable(this)`. Otherwise, if both forms support property
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.columnTypes columnTypes]], the result of their comparison
	  * is returned. If neither of the checks passes, the forms are deemed not comparable.
	  *
	  * @return the default implementation checks only if `this == other` or,
	  *         if `other` is a [[net.noresttherein.oldsql.schema.SQLReadForm.ReadFormAdapter ReadFormAdapter]],
	  *         if the adapter (and thus its adapted form) is comparable with this form.
	  */
	def comparable(other :SQLReadForm[_]) :Boolean =
		this == other || columnCount == 0 && other.columnCount == 0 || (other match {
			case _ if columnCount != other.columnCount => false
			case _ :IgnoringReadForm[_] => true
			case adapter :ReadFormAdapter[_] => adapter comparable this
			case derived :SQLReadForm[_]#ComparableReadForm if this comparable derived.parent => true
			case fallback :FallbackSQLReadForm[_] => fallback comparable this
			case _ => shape <:> other.shape
		})

}






/** A factory of [[net.noresttherein.oldsql.schema.SQLReadForm! SQLReadForm]] instances.
  * Most methods require an implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] type class
  * for the mapped type in order to implement its [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]]
  * method. It is however often optional, and in absence of an implicit value,
  * `NullValue.`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]] will be used,
  * which throws a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]] intead of returning
  * `null` or some other null representation to the application.
  */
object SQLReadForm {

	/** Summons an implicit `SQLReadForm[T].` */
	@inline def apply[T](implicit form :SQLReadForm[T]) :SQLReadForm[T] = form


	/** Creates a new `SQLReadForm` using the given function to read the values from the result set.
	  * The function is responsible for handling null columns itself; returned `null` values will be passed
	  * to the caller of [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] directly, while
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] will return `Lack`.
	  * Method [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]] method of the form is unused
	  * and throws a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  * Exceptions thrown by `reader` are not caught.
	  * Alternatively, [[net.noresttherein.oldsql.schema.SQLReadForm.notNull notNull]] method
	  * of the result may be used to obtain a form which rejects `null` values returned by the function
	  * with a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]]
	  * (and also returns `Lack` from its `opt` method).
	  * @param columns the number of columns read by the form.
	  * @param reader  a function taking an SQL `ResultSet`, a start column position, and reads the following
	  *                `columns` columns to create a value of `T`.
	  */
	def apply[T](columns :Int)(reader :(ResultSet, Int) => T) :SQLReadForm[T] =
		new CustomSQLReadForm[T](columns)(reader)

	/** Creates a new `SQLReadForm` using the given function to read the values from the result set.
	  * The function is responsible for handling null columns itself; returned `null` values will be passed
	  * to the caller of [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] directly, while
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] will return `Lack`.
	  * Method [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]] method of the form is unused
	  * and throws a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  * Exceptions thrown by `reader` are not caught.
	  * Alternatively, [[net.noresttherein.oldsql.schema.SQLReadForm.notNull notNull]] method
	  * of the result may be used to obtain a form which rejects `null` values returned by the function
	  * with a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]]
	  * (and also returns `Lack` from its `opt` method).
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name    the name of the form, identifying it and returned by its `toString` method.
	  * @param columns the number of columns read by the form.
	  * @param reader  a function taking an SQL `ResultSet`, a start column position, and reads the following
	  *                `columns` columns to create a value of `T`.
	  */ //todo: in Scala 3, change to (name :String)(columns :Int)(reader :(ResultSet, Int) => T)
	def apply[T](name :String, columns :Int)(reader :(ResultSet, Int) => T) :SQLReadForm[T] =
		new CustomSQLReadForm[T](columns, Got(name))(reader) with NamedReadForm[T]

	/** Creates a new `SQLReadForm` using the given function to read an optional value from the result set.
	  * @param columns the number of columns read by the form.
	  * @param reader  a function taking an SQL `ResultSet`, a start column position, and reads the following
	  *                `columns` columns to create a value of `T`. It becomes the implementation of
	  *                [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] method.
	  */ //todo: in Scala 3, change to (name :String)(columns :Int)(reader :(ResultSet, Int) => Opt[T])
	def opt[T :NullValue](columns :Int)(reader :(ResultSet, Int) => Opt[T]) :SQLReadForm[T] =
		new CustomOptSQLReadForm[T](columns)(reader)

	/** Creates a new `SQLReadForm` using the given function to read an optional value from the result set.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name    the name of the form, identifying it and returned by its `toString` method.
	  * @param columns the number of columns read by the form.
	  * @param reader  a function taking an SQL `ResultSet`, a start column position, and reads the following
	  *                `columns` columns to create a value of `T`. It becomes the implementation of
	  *                [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] method.
	  */ //todo: in Scala _3, change to (name :String)(columns :Int)(reader :(ResultSet, Int) => Opt[T])
	def opt[T :NullValue](name :String, columns :Int)(reader :(ResultSet, Int) => Opt[T]) :SQLReadForm[T] =
		new CustomOptSQLReadForm[T](columns, Got(name))(reader) with NamedReadForm[T]


	/** Maps the result of reading `S` with an implicit form to `T` using the specified `Extractor`.
	  * This method is equivalent to [[net.noresttherein.oldsql.schema.SQLReadForm$.map map]],
	  * [[net.noresttherein.oldsql.schema.SQLReadForm$.optMap optMap]],
	  * or [[net.noresttherein.oldsql.schema.SQLReadForm$.const]], depending on the type of the extractor.
	  *
	  * If an implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` exists,
	  * database `NULL`s will be returned as its [[net.noresttherein.oldsql.schema.SQLForm.NullValue!.value value]].
	  * In absence of this type class, the form will instead use the given extractor to map the values
	  * returned by the mapped form's [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  * If the extractor does not return a result when applied to `SQLReadForm[S].nullValue`,
	  * a [[NoSuchElementException]] will be thrown from the form's `nullValue`
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] methods.
	  */
	def apply[S :SQLReadForm, T :NullValue.Maybe](extract :S =?> T) :SQLReadForm[T] =
		extract match {
			case _ :EmptyExtractor[_, _] =>
				SQLReadForm.defaults(SQLReadForm[S].columnCount)(NullValue.orNotNull[T])
			case _ :OptionalExtractor[_, _] => optMap(extract.optional)
			case _ :IdentityExtractor[_] => SQLReadForm[S].asInstanceOf[SQLReadForm[T]]
			case const :ConstantExtractor[_, T @unchecked] =>
				SQLReadForm.const(SQLReadForm[S].columnCount)(const.constant)
			case req :RequisiteExtractor[S @unchecked, T @unchecked] => map(req.getter)
			case _ => optMap(extract.optional)
		}

	/** Creates a new `SQLReadForm` of a 'soft type' given as `name` argument by applying the given extractor
	  * to values read by an implicit base `SQLReadForm[S]`. This is equivalent to
	  * `SQLReadForm`[[net.noresttherein.oldsql.schema.SQLReadForm.apply[S,T](extract* (extract)]],
	  * but the created form will equal any other `SQLReadForm` created by this method if they have the same name
	  * and their base forms and [[net.noresttherein.oldsql.schema.SQLReadForm.nulls null values]] are equal.
	  *
	  * This method is equivalent to [[net.noresttherein.oldsql.schema.SQLReadForm$.map map]],
	  * [[net.noresttherein.oldsql.schema.SQLReadForm$.optMap optMap]],
	  * or [[net.noresttherein.oldsql.schema.SQLReadForm$.const]], depending on the type of the extractor.
	  *
	  * If an implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` exists,
	  * database `NULL`s will be returned as its [[net.noresttherein.oldsql.schema.SQLForm.NullValue!.value value]].
	  * In absence of this type class, the form will instead use the given extractor to map the values
	  * returned by the mapped form's [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  * If the extractor does not return a result when applied to `SQLReadForm[S].nullValue`,
	  * a [[NoSuchElementException]] will be thrown from the form's `nullValue`
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] methods.
	  */
	def apply[S :SQLReadForm, T :NullValue.Maybe](name :String)(extract :S =?> T) :SQLReadForm[T] =
		extract match {
			case _ :EmptyExtractor[_, _] =>
				SQLReadForm.defaults(name)(SQLReadForm[S].columnCount)(NullValue.orNotNull[T])
			case _ :OptionalExtractor[_, _] =>
				optMap(name)(extract.optional)
			case const :ConstantExtractor[_, T @unchecked] =>
				SQLReadForm.const(name)(SQLReadForm[S].columnCount)(const.constant)
			case req :RequisiteExtractor[S @unchecked, T @unchecked] =>
				map(name)(req.getter)
			case _ => optMap(name)(extract.optional)
		}

	/** Maps the result of reading `S` with an implicit form to `T` using the specified `Extractor`.
	  * The by name `nullValue` argument is used as the implementation
	  * of the form's [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]] method.
	  */
	def apply[S :SQLReadForm, T](map :S =?> T, nullValue: => T) :SQLReadForm[T] =
		apply(map)(SQLReadForm[S], NullValue.eval(nullValue))


	/** Maps the result of reading `S` with an implicit form to `T`.
	  * If an implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` exists,
	  * database `NULL`s will be returned as its [[net.noresttherein.oldsql.schema.SQLForm.NullValue!.value value]].
	  * In absence of this type class, the form will instead use the given function to map the values
	  * returned by the mapped form's [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  */
	def map[S :SQLReadForm, T :NullValue.Maybe](f :S => T) :SQLReadForm[T] =  {
		implicit val nullValue = NullValue.orElse(SQLReadForm[S].nulls.map(f))
		new MappedSQLReadForm[S, T](f)
	}

	/** Creates a new `SQLReadForm` of a 'soft type' given as `name` argument by applying the given function
	  * to values read by an implicit base `SQLReadForm[S]`.
	  * If an implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` exists,
	  * database `NULL`s will be returned as its [[net.noresttherein.oldsql.schema.SQLForm.NullValue!.value value]].
	  * In absence of this type class, the form will instead use the given function to map the values
	  * returned by the mapped form's [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  *
	  * This method equivalent to
	  * `SQLReadForm`[[net.noresttherein.oldsql.schema.SQLReadForm.map[S,T](f:S=>T)* .map(f)]],
	  * but the created form will equal any other `SQLReadForm` created by this method if they have the same name
	  * and their base forms and [[net.noresttherein.oldsql.schema.SQLReadForm.nulls null values]] are equal.
	  */
	def map[S :SQLReadForm, T :NullValue.Maybe](name :String)(f :S => T) :SQLReadForm[T] = {
		implicit val nullValue = Maybe[NullValue[T]] getOrElse SQLReadForm[S].nulls.map(f)
		new DerivedMappedSQLReadForm[S, T](name, f)
	}

	/** Maps the result of reading `S` with an implicit form to `T`. The by name `nullValue` argument
	  * is used as the implementation of the form's [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]]
	  * method.
	  */
	def map[S :SQLReadForm, T](f :S => T, nullValue: => T) :SQLReadForm[T] =
		SQLReadForm.map(f)(SQLReadForm[S], NullValue.eval(nullValue))

	/** Maps the result of reading `S` with an implicit form to `T`.
	  * @param name the name o the form, used as its identifier and in its `toString` representation.
	  *             Forms created by this method are equal if they have the same name and map equal forms.
	  */ //todo: in Scala3 make name a separate parameter group
	def map[S :SQLReadForm, T](name :String, f :S => T, nullValue: => T) :SQLReadForm[T] =
		SQLReadForm.map(name)(f)(SQLReadForm[S], NullValue.eval(nullValue))


	/** Maps the result of reading `S` with an implicit form to `T`.
	  * If an implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` exists,
	  * database `NULL`s will be returned as its [[net.noresttherein.oldsql.schema.SQLForm.NullValue!.value value]].
	  * In absence of this type class, the form will instead use the given function to map the values
	  * returned by the mapped form's [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  * If the function does not return a result when applied to `SQLReadForm[S].nullValue`,
	  * a [[NoSuchElementException]] will be thrown from the form's `nullValue`
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] methods.
	  */
	def optMap[S :SQLReadForm, T :NullValue.Maybe](f :S => Option[T]) :SQLReadForm[T] = {
		implicit val nullValue = NullValue.orElse(SQLReadForm[S].nulls.optMap(f))
		new OptMappedSQLReadForm[S, T](f)
	}

	/** Creates a new `SQLReadForm` of a 'soft type' given as `name` argument by applying the given function
	  * (returning the value as an `Option`) to values read by an implicit base `SQLReadForm[S]`. This is equivalent to
	  * `SQLReadForm.`[[net.noresttherein.oldsql.schema.SQLReadForm.optMap[S,T](f:S=>Option[T])* optMap(f)]],
	  * but the created form will equal any other `SQLReadForm` created by this method if they have the same name
	  * and their base forms and [[net.noresttherein.oldsql.schema.SQLReadForm.nulls null values]] are equal.
	  *
	  * If an implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` exists,
	  * database `NULL`s will be returned as its [[net.noresttherein.oldsql.schema.SQLForm.NullValue!.value value]].
	  * In absence of this type class, the form will instead use the given function to map the values
	  * returned by the mapped form's [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  * If the function does not return a result when applied to `SQLReadForm[S].nullValue`,
	  * a [[NoSuchElementException]] will be thrown from the form's `nullValue`
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] methods.
	  */
	def optMap[S :SQLReadForm, T :NullValue.Maybe](name :String)(f :S => Option[T]) :SQLReadForm[T] =
	{
		implicit val nullValue = Maybe[NullValue[T]] getOrElse SQLReadForm[S].nulls.optMap(f)
		new DerivedOptMappedSQLReadForm[S, T](name, f)
	}

	/** Maps the result of reading `S` with an implicit form to `T`.
	  * The by name `nullValue` argument is used as the implementation
	  * of the form's [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]] method.
	  */
	def optMap[S :SQLReadForm, T](f :S => Option[T], nullValue: => T) :SQLReadForm[T] =
		optMap(f)(SQLReadForm[S], NullValue.eval(nullValue))

	/** Maps the result of reading `S` with an implicit form to `T`.
	  * The by name `nullValue` argument is used as the implementation
	  * of the form's [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]] method.
	  */ //todo: in Scala3 make name a separate parameter group
	def optMap[S :SQLReadForm, T](name :String, f :S => Option[T], nullValue: => T) :SQLReadForm[T] =
		optMap(name)(f)(SQLReadForm[S], NullValue.eval(nullValue))



	//consider: name parameter
	/** A form reading a sequence of columns of the same type, or a sequence of columns repeated several times.
	  * The number of elements in the read sequence will equal the number of times that method
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] of the form for the individual element type `T`
	  * would return a non-empty result.
	  * @param repeats the number of time that `SQLReadForm[T]` should read from the `ResultSet`, each time
	  *                increasing the reading position by its
	  *                [[net.noresttherein.oldsql.schema.SQLReadForm.columnCount columnCount]].
	  */
	def seq[T :SQLReadForm](repeats :Int) :SQLReadForm[Seq[T]] =
		if (repeats == 0) const(0)(Seq())
		else new RepeatedSQLReadForm[T](repeats)

	/** A form applying all forms in the given sequence one after another, each starting at the position following the
	  * last column read by the preceding form, and collecting the results. Produced sequence will always have
	  * the same number of elements as there are forms. If one of the form's
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] method fails to produce a value, the form's
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]] will take its place. Only if that method
	  * throws an exception, the created form's `opt` method will return `Lack`.
	  */
	def seq[T](first :SQLReadForm[T], rest :SQLReadForm[T]*) :SQLReadForm[Seq[T]] = seq(first +: rest)

	/** A form applying all forms in the given sequence one after another, each starting at the position following the
	  * last column read by the preceding form, and collecting the results. Produced sequence will always have
	  * the same number of elements as there are forms. If one of the form's
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] method fails to produce a value, the form's
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]] will take its place. Only if that method
	  * throws an exception, the created form's `opt` method will return `Lack`.
	  */
	def seq[T](items :Seq[SQLReadForm[T]]) :SQLReadForm[Seq[T]] = new SQLReadFormSeq(items)

	/** Creates a conditional form, which will first read a discriminator value, using an implicitly passed
	  * `SQLReadForm[K]`, and then read the returned value with the form associated with it in the given list.
	  * If no such form exists, it will return
	  * [[net.noresttherein.oldsql.collection.Opt.Lack Lack]]/[[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]]
	  * (from the first form on the list if no implicit `NullValue[T]` is available).
	  * @tparam K the type of the discriminator values distinguishing between values of `T` hanled by particular forms.
	  *           Must have an implicit `SQLReadForm` type class.
	  * @tparam T the type of read values, the mapped type of the created form.
	  */
	def when[K :SQLReadForm, T :NullValue.Maybe](cases :(K, SQLReadForm[T])*) :SQLReadForm[T] =
		if (cases.isEmpty)
			throw new IllegalArgumentException("Empty case list for a conditional form.")
		else
			when(Map.from(cases))

	/** Creates a conditional form, which will first read a discriminator value, using an implicitly passed
	  * `SQLReadForm[K]`, and then read the returned value with the form associated with it in the given map.
	  * If no such form exists, it will return
	  * [[net.noresttherein.oldsql.collection.Opt.Lack Lack]]/[[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]]
	  * (from the first form on the list if no implicit `NullValue[T]` is available).
	  * @tparam K the type of the discriminator values distinguishing between values of `T` hanled by particular forms.
	  *           Must have an implicit `SQLReadForm` type class.
	  * @tparam T the type of read values, the mapped type of the created form.
	  */
	def when[K :SQLReadForm, T :NullValue.Maybe](cases :Map[K, SQLReadForm[T]]) :SQLReadForm[T] =
		new CaseSQLReadForm(SQLReadForm[K], Map.from(cases))(NullValue.orElse(cases.head._2.nulls))



	/** A proxy form which will delegate all calls to the form returned by the given expression. The expression
	  * will be evaluated only if/when needed and its result is cached, but must be thread safe and may be executed
	  * more than once if several threads trigger the initialization at the same time.
	  * The created form is however thread safe.
	  *
	  * The proxy is shallow: all methods returning another form are delegated to the underlying form,
	  * evaluating the constructor expression. If you wish them to remain lazy, pass the invocation expression
	  * again to this method:
	  * {{{
	  *     var form :SQLReadForm[T] = null //set later
	  *     val proxy = SQLReadFrom.delayed(form)
	  *     proxy.notNull //evaluates the form passed by name and throws a NullPointerException
	  *     val notNull = SQLReadForm.delayed(proxy.notNull) //ok, lazy
	  * }}}
	  *
	  * This is useful when the initialization expression cannot be successfully evaluated at this time,
	  * but the form must be passed by-reference to some method. In other cases a `lazy val` or
	  * [[net.noresttherein.oldsql.morsels.Lazy Lazy]] wrapper are preferable, as they do not incur the penalty
	  * of checking for initialization and delegation at every call.
	  */
	def delayed[T](form : => SQLReadForm[T]) :SQLReadForm[T] = new LazySQLReadForm(() => form)



	/** Creates a form reading the given number columns and always returning `Got(value)` from its `opt` method.
	  * The `nullValue` of the created form, while largely irrelevant, is likewise defined as `value`.
	  * Note that if `value` is `null`, it will be treated as a valid return value, rather than 'no value'
	  * by the caller and may result in `NullPointerException` exceptions higher up the call stack.
	  * @param columns the number of columns consumed by the form. The form does not access the result set, it is used
	  *                only as 'padding' when combined sequentially with other forms.
	  * @param text    an optional `toString` representation of this form.
	  * @param value   the value returned by the created form.
	  */
	def const[T](columns :Int, text :String = null)(value :T) :SQLReadForm[T] =
		constOpt(columns, text)(Got(value))(NullValue(value))

	/** Creates a form reading the given number columns and always returning `Got(value)` from its `opt` method.
	  * The form will use the provided name as its `toString` representation and will equal any other constant form
	  * of the same name, regardless of the value used.
	  * The `nullValue` of the created form, while largely irrelevant, is likewise defined as `value`.
	  * Note that if `value` is `null`, it will be treated as a valid return value, rather than 'no value'
	  * by the caller and may result in `NullPointerException` exceptions higher up the call stack.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name    the name of the form, identifying it and returned by its `toString` method.
	  * @param columns the number of columns consumed by the form. The form does not access the result set, it is used
	  *                only as 'padding' when combined sequentially with other forms.
	  * @param value   the value returned by the created form.
	  */
	def const[T](name :String)(columns :Int)(value :T) :SQLReadForm[T] =
		constOpt(name)(columns)(Got(value))(NullValue(value))

	/** Creates a form reading the given number of columns and always returning the provided value from its `opt` method.
	  * If `value` is `Lack`, the implicit `NullValue[T]` is used by the form's `apply` method to stand in.
	  * @param columns the number of columns consumed by the form. The form does not access the result set, it is used
	  *                only as 'padding' when combined sequentially with other forms.
	  * @param text    an optional `toString` representation of this form.
	  * @param value   the value returned by the form's [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method.
	  */
	def constOpt[T :NullValue](columns :Int, text :String = null)(value :Opt[T]) :SQLReadForm[T] =
		new ConstSQLReadForm[T](value, columns, Opt(text)) with NamedReadForm[T]

	/** Creates a form reading the given number of columns and always returning the provided value from its `opt` method.
	  * If `value` is `Lack`, the implicit `NullValue[T]` is used by the form's `apply` method to stand in.
	  * The provided name serves as the forms identifier: any other constant form with the same name will equal
	  * the created form, regardless of their values.
	  * @param name    the name of the form returned from its `toString` method.
	  * @param columns the number of columns consumed by the form. The form does not access the result set, it is used
	  *                only as 'padding' when combined sequentially with other forms.
	  * @param value   the value returned by the form's [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method.
	  */
	def constOpt[T :NullValue](name :String)(columns :Int)(value :Opt[T]) :SQLReadForm[T] =
		new ConstSQLReadForm[T](value, columns, Got(name))


	/** Creates a form consuming the given number of columns and always returning
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] method the value obtained
	  * by reevaluating the given expression. The generated value is used as-is, even if null:
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] always returns `Got(value)`, rather than `Opt(value)`.
	  *
	  * The expression must be thread safe.
	  * @param columns the number of columns consumed by the form. The form does not access the result set, it is used
	  *                only as 'padding' when combined sequentially with other forms.
	  * @param text    an optional `toString` representation of this form.
	  * @param value   a by-name expression providing the value returned by the form. It is reevaluated each time
	  *                `apply` or `opt` method is called.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue]]
	  */
	def eval[T](columns :Int, text :String = null)(value: => T) :SQLReadForm[T] =
		new EvalSQLReadForm[T](value, columns, Opt(text)) {
			override def notNull :SQLReadForm[T] =
				new EvalSQLReadForm[T](value, result, columnCount, Got(toString + ".notNull"))
					with ReadFormNullGuard[T] with ComparableReadForm
		}

	/** Creates a form consuming the given number of columns and always returning
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] method the value obtained
	  * by reevaluating the given expression. The generated value is used as-is, even if null:
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] always returns `Got(value)`, rather than `Opt(value)`.
	  * The provided name serves as the form's `toString` representation and an identifier:
	  * it is used to compare different instances of `eval` forms, with to instances being considered considered equal
	  * if their names are equal.
	  *
	  * The expression must be thread safe.
	  * @param name    the name of the form returned from its `toString` method.
	  * @param columns the number of columns consumed by the form. The form does not access the result set, it is used
	  *                only as 'padding' when combined sequentially with other forms.
	  * @param value   a by-name expression providing the value returned by the form. It is reevaluated each time
	  *                `apply` or `opt` method is called.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue]]
	  */
	def eval[T](name :String)(columns :Int)(value: => T) :SQLReadForm[T] =
		new EvalSQLReadForm[T](value, columns, Got(name)) with NamedReadForm[T]

	/** Creates a form consuming the given number of columns and always returning
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method the value obtained
	  * by reevaluating the given expression.  A value provided by an implicit
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] type class is used
	  * for its [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]],
	  * to which [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] method delegates
	  * when the expression yields `Lack`. The expression must be thread safe.
	  * @param columns the number of columns consumed by the form. The form does not access the result set, it is used
	  *                only as 'padding' when combined sequentially with other forms.
	  * @param text    an optional `toString` representation of this form.
	  * @param value   a by-name expression providing the value returned by the form's `opt` method (and, indirectly,
	  *                `apply`). It is reevaluated each time `opt` or `apply` is called.
	  */
	def evalOpt[T :NullValue](columns :Int, text :String = null)(value: => Opt[T]) :SQLReadForm[T] =
		new EvalOptSQLReadForm(value, columns) {
			override def notNull :SQLReadForm[T] =
				new EvalOptSQLReadForm[T](value, result, columnCount, Got(toString + ".notNull"))(NotNull)
					with ReadFormNullGuard[T] with ComparableReadForm
		}

	/** Creates a form consuming the given number of columns and always returning
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method the value obtained
	  * by reevaluating the given expression. The provided name is featured in its `toString` representation
	  * and used as the created form's identifier: two `eval` read forms are equal as long as they have the same name,
	  * regardless of the generator expressions used. It is a responsibility o the caller to ensure that equally named
	  * forms are interchangeable. A value provided
	  * by an implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] type class is used
	  * for its [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]],
	  * to which [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] method delegates
	  * when the expression yields `Lack`. The expression must be thread safe.
	  * @param name    the name identifying the form, returned by its `toString` method.
	  * @param columns the number of columns consumed by the form. The form does not access the result set, it is used
	  *                only as 'padding' when combined sequentially with other forms.
	  * @param value   a by-name expression providing the value returned by the form's `opt` method (and, indirectly,
	  *                `apply`). It is reevaluated each time `opt` or `apply` is called.
	  */
	def evalOpt[T :NullValue](name :String)(columns :Int)(value: => Opt[T]) :SQLReadForm[T] =
		new EvalOptSQLReadForm[T](value, columns, Got(name)) with NamedReadForm[T]


	/** Creates a form consuming the given number of columns and always returning
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] method the value obtained
	  * from [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] type class.
	  * The result of [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] is defined as `Got(nulls.value)`.
	  * The form is functionally equivalent to the one created by
	  * `SQLReadForm.`[[net.noresttherein.oldsql.schema.SQLReadForm.eval eval]], but implements `equals` as equality
	  * of the provided type class, making this method preferable when the expression is simple and can be covered
	  * by an existing or application-provided `NullValue` instance. The difference from similar
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.defaults defaults]] is that the latter's `opt` method
	  * returns `Lack` rather than the 'null' value.
	  *
	  * @param columns the number of columns consumed by the form. The form does not access the result set, it is used
	  *                only as 'padding' when combined sequentially with other forms.
	  * @param text    an optional `toString` representation of this form.
	  * @param value   provider of values returned by the form.
	  */ //implicit parameter list syntax, not a view bound, for consistency
	def nullValue[T](columns :Int, text :String = null)(implicit value :NullValue[T]) :SQLReadForm[T] =
		value.toOption match {
			case Some(const) => SQLReadForm.const(columns, text)(const)
			case _ => new EvalNullValueSQLReadForm[T](columns, Opt(text))(value)
		}

	/** Creates a form consuming the given number of columns and always returning
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] method the value obtained
	  * from [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] type class.
	  * The result of [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] is defined as `Got(nulls.value)`.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name    the name of the form, identifying it and returned by its `toString` method.
	  * @param columns the number of columns consumed by the form. The form does not access the result set, it is used
	  *                only as 'padding' when combined sequentially with other forms.
	  * @param value   provider of values returned by the form.
	  */
	def nullValue[T](name :String)(columns :Int)(implicit value :NullValue[T]) :SQLReadForm[T] =
		value.toOption match {
			case Some(const) =>
				SQLReadForm.const(name)(columns)(const)
			case _ =>
				new EvalNullValueSQLReadForm[T](columns, Got(name))(value) with NamedReadForm[T]
		}


	/** Creates a dummy form of zero column size which produces no value on every read.
	  * Its `apply` will return the value from type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]. If no implicit `NullValue[T]` is available,
	  * it will default to [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]],
	  * which throws a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  * The difference from `SQLReadForm.`[[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]]
	  * is that `opt` method will always return `Lack`, where the former would return the same value as in `apply`.
	  *
	  * The returned form has a width of zero columns
	  * @return `defaults[T](0)`.
	  */
//	def defaults[T :NullValue] :SQLReadForm[T] = defaults(0)

	/** Creates a dummy form of zero column size which produces no value on every read.
	  * Its `apply` will return the value from type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]].
	  * @param columns the span of this form in columns required for proper positioning among other forms.
	  *                The values of the columns are never actually read.
	  * @param text    an optional `toString` representation of this form.
	  */
	def defaults[T :NullValue](columns :Int, text :String = null) :SQLReadForm[T] =
		new DefaultsSQLReadForm[T](columns, Opt(text))

	/** Creates a dummy form of the given column size which produces no value on every read.
	  * The `opt` method will always return `Lack`, while `apply` will return the value from type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]].
	  * The difference from `SQLReadForm.`[[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]]
	  * is that `opt` method will always return `Lack`, where the former would return the same value as in `apply`.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name    the name of the form, identifying it and returned by its `toString` method.
	  * @param columns the span of this form in columns required for proper positioning among other forms.
	  *                The values of the columns are never actually read.
	  */
	def defaults[T :NullValue](name :String)(columns :Int) :SQLReadForm[T] =
		new DefaultsSQLReadForm[T](columns, Got(name)) with NamedReadForm[T]

	/** Creates a dummy form of the given column size which produces no value on every read.
	  * The `opt` method will always return `Lack`, while `apply`
	  * will always throw a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  * This is equivalent to [[net.noresttherein.oldsql.schema.SQLWriteForm.nullValue SQLWriteForm.defaults]]`(columnCount, name)(NullValue.NotNull)`
	  * @param columns the span of this form in columns required for proper positioning among other forms.
	  *                The values of the columns are never actually read.
	  * @return `defaults(columnCount, text)(NullValue.NotNull)`.
	  */
	def none[T](columns :Int) :SQLReadForm[T] =
		defaults("NONE" + columns)(columns)(NullValue.NotNull)

	/** Creates a dummy form of the given column size which produces no value on every read.
	  * The `opt` method will always return `Lack`, while `apply`
	  * will always throw a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  * This is equivalent to [[net.noresttherein.oldsql.schema.SQLWriteForm.defaults SQLWriteForm.defaults]]`(columnCount, name)(NullValue.NotNull)`
	  * @param columnCount the span of this form in columns required for proper positioning among other forms.
	  *                    The values of the columns are never actually read.
	  * @param text the name for the form, which will be used in its `toString` method.
	  * @return `defaults(columnCount, text)(NullValue.NotNull)`.
	  */
//	def none[T](columnCount :Int, text :String = null) :SQLReadForm[T] =
//		defaults(columnCount, text)(NullValue.NotNull)

	/** Creates a dummy form of the given column size which produces no value on every read.
	  * The `opt` method will always return `Lack`, while `apply`
	  * will always throw a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  * This is equivalent to [[net.noresttherein.oldsql.schema.SQLWriteForm.defaults SQLWriteForm.defaults]]`(columnCount, name)(NullValue.NotNull)`
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method or
	  * `SQLReadForm.`[[net.noresttherein.oldsql.schema.SQLReadForm.defaults(name:String)* defaults]]`(name)...`
	  * will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name    the name of the form, identifying it and returned by its `toString` method.
	  * @param columnCount the span of this form in columns required for proper positioning among other forms.
	  *                    The values of the columns are never actually read.
	  * @return `defaults(name)(columnCount)(NullValue.NotNull)`.
	  */
//	def none[T](name :String)(columnCount :Int) :SQLReadForm[T] =
//		defaults(name)(columnCount)(NullValue.NotNull)

	/** Creates a dummy form of the given column size which produces no value by its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] method and `null`
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]] method.
	  * @param columns the span of this form in columns required for proper positioning among other forms.
	  *                The values of the columns are never actually read.
	  * @return `defaults(columnCount, text)(NullValue.Null)`.
	  */
	def nulls[T >: Null](columns :Int) :SQLReadForm[T] =
		defaults("NULL" + columns)(columns)(NullValue.Null)

	/** Creates a dummy form of the given column size which produces no value by its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] method and `null`
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]] method.
	  * @param columnCount the span of this form in columns required for proper positioning among other forms.
	  *                    The values of the columns are never actually read.
	  * @param text the name for the form, which will be used in its `toString` method.
	  * @return `defaults(columnCount, text)(NullValue.Null)`.
	  */
//	def nulls(columnCount :Int, text :String = null) :SQLReadForm[Null] =
//		defaults(columnCount, text)(NullValue.Null)

	/** Creates a dummy form of the given column size which produces no value by its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] method and `null`
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]] method.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method or
	  * `SQLReadForm.`[[net.noresttherein.oldsql.schema.SQLReadForm.defaults(name:String)* defaults]]`(name)...`
	  * will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name    the name of the form, identifying it and returned by its `toString` method.
	  * @param columnCount the span of this form in columns required for proper positioning among other forms.
	  *                    The values of the columns are never actually read.
	  * @return `defaults(name)(columnCount)(NullValue.Null)`.
	  */
//	def nulls(name :String)(columnCount :Int) :SQLReadForm[Null] =
//		defaults(name)(columnCount)(NullValue.Null)

	/** A read form of zero columns which produces no value on every read.
	  * Its [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method always returns `Lack`,
	  * while [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]]
	  * (and [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]])
	  * throws a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  * It is similar in function to (but does not equal)
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.defaults SQLReadForm.defaults]]`(0)(`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]]`)`.
	  */
	def empty[T] :SQLReadForm[T] = SQLForm.empty//empty("EMPTY")

	/** A read form of zero columns which produces no value on every read. The `opt` method will always return `Lack`,
	  * while `apply` (and `nullValue`) will throw
	  * a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]]. This is the same as
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.defaults SQLReadForm.defaults]]`(0, name)(`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]]`)`.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method or
	  * `SQLReadForm.`[[net.noresttherein.oldsql.schema.SQLReadForm.defaults(name:String)* defaults]]`(name)...`
	  * will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name    the name of the form, identifying it and returned by its `toString` method.
	  */
//	def empty(name :String) :SQLReadForm[Nothing] = defaults[Nothing](name)(0)(NullValue.NotNull)

	/** A form of zero columns, which does not read anything from the result set, but always returns
	  *  the given constant `value` from [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]],
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]]
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]] methods.
	  * It is equal to other empty read forms for the same value (implements structural equality).
	  */
	def empty[T](value :T) :SQLReadForm[T] = SQLForm.empty(value)

//	def empty[T](name :String, value :T) :SQLReadForm[T] = SQLForm.empty(name, value)

	/** A form of zero columns, which does not read anything from the result set, but always returns the result of
	  * reevaluating by-name expression `value`
	  * from [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]],
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]]
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]] methods.
	  *
	  * All forms created by this method with the same name are equal, and comparable to all forms created
	  * by any other `empty` method.
	  */
	def empty[T](name :String)(value: => T) :SQLReadForm[T] = SQLForm.empty(name)(value)



	/** A form always throwing the specified exception instead of producing a value. The runtime class
	  * of the `Throwable` type parameter must contain at least one of constructors applicable to:
	  * `(), (String), (String, Throwable)`. Note that the exception is thrown also by
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method;
	  * see [[net.noresttherein.oldsql.schema.SQLReadForm.none none]] and
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.defaults defaults]] if you wish the form to simply return `Lack`.
	  * The form acts the same way as `eval`, but can more clearly define intent and implements `equals` as equality
	  * of the thrown exception class.
	  *
	  * Forms which throw exceptions of the same class are equal.
	  * @tparam E a `Throwable` class which defines at least one of the following constructors:
	  *           `(String, Throwable)`, `(String)`, `(Throwable)`, `()`.
	  * @param columns The width of this form in columns, used to position it among other columns.
	  *                The form does not actually read those columns.
	  * @return `nullValue(columnCount, text)(NullValue.error[E])`
	  */
	def error[E <: Throwable :ClassTag](columns :Int) :SQLReadForm[Nothing] =
		nullValue(columns, "ERROR" + columns +"[" + localNameOf[E] + "]")(NullValue.error[E])

	/** A form always throwing the given exception instead of producing a value. Note that this applies also to
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method.
	  * See [[net.noresttherein.oldsql.schema.SQLReadForm.none none]] and
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.defaults defaults]] if you wish the form to simply return `Lack`.
	  * This functions the same way as `eval`, but can more clearly define intent.
	  * @param columns The width of this form in columns, used to position it among other columns.
	  *                The form does not actually read those columns.
	  * @param text    An optional `toString` representation of this form.
	  * @param raise   an expression throwing an exception.
	  */
	def error(columns :Int, text :String = null)(raise: => Nothing) :SQLReadForm[Nothing] = {
		val name = if (text != null) text else "ERROR" + columns + "@" + doubleToLongBits(math.random()).shortHashString
		eval(columns, name)(raise)
	}

	/** A form always throwing the given exception instead of producing a value. Note that this applies also to
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method.
	  * See [[net.noresttherein.oldsql.schema.SQLReadForm.none none]] and
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.defaults defaults]] if you wish the form to simply return `Lack`.
	  * This functions the same way as `eval`, but can more clearly define intent.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method or
	  * `SQLReadForm.`[[net.noresttherein.oldsql.schema.SQLReadForm.eval(name:String)* eval]]`(name)...`
	  * will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name    The name of the form, identifying it and returned by its `toString` method.
	  * @param columns The width of this form in columns, used to position it among other columns.
	  *                The form does not actually read those columns.
	  * @param raise   An expression throwing an exception.
	  */
	def error(name :String)(columns :Int)(raise: => Nothing) :SQLReadForm[Nothing] =
		eval(name)(columns)(raise)


	/** A  form which throws an [[UnsupportedOperationException]] with the given message with every read attempt.
	  * This is a simple shorthand for [[net.noresttherein.oldsql.schema.SQLReadForm.error error]].
	  * @param columns the span of this form in columns required for proper positioning among other forms.
	  *                The values of the columns are never actually read.
	  * @param text    the name for the form, which will be used in its `toString` method.
	  * @param message the message of the thrown `UnsupportedOperationException` exceptions.
	  * @return `nullValue(columnCount, text)(NullValue.unsupported(message))`.
	  */ //consider: dropping text parameter
	def unsupported(columns :Int, text :String = null)(message :String) :SQLReadForm[Nothing] =
		nullValue(columns, if (text == null) "UNSUPPORTED" + columns else text)(NullValue.Unsupported(message))

	/** A  form which throws an [[UnsupportedOperationException]] with the given message with every read attempt.
	  * This is a simple shorthand for [[net.noresttherein.oldsql.schema.SQLReadForm.error error]].
	  * @param message     the message of the thrown `UnsupportedOperationException` exceptions.
	  * @return `unsupported(0)(message)`.
	  */
//	def unsupported(message :String) :SQLReadForm[Nothing] =
//		nullValue("UNSUPPORTED")(0)(NullValue.Unsupported(message))




	/** Base type for factories of some types `M[X]` and `S[X] <: M[X]`, which take as implicit arguments
	  * `SQLReadForm` and `ColumnReadForm` instances (or some higher type parameterized with these form types),
	  * respectively. See [[net.noresttherein.oldsql.morsels.SpecializingFactory SpecializingFactory]]
	  * for more information about this framework type.
	  */
	type ReadFormBasedFactory[A[_], M[_], S[X] <: M[X]] = SpecializingFactory[A, A, SQLReadForm, ColumnReadForm, M, S]



	/** Implements `nullValue` by throwing a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]]
	  * and sets `nulls` to `NullValue.NotNull`.
	  */
	trait NotNullReadForm[+T] extends SQLReadForm[T] {
		override def nulls :NullValue[T] = NullValue.NotNull
		override def nullValue :T = throw new NullValueException("Null value not allowed for " + this + ".")
		override def guardedNullValue(res :ResultSet, position :Int) :T =
			throw new NullValueException(errorMessage(res, position))

		override def notNull :this.type = this
	}

	/** A mixin trait overriding `opt` to make sure that it doesn't return `null`. If `super.opt` returns `Got(null)`
	  * a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]] is thrown.
	  */
	trait ReadFormNullGuard[+T] extends NotNullReadForm[T] {
		override val nulls :NullValue[T] = NullValue.NotNull

		abstract override def opt(res :ResultSet, position :Int) :Opt[T] = super.opt(res, position) match {
			case Got(null) => throw new NullValueException(errorMessage(res, position))
			case t => t
		}
		override def notNull :this.type = this
	}

	/** A proxy `SQLReadForm` throwing a `NullValueException` if the underlying form returns `null`.
	  * It is created by the default implementation of [[net.noresttherein.oldsql.schema.SQLReadForm.notNull notNull]].
	  */
	private[schema] trait NotNullReadFormProxy[+T] extends ReadFormProxy[T] with ReadFormNullGuard[T] {
		override def apply(res :ResultSet, position :Int) :T = {
			val t = form(res, position)
			if (t == null)
				throw new NullValueException("Cannot return null from " + this + ".")
			t
		}

		private lazy val cachedString = form.toString + ".notNull"
		override def toString :String = cachedString
	}


	/** Implements `nullValue` as `null` and sets `nulls` to `NullValue.Null`. */
	trait NullableReadForm[+T >: Null] extends SQLReadForm[T] {
		override def nulls :NullValue[T] = NullValue.Null
		override def nullValue :T = null
	}

	/** A minimal mix-in trait for `SQLWriteForm` implementations which use their
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.defaults defaults]] type class for
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]]. Note that this reverses the standard
	  * order of delegation: `nulls` is not abstract and its default implementation directs to `nullValue`,
	  * leading to infinite recursion unless overriden.
	  */ //todo: make this the standard behaviour and reverse the delegation here
	trait ReadFormNullValue[+T] extends SQLReadForm[T] {
		override def nullValue :T = nulls.value
	}

	/** A convenience base `SQLReadForm[T]` class which implements `nullValue` based on an implicit `NullValue[T]`
	  * (overriding also `nulls` in the process).
	  */
	abstract class AbstractSQLReadForm[+T](columns :Int, protected override val text :Opt[String] = Lack)
	                                      (implicit override val nulls :NullValue[T])
		extends SQLReadForm[T] with ReadFormNullValue[T]
	{
		override def columnCount :Int = columns
		private[schema] lazy val cachedString :String = super.toString
		override def toString :String = cachedString
	}



	/** Marker trait for forms which ignore the data in the passed result set and instead return some other value
	  * (a constant or evaluated expression). It is notable for being comparable with any other `SQLReadForm`
	  * of the same column count. The default implementation returns `Lack`
	  * from [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]]
	  * and registers no output parameters in [[net.noresttherein.oldsql.schema.SQLReadForm.register register]].
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.DefaultsSQLReadForm]]
	  */
	trait IgnoringReadForm[+T] extends SQLReadForm[T] {
		override def isUniversal = false
		override def shape :RowShape = RowShape(columnCount)
		override def columnTypes :Seq[JDBCType] =
			throw new UnsupportedOperationException("Form " + this + " has undefined column types.")

		override def opt(res :ResultSet, position :Int) :Opt[T] = Lack
		override def register(call :CallableStatement, position :Int) :Unit = ()

		protected override def errorMessage(res :ResultSet, position :Int) :String = {
			val columns = Iterator.iterate(position)(_ + 1).take(columnCount).map {
				i => res.getMetaData.getColumnName(i)
			}
			columns.mkString(this.toString + " does not allow null values for (", ", ", ").")
		}

		override def comparable(other :SQLReadForm[_]) :Boolean = columnCount == other.columnCount
	}



	/** Base trait for forms which use another form to read a value from the result set without reading anything
	  * else directly. All such forms are by default [[net.noresttherein.oldsql.schema.SQLReadForm.comparable comparable]]
	  * with their underlying [[net.noresttherein.oldsql.schema.SQLReadForm.ReadFormAdapter.form form]].
	  */
	trait ReadFormAdapter[+T] extends SQLReadForm[T] with UnspecifiedFormAdapter {
		private[schema] def adaptedReadForm :SQLReadForm[Any] = form

		protected def form :SQLReadForm[Any]
//		override def columnCount :Int = form.columnCount
//		override def isUniversal :Boolean = form.isUniversal

		override def register(call :CallableStatement, position :Int) :Unit = form.register(call, position)

		override def comparable(other :SQLReadForm[_]) :Boolean = other match {
			case _ if this eq other => true
			case other :ReadFormAdapter[_] => other comparable form //instead of other comparable this to prevent a cycle
			case _ => form comparable other
		}
//		override def hashCode :Int = form.hashCode
	}

	/** Base trait for proxy forms, which delegates all methods to their
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.ReadFormProxy.form form]] field.
	  * Together with various mixin traits provides complete form implementations for various transformation methods
	  * of `SQLReadForm`.
	  */
	trait ReadFormProxy[+T] extends ReadFormAdapter[T] {
		protected override def form :SQLReadForm[T]

		override def opt(res :ResultSet, position :Int) :Opt[T] = form.opt(res, position)
		override def apply(res :ResultSet, position :Int) :T = form(res, position)

		override def nulls :NullValue[T] = form.nulls
		override def nullValue :T = form.nullValue

		override def equals(that :Any) :Boolean = that match {
			case ref :AnyRef if ref eq this => true
			case proxy :ReadFormProxy[_] if proxy.canEqual(this) && canEqual(proxy) => form == proxy.form
			case _ => false
		}
		override def hashCode :Int = form.hashCode

		override def toString :String = if (text.isDefined) text.get + ">" else ">" + form
	}



	/** A read form with equality defined as `name` equality only (at least by default).
	  * Resets the implementations of form proxy factory methods which any class which mixes this trait in might
	  * have overridden with dedicated implementations back to their default implementations from `SQLReadForm`
	  * in order to preserve a reference to this form and thus name-based, rather than property-based equality
	  * of the created forms.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.DerivedReadForm]]
	  */
	private[schema] trait NamedReadForm[+T] extends SQLReadForm[T] with UnspecifiedNamedForm {
		override def notNull :SQLReadForm[T] = super[SQLReadForm].notNull
	}


	/** A base trait for forms defining equality as equality
	  * of their [[net.noresttherein.oldsql.schema.SQLReadForm.text names]] (which must be non-empty)
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm.ReadFormAdapter.form forms]], with `name` serving
	  * essentially as a pseudo class and type constructor. Note that this specifically excludes equality of the forms'
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nulls null values]]. It is the creator's responsibility
	  * to ensure those names are in fact unique and any two instances of the same name within an application
	  * are equivalent, or override the `equals` method to include any additional properties.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.NamedReadForm]]
	  */
	private[schema] trait DerivedReadForm[+T] extends ReadFormAdapter[T] {
		assert(name.isDefined, "Empty name in a " + getClass + " instance.")

		override def name :Opt[String] = text
		//override any optimised implementation that a subclass may provide with the default one to preserve equality
//		override def notNull :SQLReadForm[T] = super[ReadFormAdapter].notNull

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :DerivedReadForm[_] if other canEqual this =>
				text == other.text && form == other.form && nulls == other.nulls
			case _ => false
		}
		override def hashCode :Int = text.hashCode * 31 + form.hashCode

		private lazy val cachedString :String = name.get + "[" + form + "]>"
		override def toString = cachedString
	}


	/** Base trait for forms whose SQL signature consists of a sequence of other forms,
	  * that is they use those forms to read consecutive columns from the `ResultSet`, without reading anything directly.
	  */
	private[schema] trait CompositeReadForm[+T] extends SQLReadForm[T] {
		protected def forms :Seq[SQLReadForm[Any]]
		override def columnCount :Int = (0 /: forms)(_ + _.columnCount)
		override lazy val columnTypes :Seq[JDBCType] = forms.flatMap(_.columnTypes)
		override def isUniversal :Boolean = forms.forall(_.isUniversal)

		override def register(call :CallableStatement, position :Int) :Unit = {
			var i = position
			forms foreach { form => form.register(call, position); i += form.columnCount }
		}

		override def comparable(other :SQLReadForm[_]) :Boolean = other match {
			case _ if this eq other => true
			case other :CompositeReadForm[_] =>
				forms.length == other.forms.length &&
					(forms zip other.forms).forall { case (l, r) => l comparable r } ||
					super.comparable(other)
			case _ => super.comparable(other)
		}
	}






	private[schema] class ConstSQLReadForm[+T :NullValue](value :Opt[T], columns :Int,
	                                                      override val text :Opt[String] = Lack)
		extends AbstractSQLReadForm[T](columns, text) with IgnoringReadForm[T]
	{
		protected def result :Opt[T] = value

		override def opt(res :ResultSet, position :Int): Opt[T] = value

		override def notNull :SQLReadForm[T] =
			if (result.contains(null)) super.notNull else this

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case const :ConstSQLReadForm[_] if const canEqual this =>
				value == const.result && columnCount == const.columnCount && nulls == const.nulls
			case _ => false
		}
		override def hashCode :Int = value.hashCode

		override lazy val cachedString = if (text.isDefined) text.get + ">" else "=" + value + ">"
	}



	private[schema] class EvalSQLReadForm[+T](value: => T, protected val result :() => T, //for improved equality
	                                          columns :Int, override val text :Opt[String])
		extends AbstractSQLReadForm[T](columns, text)(NullValue.eval(value)) with IgnoringReadForm[T]
	{
		def this(value: => T, columnCount :Int = 0, text :Opt[String] = Lack) =
			this(value, () => value, columnCount, text)

		override def apply(res :ResultSet, position :Int) :T = value
		override def opt(res :ResultSet, position :Int) :Opt[T] = Got(value)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :EvalSQLReadForm[_] if other canEqual this =>
				result == other.result && columnCount == other.columnCount && nulls == other.nulls
			case _ => false
		}
		override def hashCode :Int = (result.hashCode * 31 + columnCount.hashCode) * 31 + nulls.hashCode

		override lazy val cachedString =
			if (text.isDefined) text.get + ">" else "={@" + this.shortHashString + "}>"
	}

	private[schema] class EvalOptSQLReadForm[+T :NullValue]
	                                        (value: => Opt[T], protected val result :() => Opt[T], //for improved equality
	                                         columns :Int, protected override val text :Opt[String])
		extends AbstractSQLReadForm[T](columns, text) with IgnoringReadForm[T]
	{
		def this(value: => Opt[T], columnCount :Int = 0, name :Opt[String] = Lack) =
			this(value, () => value, columnCount, name)

		override def opt(res :ResultSet, position :Int): Opt[T] = value

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :EvalOptSQLReadForm[_] if other canEqual this =>
				result == other.result && columnCount == other.columnCount && nulls == other.nulls
			case _ => false
		}
		override def hashCode :Int = (result.hashCode * 31 + columnCount.hashCode) * 31 + nulls.hashCode

		override lazy val cachedString =
			if (text.isDefined) text.get + ">" else "={@" + this.shortHashString + "}>"
	}

	private[schema] class EvalNullValueSQLReadForm[+T :NullValue](columns :Int = 0,
	                                                              protected override val text :Opt[String] = Lack)
		extends AbstractSQLReadForm[T](columns, text) with IgnoringReadForm[T]
	{
		override def apply(res :ResultSet, position :Int) :T = nulls.value
		override def opt(res :ResultSet, position :Int) :Opt[T] = Opt(nulls.value)

		override def notNull :SQLReadForm[T] =
			if (nulls == NotNull) this else super.notNull

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :EvalNullValueSQLReadForm[_] if other canEqual this =>
				columnCount == other.columnCount && nulls == other.nulls
			case _ => false
		}
		override def hashCode :Int = columnCount.hashCode * 31 + nulls.hashCode

		override lazy val cachedString = if (text.isDefined) text.get + ">" else "=" + nulls + ">"
	}


	/** An `SQLReadForm` which always returns its [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]]
	  * or `Lack`. It differs from similar
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.EvalNullValueSQLReadForm EvalNullValueReadForm]] in that the latter's
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] returns `Opt(nullValue)`, not `Lack`.
	  */
	private[schema] class DefaultsSQLReadForm[+T :NullValue](columns :Int = 0,
	                                                         override val text :Opt[String] = Lack)
		extends AbstractSQLReadForm[T](columns, text) with IgnoringReadForm[T]
	{
		override def isUniversal = false

		override def notNull :SQLReadForm[T] =
			if (nulls == NotNull) this else super.notNull

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case nulls :DefaultsSQLReadForm[_] if nulls.canEqual(this) =>
				nulls.columnCount == columnCount && nulls.nulls == this.nulls
			case _ => false
		}
		override def hashCode :Int = columnCount.hashCode * 31 + nulls.hashCode

		override lazy val cachedString :String =
			if (text.isDefined) text.get + ">"
			else if (columns == 1) nulls.toString + ">"
			else nulls.toString + columnCount + ">"
	}




	private[schema] class FallbackSQLReadForm[+T](overrides :SQLReadForm[T], fallback :SQLReadForm[T])
		extends SQLReadForm[T]
	{   //consider: checking overrides compatible fallback
		if (first.columnCount != fallback.columnCount)
			throw new IllegalArgumentException(
				overrides.toString + " orElse " + fallback + ": forms have different numbers of columns (" +
				first.columnCount + " vs " + second.columnCount + ")."
			)
		if (!first.comparable(fallback))
			throw new IllegalArgumentException(
				overrides.toString + " orElse " + fallback + ": forms are not comparable" +
					(try {
						" - column types are " + first.columnTypes + " vs " + fallback.columnTypes + "."
					} catch { case _ :UnsupportedOperationException => "." })
			)

		protected def first  :SQLReadForm[T] = overrides
		protected def second :SQLReadForm[T] = fallback
		override def columnCount :Int = overrides.columnCount
		override def columnTypes :Seq[JDBCType] =
			try { first.columnTypes } catch {
				case _ :UnsupportedOperationException => second.columnTypes
			}
		override def isUniversal :Boolean = first.isUniversal && second.isUniversal

		override def opt(res :ResultSet, position :Int) :Opt[T] =
			first.opt(res, position) orElse second.opt(res, position)

		override def nullValue :T = overrides.nullValue

		override def nulls :NullValue[T] = overrides.nulls

		override def register(call :CallableStatement, position :Int) :Unit =
			overrides.register(call, position)


		override def notNull :SQLReadForm[T] = overrides.notNull orElse fallback.notNull

		override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] =
			if (columnCount != fallback.columnCount && fallback.columnCount != 0)
				throw new IllegalArgumentException(
					s"($this) orElse $fallback: different number of read columns ($columnCount vs ${fallback.columnCount})."
				)
			else
				new FallbackSQLReadForm(first, second orElse fallback)

		override def comparable(other :SQLReadForm[_]) :Boolean = other match {
			case _ if this eq other => true
			case other :FallbackSQLReadForm[_] if isUniversal => //break the infinite mutual recursion with super.comparable delegating to other.comparable
				(overrides comparable other) || (fallback comparable other)
			case other :FallbackSQLReadForm[_] =>
				(overrides comparable other) && (fallback comparable other)
			case _ if isUniversal =>
				(overrides comparable other) || (fallback comparable other) || super.comparable(other)
			case _ =>
				(overrides comparable other) && (fallback comparable other) || super.comparable(other)
		}
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case fallback :FallbackSQLReadForm[_] if fallback canEqual this =>
				first == fallback.first && second == fallback.second
			case _ => false
		}
		override def hashCode :Int = overrides.hashCode * 31 + fallback.hashCode

		override val toString = "(" + overrides + ")/(" + fallback + ")"
	}

}


