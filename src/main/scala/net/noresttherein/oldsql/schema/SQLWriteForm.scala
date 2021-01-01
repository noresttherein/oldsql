package net.noresttherein.oldsql.schema

import java.sql.PreparedStatement

import scala.annotation.implicitNotFound
import scala.collection.immutable.Seq

import net.noresttherein.oldsql.morsels.{ColumnBasedFactory, Stateless}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLWriteForm.{CombinedSQLWriteForm, CustomNullSQLWriteForm, NotNullSQLWriteForm, ProxyWriteForm, WriteFormNullGuard}
import net.noresttherein.oldsql.schema.forms.{SQLForms, SuperSQLForm}






/** Encapsulates the logic of disassembling values of `T` into values for individual columns and using them
  * to set the `PreparedStatement` parameters. As an additional functionality, it knows how to format the value
  * of `T` as an sql literal for verbatim inclusion as a constant into the SQL (rather than a parameter).
  * Whether or not `null` values are allowed depends on actual implementation. No `NullPointerException` will
  * be thrown directly by forms in this library, but `null` values may be passed over to client's mapping functions
  * used to adapt a form from one type to another.
  * Implementations should provide a pure contract, in particular ''always'' setting the same number of consecutive
  * parameters, defined as `writtenColumns`. This makes it a lower-level counterpart of
  * [[net.noresttherein.oldsql.schema.Mapping Mapping]], which can be represented by possibly many forms, depending on
  * the column lists included as both the 'insert/update' and `query/where` portions of a statement.
  * All implementations must be thread safe.
  * @see [[net.noresttherein.oldsql.schema.SQLReadForm]]
  * @see [[net.noresttherein.oldsql.schema.SQLForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm]]
  */
@implicitNotFound("I do not know how to set PreparedStatement parameters from type ${T}: " +
                  "missing implicit SQLWriteForm[${T}].")
trait SQLWriteForm[-T] extends SuperSQLForm { outer =>

	/** Set the values of parameters `<position..position+writtenColumns)` of the given `PreparedStatement` to
	  * the values obtained from the given value of `T`. This method simply delegates to `set` or `setNull`, depending
	  * on whether the value is defined.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.set]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull]]
	  */
	def setOpt(statement :PreparedStatement, position  :Int, value :Option[T]) :Unit = value match {
		case Some(x) => set(statement, position, x)
		case _ => setNull(statement, position)
	}

	/** Set the values of parameters `<position..position+writtenColumns)` of the given `PreparedStatement` to
	  * the values obtained from the given value of `T`. While forms for reference types can in theory accept
	  * `null` values, client code should not assume that passing `null` to this method will be handled gracefully
	  * by arbitrary forms. This would be impossible to achieve for forms of built-in value types, which will always
	  * throw a `NullPointerException` on a `null` unboxing attempt, but also higher-level forms can depend on
	  * the internal structure of the value `T` without checking it for nullity. Instead of calling `set` for a `null`,
	  * use [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]] for `None` or explicitly `setNull`.
	  * If a value for a column/parameter cannot be obtained, a `null` value of the appropriate JDBC SQL type should
	  * be set, unless this lack is a result of illegal argument or some other error, in which case an appropriate
	  * exception should be thrown.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull]]
	  */
	def set(statement :PreparedStatement, position :Int, value :T) :Unit

	/** Set the values of parameters `<position..position+writtenColumns)` of the given `PreparedStatement` to
	  * represent the 'null value' of type `T`, however this form decides to define it. Typically, this means setting
	  * all the columns which would be set with `set` to the appropriate `null` value, but it is not required.
	  * Implementations should strive to support this method even if they do not allow `null` arguments
	  * for [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]], unless they have been specifically created
	  * for 'not null' database columns.
	  */
	def setNull(statement :PreparedStatement, position :Int) :Unit



	/** The string representation of the given value of `T` as an SQL literal, ready to be embedded as a constant part
	  * of an SQL statement. */
	def literal(value :T) :String //todo: define whether it should handle null values and, if so, provide some ease-of-life support.

	/** The string representation of of the given value of `T` ready to be embedded as part of a larger SQL tuple literal
	  * or a SELECT clause. For single column forms, this will be the same as `literal(values)`. Multi column forms
	  * omit the surrounding '(' and ')' so that concatenating results of repeated calls to `inlineLiteral` of several
	  * forms yields a flat result.
	  */
	def inlineLiteral(value :T) :String

	/** The string representation of a 'null value' of type `T` (typically an SQL NULL or a tuple of NULL values),
	  * ready to be embedded as a constant part of an SQL statement. */
	def nullLiteral :String

	/** The string representation of of the 'null value' of `T` ready to be embedded as part of a larger SQL tuple literal
	  * or a SELECT clause. For single column forms, this will be the same as `nullLiteral`. Multi column forms
	  * omit the surrounding '(' and ')' so that concatenating results of repeated calls to `inlineNullLiteral` of
	  * several forms yields a flat result.
	  */
	def inlineNullLiteral :String

	//todo: 1. inverse delegation direction; 2. verify null soundness
	def literal(value :T, inline :Boolean) :String =
		if (inline) inlineLiteral(value)
		else literal(value)

	def nullLiteral(inline :Boolean) :String =
		if (inline) inlineNullLiteral
		else nullLiteral



	/** Number of parameters set by this form each time its `set` or `setNull` is called. */
	def writtenColumns :Int



	/** Create a write form for `X` which will map received values to `T` and pass them to this form.
	  * The arguments are not tested for `null` values before being passed to `fun`, which should handle `null`s
	  * gracefully if they are considered a valid value for the adapted form's use case.
	  */
	def unmap[X](fun :X => T) :SQLWriteForm[X] = SQLWriteForm.map(fun)(this)

	/** Create a write form for `X` which will try to map received values to `T` and pass them to this form.
	  * If the given function yields `None`, this form's `setNull` method is used instead of `set`.
	  * The arguments are not tested for `null` values before being passed to `fun`, which should handle `null`s
	  * gracefully if they are considered a valid value for the adapted form's use case.
	  */
	def flatUnmap[X](fun :X => Option[T]) :SQLWriteForm[X] = SQLWriteForm.flatMap(fun)(this)

	/** Creates a write form for `X` which will use this form after extracting a value from `X` with the given
	  * extractor. This is equivalent to `unmap` or `flatUnmap`, depending on whether the extractor is
	  * a `RequisiteExtractor` instance. In corner cases, such as a constant extractor, a special `SQLWriteForm`
	  * instance may be returned.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.unmap]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.flatUnmap]]
	  */
	def from[X](extractor :X =?> T) :SQLWriteForm[X] = compose(extractor)

	/** Creates a write form for `X` which will use this form after extracting a value from `X` with the given
	  * extractor. This is equivalent to `unmap` or `flatUnmap`, depending on whether the extractor is
	  * a `RequisiteExtractor` instance.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.unmap]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.flatUnmap]]
	  */
	def compose[X](extractor :X =?> T) :SQLWriteForm[X] = SQLWriteForm(extractor)(this)



	/** Lift this form to represent `Option[T]`, where `Some` values are delegated to this instance's `set` method,
	  * while `None` results in calling this form's `setNull` instead.
	  */
	def toOpt :SQLWriteForm[Option[T]] = SQLForms.OptionWriteForm(this)

	/** A null-safe proxy to this form. All methods accepting values of `T` check first if their argument is `null`;
	  * if so, they delegate to the null-specific variant of the method in this form. Otherwise, the call is forwarded
	  * to the same method of this form. The same effect can be achieved by mixing in trait
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormNullGuard WriteFormNullGuard]] to an existing class.
	  * All bundled concrete `SQLWriteForm` implementations are already null-safe
	  * and will not throw a `NullPointerException` themselves, unless specifically created in this way in order
	  * to reject `null` values. This method can be however a useful help in creating custom forms, taking
	  * of the burden of performing the checks by the application, and even for those provided forms which encapsulate
	  * custom implementations. Note that null-safety is not a requirement for this class as many applications
	  * will opt to disallow `null` values being propagated to or used by the client code.
	  */
	def nullSafe :SQLWriteForm[T] =
		new ProxyWriteForm[T] with WriteFormNullGuard[T] {
			override def form :SQLWriteForm[T] = outer
		}

	/** An adapter or modification of this form which ensures that `null` values will never be set
	  * as statement parameters. In cases where form would write a `null` value, the returned form will throw
	  * a `NullPointerException`. Literal methods are unaffected.
	  */
	def notNull :SQLWriteForm[T] = new NotNullSQLWriteForm[T](this)

	/** A proxy to this instance which directs all calls to its null-specific methods to their general
	  * counterparts in this form, in particular [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]. The value provided by the implicit type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` is passed as the argument.
	  * A similar effect can be achieved directly by mixing in trait
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormNullValue WriteFormNullValue]] to an existing class.
	  * Note that this doesn't substitute `null` values passed explicitly to the form with the one provided by
	  * the type class; this effect can be achieved by calling
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.nullSafe nullSafe]] on the result.
	  */
	def withNull(implicit nulls :NullValue[T]) :SQLWriteForm[T] = new CustomNullSQLWriteForm[T](this)

	/** A proxy to this instance which directs all calls to its null-specific methods to their general
	  * counterparts in this form, in particular [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] with the value provided here as the argument.
	  * Note that this doesn't substitute `null` values passed explicitly to the form with the one provided by
	  * the type class; this effect can be achieved by calling
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.nullSafe nullSafe]] on the result.
	  */
	def withNull(nullValue :T) :SQLWriteForm[T] = withNull(NullValue(nullValue))



	/** Creates a write form which will apply this form `repeat` number of times, writing values from an input sequence.
	  * If the written sequence is shorter than `repeat`, for the remaining iterations this form's
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method is used instead of
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]. If the sequence is longer, an
	  * `IllegalArgumentException` is thrown.
	  */
	def *(repeat :Int) :SQLWriteForm[Seq[T]] = SQLWriteForm.seq(repeat)(this)

	/** Combine this form with another form, to create a form for the `(T, O)` pair. The parameters for the second form
	  * are expected to immediately follow this form's statement parameters.
	  */
	def *[O](other :SQLWriteForm[O]) :SQLWriteForm[(T, O)] = forms.ScalaForms.tuple2WriteForm(this, other)

	/** Creates a write form which will first write any given value with this form, and then with the argument form,
	  * starting at statement parameter position right after the position of the last written parameter by this form.
	  * This is particularly useful in conjunction with composing both forms with functions retrieving different
	  * properties of the same larger entity. The string literal representation will be that of a SQL tuple.
	  */
	def +[S <: T](next :SQLWriteForm[S]) :SQLWriteForm[S] = next match {
		case composite :CombinedSQLWriteForm[S @unchecked] =>
			SQLWriteForm.combine(this +: composite.forms :_*)
		case _ =>
			SQLWriteForm.combine(this, next)
	}

	/** Combine this form with a read form for the same type in order to obtain a read-write form. */
	def <>[O <: T](read :SQLReadForm[O]) :SQLForm[O] = SQLForm.join(this, read)


	def compatible(other :SQLWriteForm[_]) :Boolean = this == other

}






object SQLWriteForm {

	/** Summon an implicitly available `SQLWriteForm[T]`. */
	def apply[T :SQLWriteForm] :SQLWriteForm[T] = implicitly[SQLWriteForm[T]]



	/** Creates a non-literal `SQLWriteForm` using the given function to set statement parameters based on a value of `T`.
	  * An implicit `NullValue[T]` is used as the written value when the `T` argument is null.
	  * @param columns the number of set parameters
	  * @param write a function taking a statement, index of the first parameter to set and an object from which
	  *              the values of the parameters can be derived, and sets the consecutive `columns` number
	  *              of parameters on the statement.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	def apply[T :NullValue](columns :Int)(write :(PreparedStatement, Int, T) => Unit) :SQLWriteForm[T] =
		SQLWriteForm(columns, null)(write)

	/** Creates a non-literal `SQLWriteForm` using the given function to set statement parameters based on a value of `T`.
	  * An implicit `NullValue[T]` is used as the written value when the `T` argument is null.
	  * @param columns the number of set parameters
	  * @param name the name of the form returned by its `toString` method.
	  * @param write a function taking a statement, index of the first parameter to set and an object from which
	  *              the values of the parameters can be derived, and sets the consecutive `columns` number
	  *              of parameters on the statement.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	def apply[T :NullValue](columns :Int, name :String)(write :(PreparedStatement, Int, T) => Unit) :SQLWriteForm[T] =
		new CustomSQLWriteForm[T](columns, name)(write)



	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `SQLWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form.
	  * @param expr the expressions evaluated at each call to [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
	  *             and [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] methods and passed to
	  *             the `set` method of the implicit base form.
	  * @param name an optional name of the form, used in its `toString` implementation (and thrown exceptions).
	  *             If none is provided, the form's name will be derived from the provided `value`.
	  */
	def eval[T :SQLWriteForm](expr: => T, name :String = null) :SQLWriteForm[Any] =
		new EvalSQLWriteForm[T](Some(expr), name)

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `SQLWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form. If it yields
	  * `None`, the 'null' variant of the literal/write method will be called on the backing form.
	  * @param expr the expressions evaluated at each call to [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
	  *             and [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] methods and written with
	  *             the implicit base form.
	  * @param name an optional name of the form, used in its `toString` implementation (and thrown exceptions).
	  *             If none is provided, the form's name will be derived from the provided `value`.
	  */
	def evalopt[T :SQLWriteForm](expr: => Option[T], name :String = null) :SQLWriteForm[Any] =
		new EvalSQLWriteForm[T](expr, name)



	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `SQLWriteForm[T]`. This covers both calls to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  * @param value the value passed to [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method of the
	  *              implicit base form.
	  * @param name an optional name of the form, used in its `toString` implementation (and thrown exceptions).
	  *             If none is provided, the form's name will be derived from the provided `value`.
	  */
	def const[T :SQLWriteForm](value :T, name :String = null) :SQLWriteForm[Any] =
		if (value == null) new NullSQLWriteForm[T](name)
		else new ConstSQLWriteForm(value, name)

	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `SQLWriteForm[T]`. This covers both calls to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  * @param value the written value; if `Some`, all calls will be directed to
	  *              [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method of the base form;
	  *              otherwise [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] will be called each time.
	  * @param name an optional name of the form, used in its `toString` implementation (and thrown exceptions).
	  *             If none is provided, the form's name will be derived from the provided `value`.
	  */
	def constopt[T :SQLWriteForm](value :Option[T], name :String = null) :SQLWriteForm[Any] =
		if (value.isEmpty) new NullSQLWriteForm[T](name)
		else new ConstSQLWriteForm(value.get, name)



	/** An `SQLWriteForm` ignoring its input and always writing `null` values using the implicitly given
	  * `SQLWriteForm[T]`. Null is defined here by the [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]
	  * type class, which must be also present for type `T`. This form is similar to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.none SQLWriteForm.none]], but passes the written value
	  * explicitly to the backing form's [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method,
	  * rather than delegate null handling to its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]],
	  * like the latter.
	  */ //consider: renaming to something like default
	def nulls[T :SQLWriteForm :NullValue] :SQLWriteForm[Any] = nulls[T](null :String)

	/** An `SQLWriteForm` ignoring its input and always writing `null` values using the implicitly given
	  * `SQLWriteForm[T]`. Null is defined here by the [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]
	  * type class, which must be also present for type `T`. This form is similar to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.none SQLWriteForm.none]], but passes the written value
	  * explicitly to the backing form's [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method,
	  * rather than delegate null handling to its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]],
	  * like the latter. Note that this means that, if
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.Null NullValue.Null]] is the null value for `T`,
	  * than the implicit `SQLWriteForm[T]`'s `set` method must accept `null` values.
	  * @param name the name of the form, used in its `toString` implementation (and thrown exceptions).
	  */
	def nulls[T :SQLWriteForm :NullValue](name :String) :SQLWriteForm[Any] = new NullValueSQLWriteForm[T](name)

	/** An `SQLWriteForm` which will always write the 'null' value of type `T` using the implicitly available form.
	  * Only the null-specific methods of the base form are used, such as
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.nulls]]
	  */
	def none[T :SQLWriteForm] :SQLWriteForm[Any] = new NullSQLWriteForm[T]()

	/** An `SQLWriteForm` which will always write the 'null' value of type `T` using the implicitly available form.
	  * Only the null-specific methods of the base form are used, such as
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  * @param name the name of the form, used in its `toString` implementation (and thrown exceptions).
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.nulls]]
	  */
	def none[T :SQLWriteForm](name :String) :SQLWriteForm[Any] = new NullSQLWriteForm[T](name)

	/** An empty form which never writes anything. Its `writtenColumns` property is set to zero. */
	val empty :SQLWriteForm[Any] = empty("<EMPTY")

	/** An empty form which never writes anything. Its `writtenColumns` property is set to zero.
	  * @param name the name of the form, used in its `toString` implementation (and thrown exceptions).
	  */
	def empty(name :String) :SQLWriteForm[Any] =
		new EmptyWriteForm[Any] with Stateless {
			override def toString = name
		}



	/** A write form of zero columns which will throw the given exception at every write attempt.
	  * Note that this includes [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]], not only
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]].
	  */
	def error(raise: => Nothing) :SQLWriteForm[Any] = error(0, "<ERROR")(raise)

	/** A write form which will throw the given exception at every write attempt.
	  * Note that this includes [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]], not only
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]].
	  */
	def error(columns :Int, name :String)(raise: => Nothing) :SQLWriteForm[Any] =
		new ErrorSQLWriteForm(columns, raise, if (name != null) name else "<ERROR")

	/** A write form which will throw an `UnsupportedOperationException` with the given message at every write attempt.
	  * This is a simple shorthand for [[net.noresttherein.oldsql.schema.SQLWriteForm.error error]].
	  */
	def unsupported(columns :Int, name :String = null)(message :String) :SQLWriteForm[Any] =
		error(columns, if (name != null) name else "<UNSUPPORTED") {
			throw new UnsupportedOperationException(message)
		}

	/** A write form which will throw an `UnsupportedOperationException` with the given message at every write attempt.
	  * This is a simple shorthand for [[net.noresttherein.oldsql.schema.SQLWriteForm.error error]].
	  */
	def unsupported(message :String) :SQLWriteForm[Any] = unsupported(0)(message)



	/** Composes an implicitly available write form `SQLWriteForm[S]` with a given extractor to create
	  * an `SQLWriteForm[T]`. This has the effect of calling either
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm$.map map]] or
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm$.flatMap flatMap]], depending on the type of the extractor.
	  * If the extractor yields `Some`, the value will be passed to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method of the implicit backing form. Otherwise
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] will be called instead.
	  * @param extractor producer of values for the implicit base form to write, called for every value written
	  *                  with the produced form.
	  */
	def apply[S, T](extractor :T =?> S)(implicit writer :SQLWriteForm[S]) :SQLWriteForm[T] = extractor match {
		case _ :IdentityExtractor[_] => writer.asInstanceOf[SQLWriteForm[T]]
		case const :ConstantExtractor[_, S @unchecked] => SQLWriteForm.const(const.constant)
		case req :RequisiteExtractor[T @unchecked, S @unchecked] => map(req.getter)
		case _ :EmptyExtractor[_, _] => SQLWriteForm.none
		case _ => flatMap(extractor.optional)
	}

	/** Composes an implicitly available write form `SQLWriteForm[S]` with a given extractor to create
	  * an `SQLWriteForm[T]`. This has the effect of calling either
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm$.map map]] or
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm$.flatMap flatMap]], depending on the type of the extractor.
	  * If the extractor yields `Some`, the value will be passed to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method of the implicit backing form. Otherwise
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] will be called instead.
	  * @param extractor producer of values for the implicit base form to write, called for every value written
	  *                  with the produced form.
	  * @param name the name for the created form, used in its `toString` implementation.
	  */
	def apply[S :SQLWriteForm, T](name :String)(extractor :T =?> S) :SQLWriteForm[T] =
		extractor match {
//			case _ :IdentityExtractor[_] => writer.asInstanceOf[SQLWriteForm[T]]
			case const :ConstantExtractor[_, S @unchecked] => SQLWriteForm.const(const.constant, name)
			case req :RequisiteExtractor[T @unchecked, S @unchecked] => map(name)(req.getter)
			case _ :EmptyExtractor[_, _] => SQLWriteForm.none(name)
			case _ => flatMap(name)(extractor.optional)
		}



	/** Composes an implicitly available write form `SQLWriteForm[S]` with a given getter function to create
	  * an `SQLWriteForm[T]`. This function can in particular be used to present an `SQLWriteForm` for the type
	  * of a property of some entity type as a write form for said entity type. Such forms can be later combined
	  * together with [[net.noresttherein.oldsql.schema.SQLWriteForm.combine combine(forms)]] to create
	  * an `SQLWriteForm` for the whole entity.
	  * @param map the function mapping the arguments of the created form to values accepted by the implicit base form.
	  */
	def map[S :SQLWriteForm, T](map :T => S) :SQLWriteForm[T] =
		SQLWriteForm.map(null :String)(map)

	/** Composes an implicitly available write form `SQLWriteForm[S]` with a given getter function to create
	  * an `SQLWriteForm[T]`. This function can in particular be used to present an `SQLWriteForm` for the type
	  * of a property of some entity type as a write form for said entity type. Such forms can be later combined
	  * together with [[net.noresttherein.oldsql.schema.SQLWriteForm.combine combine(forms)]] to create
	  * an `SQLWriteForm` for the whole entity.
	  * @param map the function mapping the arguments of the created form to values accepted by the implicit base form.
	  * @param name the name for the created form, used in its `toString` implementation.
	  */
	def map[S :SQLWriteForm, T](name :String)(map :T => S) :SQLWriteForm[T] =
		new MappedSQLWriteForm[S, T](map, name)



	/** Composes an implicitly available write form `SQLWriteForm[S]` with a given getter to create
	  * an `SQLWriteForm[T]`. If the function returns `None`, the created form will use
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method of the base form instead of
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]].
	  * @param map the function mapping the arguments of the created form to values accepted by the implicit base form.
	  */
	def flatMap[S :SQLWriteForm, T](map :T => Option[S]) :SQLWriteForm[T] =
		flatMap[S, T](null :String)(map)

	/** Composes an implicitly available write form `SQLWriteForm[S]` with a given getter to create
	  * an `SQLWriteForm[T]`. If the function returns `None`, the created form will use
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method of the base form instead of
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]].
	  * @param map the function mapping the arguments of the created form to values accepted by the implicit base form.
	  * @param name the name for the created form, used in its `toString` implementation.
	  */
	def flatMap[S :SQLWriteForm, T](name :String)(map :T => Option[S]) :SQLWriteForm[T] =
		new FlatMappedSQLWriteForm[S, T](map, name)



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
	def delayed[T](delayed: => SQLWriteForm[T]) :SQLWriteForm[T] =
		new LazyWriteForm[T] {
			override protected[this] var init = () => delayed
		}



	/** Create an `SQLWriteForm[T]` which will delegate all `set`/`setNull` calls to ''all'' forms in the given sequence,
	  * in the exact order they appear. The 'position' argument of every form after the last is increased by the sum
	  * of `writtenColumns` of all preceding forms.
	  */
	def combine[T](forms :SQLWriteForm[T]*) :SQLWriteForm[T] = combine(null :String)(forms :_*)

	/** Create an `SQLWriteForm[T]` which will delegate all `set`/`setNull` calls to ''all'' forms in the given sequence,
	  * in the exact order they appear. The 'position' argument of every form after the last is increased by the sum
	  * of `writtenColumns` of all preceding forms.
	  */
	def combine[T](name :String)(forms :SQLWriteForm[T]*) :SQLWriteForm[T] = forms match {
		case Seq() =>
			if (name == null) empty
			else new EmptyWriteForm[T] with Stateless {}
		case Seq(f) =>
			if (name == null) f
			else new ProxyWriteForm[T] with EmptyWriteForm[T] {
				protected override val form = f
				override val toString = name
			}
		case _ => new CombinedSQLWriteForm(forms, name)
	}



	/** Creates a write form applying the form `SQLWriteform[T]` `repeats` number of times, writing values
	  * from an input sequence. If the written sequence is shorter than `repeats`, for the remaining iterations
	  * the element form's [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method is used instead of
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]. If the sequence is longer, an
	  * `IllegalArgumentException` is thrown.
	  */
	def seq[T :SQLWriteForm](repeats :Int) :SQLWriteForm[Seq[T]] = new SeqSQLWriteForm[T](repeats)

	/** A form writing sequences of constant length, each element with the corresponding form from the list
	  * provided as the argument.
	  */
	def seq[T](items :Seq[SQLWriteForm[T]]) :SQLWriteForm[Seq[T]] = new SQLWriteFormSeq[T](items)





	/** Mixin trait for `SQLWriteForm` implementations which provides abstract overrides for `literal` and
	  * `inlineLiteral` which test the value for nullity before passing them to `super`. Calls for null values
	  * get instead delegated to the specialized null variant of the method.
	  */
	trait NullableWriteForm[-T >: Null] extends SQLWriteForm[T] {
		abstract override def literal(value :T) :String =
			if (value == null) nullLiteral else super.literal(value)

		abstract override def inlineLiteral(value :T) :String =
			if (value == null) inlineNullLiteral else super.inlineLiteral(value)

		abstract override def literal(value :T, inline :Boolean) :String =
			if (value == null) super.nullLiteral(inline)
			else super.literal(value, inline)
	}



	/** A mix-in trait for write forms of values which can't appear as literals in SQL statements.
	  * Throws an `UnsupportedOperationException` from all non null specific literal-related methods.
	  */
	trait NonLiteralWriteForm[-T] extends SQLWriteForm[T] {
		override def literal(value: T): String =
			if (value == null) nullLiteral
			else throw new UnsupportedOperationException(toString + ".literal")

		override def nullLiteral :String =
			throw new UnsupportedOperationException(toString + ".nullLiteral")

		override def inlineLiteral(value: T): String =
			if (value == null) inlineNullLiteral
			else throw new UnsupportedOperationException(toString + ".inlineLiteral")

		override def inlineNullLiteral :String =
			throw new UnsupportedOperationException(toString + ".inlineNullLiteral")
	}



	/** Reverses the delegation direction of the methods returning literals as strings. ''All'' 'null' methods now
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.nullLiteral(inline:Boolean) nullLiteral]]`(inline)`,
	  * while all literal methods accepting a value delegate to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.literal(value:T,inline:Boolean) literal]]`(inline)`.
	  * Note that these delegation targets are not abstract and their default implementations delegate back
	  * to the individual methods, resulting in an endless loop unless overriden!
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NullableWriteFormLiterals]]
	  */
	trait WriteFormLiterals[-T] extends SQLWriteForm[T] {
		override def literal(value :T) :String = literal(value, false)
		override def inlineLiteral(value :T) :String = literal(value, true)
		override def nullLiteral :String = nullLiteral(false)
		override def inlineNullLiteral :String = nullLiteral(true)
	}

	/** An extension of [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals WriteFormLiterals]]
	  * which delegates [[net.noresttherein.oldsql.schema.SQLWriteForm.nullLiteral(inline:Boolean) nullLiteral]]
	  * to [[net.noresttherein.oldsql.schema.SQLWriteForm.literal(value:T,inline:Boolean) literal]]`(null, inline)`.
	  * This means that now all literal methods delegate to the same method. Note that this target is not abstract
	  * and default implementation delgates back to more specific methods, leading to an endless recursion unless
	  * overriden!
	  */
	trait NullableWriteFormLiterals[-T >: Null] extends WriteFormLiterals[T] {
		override def nullLiteral(inline :Boolean) :String = literal(null, false)
	}



	/** An `SQLWriteForm` mixin trait which throws a `NullPointerException` from
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method and from
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] if the argument given is `null` and from
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]] if the argument given is `None`.
	  * It still allows null literals.
	  */
	trait NotNullWriteForm[-T] extends SQLWriteForm[T] {
		abstract override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			if (value == null) setNull(statement, position)
			else super.set(statement, position, value)

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			throw new NullPointerException("Null values not allowed for " + this + ".")

		override def notNull :this.type = this
	}



	/** A late mix-in trait for `SQLWriteForm[T]` implementations which overrides all methods accepting a value of `T`
	  * and calls the super method only if the argument is not null. For `null` values it delegates the call
	  * to the null-specific counterpart method. It is an easy way for a class to achieve null safety if the application
	  * actually allows `null` values.
	  */
	trait WriteFormNullGuard[-T] extends SQLWriteForm[T] {
		abstract override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			if (value == null) setNull(statement, position)
			else super.set(statement, position, value)

		abstract override def literal(value :T) :String =
			if (value == null) nullLiteral else super.literal(value)

		abstract override def inlineLiteral(value :T) :String =
			if (value == null) inlineNullLiteral else super.inlineLiteral(value)

		override def nullSafe :SQLWriteForm[T] = this
	}



	/** A mix-in trait for `SQLWriteForm[T]` implementations which wish to provide a custom `null` representation,
	  * different from value `null` (or, in case of proxy/composite forms, from how the base form handles nulls
	  * in [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] and other null-specific methods.
	  * This trait overrides all these methods, delegating every call to the generic, non-null method counterpart,
	  * passing its `nullValue` property as the argument. It doesn't override any methods accepting `T` as an argument
	  * and doesn't deal in any way with actual `null` values, in particular it doesn't attempt to map them
	  * to the `nullValue` property. For this, you can use/mix-in
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormNullGuard WriteFormNullGuard]]
	  */
	trait WriteFormNullValue[T] extends SQLWriteForm[T] {
		protected[this] def nullValue :T

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			set(statement, position, nullValue)

		override def nullLiteral :String = literal(nullValue)
		override def inlineNullLiteral :String = inlineLiteral(nullValue)

		override def toString :String =
			try { super.toString + ":NULL=" + nullValue }
			catch { case e :Exception => super.toString + ":NULL=" + e }
	}






	/** A base trait for forms which write nothing. Sets the `writtenColumns` property to zero. */
	trait EmptyWriteForm[-T] extends SQLWriteForm[T] {
		override def set(statement :PreparedStatement, position :Int, value :T) :Unit = ()
		override def setNull(statement :PreparedStatement, position :Int) :Unit = ()
		override def inlineLiteral(value: T): String = ""
		override def inlineNullLiteral: String = ""
		override def literal(value: T): String = ""
		override def nullLiteral: String = ""
		final override def writtenColumns: Int = 0

		override def notNull :SQLWriteForm[T] = this

		override def toString = "<EMPTY"
	}



	/** Base/mixin trait for write forms which are based on another form. Implements
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.writtenColumns writtenColumns]] and null-specific
	  * literal methods by direct delegation to the member `form`.
	  */
	trait WriteFormAdapter[-T] extends SQLWriteForm[T] {
		protected def form :SQLWriteForm[Nothing]
		override def writtenColumns :Int = form.writtenColumns

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			form.setNull(statement, position)

//		override def nullLiteral(inline :Boolean) :String = form.nullLiteral(inline)
		override def nullLiteral :String = form.nullLiteral
		override def inlineNullLiteral :String = form.inlineNullLiteral

		override def hashCode :Int = form.hashCode
	}



	trait ProxyWriteForm[-T] extends WriteFormAdapter[T] {
		protected override def form :SQLWriteForm[T]

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			form.set(statement, position, value)

		override def literal(value: T): String = form.literal(value)
		override def inlineLiteral(value: T): String = form.inlineLiteral(value)

		override def equals(that :Any) :Boolean = that match {
			case proxy :ProxyWriteForm[_] =>
				(proxy eq this) || canEqual(proxy) && (proxy canEqual this) && proxy.form == form
			case _ => false
		}

		override def hashCode :Int = form.hashCode

		override def toString :String = "~" + form
	}






	/** Base type for factories of some types `M[X]` and `S[X] <: M[X]`, which take as arguments
	  * `SQLWriteForm` and `ColumnWriteForm` instances (or some higher type parameterized with these form types),
	  * respectively See [[net.noresttherein.oldsql.morsels.ColumnBasedFactory ColumnBasedFactory]]
	  * for more information about this framework type.
	  */
	type WriteFormFunction[A[_], M[_], S[X] <: M[X]] = ColumnBasedFactory[A, A, SQLWriteForm, ColumnWriteForm, M, S]





	/** A wrapper over any write form which will throw a `NullPointerException` instead of setting a `null` parameter. */
	private[schema] class NotNullSQLWriteForm[-T](protected override val form :SQLWriteForm[T])
		extends ProxyWriteForm[T] with NotNullWriteForm[T]
	{
		override val toString = form.toString + ".notNull"
	}


	/** A simple mix-in trait for write forms which ignore passed arguments when setting parameters.
	  * It directs [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method
	  * to [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  */
	private[schema] trait IgnoringWriteForm[T] extends SQLWriteForm[T] {
		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			setNull(statement, position)
	}


	private[schema] class NullSQLWriteForm[T](name :String = null)
	                                         (implicit protected override val form :SQLWriteForm[T])
		extends WriteFormAdapter[Any] with IgnoringWriteForm[Any] with NullableWriteFormLiterals[Any]
	{
		override def literal(value :Any, inline :Boolean) :String = form.nullLiteral(inline)

		override def notNull :SQLWriteForm[Any] = new NullSQLWriteForm[T]()(form.notNull)

		override val toString = if (name != null) name else form.toString + ":NULL"
	}


	private[schema] class NullValueSQLWriteForm[T](name :String = null)
	                                              (implicit protected override val form :SQLWriteForm[T],
	                                               nulls :NullValue[T])
		extends WriteFormAdapter[Any] with IgnoringWriteForm[Any] with NullableWriteFormLiterals[Any]
	{
		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			form.set(statement, position, nulls.value)

		override def literal(value :Any, inline :Boolean) :String = form.literal(nulls.value, inline)

		override def notNull :SQLWriteForm[Any] = new NullValueSQLWriteForm[T]()(form.notNull, nulls)

		override val toString =
			try {
				if (name != null) name else form.toString + ":NULL=" + nulls.value
			} catch {
				case e :Exception => form.toString + ":NULL=" + e
			}
	}



	private[schema] class CustomSQLWriteForm[-T :NullValue](columns :Int, name :String = null)
	                                                       (write :(PreparedStatement, Int, T) => Unit)
		extends NonLiteralWriteForm[T]
	{
		override def writtenColumns = columns

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			write(statement, position, value)

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			try {
				write(statement, position, NullValue.value)
			} catch {
				case e :NullPointerException =>
					throw new NullPointerException(this.toString + " does not support null values.").initCause(e)
				case e :NoSuchElementException =>
					throw new NoSuchElementException(this.toString + ": " + e.getMessage).initCause(e)
			}

		override def notNull :SQLWriteForm[T] =
			new CustomSQLWriteForm[T](writtenColumns, toString + ".notNull")(write)
				with NotNullWriteForm[T]

		override val toString = if (name != null) name else "SQLWriteForm@" + System.identityHashCode(this)
	}



	private[schema] class ConstSQLWriteForm[T](private val value :T, name :String = null)
	                                          (implicit protected override val form :SQLWriteForm[T])
		extends WriteFormAdapter[Any] with IgnoringWriteForm[Any] with NullableWriteFormLiterals[Any]
	{
		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			form.set(statement, position, value)

		override def literal(ignored: Any, inline :Boolean): String = form.literal(value, inline)

		override def notNull :SQLWriteForm[Any] =
			new ConstSQLWriteForm[T](value, toString + ".notNull")(form.notNull)

		override def equals(that :Any) :Boolean = that match {
			case const :ConstSQLWriteForm[_] =>
				(const eq this) || (const canEqual this) && const.value == value && const.form == form
			case _ => false
		}

		override def hashCode :Int = value.hashCode * 31 + form.hashCode

		override val toString = if (name != null) name else s"$form='$value'"
	}



	private[schema] class EvalSQLWriteForm[T](value: => Option[T], name :String = null)
	                                         (implicit protected override val form :SQLWriteForm[T])
		extends WriteFormAdapter[Any] with IgnoringWriteForm[Any] with NullableWriteFormLiterals[Any]
	{
		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			form.setOpt(statement, position, value)

		override def literal(ignored: Any, inline :Boolean): String = value match {
			case Some(x) => form.literal(x, inline)
			case _ => form.nullLiteral(inline)
		}

		override def notNull :SQLWriteForm[Any] = new EvalSQLWriteForm(value, toString + ".notNull")(form.notNull)

		override val toString = if (name != null) name else s"$form=_"
	}



	private[schema] class EvalOrNullSQLWriteForm[T](value: => Option[T])
	                                               (implicit protected override val form :SQLWriteForm[T],
	                                                orElse :NullValue[T])
		extends WriteFormAdapter[Any] with IgnoringWriteForm[Any] with NullableWriteFormLiterals[Any]
	{
		override def setNull(statement :PreparedStatement, position :Int) :Unit = value match {
			case Some(x) => form.set(statement, position, x)
			case _ => form.set(statement, position, orElse.value)
		}

		override def literal(ignored: Any, inline :Boolean): String = value match {
			case Some(x) => form.literal(x, inline)
			case _ => form.literal(orElse.value, inline)
		}

		override def notNull :SQLWriteForm[Any] = new EvalOrNullSQLWriteForm[T](value)(form.notNull, orElse)

		override val toString =
			try { form.toString + "=_:NULL='" + orElse.value + "'"}
			catch { case e :Exception => form.toString + "=_:NULL=" + e }

	}



	private[schema] class ErrorSQLWriteForm[T](columns :Int, error: => Nothing, name :String = null)
		extends SQLWriteForm[T] with WriteFormLiterals[T]
	{
		override def setNull(statement :PreparedStatement, position :Int) :Unit = error
		override def set(statement :PreparedStatement, position :Int, value :T) :Unit = error
		override def literal(value :T, inline :Boolean) :String = error
		override def nullLiteral(inline :Boolean) :String = error
		override def writtenColumns = columns
		override def notNull :SQLWriteForm[T] = this
		override def toString = if (name == null) "<ERROR" else name
	}






	private[schema] class CustomNullSQLWriteForm[T](protected override val form :SQLWriteForm[T])
	                                               (implicit val nulls :NullValue[T])
		extends ProxyWriteForm[T] with WriteFormNullValue[T]
	{
		override def nullValue :T = nulls.value

		override def notNull :SQLWriteForm[T] = new CustomNullSQLWriteForm[T](form.notNull)

		override def withNull(implicit nulls :NullValue[T]) :SQLWriteForm[T] = new CustomNullSQLWriteForm(form)(nulls)

		private[this] val string =
			try { form.toString + "/NULL=" + nulls.value }
			catch { case e :Exception => form.toString + "/NULL=" + e }

		override def toString = string
	}



	private[schema] trait FlatMappedWriteForm[S, -T] extends WriteFormAdapter[T] with WriteFormLiterals[T] {
		protected override val form :SQLWriteForm[S]
		protected val unmap :T => Option[S]

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			form.setOpt(statement, position, unmap(value))

		override def literal(value: T, inline :Boolean): String = unmap(value) match {
			case Some(x) => form.literal(x, inline)
			case _ => form.nullLiteral(inline)
		}

		override def nullLiteral(inline :Boolean): String = form.nullLiteral(inline)

		//map/flatMap not overriden to preserve potentially overriden toString.

		override def toString :String = "<=" + form
	}

	private[schema] class FlatMappedSQLWriteForm[S, -T](protected override val unmap :T => Option[S], name :String = null)
	                                                   (implicit protected override val form :SQLWriteForm[S])
		extends FlatMappedWriteForm[S, T]
	{
		override def notNull :SQLWriteForm[T] =
			new FlatMappedSQLWriteForm[S, T](unmap, toString + ".notNull") with NotNullWriteForm[T]

		override val toString = if (name == null) "<=" + form else name
	}



	private[schema] trait MappedWriteForm[S, -T] extends WriteFormAdapter[T] with WriteFormLiterals[T] {
		protected override val form :SQLWriteForm[S]
		protected val unmap :T => S

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			form.set(statement, position, unmap(value))

		override def literal(value :T, inline :Boolean) :String = form.literal(unmap(value), inline)
		override def nullLiteral(inline :Boolean) :String = form.nullLiteral(inline)

		//map/flatMap not overriden to preserve potentially overriden toString.

		override def toString :String = "<=" + form
	}

	private[schema] class MappedSQLWriteForm[S, -T](protected override val unmap :T => S, name :String = null)
	                                               (implicit protected override val form :SQLWriteForm[S])
		extends MappedWriteForm[S, T]
	{
		override def notNull :SQLWriteForm[T] =
			new MappedSQLWriteForm(unmap, toString + ".notNull") with NotNullWriteForm[T]

		override val toString = if (name == null) "<=" + form else name
	}






	private[schema] trait LazyWriteForm[-T] extends ProxyWriteForm[T] {
		protected[this] var init: () => SQLWriteForm[T]
		@volatile
		protected[this] var initialized :SQLWriteForm[T] = _
		protected[this] var fastAccess :SQLWriteForm[T] = _

		def isInitialized :Boolean = fastAccess !=  null || initialized != null

		protected override def form :SQLWriteForm[T] = {
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
		override def unmap[X](fun :X => T) :SQLWriteForm[X] = form.unmap(fun)
		override def flatUnmap[X](fun :X => Option[T]) :SQLWriteForm[X] = form.flatUnmap(fun)

		override def toOpt = form.toOpt
		override def nullSafe = form.nullSafe
		override def notNull = form.notNull
		override def withNull(implicit nulls :NullValue[T]) = form.withNull
		override def withNull(nullValue :T) = form.withNull(nullValue)

		override def *(repeat :Int) = form.*(repeat)

		override def *[O](other :SQLWriteForm[O]) :SQLWriteForm[(T, O)] = form * other
		override def +[S <: T](next :SQLWriteForm[S]) :SQLWriteForm[S] = form + next
		override def <>[O <: T](read :SQLReadForm[O]) :SQLForm[O] = form <> read


		override def toString :String =
			if (fastAccess == null && initialized == null) "<Lazy"
			else "Lazy(" + form + ")"
	}



	private class CombinedSQLWriteForm[-T](val forms :Seq[SQLWriteForm[T]], name :String = null)
		extends SQLWriteForm[T]
	{
		override val writtenColumns :Int = (0 /: forms)(_ + _.writtenColumns)

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit = {
			var i = position
			forms foreach { form => form.set(statement, i, value); i += form.writtenColumns }
		}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			var i = position
			forms foreach { form => form.setNull(statement, i); i += form.writtenColumns }
		}

		override def literal(value: T): String =
			forms.map(_.literal(value)).mkString("(", ", ", ")")

		override def inlineLiteral(value: T): String =
			forms.map(_.inlineLiteral(value)).mkString("", ", " ,"")

		override def nullLiteral: String = forms.map(_.nullLiteral).mkString("(", ", ", ")")

		override def inlineNullLiteral: String = forms.map(_.inlineNullLiteral).mkString("", ", ", "")


		override def notNull :SQLWriteForm[T] =
			new CombinedSQLWriteForm[T](forms.map(_.notNull), toString + ".notNull")

		override def +[S <: T](next :SQLWriteForm[S]) :SQLWriteForm[S] = next match {
			case seq :CombinedSQLWriteForm[S @unchecked] =>
				new CombinedSQLWriteForm(forms ++: seq.forms)
			case _ =>
				new CombinedSQLWriteForm(forms :+ next)
		}


		override def equals(that :Any) :Boolean = that match {
			case composite :CombinedSQLWriteForm[_] =>
				(composite eq this) || (composite canEqual this) && composite.forms == forms
			case _ => false
		}

		override def hashCode :Int = forms.hashCode

		override val toString :String =
			if (name != null) name else forms.mkString("<(", "+", ")")
	}






	private[schema] trait SeqWriteForm[-T] extends SQLWriteForm[Seq[T]] with WriteFormLiterals[Seq[T]] {
		protected def form :SQLWriteForm[T]
		protected def repeats :Int

		override def writtenColumns :Int = form.writtenColumns * repeats

		override def set(statement :PreparedStatement, position :Int, value :Seq[T]) :Unit =
			if (value == null)
				setNull(statement, position)
			else if (value.length > repeats)
				throw new IllegalArgumentException(
					s"Expected maximum $repeats values for form $this, got ${value.length}: $value."
				)
			else {
				val f = form
				var i = position; val jump = f.writtenColumns
				value foreach { x => f.set(statement, i, x); i += jump }
				val limit = writtenColumns
				while (i < limit) {
					f.setNull(statement, i); i += jump
				}
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			val f = form
			var i = position; val jump = f.writtenColumns; val limit = writtenColumns
			while (i < limit) {
				f.setNull(statement, i); i += jump
			}
		}

		override def literal(value: Seq[T], inline :Boolean): String =
			if (value == null) nullLiteral
			else if (value.length > repeats)
				throw new IllegalArgumentException(
					s"Expected maximum $repeats values for form $this, got ${value.length}: $value."
				)
			else if (inline)
				value.view.map(form.inlineLiteral).mkString(", ")
			else
				value.view.map(form.inlineLiteral).mkString("(", ", ", ")")

		override def nullLiteral(inline :Boolean): String = {
			val item = form.inlineNullLiteral
			if (inline)
				Iterator.continually(item).take(repeats).mkString(", ")
			else
				Iterator.continually(item).take(repeats).mkString("(", ", ", ")")
		}

		override def toString :String = "(" + form.toString + "*" + repeats + ")"
	}



	private case class SeqSQLWriteForm[-T](form :SQLWriteForm[T], repeats :Int) extends SeqWriteForm[T] {
		def this(count :Int)(implicit form :SQLWriteForm[T]) = this(form, count)

		override def notNull :SQLWriteForm[Seq[T]] = new SeqSQLWriteForm[T](form.notNull, repeats)

		override val writtenColumns :Int = super.writtenColumns
		override val toString :String = super.toString
	}



	private[schema] case class SQLWriteFormSeq[-T](forms :Seq[SQLWriteForm[T]])
		extends SQLWriteForm[Seq[T]] with WriteFormLiterals[Seq[T]]
	{
		override val writtenColumns = (0 /: forms)(_ + _.writtenColumns)

		private def validateLength(value :Seq[T]) :Unit =
			if (value.length != forms.length)
				throw new IllegalArgumentException(
					s"Passed sequence's length differs from the number of forms: $value vs $forms"
				)

		override def set(statement :PreparedStatement, position :Int, value :Seq[T]) :Unit =
			if (value == null)
				setNull(statement, position)
			else {
				var i = position
				validateLength(value)
				value.iterator.zip(forms.iterator) foreach {
					case (x, form) => form.set(statement, i, x); i += form.writtenColumns
				}
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			var i = position
			forms foreach { form => form.setNull(statement, i); i += form.writtenColumns }
		}

		override def literal(value :Seq[T], inline :Boolean):String =
			if (value == null)
				nullLiteral(inline)
			else {
				validateLength(value)
				val literals = forms.view.zip(value).filter(_._1.writtenColumns > 0).map {
					case (form, x) => form.inlineLiteral(x)
				}
				if (inline) literals.mkString(", ")
				else literals.mkString("(", ", ", ")")
			}

		override def nullLiteral(inline :Boolean) :String = {
			val literals = forms.view.filter(_.writtenColumns > 0).map(_.inlineNullLiteral)
			if (inline) literals.mkString(", ")
			else literals.mkString("(", ", ", ")")
		}

		override def notNull :SQLWriteForm[Seq[T]] = new SQLWriteFormSeq[T](forms.map(_.notNull))

		private val string :String = forms.iterator.map(_.toString).mkString("::")
		override def toString :String = string
	}


}



