package net.noresttherein.oldsql.schema

import java.sql.JDBCType.NULL
import java.sql.{JDBCType, PreparedStatement}

import scala.annotation.{implicitNotFound, tailrec}
import scala.collection.immutable.{ArraySeq, Seq}
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.{ConstSeq, Opt, PassedArray, ReversedList}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{InseparableExpressionException, NullValueException}
import net.noresttherein.oldsql.morsels.SpecializingFactory
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.pixies.RearrangedIndexing
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLWriteForm.{AbstractWriteFormProxy, DefaultsSQLWriteForm, JoinedSQLWriteForm, NotNullWriteFormProxy, NullSafeWriteFormProxy, SQLWriteFormList, WriteFormAdapter}
import net.noresttherein.oldsql.schema.forms.{CaseSQLWriteForm, CustomOptSQLWriteForm, CustomSQLWriteForm, DerivedMappedSQLWriteForm, DerivedOptMappedSQLWriteForm, LazyWriteForm, MappedSQLWriteForm, OffsetWriteForm, OptMappedSQLWriteForm, ReorderedWriteForm, RepeatedSQLWriteForm, SQLForms, SQLWriteFormSeq, UnspecifiedForm}
import net.noresttherein.oldsql.schema.forms.UnspecifiedForm.{BaseFormAdapter, UnspecifiedFormAdapter, UnspecifiedNamedForm}

//implicits
import net.noresttherein.oldsql.slang._






/** Encapsulates the logic of disassembling values of `T` into values for individual columns and using them
  * to set `PreparedStatement` parameters. As an additional functionality, it knows how to format the value
  * of `T` as an SQL literal for verbatim inclusion as a constant (rather than a parameter) into an SQL expression.
  * Whether or not `null` values are allowed depends on actual implementation. No `NullPointerException` will
  * be thrown directly by forms in this library, but `null` values may be passed over to client's mapping functions
  * used to adapt a form from one type to another.
  * Implementations should provide a pure contract, in particular ''always'' setting the same number of consecutive
  * parameters, defined as `columnCount`. This makes it a lower-level counterpart of
  * [[net.noresttherein.oldsql.schema.Mapping Mapping]], which can be represented by possibly many forms, depending on
  * the column lists included as both the 'insert/update' and `query/where` portions of a statement.
  * All implementations must be thread safe.
  * @see [[net.noresttherein.oldsql.schema.SQLReadForm]]
  * @see [[net.noresttherein.oldsql.schema.SQLForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm]]
  */
@implicitNotFound("I do not know how to set PreparedStatement parameters from type ${T}: " +
                  "missing implicit SQLWriteForm[${T}].")
trait SQLWriteForm[-T] extends UnspecifiedForm { outer =>

	/** Number of parameters set by this form each time its `set` or `setNull` is called. */
	override def columnCount :Int

	/** Set the values of parameters `<position..position+columnCount)` of the given `PreparedStatement` to
	  * the values obtained from the given value of `T`. This method simply delegates to `set` or `setNull`, depending
	  * on whether the value is defined.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.set]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull]]
	  */
	def setOpt(statement :PreparedStatement, position  :Int, value :Opt[T]) :Unit = value match {
		case Got(x) => set(statement, position, x)
		case _ => setNull(statement, position)
	}

	/** Set the values of parameters `[position..position+columnCount)` of the given `PreparedStatement` to
	  * the values obtained from the given value of `T`. While forms for reference types can in theory accept
	  * `null` values, client code should not assume that passing `null` to this method will be handled gracefully
	  * by all forms. This would be impossible to achieve for forms of built-in value types, which will always
	  * throw a `NullPointerException` on a `null` unboxing attempt, but also higher-level forms can depend on
	  * the internal structure of the value `T` without checking it for nullity. Instead of calling `set` for a `null`,
	  * use [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]] for `Lack` or explicitly `setNull`.
	  * If a value for a column/parameter cannot be obtained, a `null` value of the appropriate JDBC SQL type should
	  * be set, unless this lack is a result of illegal argument or some other error, in which case an appropriate
	  * exception should be thrown. However, method [[net.noresttherein.oldsql.schema.SQLWriteForm.nullSafe nullSafe]]
	  * can be used to obtain a proxy to this form which allows `null` values.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull]]
	  */
	def set(statement :PreparedStatement, position :Int, value :T) :Unit

	/** Set the values of parameters `[position..position+columnCount)` of the given `PreparedStatement` to
	  * represent the 'null value' of type `T`, however this form decides to define it. Typically, this means setting
	  * all the columns which would be set with `set` to the appropriate `null` value, but it is not required.
	  * Implementations should strive to support this method even if they do not allow `null` arguments
	  * for [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]], unless they have been specifically created
	  * for 'not null' database columns.
	  */
	def setNull(statement :PreparedStatement, position :Int) :Unit



	/** A, possibly multi column, string representation of the given value, for embedding as a fragment
	  * of an SQL statement. The `inline` parameter specifies if the value should be formatted as a tuple/sequence,
	  * with literals for individual columns surrounded by a pair of `(` and `)`, or inlined: only separated with commas.
	  * Inlining is recursive, that is any component forms used must also use an inline format their corresponding
	  * values. This is used in contexts such as a ''select'' clause, where the columns must be listed individually.
	  *
	  * Whether null arguments are allowed depends on the form's implementation
	  * and the mapped type; in general, if `null` is considered a valid value for `T` ''by the application'',
	  * the form should not throw an exception and behave
	  * as if [[net.noresttherein.oldsql.schema.SQLWriteForm.nullLiteral nullLiteral]] method was called.
	  * No [[NullPointerException]] will however be thrown directly by any bundled form and the framework never
	  * uses this method to ''specifically'' set a `null`.
	  *
	  * This method is equivalent to `literal(value)` or `inlineLiteral(value)`, depending on the value of
	  * `inline` parameter. It remains abstract in `SQLWriteForm` because various implementations
	  * require different delegation direction. There is however a handful of mixin traits which implement them
	  * based on the preferred behaviour.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.inlineLiteral]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NullableWriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormOptLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormSeparateLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm.DirectColumnWriteForm]]
	  */ //consider: default value of false for inline and removal of literal(Boolean), and inlineLiteral delegate here
	@throws[UnsupportedOperationException](
		"if this type doesn't have a non-null representation in SQL, such as with various BLOB types.")
	def literal(value :T, inline :Boolean) :String

	/** A string representation of a given value of `T` as an SQL literal, ready to be embedded as a constant part
	  * of an SQL statement. The result is a single SQL expression, possibly formatted as a sequence of expressions,
	  * surrounded with parenthesis if needed. If the form consists of zero columns, an empty `String` is returned.
	  * In most cases, this will be a wrapped in parenthesis, concatenated and comma separated, list of literals
	  * for properties of `value` mapped to individual columns in this form, as returned
	  * by [[net.noresttherein.oldsql.schema.SQLWriteForm.columnLiterals columnLiterals]].
	  * It is however simply a common default, rather than a strict requirement.
	  *
	  * Whether null arguments are allowed depends on the form's implementation
	  * and the mapped type; in general, if `null` is considered a valid value for `T` ''by the application'',
	  * the form should not throw an exception and behave
	  * as if [[net.noresttherein.oldsql.schema.SQLWriteForm.nullLiteral nullLiteral]] method was called.
	  * No [[NullPointerException]] will however be thrown directly by any bundled form and the framework never
	  * uses this method to ''specifically'' set a `null`.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.inlineLiteral]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.columnLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NullableWriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormSeparateLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormOptLiterals]]
	  */ //todo: define whether it should handle null values and, if so, provide some ease-of-life support.
	@throws[UnsupportedOperationException](
		"if this type doesn't have a non-null representation in SQL, such as with various BLOB types.")
	def literal(value :T) :String

	/** A string representation of a given value of `T` as an SQL literal, ready to be embedded as a constant
	  * or constants within a a sequence of single column expressions, such as a ''select'' clause of an SQL ''select''.
	  * Multi column expressions are formatted as inlined comma separated values. If the form consists of zero columns,
	  * an empty `String` is returned. 	The result, when used in a ''select'' or ''group by'' clause, should parse
	  * as `this.`[[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.columnCount columnCount]] independent columns.
	  * Barring special needs, this is virtually always a concatenated sequence of literals for properties
	  * of `value` mapping to individual columns within this form, i.e.
	  * {{{
	  *     def inlineLiteral(value :T) = this.columnLiterals(value).mkString(", ")
	  * }}}
	  *
	  * Whether null arguments are allowed depends on the form's implementation
	  * and the mapped type; in general, if `null` is considered a valid value for `T` ''by the application'',
	  * the form should not throw an exception and behave
	  * as if [[net.noresttherein.oldsql.schema.SQLWriteForm.inlineNullLiteral inlineNullLiteral]] method was called.
	  * No [[NullPointerException]] will however be thrown directly by any bundled form and the framework never
	  * uses this method to ''specifically'' set a `null`.
	  * @return a `String` equal to `this.literal(value, true)`, although the delegation direction may differ
	  *         between forms.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.columnLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NullableWriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormSeparateLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormOptLiterals]]
	  */ //todo: define whether it should handle null values and, if so, provide some ease-of-life support.
	@throws[UnsupportedOperationException](
		"if this type doesn't have a non-null representation in SQL, such as with various BLOB types.")
	def inlineLiteral(value :T) :String

	/** A, possibly multi column, SQL representation of a 'null value' for the mapped type. The meaning of 'null value'
	  * here depends on the use case, but typically this is simply a comma separated list of `NULL` values matching
	  * the [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.columnCount number of columns]] in this form.
	  * The `inline` parameter specifies if the columns are used as a single expression, or a part
	  * of a larger expression, for example within a ''select'' clause. In the former case, the literal is formatted
	  * as a tuple/sequence by surrounding it in parenthesis, while in the later case simply the list of columns
	  * is returned. This method is equivalent to the combination of parameterless `nullLiteral`
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.inlineNullLiteral inlineNullLiteral]], depending
	  * on the value of parameter `inline`. Neither of them is implemented on the level of `SQLWriteForm`,
	  * leaving subclasses the choice of the delegation direction. However, several mixin traits
	  * with commonly used defaults are available.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.nullLiteral]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.inlineNullLiteral]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NullableWriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormSeparateLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormOptLiterals]]
	  */
	def nullLiteral(inline :Boolean) :String //= singleLiteral(nullColumnLiterals, inline)

	/** A string representation of a 'null value' of type `T` (typically an SQL NULL or a tuple of NULL values),
	  * ready to be embedded as a constant part of an SQL statement. The literal should be a valid single SQL expression,
	  * possibly listing multiple values as a sequence. Single columns by default return simply "null",
	  * while forms for multiple expressions list null literals as a comma separated sequence - again, in virtually
	  * all cases, simply "null" strings. Zero-width forms return always return an empty `String`.
	  * Ultimately how a 'null value' for type `T` (if it exists at all, and regardless of whether it is actual 'null'
	  * or some other default value) depends on the form mapping that type.
	  * As long as it is technically possible, forms should however implement this method,
	  * even if a concept of a 'null value for type T' does not exist in the application.
	  * @return a `String` equal to `nullLiteral(false)`, although the order of actual delegation differs between forms.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.inlineNullLiteral]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NullableWriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormSeparateLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormOptLiterals]]
	  */
	def nullLiteral :String //= nullLiteral(false)

	/** A, possibly multi column, representation of a 'null value' of type `T` in SQL, for use as a constant fragment
	  * of an SQL statement. In single column forms and empty forms,
	  * it is equal to [[net.noresttherein.oldsql.schema.SQLWriteForm.nullLiteral nullLiteral]].
	  * Multi column forms however list all their constituent columns, be they direct or handled by a component form
	  * for some property of `T`, on the top level, separated with commas. The result does not in general constitute
	  * a valid SQL expression, but should be possible to use, among other inline literals, in scopes such
	  * as a ''select'' clause of an SQL ''select''. When used in these contexts, the SQL fragment should parse
	  * as [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.columnCount columnCount]] columns.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NullableWriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormSeparateLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormOptLiterals]]
	  */
	def inlineNullLiteral :String

	/** String representations of all column expressions to which this form maps the given argument. Each element
	  * on the list should be a valid SQL/JDBC single column expression.For single column forms,
	  * it is the same as `literal(value)`. Empty forms return an empty sequence. In all cases, the list should have
	  * `this.`[[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.columnCount columnCount]] elements.
	  * Individual literals may be used in tandem, combined together
	  * in a [[net.noresttherein.oldsql.schema.SQLWriteForm.literal literal]] expression of this form, or individually,
	  * possibly interspersed with other expressions, for example in case of a column by column comparison
	  * of two compatible SQL expressions.
	  */
	@throws[UnsupportedOperationException](
		"if this type doesn't have a non-null representation in SQL, such as with various BLOB types.")
	def columnLiterals(value :T) :Seq[String]

	/** String literals for all columns in this form, mapping to a 'null value' of the mapped type. What exactly
	  * is a 'null value', especially in the case of multi column forms, ultimately depends on the form's implementation.
	  * In virtually all cases however, this is simply a sequence
	  * of [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.columnCount columnCount]] "NULL" strings,
	  * but a form may choose instead to use some default values. Regardless of implementation,
	  * the list should contain `this.`[[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.columnCount columnCount]]
	  * elements.
	  */
	def nullColumnLiterals :Seq[String]


	/** The '?' character repeated the number of this form's columns time, separated by ','.
	  * If the form has more than one column, the returned string will be surrounded by '(' and ')'.
	  */ //todo: this is a good candidate to override with a val
	def param :String = columnCount match {
		case 0 => ""
		case 1 => "?"
		case n =>
			val res = new java.lang.StringBuilder(n * 3 + 2)
			res append '('
			var i = n
			while (i > 0) {
				res append "?, "
				i -= 1
			}
			res append '?' append ')'
			res.toString
	}

	/** The '?' character repeated the number of this form's columns time, separated by ','. */
	def inlineParam :String = columnCount match {
		case 0 => ""
		case 1 => "?"
		case n =>
			val res = new java.lang.StringBuilder(n * 3)
			var i = n
			while (i > 0) {
				res append "?, "
				i -= 1
			}
			res append '?'
			res.toString
	}

	/** Calls `this.`[[net.noresttherein.oldsql.schema.SQLWriteForm.inlineParam inlineParam]] if `inline` is true,
	  * or [[net.noresttherein.oldsql.schema.SQLWriteForm.param param]] otherwise.
	  */
	def param(inline :Boolean) :String = if (inline) inlineParam else param

	/** A sequence of JDBC parameter placeholders equal to the number of columns in this form.
	  * @return a `Seq` of length `columnCount`, with all elements equal to "?".
	  */
	def params :Seq[String] = ConstSeq("?", columnCount)


	/** A form is not universal if some of the column values it writes do not depend on the given argument
	  * (for example, in a [[net.noresttherein.oldsql.schema.SQLWriteForm.const const]] form).
	  */
	override def isUniversal :Boolean = true

	/** A mixin trait for anonymous inner proxy classes to this form, which should remain comparable to this instance.
	  * Method [[net.noresttherein.oldsql.schema.SQLWriteForm.comparable comparable]] of this trait checks
	  * if the argument is a `ComparableWriteForm` of this instance or vice versa: if this check succeeds,
	  * the forms are automatically comparable without further comparisons.
	  */
	protected[schema] trait ComparableWriteForm extends SQLWriteForm[T] {
		private[SQLWriteForm] def parent :SQLWriteForm[T] = SQLWriteForm.this
		override def comparable(other :SQLWriteForm[_]) :Boolean =
			SQLWriteForm.this.comparable(other) || super.comparable(other)
	}


	/** Splits this form into a sequence of forms for individual columns, such that form
	  * `SQLWriteForm.`[[net.noresttherein.oldsql.schema.SQLWriteForm.seq[T](items* seq(this.split)]]
	  * is equivalent to this one. Column forms should return singleton collections containing themselves.
	  * Note that using the column forms individually will be generally slower than using this form directly;
	  * neither are existing forms optimized for efficient implementation of this method itself.
	  */
	@throws[UnsupportedOperationException]("if this form cannot be split into individual columns " +
	                                       "(for example, because it adapts a custom function)")
	def split :Seq[ColumnWriteForm[T]]


	/** Create a write form for `X` which will map received values to `T` and pass them to this form.
	  * The arguments are not tested for `null` values before being passed to `fun`, which should handle `null`s
	  * gracefully if they are considered a valid value for the adapted form's use case.
	  */ //todo: rename to imap
	def unmap[X](f :X => T) :SQLWriteForm[X] = SQLWriteForm.map(f)(this)

	/** Create a write form for `X` which will try to map received values to `T` and pass them to this form.
	  * If the given function yields `None`, this form's `setNull` method is used instead of `set`.
	  * The arguments are not tested for `null` values before being passed to `fun`, which should handle `null`s
	  * gracefully if they are considered a valid value for the adapted form's use case.
	  */
	def optUnmap[X](f :X => Option[T]) :SQLWriteForm[X] = SQLWriteForm.optMap(f)(this)

	/** Creates a write form for `X` which will use this form after extracting a value from `X` with the given
	  * extractor. This is equivalent to `unmap` or `flatUnmap`, depending on whether the extractor is
	  * a `RequisiteExtractor` instance. In corner cases, such as a constant extractor, a special `SQLWriteForm`
	  * instance may be returned.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.unmap]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.optUnmap]]
	  */
	def from[X](extractor :X =?> T) :SQLWriteForm[X] = compose(extractor)

	/** Creates a write form for `X` which will use this form after extracting a value from `X` with the given
	  * extractor. This is equivalent to `unmap` or `flatUnmap`, depending on whether the extractor is
	  * a `RequisiteExtractor` instance.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.unmap]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.optUnmap]]
	  */
	def compose[X](extractor :X =?> T) :SQLWriteForm[X] = SQLWriteForm(extractor)(this)



	/** Lift this form to represent `Option[T]`, where `Some` values are delegated to this instance's `set` method,
	  * while `None` results in calling this form's `setNull` instead.
	  */
	def toOpt :SQLWriteForm[Option[T]] = SQLForms.OptionWriteForm(this)

	/** A null-safe proxy to this form. All methods accepting values of `T` check first if their argument is `null`;
	  * if so, they delegate to the null-specific variant of the method in this form. Otherwise, the call is forwarded
	  * to the same method of this form. The same effect can be achieved by mixing in trait
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.NullSafeWriteForm NullSafeWriteForm]] to an existing class.
	  * All bundled concrete `SQLWriteForm` implementations are already null-safe
	  * and will not throw a `NullPointerException` themselves, unless specifically created in this way in order
	  * to reject `null` values. This method can be however a useful help in creating custom forms, taking
	  * of the burden of performing the checks by the application, and even for those provided forms which encapsulate
	  * custom implementations. Note that null-safety is not a requirement for this class as many applications
	  * will opt to disallow `null` values being propagated to or used by the client code.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.notNull]]
	  */
	def nullSafe :SQLWriteForm[T] = new AbstractWriteFormProxy[T](this) with NullSafeWriteFormProxy[T]

	/** An adapter or modification of this form which ensures that `null` values will never be set
	  * as statement parameters. In cases where this form would write a `null` value, the returned form will throw
	  * a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]]. Literal methods are unaffected.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.notNull]]
	  */
	override def notNull :SQLWriteForm[T] = new AbstractWriteFormProxy[T](this) with NotNullWriteFormProxy[T]

	/** A proxy to this instance which directs all calls of its null-specific methods to their general
	  * counterparts in this form, in particular [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]. The value provided by the implicit type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` is passed as the argument.
	  * A similar effect can be achieved directly by mixing in trait
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormNullValue WriteFormNullValue]] to an existing class.
	  * Note that this doesn't substitute `null` values passed explicitly to the form with the one provided by
	  * the type class; this effect can be achieved by calling
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.nullSafe nullSafe]] on the result.
	  */
	def withNull(implicit nulls :NullValue[T]) :SQLWriteForm[T] = new DefaultsSQLWriteForm[T](Lack)(this, nulls)

	/** A proxy to this instance which directs all calls to its null-specific methods to their general
	  * counterparts in this form, in particular [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] with the value provided here as the argument.
	  * Note that this doesn't substitute `null` values passed explicitly to the form with the one provided by
	  * the type class; this effect can be achieved by calling
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.nullSafe nullSafe]] on the result.
	  */
	def withNull(nullValue :T) :SQLWriteForm[T] = withNull(NullValue(nullValue))

	/** Creates a conditional form, which will first, with the function given as the first argument,
	  * get a discriminator value for the written entity `T`, write it with this form,
	  * and then use the form associated with that key in the `key -> form` list given as the second argument
	  * to write the whole entity `T` at positions following those written by the discriminator form.
	  */
	def when[X](discriminator :X => T)(cases :(T, SQLWriteForm[X])*) :SQLWriteForm[X] =
		if (cases.isEmpty)
			throw new IllegalArgumentException("Empty case list for a conditional form.")
		else
			when(discriminator, Map.from(cases))

	/** Creates a conditional form, which will first, with the function given as the first argument,
	  * get a discriminator value for the written entity `T`, write it with this form,
	  * and then use the form associated with that key in the `key -> form` map given as the second argument
	  * to write the whole entity `T` at positions following those written by the discriminator form.
	  * The handling of discriminator values which do not have a form associated with them depends
	  * on [[scala.collection.Map.defaultValue]]
	  */
	def when[K <: T, X](discriminator :X => K, cases :Map[K, SQLWriteForm[X]]) :SQLWriteForm[X] =
		new CaseSQLWriteForm(discriminator, this, cases)

	/** An adapter to this form which sets [[java.sql.PreparedStatement PreparedStatement]] parameters out of order.
	  * A call to `proxyForm.`[[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]`(statement, start, value)`
	  * will delegate to `this.set(wrapperStatement, 1, value)`, where `wrapperStatement` is a wrapper
	  * of the original `statement` exposing the `n`-th parameter of the latter at position
	  * `permutation.indexOf(n - start) + 1`. In other words, the `n`-th parameter set by this form
	  * will actually set parameter `permutation(n - 1) + start` in the underlying prepared statement
	  * (the one passed to the proxy).
	  *
	  * Note that the index of the first parameter of a `PreparedStatement` is `1` rather than `0`, but `permutation`
	  * is zero-based, that is `permutation(n - 1) + 1` is the real index of the `n`-th set parameter
	  * (if starting writing from the first parameter).
	  * @param permutation a permutation vector of length equal to
	  *                    `this.`[[net.noresttherein.oldsql.schema.SQLWriteForm.columnCount columnCount]], where
	  *                    every value from range `[0, this.columnCount)` appears exactly once.
	  */
	def reorder(permutation :IndexedSeq[Int]) :SQLWriteForm[T] = ReorderedWriteForm.shuffle(this, permutation)

	/** An adapter to this form which writes columns out of order, possibly only a subset, or ignoring some
	  * intermittent parameters in the [[java.sql.PreparedStatement PreparedStatement]].
	  * The argument realises index translation from the consecutive `[1..this.columnCount]` parameters
	  * set by this form on any `PreparedStatement` passed to `set`, to the indices which should be set
	  * on statements received by the adapter. The latter will wrap wrap prepared statements it receives in proxies
	  * which change the apparent order of parameters: `n`-th parameter of the underlying statement will be exposed
	  * at position `order.inverse(n - start + 1)` or, in other words, the `n`-th parameter set by this form will
	  * actually set parameter `order(n) + start - 1` on the statement passed to the adapter.
	  * The adapter will always expose the actual parameters as a range
	  * starting with `1` (i.e., this form's [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method
	  * will always be called with `1` given as `start` argument).
	  *
	  * Note that if `order` is not an [[net.noresttherein.oldsql.pixies.RearrangedIndexing.isInjection injection]],
	  * the parameters set by this form will not be consecutive parameters of the underlying statement
	  * (they can have gaps between them).
	  * @param order a mapping translating the indices of the argument `PreparedStatement` to the indices in the order
	  *              expected by this form and back.
	  *              Its [[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]]
	  *              must equal this form's [[net.noresttherein.oldsql.schema.SQLWriteForm.columnCount columnCount]].
	  */
	def reorder(order :RearrangedIndexing) :SQLWriteForm[T] = ReorderedWriteForm(this, order)

	/** An adapter to this form which adds `shift` to parameter indices before setting parameters with this form.
	  * This method breaks implicit expectations for a form and the created instance will almost certainly result
	  * in unexpected behaviour if exposed to the outside. For this reason the reference to it should always
	  * be contained within a class which ensures that all its uses are valid.
	  */
	def >>(shift :Int) :SQLWriteForm[T] =
		if (shift == 0) this
		else new AbstractWriteFormProxy[T](this) with OffsetWriteForm[T] {
			override val offset = shift
		}

	/** An adapter to this form which subtracts `shift` from parameter indices before setting parameters with this form. */
	def <<(shift :Int) :SQLWriteForm[T] = this >> -shift



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
	def *[O](other :SQLWriteForm[O]) :SQLWriteForm[(T, O)] = {
		implicit val first = this; implicit val second = other
		SQLWriteForm[(T, O)]
	}

	/** Creates a write form which will first write any given value with this form, and then with the argument form,
	  * starting at statement parameter position right after the position of the last written parameter by this form.
	  * This is particularly useful in conjunction with composing both forms with functions retrieving different
	  * properties of the same larger entity. The string literal representation will be that of a SQL tuple.
	  */
	def +[S <: T](next :SQLWriteForm[S]) :SQLWriteForm[S] = next match {
		case _ if columnCount == 0 => next
		case _ if next.columnCount == 0 => this
		case composite :SQLWriteFormList[S @unchecked] =>
			SQLWriteForm.join(this +: composite.forms :_*)
		case _ =>
			new JoinedSQLWriteForm(this, next)
	}

	/** Combine this form with a read form for the same type in order to obtain a read-write form. */
	def <>[O <: T](read :SQLReadForm[O]) :SQLForm[O] = SQLForm.combine(read, this)


	/** Checks if this form and the other form consist of columns comparable on the database level.
	  * This is defined as having the same number
	  * of [[net.noresttherein.oldsql.schema.SQLWriteForm.columnCount columnCount]] and corresponding columns
	  * having the same [[java.sql.SQLType SQLType]]. Forms being comparable says nothing about the relation between
	  * Scala type arguments of the forms (i.e, the types/classes of read objects).
	  * This condition is strictly weaker than form equality, with two equal forms always being comparable.
	  * In particular, an `SQLWriteForm` can (and should, if only technically possible) be comparable
	  * with a full [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] of the same columns,
	  * while they would rarely compare equal.
	  * Note that due to technical limitations, this method may return false negatives.
	  *
	  * If the forms are unequal, the default implementation checks first for equality
	  * of [[net.noresttherein.oldsql.schema.SQLWriteForm.columnTypes column types]], if the property is supported
	  * by both objects, and then tries to [[net.noresttherein.oldsql.schema.SQLWriteForm.split split]] both forms,
	  * comparing the constituent column forms for comparability.
	  * If `other` is a [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormAdapter WriteFormAdapter]] then
	  * the check is reversed to `other comparable this` so that the adapter can compare the adapted form
	  * with this instance; the adapter trait overrides this method to prevent infinite recursion.
	  * Similarly, if either of the forms
	  * is a [[net.noresttherein.oldsql.schema.SQLWriteForm.ComparableWriteForm ComparableWriteForm]],
	  * its parent form is compared with the other.
	  */
	def comparable(other :SQLWriteForm[_]) :Boolean =
		this == other || (columnCount == 0 && other.columnCount == 0) || (other match {
			case _ if columnCount != other.columnCount => false
			case adapter :WriteFormAdapter[_] => adapter comparable this
			case derived :SQLWriteForm[_]#ComparableWriteForm if this comparable derived.parent => true
			case _ => shape <:> other.shape
//				try { shape <:> other.shape } catch {
//					case _ :UnsupportedOperationException => try {
//						split.view.map(_.sqlType) == other.split.view.map(_.sqlType)
//					} catch {
//						case _ :UnsupportedOperationException | _ :InseparableExpressionException => false
//					}
//				}
		})
}






/** A factory of [[net.noresttherein.oldsql.schema.SQLWriteForm! SQLWriteForm]] instances. */
object SQLWriteForm {

	/** Summon an implicitly available `SQLWriteForm[T]`. */
	@inline def apply[T](implicit form :SQLWriteForm[T]) :SQLWriteForm[T] = form

	//we override notNull and similar decorator factory methods in the anonymous classes in order to avoid
	//head aches in other subclasses of their base classes, as they almost certainly would need to override it.
	/** Creates a non-literal `SQLWriteForm` using the given function to set statement parameters from a value of `T`.
	  * The function becomes the body of the form's [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method.
	  * Its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method is however implemented separately
	  * and sets the [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.columnCount columnCount]] parameters
	  * directly to null using the statement's [[java.sql.PreparedStatement.setNull setNull]] method, with
	  * `JDBCType.`[[java.sql.JDBCType.NULL NULL]] specified as their types.
	  * This happens only on direct calls to `setNull`: arguments to `set` are not inspected for nullity.
	  * This can be achieved by using the [[net.noresttherein.oldsql.schema.SQLWriteForm.notNull notNull]] variant
	  * of the returned form.
	  * @param columns the number of set parameters.
	  * @param writer  a function taking a statement, index of the first parameter to set, and an object from which
	  *                the values of the parameters can be derived, and sets the consecutive `columns` number
	  *                of parameters on the statement.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	def apply[T](columns :Int)(writer :(PreparedStatement, Int, T) => Unit) :SQLWriteForm[T] =
		new CustomSQLWriteForm[T](columns)(writer) {
			override def notNull :SQLWriteForm[T] =
				new CustomSQLWriteForm[T](columnCount, Got(toString + ".notNull"))(writer) with NotNullWriteForm[T]
		}

	/** Creates a non-literal `SQLWriteForm` using the given function to set statement parameters from a value of `T`.
      * The function becomes the body of the form's [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method.
	  * Its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method is however implemented separately
	  * and sets the [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.columnCount columnCount]] parameters
	  * directly to null using the statement's [[java.sql.PreparedStatement.setNull setNull]] method, with
	  * `JDBCType.`[[java.sql.JDBCType.NULL NULL]] specified as their types.
	  * This happens only on direct calls to `setNull`: arguments to `set` are not inspected for nullity.
	  * This can be achieved by using the [[net.noresttherein.oldsql.schema.SQLWriteForm.notNull notNull]] variant
	  * of the returned form.
	  *
	  * All forms created by this method with the same name are equal, regardless of other arguments given.
	  * @param name    the name of the form, used as its identifier and returned by its `toString` method.
	  * @param columns the number of set parameters.
	  * @param writer  a function taking a statement, index of the first parameter to set, and an object from which
	  *                the values of the parameters can be derived, and sets the consecutive `columns` number
	  *                of parameters on the statement.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */ //todo: split into two parameter lists in Scala 3
	def apply[T](name :String, columns :Int)(writer :(PreparedStatement, Int, T) => Unit) :SQLWriteForm[T] =
		new CustomSQLWriteForm[T](columns, Got(name))(writer) with NamedWriteForm[T]


	/** Creates a non-literal `SQLWriteForm` using the given function to set statement parameters based on a value of `T`.
	  * The function becomes the body of the form's [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]]
	  * method, and methods [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] delegate to it by wrapping the parameter
	  * in `Got` or passing `Lack`, respectively.
	  * @param columns the number of set parameters.
	  * @param writer  a function taking a statement, index of the first parameter to set, and an optional object
	  *                from which the values of the parameters can be derived, and sets the consecutive `columns` number
	  *                of parameters on the statement.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	def opt[T](columns :Int)(writer :(PreparedStatement, Int, Opt[T]) => Unit) :SQLWriteForm[T] =
		new CustomOptSQLWriteForm[T](columns)(writer) {
			override def notNull :SQLWriteForm[T] =
				new CustomOptSQLWriteForm[T](columnCount, Got(toString + ".notNull"))(writer) with NotNullWriteForm[T]
		}

	/** Creates a non-literal `SQLWriteForm` using the given function to set statement parameters based on a value of `T`.
	  * The function becomes the body of the form's [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]]
	  * method, and methods [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] delegate to it by wrapping the parameter
	  * in `Got` or passing `Lack`, respectively.
	  *
	  * All forms created by this method with the same name are equal, regardless of other arguments given.
	  * @param name    the name of the form, used as its identifier and returned by its `toString` method.
	  * @param columns the number of set parameters.
	  * @param writer  a function taking a statement, index of the first parameter to set, and an optional object
	  *                from which the values of the parameters can be derived, and sets the consecutive `columns` number
	  *                of parameters on the statement.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	def opt[T](name :String, columns :Int)(writer :(PreparedStatement, Int, Opt[T]) => Unit) :SQLWriteForm[T] =
		new CustomOptSQLWriteForm[T](columns, Got(name))(writer) with NamedWriteForm[T] {
			override def nullSafe :this.type = this
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
	  */
	def apply[S :SQLWriteForm, T](extractor :T =?> S) :SQLWriteForm[T] = extractor match {
		case _ :IdentityExtractor[_] => SQLWriteForm[S].asInstanceOf[SQLWriteForm[T]]
		case const :ConstantExtractor[_, S @unchecked] => SQLWriteForm.const(const.constant)
		case req :RequisiteExtractor[T @unchecked, S @unchecked] => map(req.getter)
		case _ :EmptyExtractor[_, _] => SQLWriteForm.none
		case _ => optMap(extractor.optional)
	}

	/** Creates a new form with a 'soft type' given as `name` argument, by combining an implicit `SQLWriteForm[S]`
	  * with a given extractor to create an `SQLWriteForm[T]`. It is equivalent to
	  * `SQLWriteForm`[[net.noresttherein.oldsql.schema.SQLWriteForm.apply[S,T](extractor)* (name)(extractor)]],
	  * but the created form will equal any other `SQLWriteForm` created by this method if they have the same name
	  * and their base forms and are equal.
	  * @param extractor producer of values for the implicit base form to write, called for every value written
	  *                  with the produced form.
	  * @param name the name of this form constructor, used to recognize compatible forms and in `toString` implementation.
	  */
	def apply[S :SQLWriteForm, T](name :String)(extractor :T =?> S) :SQLWriteForm[T] =
		extractor match {
			case const :ConstantExtractor[_, S @unchecked] => SQLWriteForm.const(const.constant, name)
			case req :RequisiteExtractor[T @unchecked, S @unchecked] => map(name)(req.getter)
			case _ :EmptyExtractor[_, _] =>
				new NullifiedSQLWriteForm[S](Got(name))(SQLWriteForm[S]) with NamedWriteForm[Any]
			case _ => optMap(name)(extractor.optional)
		}


	/** Composes an implicitly available write form `SQLWriteForm[S]` with a given getter function to create
	  * an `SQLWriteForm[T]`. This function can in particular be used to present an `SQLWriteForm` for the type
	  * of a property of some entity type as a write form for said entity type. Such forms can be later combined
	  * together with [[net.noresttherein.oldsql.schema.SQLWriteForm.join join(forms)]] to create
	  * an `SQLWriteForm` for the whole entity.
	  * @param f the function mapping the arguments of the created form to values accepted by the implicit base form.
	  */
	def map[S :SQLWriteForm, T](f :T => S) :SQLWriteForm[T] =
		new MappedSQLWriteForm[S, T](f)

	/** Creates a new form with a 'soft type' given as `name` argument, by combining an implicit `SQLWriteForm[S]`
	  * with a given function to create an `SQLWriteForm[T]`. It is equivalent to
	  * `SQLWriteForm`[[net.noresttherein.oldsql.schema.SQLWriteForm.map[S,T](f:T=>S)* .map(f)]],
	  * but the created form will equal any other `SQLWriteForm` created by this method if they have the same name
	  * and their base forms and are equal.
	  * @param name the name of this form constructor, used to recognize compatible forms and in `toString` implementation.
	  * @param f    the function mapping the arguments of the created form to values accepted by the implicit base form.
	  */
	def map[S :SQLWriteForm, T](name :String)(f :T => S) :SQLWriteForm[T] =
		new DerivedMappedSQLWriteForm[S, T](name, f)


	/** Composes an implicitly available write form `SQLWriteForm[S]` with a given getter to create
	  * an `SQLWriteForm[T]`. If the function returns `None`, the created form will use
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method of the base form instead of
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]].
	  * @param f the function mapping the arguments of the created form to values accepted by the implicit base form.
	  */
	def optMap[S :SQLWriteForm, T](f :T => Option[S]) :SQLWriteForm[T] =
		new OptMappedSQLWriteForm[S, T](f)

	/** Creates a new form with a 'soft type' given as `name` argument, by combining an implicit `SQLWriteForm[S]`
	  * with a given function (returning the result value as an `Option`) to create an `SQLWriteForm[T]`. It is equivalent
	  * to `SQLWriteForm`[[net.noresttherein.oldsql.schema.SQLWriteForm.optMap[S,T](f:T=>Option[S])* .optMap(f)]],
	  * but the created form will equal any other `SQLWriteForm` created by this method if they have the same name
	  * and their base forms and are equal.
	  * @param name the name of this form constructor, used to recognize compatible forms and in `toString` implementation.
	  * @param f    the function mapping the arguments of the created form to values accepted by the implicit base form.
	  */
	def optMap[S :SQLWriteForm, T](name :String)(f :T => Option[S]) :SQLWriteForm[T] =
		new DerivedOptMappedSQLWriteForm[S, T](name, f)



	/** Create an `SQLWriteForm[T]` which will delegate all `set`/`setNull` calls to ''all'' forms in the given sequence,
	  * in the exact order they appear. The 'position' argument of every form after the last is increased by the sum
	  * of `columnCount` of all preceding forms.
	  */
	def join[T](forms :SQLWriteForm[T]*) :SQLWriteForm[T] = forms match {
		case Seq() => empty
		case Seq(f) => f
		case _ => new SQLWriteFormList[T](forms)
	}

	/** Create an `SQLWriteForm[T]` which will delegate all `set`/`setNull` calls to ''all'' forms in the given sequence,
	  * in the exact order they appear. The 'position' argument of every form after the last is increased by the sum
	  * of `columnCount` of all preceding forms.
	  */
	def join[T](name :String)(forms :SQLWriteForm[T]*) :SQLWriteForm[T] = forms match {
		case Seq() => empty(name)
		case Seq(f) => new NamedSQLWriteForm(f, Got(name))
		case _ => new SQLWriteFormList(forms, Got(name)) with NamedWriteForm[T]
	}


	/** Creates a write form applying the form `SQLWriteForm[T]` `repeats` number of times, writing values
	  * from an input sequence. If the written sequence is shorter than `repeats`, for the remaining iterations
	  * the element form's [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method is used instead of
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]. If the sequence is longer, an
	  * `IllegalArgumentException` is thrown.
	  */
	def seq[T :SQLWriteForm](repeats :Int) :SQLWriteForm[Seq[T]] = new RepeatedSQLWriteForm[T](repeats)

	/** A form writing sequences of constant length, each element with the corresponding form from the list
	  * provided as the argument.
	  */
	def seq[T](items :Seq[SQLWriteForm[T]]) :SQLWriteForm[Seq[T]] = new SQLWriteFormSeq[T](items)

	/** Creates a conditional form, which will first, with the function given as the first argument,
	  * get a discriminator value for the written entity `T`, write it with the implicit `SQLWriteForm[K]`,
	  * and then use the form associated with that key in the `key -> form` list given as the second argument
	  * to write the whole entity `T` at positions following those written by the discriminator form.
	  */
	def when[K :SQLWriteForm, T](discriminator :T => K)(cases :(K, SQLWriteForm[T])*) :SQLWriteForm[T] =
		if (cases.isEmpty)
			throw new IllegalArgumentException("Empty case list for a conditional form.")
		else
			when(discriminator, Map.from(cases))

	/** Creates a conditional form, which will first, with the function given as the first argument,
	  * get a discriminator value for the written entity `T`, write it with the implicit `SQLWriteForm[K]`,
	  * and then use the form associated with that key in the `key -> form` map given as the second argument
	  * to write the whole entity `T` at positions following those written by the discriminator form.
	  */
	def when[K :SQLWriteForm, T](discriminator :T => K, cases :Map[K, SQLWriteForm[T]]) :SQLWriteForm[T] =
		new CaseSQLWriteForm(discriminator, SQLWriteForm[K], cases)



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
			@volatile protected[this] override var initializer = () => delayed
		}



	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `SQLWriteForm[T]`. This covers both calls to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  * @param value The value passed to [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method of the
	  *              implicit base form.
	  * @param text  An optional textual representation of the form, used in its `toString` implementation
	  *              (and thrown exceptions). If none is provided, the form's name will be derived
	  *              from the provided `value`.
	  */
	def const[T :SQLWriteForm](value :T, text :String = null) :SQLWriteForm[Any] =
		if (value == null) new NullifiedSQLWriteForm[T](Opt(text))
		else new ConstSQLWriteForm(value, Opt(text))

	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `SQLWriteForm[T]`. This covers both calls to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  * @param value The value passed to [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method of the
	  *              implicit base form.
	  * @param name  A name of the form, used as its identifier and in its `toString` implementation.
	  *              All `const` forms of the same name are equal, regardless of the constant used.
	  */
	def const[T :SQLWriteForm](name :String)(value :T) :SQLWriteForm[Any] =
		if (value == null) new NullifiedSQLWriteForm[T](Got(name)) with NamedWriteForm[Any]
		else new ConstSQLWriteForm(value, Got(name)) with NamedWriteForm[Any]

	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `SQLWriteForm[T]`. This covers both calls to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  * @param value The written value; if `Got`, all calls will be directed to
	  *              [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method of the base form;
	  *              otherwise [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] will be called each time.
	  * @param text  An optional textual representation of the form, used in its `toString` implementation
	  *              and thrown exceptions. If none is provided, the form's name will be derived from the provided `value`.
	  */
	def constOpt[T :SQLWriteForm](value :Opt[T], text :String = null) :SQLWriteForm[Any] =
		if (value.isEmpty) new NullifiedSQLWriteForm[T](Opt(text))
		else new ConstSQLWriteForm(value.get, Opt(text))

	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `SQLWriteForm[T]`. This covers both calls to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  * @param name  an optional name of the form, used as its identifier and in its `toString` implementation.
	  *              All `const` forms created with the same name are equal, regardless of their values.
	  * @param value the written value; if `Got`, all calls will be directed to
	  *              [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method of the base form;
	  *              otherwise [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] will be called each time.
	  */
	def constOpt[T :SQLWriteForm](name :String)(value :Opt[T]) :SQLWriteForm[Any] =
		if (value.isEmpty) new NullifiedSQLWriteForm[T](Got(name)) with NamedWriteForm[Any]
		else new ConstSQLWriteForm(value.get, Got(name)) with NamedWriteForm[Any]


	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `SQLWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form.
	  * @param value the expressions evaluated at each call to [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
	  *              and [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] methods and passed to
	  *              the `set` method of the implicit base form.
	  * @param text  an optional textual representation of the form, used in its `toString` implementation
	  *              and thrown exceptions. If none is provided, the form's name will be derived from the provided `value`.
	  */
	def eval[T :SQLWriteForm](value: => T, text :String = null) :SQLWriteForm[Any] =
		new EvalSQLWriteForm[T](Got(value), Opt(text))

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `SQLWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form.
	  * @param name  an optional name of the form, used as its identifier and in its `toString` implementation.
	  *              All `eval` forms created with the same name are equal, regardless of their values.
	  * @param value the expressions evaluated at each call to [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
	  *              and [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] methods and passed to
	  *              the `set` method of the implicit base form.
	  */
	def eval[T :SQLWriteForm](name :String)(value: => T) :SQLWriteForm[Any] =
		new EvalSQLWriteForm[T](Got(value), Got(name)) with NamedWriteForm[Any]

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `SQLWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form. If it yields
	  * `None`, the 'null' variant of the literal/write method will be called on the backing form.
	  * @param value the expressions evaluated at each call to [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
	  *              and [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] methods and written with
	  *              the implicit base form.
	  * @param text  an optional textual representation of the form, used in its `toString` implementation
	  *              and thrown exceptions. If none is provided, the form's name will be derived from the provided `value`.
	  */
	def evalOpt[T :SQLWriteForm](value: => Opt[T], text :String = null) :SQLWriteForm[Any] =
		new EvalSQLWriteForm[T](value, Opt(text))

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `SQLWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form. If it yields
	  * `None`, the 'null' variant of the literal/write method will be called on the backing form.
	  * @param name  an optional name of the form, used as its identifier and in its `toString` implementation.
	  *              All `eval` forms created with the same name are equal, regardless of their values.
	  * @param value the expressions evaluated at each call to [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
	  *              and [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] methods and written with
	  *              the implicit base form.
	  */
	def evalOpt[T :SQLWriteForm](name :String)(value: => Opt[T]) :SQLWriteForm[Any] =
		new EvalSQLWriteForm[T](value, Got(name)) with NamedWriteForm[Any]



	/** An `SQLWriteForm` ignoring its input and always writing `null` values using the implicitly given
	  * `SQLWriteForm[T]`. Null is defined here by the [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]
	  * type class, which must be also present for type `T`. This form is similar to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.none SQLWriteForm.none]], but passes the written value
	  * explicitly to the backing form's [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method,
	  * rather than delegating null handling to its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]],
	  * like the latter.
	  */ //new name: nullValue
	def nullValue[T :SQLWriteForm :NullValue] :SQLWriteForm[Any] = new NullValueSQLWriteForm[T]

	/** An `SQLWriteForm` ignoring its input and always writing `null` values using the implicitly given
	  * `SQLWriteForm[T]`. Null is defined here by the [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]
	  * type class, which must be also present for type `T`. This form is similar to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.none SQLWriteForm.none]], but passes the value obtained
	  * from the type class explicitly to the underlying form's [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
	  * method, rather than delegate null handling to its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]],
	  * like the latter. Note that this means that, if
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.Null NullValue.Null]] is the null value for `T`,
	  * than the implicit `SQLWriteForm[T]`'s `set` method must accept `null` values.
	  * @param name  the name of the form, used as its identifier and in its `toString` implementation.
	  *              All `nullValue` forms created with the same name are equal, regardless of their values.
	  */
	def nullValue[T :SQLWriteForm :NullValue](name :String) :SQLWriteForm[Any] =
		new NullValueSQLWriteForm[T](Got(name)) with NamedWriteForm[Any]

//	/** An `SQLWriteForm` proxy to an implicit instance of `SQLWriteForm[T]` which uses the implicit
//	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` type class to obtain values
//	  * to use for the set parameters when the given argument is `null`
//	  * or its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method is called.
//	  * All other calls to method [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
//	  * are delegated normally to the implicit form's `set` method with the same arguments.
//	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.nullValue]]
//	  */
//	def defaults[T :SQLWriteForm :NullValue] :SQLWriteForm[T] = new DefaultsSQLWriteForm[T]
//
//	/** An `SQLWriteForm` proxy to an implicit instance of `SQLWriteForm[T]` which uses the implicit
//	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` type class to obtain values
//	  * to use for the set parameters when the given argument is `null`
//	  * or its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method is called.
//	  * All other calls to method [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
//	  * are delegated normally to the implicit form's `set` method with the same arguments.
//	  * Two forms created by this method are equal if they wrap equal forms and their names are equal.
//	  * @param name The name of the form, used as its identifier and in its `toString` representation.
//	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.nullValue]]
//	  */
//	def defaults[T :SQLWriteForm :NullValue](name :String) :SQLWriteForm[T] =
//		new DefaultsSQLWriteForm[T](Got(name)) with DerivedWriteForm[T]

	/** An `SQLWriteForm` which will always write the 'null' value of type `T` using the implicitly available form.
	  * Only the null-specific methods of the base form are used, such as
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.nullValue]]
	  */ //defaults
	def none[T :SQLWriteForm] :SQLWriteForm[Any] = new NullifiedSQLWriteForm[T]//(Got("NONE"))

//	/** An `SQLWriteForm` which will always write the 'null' value of type `T` using the implicitly available form.
//	  * Only the null-specific methods of the base form are used, such as
//	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
//	  * @param name  an optional name of the form, used as its identifier and in its `toString` implementation.
//	  *              All `const` forms created with the same name are equal, regardless of their values.
//	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.nullValue]]
//	  */
//	def none[T :SQLWriteForm](name :String) :SQLWriteForm[Any] =
//		new NullifiedSQLWriteForm[T](Got(name)) with NamedWriteForm[Any]

	/** An `SQLWriteForm` which always sets the `columns` consecutive columns directly to `null`, with a
	  * [[net.noresttherein.oldsql.sql.RowShape RowShape]] consisting of `JDBCType.NULL` repeated `columns` times.
	  */
	def nulls[T](columns :Int) :SQLWriteForm[T] = new NullSQLWriteForm(columns)

	/** A form which serves as a padding between other forms, shifting the starting parameter offsets of the following
	  * forms. All operations are ignored, no JDBC parameters within the index gap are set. The literal consists
	  * of `null` repeated `columnCount` times (and separated with a ',').
	  */ //not setting statement parameters just results in an SQLException
	def gap[T](columnCount :Int) :SQLWriteForm[T] = new GapSQLWriteForm(columnCount)

	/** An empty form which never writes anything. Its `columnCount` property is set to zero. */
	def empty[T] :SQLWriteForm[T] = SQLForm.empty //empty("EMPTY")

	/** An empty form which never writes anything. Its `columnCount` property is set to zero.
	  * @param name the name of the form, used in its `toString` implementation (and thrown exceptions).
	  */
	private def empty[T](name :String) :SQLWriteForm[T] =
		new AbstractSQLWriteForm[Any](0, Got(name)) with EmptyWriteForm[Any] with NamedWriteForm[Any]



	/** A write form which throws the given exception on every write attempt.
	  * Two forms created by this method are equal if they throw the same exception type.
	  * @tparam E an exception class defining at least one of the following standard constructors:
	  *           `(String, Throwable)`, `(String)`, `(Throwable)` or `()`.
	  * @param columns the width of this form in columns, specifying how many statement parameters it is responsible for;
	  *                used to position this form among other forms.
	  */
	def error[E <: Throwable :ClassTag](columns :Int) :SQLWriteForm[Any] = {
		val nulls = NullValue.error[E]
		new ErrorSQLWriteForm(columns, nulls, Got("ERROR" + columns + "[" + localNameOf[E] + "]"))
	}

	/** A write form which will evaluate the given exception throwing expression on every write attempt.
	  * Note that this includes [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]], not only
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]].
	  * Two forms created by this method will be equal if only their names are equal.
	  * @param columns the width of this form, defining how many consecutive statement parameters it is responsible for,
	  *                used to position it among other forms.
	  * @param text    an optional textual representation of this form, used in its `toString` method.
	  * @param raise   an expression throwing an exception (and possibly doing something else beforehand).
	  */
	def error(columns :Int, text :String = null)(raise: => Nothing) :SQLWriteForm[Any] = {
		val nulls = NullValue.eval(raise)
		val name = if (text != null) text else "ERROR" + columns + "@" + nulls.shortHashString
		new ErrorSQLWriteForm(columns, nulls, Got(name))
	}

	/** A write form which will evaluate the given exception throwing expression on every write attempt.
	  * Note that this includes [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]], not only
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]].
	  * Two forms created by this method will be equal if only their names are equal.
	  * @param name    an optional name of the form, used as its identifier and in its `toString` implementation.
	  *                All `error` forms created with the same name are equal, regardless of their values.
	  * @param columns the width of this form, defining how many consecutive statement parameters it is responsible for,
	  *                used to position it among other forms.
	  * @param raise   an expression throwing an exception (and possibly doing something else beforehand).
	  */
	def error(name :String)(columns :Int)(raise: => Nothing) :SQLWriteForm[Any] =
		error(name, columns)(NullValue.eval(name, raise))

	private[schema] def error(columns :Int, text :Opt[String], value :NullValue[Nothing]) :SQLWriteForm[Any] =
		new ErrorSQLWriteForm(columns, value, text)

	/** A write form which will evaluate the given exception throwing expression on every write attempt.
	  * Note that this includes [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]], not only
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]. It is equivalent in function to
	  * `error(name :String)(columns :Int)(raise: => Nothing)`,
	  * but taking a [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] type class instead of
	  * a by-name expression, which allows its potential reuse and equality. The same effect can also be
	  * obtained with the more generic [[net.noresttherein.oldsql.schema.SQLWriteForm.nullValue nullValue]] method,
	  * but this method does not require a preexisting implicit `SQLWriteForm[T]` like the former.
	  * Two forms created by this method will be equal if only their names are equal.
	  * @param name    an optional name of the form, used as its identifier and in its `toString` implementation.
	  *                All `error` forms created with the same name are equal, regardless of their values.
	  * @param columns the width of this form, defining how many consecutive statement parameters it is responsible for,
	  *                used to position it among other forms.
	  * @param value   a wrapped expression throwing an exception.
	  */
	private def error(name :String, columns :Int)(value :NullValue[Nothing]) :SQLWriteForm[Any] =
		new ErrorSQLWriteForm(columns, value, Got(name)) with NamedWriteForm[Any]

	/** A write form which will throw an [[UnsupportedOperationException]] with the given message at every write attempt.
	  * All forms with the same column counts created with this method are equal.
	  * @param columns the width of this form in columns, specifying how many statement parameters it is responsible for;
	  *                used to position this form among other forms.
	  * @param text    an optional textual representation of this form, used in its `toString` method.
	  * @param message the message included in thrown exceptions.
	  */
	def unsupported(columns :Int, text :String = null)(message :String) :SQLWriteForm[Any] = {
		val nulls = NullValue.Unsupported(message)
		val name = if (text != null) text else "UNSUPPORTED" + columns
		new ErrorSQLWriteForm(columns, nulls, Got(name))
	}

//	/** A write form which will throw an `UnsupportedOperationException` with the given message at every write attempt.
//	  * This is a simple shorthand for [[net.noresttherein.oldsql.schema.SQLWriteForm.error error]].
//	  */
//	def unsupported(message :String) :SQLWriteForm[Any] = unsupported(0)(message)




	/** Base type for factories of some types `M[X]` and `S[X] <: M[X]`, which take as implicit arguments
	  * `SQLWriteForm` and `ColumnWriteForm` instances (or some higher type parameterized with these form types),
	  * respectively See [[net.noresttherein.oldsql.morsels.SpecializingFactory SpecializingFactory]]
	  * for more information about this framework type.
	  */
	type WriteFormBasedFactory[A[_], M[_], S[X] <: M[X]] = SpecializingFactory[A, A, SQLWriteForm, ColumnWriteForm, M, S]




	/** A mixin trait for write forms overriding
	  *  [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormNullGuard.set set]] to check the argument for nullity,
	  *  and delegating either to `super.set` or `setNull`, depending on the result.
	  */
	trait WriteFormNullGuard[-T] extends SQLWriteForm[T] {
		abstract override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			if (value == null) setNull(statement, position)
			else super.set(statement, position, value)
	}

	/** An `SQLWriteForm` mixin trait which throws
	  * a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]] from
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method and from
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] if the argument given is `null` and from
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]] if the argument given is `Lack`.
	  * It still allows null literals. Overriden
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormNullGuard.set set]] method calls super for non null
	  * arguments, so this mixin does not prevent a form from actually setting a `null` parameter
	  * if some non-null value of `T` translates to an ŚQL null, although this scenario is unlikely.
	  */
	trait NotNullWriteForm[-T] extends WriteFormNullGuard[T] {
		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			throw new NullValueException("Null values not allowed for " + this + ".")

		abstract override def split :Seq[ColumnWriteForm[T]] = super.split.map(_.notNull)
		override def notNull :this.type = this
	}

	/** A wrapper over any write form which will throw
	  * a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]]
	  * instead of setting a `null` parameter.
	  */
	private[schema] trait NotNullWriteFormProxy[-T] extends WriteFormProxy[T] with NotNullWriteForm[T] {
		private lazy val cachedString = form.toString + ".notNull"
		override def toString :String = cachedString
	}


	/** A late mix-in trait for `SQLWriteForm[T]` implementations which overrides all methods accepting a value of `T`
	  * and calls the super method only if the argument is not null. For `null` values it delegates the call
	  * to the null-specific counterpart method. It is an easy way for a class to achieve null safety if the application
	  * actually allows `null` values.
	  *
	  * Classes mixing in this trait cannot implement any `nullLiteral` methods by delegating to (non null) `literal`
	  * methods (for example, by extending
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.NullableWriteFormLiterals NullableWriteFormLiterals]],
	  * or an infinite cycle will occur.
	  */
	trait NullSafeWriteForm[-T] extends WriteFormNullGuard[T] {
		//always use super to avoid infinite cycles if the delegation order is from specific to generic
		abstract override def literal(value :T, inline :Boolean) :String =
			if (value == null) nullLiteral(inline)
			else super.literal(value, inline)

		abstract override def literal(value :T) :String =
			if (value == null) nullLiteral else super.literal(value)

		abstract override def inlineLiteral(value :T) :String =
			if (value == null) inlineNullLiteral else super.inlineLiteral(value)

		abstract override def columnLiterals(value :T) :Seq[String] =
			if (value == null) nullColumnLiterals else super.columnLiterals(value)

		abstract override def split :Seq[ColumnWriteForm[T]] = super.split.map(_.nullSafe)

		override def nullSafe :this.type = this
	}

	/** Base trait for write form adapters which delegate calls with `null` as an argument to the `null`-specific
	  * method counterpart.
	  */
	private[schema] trait NullSafeWriteFormProxy[-T] extends WriteFormProxy[T] with NullSafeWriteForm[T] {
		private lazy val cachedString = form.toString + ".nullSafe"
		override def toString :String = cachedString
	}


	/** A mix-in trait for write forms of values which can't appear as literals in SQL statements.
	  * Implements [[net.noresttherein.oldsql.schema.SQLWriteForm.columnLiterals columnLiterals]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.nullColumnLiterals nullColumnLiterals]]
	  * by throwing an [[UnsupportedOperationException]]. Other literal methods delegate to the former,
	  * so the exception propagates. At the same time, overriding `nullColumnLiterals` will result
	  * in default null literals without throwing any exceptions from null specific methods.
	  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm.NonLiteralColumnWriteForm]]
	  */
	trait NonLiteralWriteForm[-T] extends WriteFormLiterals[T] {
		override def columnLiterals(value: T): Seq[String] =
			if (value == null) nullColumnLiterals
			else throw new UnsupportedOperationException(toString + ".columnLiterals")

		override def nullColumnLiterals :Seq[String] =
			throw new UnsupportedOperationException(toString + ".nullColumnLiterals")
	}


	/** A mixin trait implementing literal formatting methods of `SQLWriteForm` by delegating more specific variants
	  * to their more generic counterparts. The only method remaining abstract
	  * is [[net.noresttherein.oldsql.schema.SQLWriteForm.columnLiterals columnLiterals]];
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.nullColumnLiterals nullColumnLiterals]] returns simply
	  * a sequence consisting of string `"null"` repeated
	  * [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.columnCount columnCount]] number of times.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NullableWriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormSeparateLiterals]]
	  */
	trait WriteFormLiterals[-T] extends SQLWriteForm[T] {
		override def literal(value :T, inline :Boolean) :String = singleLiteral(columnLiterals(value), inline)
		override def literal(value :T) :String = literal(value, false)
		override def inlineLiteral(value :T) :String = literal(value, true)
		override def nullLiteral(inline :Boolean) :String = singleLiteral(nullColumnLiterals, inline)
		override def nullLiteral :String = nullLiteral(false)
		override def inlineNullLiteral :String = nullLiteral(true)
		override def nullColumnLiterals :Seq[String] = ConstSeq("null", columnCount)
	}

	/** An mixin for `SQLWriteForm` implementations diverting the calls to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.nullLiteral(inline:Boolean) nullLiteral]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.nullColumnLiterals nullColumnLiterals]]
	  * to their non-null counterparts, passing `null` as the argument.
	  * This means that now all literal methods delegate to the same method:
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.literal(value:T,inline:Boolean)* literal]]`(value, inline)`
	  * (which, by default, composes the literal from literals of component columns returned by
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.columnLiterals columnLiterals]]).
	  */
	private[schema] trait NullableWriteFormLiterals[-T >: Null] extends WriteFormLiterals[T] {
		override def nullLiteral(inline :Boolean) :String = literal(null, inline)
		override def nullColumnLiterals = columnLiterals(null)
	}

	/** A mixin trait implementing literal formatting methods by delegating from more generic ones
	  * to one of the more specific variants:
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormSeparateLiterals.inlineNullLiteral inlineNullLiteral]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.nullLiteral nullLiteral]] from `nullLiteral(inline :Boolean)`
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormSeparateLiterals.inlineLiteral inlineLiteral]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.literal literal]]`(value :T)`
	  * from `literal(value :T, inline :Boolean)`. Inline literal strings concatenate the literals for individual
	  * columns returned by [[net.noresttherein.oldsql.schema.SQLWriteForm.columnLiterals columnLiterals]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.nullColumnLiterals nullColumnLiterals]] using a comma
	  * as a separator, and non-inline literals wrap their inline counterparts in a pair of parenthesis, unless
	  * they are empty strings or `"null"`.
	  */
	trait WriteFormSeparateLiterals[-T] extends SQLWriteForm[T] {
		override def literal(value :T, inline :Boolean) :String =
			if (value == null) nullLiteral(inline)
			else if (inline) inlineLiteral(value)
			else literal(value)

		override def literal(value :T) :String = inlineLiteral(value) match {
			case "" => ""
			case literals => "(" + literals + ")"
		}
		override def inlineLiteral(value :T) :String =
			if (value == null) inlineNullLiteral else singleLiteral(columnLiterals(value), true)

		override def nullLiteral(inline :Boolean) :String =
			if (inline) inlineNullLiteral else nullLiteral

		override lazy val nullLiteral :String = inlineNullLiteral match {
			case "" => ""
			case "null" | "NULL" | "Null" => "null"
			case literals => "(" + literals + ")"
		}
		override lazy val inlineNullLiteral :String = singleLiteral(nullColumnLiterals, true)

		override def nullColumnLiterals :Seq[String] = ConstSeq("null", columnCount)
	}

	/** A mixing trait implementing methods of `SQLWriteForm` which create literals.
	  * All methods eventually delegate to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormOptLiterals.optLiteral optLiteral]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormOptLiterals.optColumnLiterals optColumnLiterals]].
	  * The difference from [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals]] is that if the argument
	  * is `null`, a cached [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormOptLiterals.nullLiteral nullLiteral]]
	  * or [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormOptLiterals.inlineNullLiteral inlineNullLiteral]]
	  * is used. The same applies to parameterless `nullLiteral`.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NullableWriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormSeparateLiterals]]
	  */
	trait WriteFormOptLiterals[-T] extends SQLWriteForm[T] {
		override def literal(value :T, inline :Boolean) :String =
			if (value == null) nullLiteral(inline) else optLiteral(Got(value), inline)

		override def literal(value :T) :String =
			if (value == null) nullLiteral else literal(value, false)

		override def inlineLiteral(value :T) :String =
			if (value == null) inlineNullLiteral else literal(value, true)

		override def columnLiterals(value :T) :Seq[String] =
			if (value == null) nullColumnLiterals else optColumnLiterals(Got(value))

		override def nullLiteral(inline :Boolean) :String =
			if (inline) inlineNullLiteral else nullLiteral
		override lazy val nullLiteral        :String = optLiteral(Lack, false)
		override lazy val inlineNullLiteral  :String = optLiteral(Lack, true)

		override lazy val nullColumnLiterals :Seq[String] = optColumnLiterals(Lack)

		protected def optLiteral(value :Opt[T], inline :Boolean) :String =
			singleLiteral(optColumnLiterals(value), inline)

		protected def optColumnLiterals(value :Opt[T]) :Seq[String]
	}

	trait WriteFormNullLiterals[-T] extends SQLWriteForm[T] {
		override def literal(value :T, inline :Boolean) :String =
			if (value == null) nullLiteral(inline) else singleLiteral(columnLiterals(value), inline)

		override def literal(value :T) :String =
			if (value == null) nullLiteral else literal(value, false)

		override def inlineLiteral(value :T) :String =
			if (value == null) inlineNullLiteral else literal(value, true)

		override def nullLiteral(inline :Boolean) :String =
			if (inline) inlineNullLiteral else nullLiteral

		override lazy val nullLiteral        :String = inlineNullLiteral match {
			case "" => ""
			case "NULL" | "null" | "Null" => "null"
			case literals => "(" + literals + ")"
		}
		override lazy val inlineNullLiteral  :String = singleLiteral(nullColumnLiterals, true)//optLiteral(Lack, true)

		override def nullColumnLiterals :Seq[String] = ConstSeq("null", columnCount) //optColumnLiterals(Lack)
	}

	private def singleLiteral(literals :Seq[String], inline :Boolean) :String =
		if (literals.isEmpty) ""
		else if (literals.sizeIs == 1) literals.head
		else if (inline) literals.mkString(", ")
		else literals.mkString("(", ", ", ")")


	/** A read form with equality defined as `name` equality only (at least by default).
	  * Resets the implementations of form proxy factory methods which any class which mixes this trait in might
	  * have overriden with dedicated implementations back to their default implementations from `SQLWriteForm`
	  * in order to preserve a reference to this form and thus name-based, rather than property-based equality
	  * of the created forms.
	  */
	private[schema] trait NamedWriteForm[-T] extends SQLWriteForm[T] with UnspecifiedNamedForm {
		override def nullSafe :SQLWriteForm[T] = super[SQLWriteForm].nullSafe
		override def notNull :SQLWriteForm[T] = super[SQLWriteForm].notNull
		override def withNull(implicit nulls :NullValue[T]) :SQLWriteForm[T] = super[SQLWriteForm].withNull
	}

	private[schema] class NamedSQLWriteForm[-T](override val form :SQLWriteForm[T], override val text :Got[String])
		extends AbstractWriteFormProxy[T](form, text) with NamedWriteForm[T]


	private[schema] abstract class AbstractSQLWriteForm[-T](columns :Int, protected override val text :Opt[String])
		extends SQLWriteForm[T]
	{
		override def columnCount :Int = columns
		private[schema] lazy val cachedString = if (text.isEmpty) super.toString else "<" + text.get
		override def toString :String = cachedString
	}




	/** Base/mixin trait for write forms which are based on another form. Implements
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.columnCount columnCount]] and null-specific
	  * literal methods by direct delegation to member `form`.
	  */
	trait WriteFormAdapter[-T] extends SQLWriteForm[T] with UnspecifiedFormAdapter {
		private[schema] def adaptedWriteForm :SQLWriteForm[Nothing] = form
		protected def form :SQLWriteForm[Nothing]


//		override def columnCount :Int = form.columnCount
//		override def isUniversal :Boolean = form.isUniversal

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			form.setNull(statement, position)

		override def literal(value :T) :String = literal(value, false)
		override def inlineLiteral(value :T) :String = literal(value, true)
		override def nullLiteral(inline :Boolean) :String = form.nullLiteral(inline)
		override def nullLiteral :String = form.nullLiteral
		override def inlineNullLiteral :String = form.inlineNullLiteral
		override def nullColumnLiterals :Seq[String] = form.nullColumnLiterals

		override def comparable(other :SQLWriteForm[_]) :Boolean = other match {
			case _ if this eq other => true
			case other :WriteFormAdapter[_] => form comparable other //instead of `other comparable this` to prevent a cycle
			case _ => form comparable other
		}
	}

	/** Base trait for proxy forms, which delegates all methods to their
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormProxy.form form]] field.
	  * Together with various mixin traits provides complete form implementations for various transformation methods
	  * of `SQLWriteForm`.
	  */
	trait WriteFormProxy[-T] extends WriteFormAdapter[T] {
		//consider: if we renamed it to writer, we could allow CombinedForms to extend both ProxyWriteForm
		// and ProxyReadForm. The drawback is that all other classes extending it now would need to override two properties
		// also, it conflicts with SQLForm.writer in a potentially unresolvable manner
		protected override def form :SQLWriteForm[T]

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			form.set(statement, position, value)

		override def literal(value :T, inline :Boolean) :String = form.literal(value, inline)
		override def literal(value: T): String = form.literal(value)
		override def inlineLiteral(value :T) :String = form.inlineLiteral(value)
		override def columnLiterals(value: T): Seq[String] = form.columnLiterals(value)

		override def split :Seq[ColumnWriteForm[T]] = form.split

		override def equals(that :Any) :Boolean = that match {
			case proxy :WriteFormProxy[_] =>
				(proxy eq this) || canEqual(proxy) && (proxy canEqual this) && proxy.form == form
			case _ => false
		}
		override def hashCode :Int = form.hashCode

		override def toString :String =
			if (text.isDefined) "<" + text.get
			else "<" + this.innerClassName + "@" + this.shortHashString + "[" + form + "]"
	}

	private[schema] abstract class AbstractWriteFormProxy[-T](protected override val form :SQLWriteForm[T],
	                                                          protected override val text :Opt[String] = Lack)
		extends BaseFormAdapter[SQLWriteForm[T]](form) with WriteFormProxy[T]
	{
		private[schema] override lazy val cachedString :String =
			if (text.isDefined) "<" + text.get else super[WriteFormProxy].toString
	}


	/** An `SQLWriteForm` mixin trait for adapter forms, defining equality as equality of their names and adapted forms.
	  * Overrides `name` delegating it to `text`, which is a commonly accepted field parameter of all forms.
	  */
	private[schema] trait DerivedWriteForm[-T] extends WriteFormAdapter[T] {
		assert(name.isDefined, "Empty name in a " + getClass + " instance.")

		override def name :Opt[String] = text

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[DerivedWriteForm[_]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :DerivedWriteForm[_] if other canEqual this => name == other.name && form == other.form
			case _ => false
		}
		override def hashCode :Int = name.hashCode * 31 + form.hashCode

		private[schema] lazy val cachedString = name.get + "[" + form + "]>"
		override def toString :String = cachedString
	}


	/** A base trait for forms which write nothing. Sets the `columnCount` property to zero.
	  * It is used for empty tuples and in similar cases of application-level only constants.
	  */
	private[schema] trait EmptyWriteForm[-T] extends WriteFormLiterals[T] {
		final override def columnCount: Int = 0
		final override def columnTypes :Seq[JDBCType] = PassedArray.empty

		override def isUniversal = false
		override def set(statement :PreparedStatement, position :Int, value :T) :Unit = ()
		override def setNull(statement :PreparedStatement, position :Int) :Unit = ()
		override def literal(value: T, inline :Boolean): String = ""
		override def nullLiteral(inline :Boolean): String = ""
		override def columnLiterals(value: T): Seq[String] = Nil
		override def nullColumnLiterals: Seq[String] = Nil

		override def split :Seq[ColumnWriteForm[T]] = Nil
		override def nullSafe :SQLWriteForm[T] = this
		override def notNull :SQLWriteForm[T] = this

		override def comparable(other :SQLWriteForm[_]) :Boolean = other.columnCount == 0

		override def toString = "<EMPTY"
	}


	/** A simple mix-in trait for write forms which ignore passed arguments when setting parameters.
	  * It directs [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method
	  * to [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  */
	private[schema] trait IgnoringWriteForm extends SQLWriteForm[Any] {
		override def set(statement :PreparedStatement, position :Int, value :Any) :Unit =
			setNull(statement, position)

		override def literal(value :Any, inline :Boolean) :String = nullLiteral(inline)
		override def literal(value :Any) :String = nullLiteral
		override def inlineLiteral(value :Any) :String = inlineNullLiteral
//		override def nullLiteral = nullLiteral(false)
		override def columnLiterals(value :Any) :Seq[String] = nullColumnLiterals

		override def unmap[X](f :X => Any) :SQLWriteForm[X] = this
		override def optUnmap[X](f :X => Option[Any]) :SQLWriteForm[X] = this
		override def compose[X](extractor :X =?> Any) :SQLWriteForm[X] = this
		override def toOpt :SQLWriteForm[Option[Any]] = this

		override def nullSafe :SQLWriteForm[Any] = this
		override def isUniversal = false
	}



	/** A mix-in trait for `SQLWriteForm[T]` implementations which wish to provide a custom `null` representation,
	  * different from value `null` (or, in case of proxy/composite forms, from how the base form handles nulls
	  * in [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] and other null-specific methods.
	  * This trait overrides all these methods, delegating every call to the generic, non-null method counterpart,
	  * passing its `nullValue` property as the argument. It doesn't override any methods accepting `T` as an argument
	  * and doesn't deal in any way with actual `null` values, in particular it doesn't attempt to map them
	  * to the `nullValue` property. For this, you can use/mix-in
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.NullSafeWriteForm NullSafeWriteForm]]
	  */
	private[schema] trait WriteFormNullValue[T] extends SQLWriteForm[T] {
		protected[this] def nullValue :T

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			set(statement, position, nullValue)

		override def nullLiteral :String = literal(nullValue)
		override def inlineNullLiteral :String = inlineLiteral(nullValue)
		override def nullColumnLiterals :Seq[String] = columnLiterals(nullValue)
	}

	/** An implementation of [[net.noresttherein.oldsql.schema.SQLWriteForm.withNull withNull]] method.
	  * Separate from its `WriteFormNullValue` super trait because it mixes in `NullSafeWriteForm` with
	  * `abstract override` methods.
	  */
	private[schema] trait WriteFormWithNull[T]
		extends WriteFormProxy[T] with WriteFormNullValue[T] with NullSafeWriteForm[T]
	{
		protected def nulls :NullValue[T]
		override def nullValue :T = nulls.value

//		override def nullSafe :SQLWriteForm[T] = form.nullSafe match {
//			case safe if safe eq form => this
//			case safe => safe.withNull(nulls)
//		}
		override def withNull(implicit nulls :NullValue[T]) :SQLWriteForm[T] = form.withNull(nulls)

		abstract override def split :Seq[ColumnWriteForm[T]] = form.split.map(_.withNull(nulls))

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :WriteFormWithNull[_] if other canEqual this => form == other.form && nulls == other.nulls
			case _ => false
		}
		override def hashCode :Int = form.hashCode * 31 + nulls.hashCode

		private lazy val cachedString = if (text.isDefined) "<" + text.get else form.toString + "(null="  + nulls + ")"
		override def toString :String = cachedString
	}


	private[schema] class DefaultsSQLWriteForm[T](protected override val text :Opt[String] = Lack)
	                                             (implicit protected override val form :SQLWriteForm[T],
	                                              protected override val nulls :NullValue[T])
		extends AbstractWriteFormProxy[T](form, text) with WriteFormWithNull[T]




	private[schema] class ConstSQLWriteForm[T](private val value :T, protected override val text :Opt[String] = Lack)
	                                          (implicit protected override val form :SQLWriteForm[T])
		extends AbstractSQLWriteForm[Any](form.columnCount, text) with WriteFormAdapter[Any]
		   with WriteFormSeparateLiterals[Any] with IgnoringWriteForm
	{
		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			form.set(statement, position, value)

		override lazy val nullColumnLiterals = form.columnLiterals(value)
		override def split = form.split.map(ColumnWriteForm.const(value)(_))

		override def notNull :SQLWriteForm[Any] =
			new ConstSQLWriteForm[T](value, Got(toString + ".notNull"))(form.notNull)

		override def equals(that :Any) :Boolean = that match {
			case const :ConstSQLWriteForm[_] =>
				(const eq this) || (const canEqual this) && const.value == value && const.form == form
			case _ => false
		}
		override def hashCode :Int = value.hashCode * 31 + form.hashCode

		private[schema] override lazy val cachedString = if (text.isDefined) "<" + text.get else s"$form=`$value`"
	}


	private[schema] class EvalSQLWriteForm[T](value: => Opt[T], protected override val text :Opt[String] = Lack)
	                                         (implicit protected override val form :SQLWriteForm[T])
		extends AbstractSQLWriteForm[Any](form.columnCount, text) with WriteFormAdapter[Any] //inherits only comparable
		   with IgnoringWriteForm
	{
		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			form.setOpt(statement, position, value)

		override def nullLiteral(inline :Boolean): String = value match {
			case Got(x) => form.literal(x, inline)
			case _ => form.nullLiteral(inline)
		}
		override def nullLiteral = nullLiteral(false)

		override def nullColumnLiterals :Seq[String] = value match {
			case Got(x) => form.columnLiterals(x)
			case _ => form.nullColumnLiterals
		}

		override def split :Seq[ColumnWriteForm[Any]] = form.split.map(ColumnWriteForm.evalOpt(value)(_))

		override def notNull :SQLWriteForm[Any] = new EvalSQLWriteForm(value, Got(toString + ".notNull"))(form.notNull)

		private[schema] override lazy val cachedString =
			if (text.isDefined) "<" + text.get else form.toString + "={@" + this.shortHashString + "}"
	}


	/** A form which always writes the value returned by its `NullValue` type class instead of the input. */
	private[schema] class NullValueSQLWriteForm[T](protected override val text :Opt[String] = Lack)
	                                              (implicit protected override val form :SQLWriteForm[T],
	                                               nulls :NullValue[T])
		extends AbstractSQLWriteForm[Any](form.columnCount, text) with WriteFormAdapter[Any]
		   with IgnoringWriteForm //with NullWriteFormLiterals[Any]
	{
		private def nullValue :NullValue[T] = nulls

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			form.set(statement, position, nulls.value)

		override def nullLiteral(inline :Boolean): String = form.literal(nulls.value, inline)
		override def nullLiteral = form.literal(nulls.value)
		override def nullColumnLiterals :Seq[String] = form.columnLiterals(nulls.value)

		override def split = form.split.map(ColumnWriteForm.nullValue(_, nulls))

		override def notNull :SQLWriteForm[Any] = new NullValueSQLWriteForm[T]()(form.notNull, nulls)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :NullValueSQLWriteForm[_] => form == other.form && nullValue == other.nullValue
			case _ => false
		}
		override def hashCode :Int = form.hashCode * 31 + nulls.hashCode

		private[schema] override lazy val cachedString =
			if (text.isDefined) "<" + text.get else form.toString + "=" + nulls
	}


	/** Writes only `null`s using the adapted form's `setNull` method. */
	private[schema] class NullifiedSQLWriteForm[T](protected override val text :Opt[String] = Lack)
	                                              (implicit protected override val form :SQLWriteForm[T])
		extends AbstractSQLWriteForm[Any](form.columnCount, text) with WriteFormAdapter[Any]
		   with IgnoringWriteForm //with NullWriteFormLiterals[Any]
	{ //it could conceivably be comparable to any other form of the same column count
		override def split :Seq[ColumnWriteForm[Any]] = form.split.map(ColumnWriteForm.none[T](_))
		override def notNull :SQLWriteForm[Any] = new NullifiedSQLWriteForm[T]()(form.notNull)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :NullifiedSQLWriteForm[_] => other.form == form
			case _ => false
		}
		override def hashCode :Int = form.hashCode

		private[schema] override lazy val cachedString =
			if (text.isDefined) "<" + text.get else form.toString + "=NULL"
	}


	private[schema] class GapSQLWriteForm(columns :Int, protected override val text :Opt[String] = Lack)
		extends AbstractSQLWriteForm[Any](columns, text) with WriteFormSeparateLiterals[Any] with IgnoringWriteForm
	{
		override def columnTypes :Seq[JDBCType] =
			throw new UnsupportedOperationException("Column types of form " + this + " are undefined.")

		override def setNull(statement :PreparedStatement, position :Int) :Unit = ()

		override val nullColumnLiterals: Seq[String] = ConstSeq("null", columnCount)

		override def split :Seq[ColumnWriteForm[Any]] = ConstSeq(ColumnWriteForm.gap, columns)
		override def nullSafe :SQLWriteForm[Any] = this
		override def notNull :SQLWriteForm[Any] = this

		override def comparable(other :SQLWriteForm[_]) :Boolean =
			this == other || columnCount ==  other.columnCount

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case gap :GapSQLWriteForm if gap canEqual this => columnCount == gap.columnCount
			case _ => false
		}
		override def hashCode :Int = columnCount.hashCode

		private[schema] override lazy val cachedString = if (text.isDefined) "<" + text.get else "<" + nullLiteral
	}


	/** Writes only explicit `null`s using `PreparedStatement`'s `setNull` method.
	  * implementation behind method `SQLWriteForm.nulls`.
	  */
	private[schema] class NullSQLWriteForm(columns :Int, protected override val text :Opt[String] = Lack)
		extends GapSQLWriteForm(columns, text) with IgnoringWriteForm
	{
		override def columnTypes = ConstSeq(JDBCType.NULL, columnCount)
		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			var i = position + columnCount - 1
			while (i >= 0) {
				statement.setNull(position, NULL.getVendorTypeNumber)
				i -= 1
			}
		}

		override def split :Seq[ColumnWriteForm[Any]] = ConstSeq(ColumnWriteForm.nulls, columns)

		override def comparable(other :SQLWriteForm[_]) :Boolean =
			this == other || columnCount == 0 && other.columnCount == 0

		private[schema] override lazy val cachedString :String =
			if (text.isDefined) "<" + text.get
			else if (columnCount == 1) "<NULL"
			else "<NULL" + columnCount
	}



	private[schema] class ErrorSQLWriteForm(columns :Int, private val error :NullValue[Nothing],
	                                        override val text :Opt[String] = Lack)
		extends AbstractSQLWriteForm[Any](columns, text) with IgnoringWriteForm with WriteFormLiterals[Any]
	{
		override def columnTypes :Seq[JDBCType] =
			throw new UnsupportedOperationException("Column types of form " + this + " are undefined.")

		override def setNull(statement :PreparedStatement, position :Int) :Unit = error.value

		override def nullColumnLiterals :Seq[String] = error.value

		override def split :Seq[ColumnWriteForm[Any]] =
			ConstSeq(ColumnWriteForm.error(JDBCType.OTHER, Lack, error), columns)

		override def notNull :SQLWriteForm[Any] = this

		override def comparable(other :SQLWriteForm[_]) :Boolean = this == other
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :ErrorSQLWriteForm => columnCount == other.columnCount && error == other.error
			case _ => false
		}
		override def hashCode :Int = columns

		private[schema] override lazy val cachedString =
			if (text.isDefined) "<" + text.get
			else if (columns == 1) "<ERROR@" + this.shortHashString
			else "<ERROR" + columns + "@" + this.shortHashString
	}




	/** A base trait for writing forms which split the value of `T` in its components and use multiple
	  * write forms in sequence to set the parameter(s) for those component values.
	  * Requires subclasses to define [[net.noresttherein.oldsql.schema.SQLWriteForm.CompositeWriteForm.forms forms]]
	  * property with a sequence of all component forms in the order in which the parameters should be set.
	  */
	trait CompositeWriteForm[-T] extends SQLWriteForm[T] with WriteFormNullLiterals[T] {
		protected def forms :Seq[SQLWriteForm[Nothing]]

		private val columns :Int = (0 /: forms)(_ + _.columnCount)
		private lazy val universal :Boolean = forms.forall(_.isUniversal)

		override lazy val columnTypes :Seq[JDBCType] = forms.flatMap(_.columnTypes)
		override def columnCount :Int = columns
		override def isUniversal :Boolean = universal

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			var i = position
			forms foreach { form => form.setNull(statement, i); i += form.columnCount }
		}

		override lazy val inlineNullLiteral :String =
			forms.view.filter(_.columnCount > 0).map(_.inlineNullLiteral).mkString(", ")
		override lazy val nullColumnLiterals: Seq[String] = forms.flatMap(_.nullColumnLiterals)

		override def comparable(other :SQLWriteForm[_]) :Boolean = other match {
			case _ if this eq other => true
			case other :CompositeWriteForm[_] =>
				columnCount == other.columnCount &&
					(forms.length == other.forms.length &&
						(forms.view zip other.forms).forall { case (l, r) => l comparable r }
						|| super.comparable(other)
					)
			case _ => super.comparable(other)
		}
	}



	private class SQLWriteFormList[-T](override val forms :Seq[SQLWriteForm[T]], override val text :Opt[String] = Lack)
		extends CompositeWriteForm[T]
	{
		override def set(statement :PreparedStatement, position :Int, value :T) :Unit = {
			var i = position
			forms foreach { form => form.set(statement, i, value); i += form.columnCount }
		}

		override def literal(value :T) :String =
			forms.view.filter(_.columnCount > 0).map(_.inlineLiteral(value)).mkString("(", ", ", ")")
		override def inlineLiteral(value :T) :String =
			forms.view.filter(_.columnCount > 0).map(_.inlineLiteral(value)).mkString(",")
		override def columnLiterals(value: T): Seq[String] =
			forms.flatMap(_.columnLiterals(value))

		override def split :Seq[ColumnWriteForm[T]] = forms.flatMap(_.split)

		override def notNull :SQLWriteForm[T] =
			new SQLWriteFormList[T](forms.map(_.notNull), Got(toString + ".notNull"))

		override def +[S <: T](next :SQLWriteForm[S]) :SQLWriteForm[S] = next match {
			case seq :SQLWriteFormList[S @unchecked] =>
				new SQLWriteFormList(forms ++: seq.forms)
			case _ =>
				new SQLWriteFormList(forms :+ next)
		}

		override def equals(that :Any) :Boolean = that match {
			case composite :SQLWriteFormList[_] =>
				(composite eq this) || (composite canEqual this) && composite.forms == forms
			case _ => false
		}
		override def hashCode :Int = forms.hashCode

		override lazy val toString :String =
			if (text.isDefined) "<" + text.get else forms.mkString("<(", "+", ")")
	}



	private final case class JoinedSQLWriteForm[-T](preceding :SQLWriteForm[T], last :SQLWriteForm[T],
	                                                protected override val text :Opt[String] = Lack)
		extends AbstractSQLWriteForm[T](preceding.columnCount + last.columnCount, text)
		   with WriteFormOptLiterals[T]
	{ //consider: reversing the order to a normal list
		if (preceding.columnCount == 0)
			throw new IllegalArgumentException(
				"Cannot append form " + last + " to an empty form " + preceding + "."
			)
		if (last.columnCount == 0)
			throw new IllegalArgumentException(
				"I don't want to append an empty form "  + last + " to " + preceding + "."
			)
		override lazy val columnTypes :Seq[JDBCType] = preceding.columnTypes :++ last.columnTypes

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit = {
			@tailrec def rec(form :SQLWriteForm[T]) :Unit = form match {
				case list :JoinedSQLWriteForm[T] =>
					val left = list.preceding
					list.last.set(statement, position + left.columnCount, value)
					rec(left)
				case _ => form.set(statement, position, value)
			}
			rec(this)
		}
		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			@tailrec def rec(form :SQLWriteForm[T]) :Unit = form match {
				case list :JoinedSQLWriteForm[T] =>
					val left = list.preceding
					list.last.setNull(statement, position + left.columnCount)
					rec(left)
				case _ => form.setNull(statement, position)
			}
			rec(this)
		}

		override def optLiteral(value :Opt[T], inline :Boolean) :String = {
			def rec(value :Opt[T], form :SQLWriteForm[T], res :java.lang.StringBuilder) :java.lang.StringBuilder =
				form match {
					case list :JoinedSQLWriteForm[T] =>
						rec(value, list.last, rec(value, list.preceding, res) append ", ")
					case other if value.isEmpty =>
						res append other.nullLiteral(true)
					case other =>
						res append other.literal(value.get, true)
				}
			val res = new java.lang.StringBuilder
			if (!inline)
				res append '('
			rec(value, this, res)
			if (!inline)
				res append ')'
			res.toString
		}

		override def optColumnLiterals(value :Opt[T]) :Seq[String] = {
			def rec(value :Opt[T], form :SQLWriteForm[T], res :Builder[String, Seq[String]]) :res.type =
				form match {
					case list :JoinedSQLWriteForm[T] =>
						rec(value, list.last, rec(value, list.preceding, res))
					case other if value.isEmpty => res ++= other.nullColumnLiterals
					case other => res ++= other.columnLiterals(value.get)
				}
			rec(value, this, ArraySeq.newBuilder[String]).result()
		}

		override def split :Seq[ColumnWriteForm[T]] = preceding.split :++ last.split

		override def notNull :SQLWriteForm[T] = {
			val l = preceding.notNull; val r = last.notNull
			if ((l eq preceding) && (r eq last)) this
			else new JoinedSQLWriteForm(l, r)
		}

		override def +[S <: T](next :SQLWriteForm[S]) =
			if (next.columnCount == 0) this else new JoinedSQLWriteForm(this, next)

		override def comparable(other :SQLWriteForm[_]) :Boolean = other match {
			case _ if this eq other => true
			case other :JoinedSQLWriteForm[_] =>
				columnCount == other.columnCount && (
					(last comparable other.last) && (preceding comparable other.preceding)
						|| super.comparable(other)
					)
			case _ => super.comparable(other)
		}

		private[schema] override lazy val cachedString :String =
			if (text.isDefined) "<" + text.get else preceding.toString + "::" + last
	}

}



