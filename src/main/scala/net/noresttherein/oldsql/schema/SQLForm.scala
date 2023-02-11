package net.noresttherein.oldsql.schema

import java.lang.Double.doubleToLongBits
import java.sql.{JDBCType, PreparedStatement, ResultSet}
import java.util.Optional

import scala.annotation.implicitNotFound
import scala.collection.Factory
import scala.reflect.{classTag, ClassTag}
import scala.util.Try

import net.noresttherein.oldsql.collection.{Opt, ReversedList}
import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{newThrowable, rethrow, NullValueException}
import net.noresttherein.oldsql.model.Kin
import net.noresttherein.oldsql.morsels.{Extractor, SpecializingFactory, Stateless}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.morsels.witness
import net.noresttherein.oldsql.pixies.RearrangedIndexing
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLForm.NullValue.{MappedNullEval, NotNull}
import net.noresttherein.oldsql.schema.SQLReadForm.{IgnoringReadForm, NamedReadForm, NotNullReadForm, ReadFormAdapter, ReadFormNullGuard, ReadFormNullValue, ReadFormProxy}
import net.noresttherein.oldsql.schema.SQLWriteForm.{EmptyWriteForm, NamedWriteForm, NonLiteralWriteForm, NotNullWriteForm, NotNullWriteFormProxy, NullSafeWriteFormProxy, WriteFormAdapter, WriteFormLiterals, WriteFormOptLiterals, WriteFormWithNull}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.forms.{CustomSQLForm, DerivedMappedSQLForm, DerivedOptMappedSQLForm, LazySQLForm, MappedSQLForm, OffsetSQLForm, OptMappedSQLForm, ReorderedForm, RepeatedSQLForm, SQLForms, SQLFormSeq}
import net.noresttherein.oldsql.schema.forms.UnspecifiedForm.BaseFormAdapter
import net.noresttherein.oldsql.slang.{castTypeParam, classNameMethods, localNameOf}
import net.noresttherein.oldsql.sql.ParamClause.NamedParamRelation






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
@implicitNotFound("I do not know how to map type ${T} into SQL type(s): missing implicit SQLForm[${T}].")
trait SQLForm[T] extends SQLReadForm[T] with SQLWriteForm[T] { outer =>

	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values. If the underlying
	  * column(s) is null, implicitly provided 'null' value for type `X` is returned directly from `opt`/`apply`
	  * reading methods without mapping the 'null' value of this type.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`. Will never be called
	  *            for `null` values unless this form returns `Some(null)` from `opt` in a non-standard practice.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement parameters.
	  */
	def bimap[X :NullValue](map :T => X)(unmap :X => T) :SQLForm[X] =
		SQLForm.map(map)(unmap)(this, NullValue[X])

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
	  */
	def optBimap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
		SQLForm.optMap(map)(unmap)(this, NullValue[X])

	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values. If the underlying
	  * column(s) is null, or `map` returns `None`, the `nullValue` provided  here is returned directly
	  * from `opt`/`apply` reading methods without mapping the 'null' value of this type. Similarly, if `unmap`
	  * returns `None`, the form will call on this instance `setNull` instead of `set`.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`. Will never be called
	  *            for `null` values unless this form returns `Some(null)` from `opt` in a non-standard practice.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement
	  *              parameters.
	  */
	def optBimap[X](map :T => Option[X], nullValue :X)(unmap :X => Option[T]) :SQLForm[X] =
		optBimap(map)(unmap)(NullValue(nullValue))

	/** Adapt this form to a new value type `X` by bidirectionally mapping read and written values.
	  * The `nullValue` of the new form is the result of mapping this instance's `nulls` with the given function,
	  * meaning it must handle `null` (or its counterpart for `T`) without throwing an exception. If `map` returns
	  * `None` for `this.nullValue`, a `NoSuchElementException` will be thrown when `nullValue` for the created form
	  * is accessed. Similarly, any exceptions thrown by the `map` function will be propagated.
	  * @param map a function mapping the result read from the `ResultSet` to the new type `X`.
	  * @param unmap a function mapping values of `X` for passing them to this form before setting the statement
	  *              parameters.
	  */
	def nullOptBimap[X](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
		optBimap(map)(unmap)(nulls.optMap(map))



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
	def as[X :NullValue](map :T =?> X)(unmap :X =?> T) :SQLForm[X] =
		SQLForm(map)(unmap)(this, NullValue[X])

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
		as(map)(unmap)(nulls.andThen(map))


	/** Lifts this form to represent `Option[T]`. The created form maps all values returned by this form using
	  * `Option(...)`. This means that `null` values (actual JVM nulls, not the somewhat arbitrary value provided
	  * by this form) are mapped to `Got(None)`, while a returned `Lack` indicates that this form's `opt`
	  * returned `Lack`. Basically this means that the returned form uses this form's `opt` method
	  * as its `apply` implementation.
	  */
	override def toOpt :SQLForm[Option[T]] = SQLForms.OptionForm(this)


	override def nullSafe :SQLForm[T] =
		new BaseFormAdapter[SQLForm[T]](this)
			with SQLForm[T] with ReadFormProxy[T] with NullSafeWriteFormProxy[T]

	/** An adapter or modification of this form which
	  * throws a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]] whenever it would output
	  * a `null`. This includes both setting statement parameters and reading values from a `ResultSet`.
	  */
	override def notNull :SQLForm[T] =
		new BaseFormAdapter[SQLForm[T]](this)
			with SQLForm[T] with ReadFormProxy[T] with NotNullWriteFormProxy[T]

	override def withNull(implicit nulls :NullValue[T]) :SQLForm[T] = {
		val nullVal = nulls
		new BaseFormAdapter(this) with SQLForm[T] with ReadFormProxy[T] with WriteFormWithNull[T] {
			override def nulls = nullVal
			override def nullValue = nullVal.value
			override def withNull(implicit nulls :NullValue[T]) :SQLForm[T] = form.withNull
		}
	}

	override def withNull(nullValue :T) :SQLForm[T] = withNull(NullValue(nullValue))

	/** An adapter form changing the order in which columns are read by this form.
	  * The `n`-th read column or set parameter translates to the `(start + inverse(n - 1))`-th column/parameter
	  * of the [[java.sql.ResultSet ResultSet]]/[[java.sql.PreparedStatement PreparedStatement]] passed
	  * as an argument to the returned proxy,
	  * with `inverse` being the inverse of `permutation`, i.e. `inverse(permutation(i)) == i`.
	  * `-1` modifier stems from the fact that [[java.sql.ResultSet ResultSet]] columns
	  * and [[java.sql.PreparedStatement PreparedStatement]] parameters are indexed starting with `1`.
	  * The index of the first column seen by this form is always mapped to `1`, that is reading/writing by this form
	  * starts from the beginning of the `ResultSet`/`PreparedStatement` wrapper given to it as an argument
	  * by the created proxy.
	  * @param permutation A permutation vector of length equal to the number of columns in this form,
	  *                    containing each value from range `[0,permutation.length)` exactly once.
	  *                    It is used as a function mapping the real indices of the `ResultSet`/`PreparedStatement`
	  *                    passed to the returned proxy to the indices seen by this form:
	  *                    {{{
	  *                        exposedIndex = permutation(underlyingIndex - start) + 1
	  *                    }}}
	  *                    where `start` is the index of the first read/written column given as an argument
	  *                    to [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]]/[[net.noresttherein.oldsql.schema.SQLWriteForm.set set]].
	  */
	override def reorder(permutation :IndexedSeq[Int]) :SQLForm[T] =
		ReorderedForm.shuffle(this, permutation)

	/** An adapter to this form which reads columns out of order, possibly ignoring some additional columns
	  * in the [[java.sql.ResultSet ResultSet]]. If the index translation
	  * is not a [[net.noresttherein.oldsql.pixies.RearrangedIndexing.isSurjection surjection]], then `null`
	  * will be returned for columns missing in the underlying `ResultSet` and no parameter will be set
	  * on a `PreparedStatement` for that column. If the index translation
	  * is not an [[net.noresttherein.oldsql.pixies.RearrangedIndexing.isInjection injection]], then extra intermittent
	  * columns in a `ResultSet` or parameters in a `PreparedStatement` will be ignored.
	  * @param order a mapping translating the indices of the argument `ResultSet` to the indices in the order
	  *              expected by this form and back.
	  *              Its [[net.noresttherein.oldsql.pixies.RearrangedIndexing.columnCount columnCount]] must equal
	  *              this form's [[net.noresttherein.oldsql.schema.SQLReadForm.columnCount columnCount]].
	  */
	override def reorder(order :RearrangedIndexing) :SQLForm[T] =
		ReorderedForm(this, order)

	/** An adapter to this form which adds `shift` to the read column indices and set parameter indices
	  * before delegating to this form.
	  */
	override def >>(shift :Int) :SQLForm[T] =
		if (shift == 0) this else new OffsetSQLForm(this, shift)

	/** An adapter to this form which subtracts `shift` to the read column indices and set parameter indices
	  * before delegating to this form.
	  */
	override def <<(shift :Int) :SQLForm[T] = this >> -shift


	/** Creates a form which will apply this form `repeat` number types, to consecutive columns of the `ResultSet`
	  * and statement parameters, mapping a sequence of individual elements.
	  * The number of elements in the read sequence will equal the number of times that method
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] of the form for the individual element type `T`
	  * would return a non-empty result.
	  * If the written sequence is shorter than `repeat`, for the remaining iterations this form's
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method is used instead of
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]. If the sequence is longer, an
	  * `IllegalArgumentException` is thrown.
	  */
	override def *(repeat :Int) :SQLForm[Seq[T]] = SQLForm.seq(repeat)(this)

	/** Combine this form with another form, to create one for the `(T, O)` pair. The columns of the form argument
	  * will directly follow the columns of this form.
	  */
	def *[O](other :SQLForm[O]) :SQLForm[(T, O)] = forms.ScalaForms.tuple2Form(this, other)

	override def <>[O >: T](write :SQLWriteForm[O]) :SQLForm[O] =
		if (write == writer || write == this) this.castParam[O]
		else SQLForm.combine[O](reader, write)

	override def <>[O <: T](read :SQLReadForm[O]) :SQLForm[O] =
		if (read == reader || read == this) this.castParam[O]
		else SQLForm.combine[O](read, writer)

	/** Converts this form to a write form. It defaults to `this`, but forms which are composed of reader and writer
	  * components can return the appropriate constituent removing a delegation layer.
	  */
	def writer :SQLWriteForm[T] = this

	/** Converts this form to a write form. It defaults to `this`, but forms which are composed of reader and writerer
	  * components can return the appropriate constituent removing a delegation layer.
	  */
	def reader :SQLReadForm[T] = this

//	/** An `SQLForm` is not universal if it does not depend on the argument when writing, or the `ResultSet` values
//	  * when reading, for at least some if its columns.
//	  */
//	override def isUniversal :Boolean = super.isUniversal

	/** Creates a synthetic [[net.noresttherein.oldsql.schema.Relation.StaticRelation StaticRelation]]
	  * for an unbound SQL [[net.noresttherein.oldsql.sql.ParamClause.NamedParamRelation parameter]]
	  * using the (left) argument `String` literal as relation the name, SQL statement parameter name
	  * and mapping label for access from [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]].
	  */
	def ?:[N <: Label](name :N) :NamedParamRelation[N, T] = NamedParamRelation[N, T](name)(this)

	/** Two `SQLForm` instances are comparable if they are comparable both as
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.comparable read]] forms
	  * and as [[net.noresttherein.oldsql.schema.SQLWriteForm.comparable write]] forms.
	  * The comparability in either aspect should in principle imply the comparability in the other,
	  * and, in most cases, being comparable in one aspect implies being comparable in the other one.
	  * However, some forms ignore their arguments/data in the result set in favor of some predefined values;
	  * these forms are considered comparable with any other form of the same kind (read/write/read+write) as long
	  * as their column counts are equal. When paired
	  * with a matching [[net.noresttherein.oldsql.schema.forms.UnspecifiedForm.isUniversal universal]] form
	  * in a [[net.noresttherein.oldsql.schema.SQLForm.combine combined]] `SQLForm`, both aspects need to be comparable.
	  *
	  * However, due to technical limitations, fully reliable implementation may be impossible and any of `comparable`
	  * method variants are allowed to return false negatives (but not false positives).
	  * The default implementation requires comparability in both directions, unless both `this` and `other`
	  * are universal (not ignoring 'wildcard' forms), in which case comparability in a single direction is sufficient.
	  * Subclasses are encouraged to override this method and its overloaded variants in order to recognize
	  * more specific cases.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.comparable]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.comparable]]
	  */
	def comparable(other :SQLForm[_]) :Boolean =
		if (isUniversal && other.isUniversal)
			comparable(other :SQLReadForm[_]) || comparable(other :SQLWriteForm[_])
		else
			comparable(other :SQLReadForm[_]) && comparable(other :SQLWriteForm[_])


}






/** A factory of [[net.noresttherein.oldsql.schema.SQLForm! SQLForm]] instances. */
object SQLForm {

	/** Summon an implicit instance of `SQLForm[T]`. */
	@inline def apply[T](implicit form :SQLForm[T]) :SQLForm[T] = form


	/** Creates an `SQLForm` delegating all calls to the implicitly provided read and write forms. */
	def combine[T :SQLReadForm :SQLWriteForm] :SQLForm[T] = SQLForm(SQLReadForm[T], SQLWriteForm[T])

	/** Creates an `SQLForm` delegating all calls to the implicitly provided read and write forms.
	  * Both forms must have the same column width, or an [[IllegalArgumentException]] is thrown.
	  * @param name the name of the created form, used as its identifier and returned by its `toString` method.
	  */ //consider: switching the order of parameters; it is chosen because of '<' and '>' prefix-suffix pair
	def combine[T :SQLReadForm :SQLWriteForm](name :String) :SQLForm[T] =
		SQLForm(name, SQLReadForm[T], SQLWriteForm[T])

	/** Creates an `SQLForm` delegating all calls to the provided read and write forms. */
	def apply[T](read :SQLReadForm[T], write :SQLWriteForm[T]) :SQLForm[T] = write match {
		case form :SQLForm[T @unchecked] if form == read => form
		case _ => new CombinedForms[T](read, write)
	}

	/** Creates an `SQLForm` delegating all calls to the provided read and write forms.
	  * Both forms must have the same column width, or an [[IllegalArgumentException]] is thrown.
	  * @param name the name of the created form, used as its identifier and returned by its `toString` method.
	  */ //todo: make name a separate parameter list
	def apply[T](name :String, read :SQLReadForm[T], write :SQLWriteForm[T]) :SQLForm[T] =
		new CombinedForms[T](read, write, Got(name)) with NamedForm[T]



	/** An `SQLForm` using the specified functions to read column values from a [[java.sql.ResultSet ResultSet]]
	  * and set parameters of a [[java.sql.PreparedStatement PreparedStatement]].
	  * Method [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]] method of the form is unused
	  * and throws a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  * Exceptions thrown by `reader` are not caught.
	  * Alternatively, [[net.noresttherein.oldsql.schema.SQLReadForm.notNull notNull]] method
	  * of the result may be used to obtain a form which rejects `null` values returned by the function
	  * with a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]]
	  * (and also returns `Lack` from its `opt` method).
	  *
	  * This form is not [[net.noresttherein.oldsql.schema.SQLWriteForm.split splittable]], which prevents
	  * its use in certain contexts, in particular column-wise comparisons of two SQL
	  * [[net.noresttherein.oldsql.sql.SQLExpression expressions]].
	  * @param columns the number of columns read by the `reader` function and set parameters by `writer` function.
	  * @param writer  a function accepting a statement and parameter position and setting the following `columns`
	  *                parameters. Null arguments are passed to it without diverting them to the form's
	  *                [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method,
	  *                which directly sets `columns` consecutive parameters to `null` using the JDBC API.
	  * @param reader  a function, accepting a result set and a column index, and returning a value of `T` constructed
	  *                from the following `columns` column values. It must handle null columns in the `ResultSet`
	  *                itself: the form's `nullValue` method is not used in this case.
	  */ //todo: in Scala3 verify that type inference correctly guesses type T for writer based on reader
	def apply[T](columns :Int)(reader :(ResultSet, Int) => T)(writer :(PreparedStatement, Int, T) => Unit) :SQLForm[T] =
		new CustomSQLForm[T](columns)(reader, writer)
//		combine(SQLReadForm(columns)(reader), SQLWriteForm(columns)(writer))

	/** An `SQLForm` using the specified functions to read column values from a [[java.sql.ResultSet ResultSet]]
	  * and set parameters of a [[java.sql.PreparedStatement PreparedStatement]]. It is equivalent to using
	  * a pair of independent [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]] forms.
	  *
	  * This form is not [[net.noresttherein.oldsql.schema.SQLWriteForm.split splittable]], which prevents
	  * its use in certain contexts, in particular column-wise comparisons of two SQL
	  * [[net.noresttherein.oldsql.sql.SQLExpression expressions]].
	  * @param name    a name for this form, used as its identifier and by its `toString` method.
	  * @param columns the number of columns read by the `reader` function and set parameters by `writer` function.
	  * @param writer  a function accepting a statement and parameter position and setting the following `columns`
	  *                parameters. Null arguments are passed to it without diverting them to the form's
	  *                [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method,
	  *                which directly sets `columns` consecutive parameters to `null` using the JDBC API.
	  * @param reader  a function, accepting a result set and a column index, and returning a value of `T` constructed
	  *                from the following `columns` column values. It must handle null columns in the `ResultSet`
	  *                itself: the form's `nullValue` method is not used in this case.
	  */
	def apply[T](name :String, columns :Int)
	            (reader :(ResultSet, Int) => T)(writer :(PreparedStatement, Int, T) => Unit) :SQLForm[T] =
		new CustomSQLForm[T](columns, Got(name))(reader, writer) with NamedForm[T]
//		combine(name)(SQLReadForm(name, columns)(reader), SQLWriteForm(name, columns)(writer))



	/** Creates a new `SQLForm` for type `T` based on an implicit form for type `S` and an optional `NullValue[T]`.
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue one present in the source form]] will be mapped
	  * with `map`.
	  */
	def apply[S :SQLForm, T :NullValue.Maybe](map :S =?> T)(unmap :T =?> S) :SQLForm[T] =
		(map, unmap) match {
			case (_ :IdentityExtractor[_], _:IdentityExtractor[_]) => SQLForm[S].asInstanceOf[SQLForm[T]]
			case (Extractor.Requisite(there), Extractor.Requisite(back)) => SQLForm.map(there)(back)
			case _ => optMap(map.optional)(unmap.optional)
		}

	/** Creates a new `SQLForm[T]` of 'soft type' `name`, based on an implicit form for type `S`
	  * and an optional `NullValue[T]`. This is equivalent in function to
	  * `SQLForm`[[net.noresttherein.oldsql.schema.SQLForm.apply[S,T](map* (map)(unmap)]],
	  * but the created form will equal any other `SQLForm` created by this method if they have the same name
	  * and their underlying forms and [[net.noresttherein.oldsql.schema.SQLReadForm.nulls null values]] are equal.
	  *
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue one present in the source form]] will be mapped
	  * with `map`.
	  */
	def apply[S :SQLForm, T :NullValue.Maybe](name :String)(map :S =?> T)(unmap :T =?> S) :SQLForm[T] =
		(map, unmap) match {
			case (Extractor.Requisite(there), Extractor.Requisite(back)) => SQLForm.map(name)(there)(back)
			case _ => optMap(name)(map.optional)(unmap.optional)
		}


	/** Creates a new `SQLForm` for type `T` based on an implicit form for type `S` and an optional `NullValue[T]`.
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue present in the source form]] will be mapped
	  * with `map`.
	  */
	def map[S :SQLForm, T :NullValue.Maybe](map :S => T)(unmap :T => S) :SQLForm[T] = {
		implicit val nullValue = NullValue.orElse(SQLForm[S].nulls.map(map))
		new MappedSQLForm[S, T](map, unmap)
	}

	/** Creates a new `SQLForm[T]` of 'soft type' `name`, based on an implicit form for type `S`
	  * and an optional `NullValue[T]`. This is equivalent in function to
	  * `SQLForm`[[net.noresttherein.oldsql.schema.SQLForm.map[S,T](map:S=>T)* .map(map)(unmap)]],
	  * but the created form will equal any other `SQLForm` created by this method if they have the same name
	  * and their underlying forms and [[net.noresttherein.oldsql.schema.SQLReadForm.nulls null values]] are equal.
	  *
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue one present in the source form]] will be mapped
	  * with `map`.
	  */
	def map[S :SQLForm, T :NullValue.Maybe](name :String)(map :S => T)(unmap :T => S) :SQLForm[T] = {
		implicit val nullValue = NullValue.orElse(SQLForm[S].nulls.map(map))
		new DerivedMappedSQLForm[S, T](name, map, unmap)
	}


	/** Creates a new `SQLForm` for type `T` based on an implicit form for type `S` and an optional `NullValue[T]`.
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue one present in the source form]] will be mapped
	  * with `map`.
	  */
	def optMap[S :SQLForm, T :NullValue.Maybe](map :S => Option[T])(unmap :T => Option[S]) :SQLForm[T] = {
		implicit val nullValue = NullValue.orElse(SQLForm[S].nulls.optMap(map))
		new OptMappedSQLForm[S, T](map, unmap)(SQLForm[S], nullValue)
	}

	/** Creates a new `SQLForm[T]` of 'soft type' `name`, based on an implicit form for type `S`
	  * and an optional `NullValue[T]`. This is equivalent in function to
	  * `SQLForm`[[net.noresttherein.oldsql.schema.SQLForm.optMap[S,T](map:S=>Option[T])(unmap :T=>Option[S])* .optMap(map)(unmap)]],
	  * but the created form will equal any other `SQLForm` created by this method if they have the same name
	  * and their underlying forms and [[net.noresttherein.oldsql.schema.SQLReadForm.nulls null values]] are equal.
	  *
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue one present in the source form]] will be mapped
	  * with `optMap`.
	  */
	def optMap[S :SQLForm, T :NullValue.Maybe](name :String)(map :S => Option[T])(unmap :T => Option[S]) :SQLForm[T] = {
		implicit val nullValue = NullValue.orElse(SQLForm[S].nulls.optMap(map))
		new DerivedOptMappedSQLForm[S, T](name, map, unmap)
	}



	/** Creates a form which will apply form `SQLForm[T]` `repeat` number types, to consecutive columns of
	  * the `ResultSet` and statement parameters, mapping a sequence of individual elements.
	  * The number of elements in the read sequence will equal the number of times that method
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] of the form for the individual element type `T`
	  * would return a non-empty result.
	  * If the written sequence is shorter than `repeat`, for the remaining iterations this form's
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] method is used instead of
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]. If the sequence is longer, an
	  * `IllegalArgumentException` is thrown.
	  */
	def seq[T :SQLForm](repeats :Int) :SQLForm[Seq[T]] = new RepeatedSQLForm[T](repeats)

	/** A form mapping sequences of constant length, each element with the corresponding form given as the argument.
	  * @see [[[net.noresttherein.oldsql.schema.SQLReadForm$.seq[T](items:Seq[SQLReadForm[T]])]]]
	  * @see [[[net.noresttherein.oldsql.schema.SQLWriteForm$.seq[T](items:Seq[SQLWriteForm[T]])]]]
	  */
	def seq[T](items :Seq[SQLForm[T]]) :SQLForm[Seq[T]] =
		new SQLFormSeq(items)

	/** A conditional form, choosing one of the listed forms to read/write mapped type `T` based on the value of/for
	  * initial columns handled by an implicit `discriminator` form `SQLForm[K]`.
	  * When writing, the form first extracts the discriminator value `K` using `discriminator`, picks a form
	  * in the `cases` list associated with that value, and uses it to set parameters following the parameters
	  * set by `discriminator` (i.e., the index of the first parameter is increased by
	  * `discriminator.`[[net.noresttherein.oldsql.schema.SQLWriteForm.columnCount columnCount]]).
	  * When reading, the situation is analogous: `discriminator` is used to read a value of `K` from the `ResultSet`
	  * at the column index provided to this form, picks a form from the `cases` list associated with that value,
	  * and reads the following columns with the selected form.
	  *
	  * If no form matching a given discriminator value exists, the created form behaves
	  * as if there was no value in the `ResultSet` / was instructed to set the parameters to a 'null value'.
	  * The first form of the list is used for the appropriate
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] instance in case an implicit value is missing,
	  * and as the delegate target to method
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  *
	  * All forms must have the same column width.
	  * @param choose a function extracting the discriminator value from the mapped type `T`.
	  * @param cases  a list of pairs (''discriminator-value'', ''specific-form''), which associates
	  *               discriminator values identifying specific subtypes of `T` with forms specific to those subtypes.
	  */
	def when[K :SQLForm, T :NullValue.Maybe](choose :T => K)(cases :(K, SQLForm[T])*) :SQLForm[T] =
		when(choose, Map.from(cases))

	/** A conditional form, choosing one of the listed forms to read/write mapped type `T` based on the value of/for
	  * initial columns handled by an implicit `discriminator` form `SQLForm[K]`.
	  * When writing, the form first extracts the discriminator value `K` using `discriminator`, picks a form
	  * in the `cases` map associated with that value, and uses it to set parameters following the parameters
	  * set by `discriminator` (i.e., the index of the first parameter is increased by
	  * `discriminator.`[[net.noresttherein.oldsql.schema.SQLWriteForm.columnCount columnCount]]).
	  * When reading, the situation is analogous: `discriminator` is used to read a value of `K` from the `ResultSet`
	  * at the column index provided to this form, picks a form from the `cases` list associated with that value,
	  * and reads the following columns with the selected form.
	  *
	  * If no form matching a given discriminator value exists, the created form behaves
	  * as if there was no value in the `ResultSet` / was instructed to set the parameters to a 'null value'.
	  * The `head` form of the map is used for the appropriate
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] instance (if an implicit one is missing)
	  * and as the delegate target to method [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  *
	  * Note that `Map` methods [[scala.collection.immutable.Map.withDefault withDefault]]
	  * and [[scala.collection.immutable.Map.withDefaultValue withDefaultValue]] can be used to cover cases
	  * not handled specifically by any individual form.
	  *
	  * All forms must have the same column width.
	  * @param choose a function extracting the discriminator value from the mapped type `T`.
	  * @param cases  a `Map` associating discriminator values identifying specific subtypes of `T`
	  *               with forms specific to those subtypes.
	  */
	def when[K :SQLForm, T :NullValue.Maybe](choose :T => K, cases :Map[K, SQLForm[T]]) :SQLForm[T] =
		combine(SQLReadForm.when(cases), SQLWriteForm.when(choose, cases))



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
	def delayed[T](init: => SQLForm[T]) :SQLForm[T] = new LazySQLForm[T](() => init)



	/** Creates a dummy form which always returns and writes the same value. The width of the form in columns
	  * is specified by the implicit `SQLWriteForm[T]` used by the form to set the parameters. If `value` is `null`,
	  * it is used as any other value, not a lack of value, and not substituted for the custom
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue null value]] of the implicit `SQLReadForm`.
	  * @param value the value of set parameters and returned instead of reading from a [[java.sql.ResultSet ResultSet]].
	  * @param text  an optional textual representation of the forrm, used by its `toString` method.
	  */
	def const[T :SQLWriteForm](value :T, text :String = null) :SQLForm[T] =
		combine(SQLReadForm.const(SQLWriteForm[T].columnCount, text)(value), SQLWriteForm.const(value, text))

	/** Creates a dummy form which always returns and writes the same value. The width of the form in columns
	  * is specified by the implicit `SQLWriteForm[T]` used by the form to set the parameters. If `value` is `null`,
	  * it is used as any other value, not a lack of value, and not substituted for the custom
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue null value]] of the implicit `SQLReadForm`.
	  * @param name  a name for this form, used as its identifier: two forms created by this method which share
	  *              the same name are equal.
	  * @param value the value of set parameters and returned instead of reading from a [[java.sql.ResultSet ResultSet]].
	  */
	def const[T :SQLWriteForm](name :String)(value :T) :SQLForm[T] =
		combine(name)(SQLReadForm.const(SQLWriteForm[T].columnCount)(value), SQLWriteForm.const(value))

	/** Creates a dummy form which always returns and writes the same value. The width of the form in columns
	  * is specified by the implicit `SQLWriteForm[T]` used by the form to set the parameters.
	  * @param value the value used as the argument to [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]]
	  *              and returned from its [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method.
	  * @param text  an optional textual representation of the form, used by its `toString` method.
	  */
	def constOpt[T :SQLWriteForm :NullValue](value :Opt[T], text :String = null) :SQLForm[T] =
		combine(SQLReadForm.constOpt(SQLWriteForm[T].columnCount, text)(value), SQLWriteForm.constOpt(value, text))

	/** Creates a dummy form which always returns and writes the same value. The width of the form in columns
	  * is specified by the implicit `SQLWriteForm[T]` used by the form to set the parameters.
	  * @param name  a name for this form, used as its identifier: two forms created by this method which share
	  *              the same name are equal.
	  * @param value the value used as the argument to [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]]
	  *              and returned from its [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method.
	  */
	def constOpt[T :SQLWriteForm :NullValue](name :String)(value :Opt[T]) :SQLForm[T] =
		combine(name)(SQLReadForm.constOpt(name)(SQLWriteForm[T].columnCount)(value), SQLWriteForm.constOpt(name)(value))


	/** Creates a dummy form which ignores its input and always reads and writes the value of the ''by-name'' argument
	  * `value`. The column width o the form is the same as the width of the implicit `SQLWriteForm[T]` used to set
	  * statement parameters. If the expression evaluates to `null`, it is used as any other value, rather than
	  * a missing value, and not substituted for the 'null value' defined by type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] of the implicit `SQLReadForm`.
	  * @param value the value of set parameters and returned instead of reading from a [[java.sql.ResultSet ResultSet]].
	  * @param text  an optional textual representation of the form, used by its `toString` method.
	  */
	def eval[T :SQLWriteForm](value: => T, text :String = null) :SQLForm[T] =
		combine(SQLReadForm.eval(SQLWriteForm[T].columnCount, text)(value), SQLWriteForm.eval(value, text))

	/** Creates a dummy form which ignores its input and always reads and writes the value of the ''by-name'' argument
	  * `value`. The column width o the form is the same as the width of the implicit `SQLWriteForm[T]` used to set
	  * statement parameters. If the expression evaluates to `null`, it is used as any other value, rather than
	  * a missing value, and not substituted for the 'null value' defined by type class
	  * @param name  a name for this form, used as its identifier: two forms created by this method which share
	  *              the same name are equal.
	  * @param value the value of set parameters and returned instead of reading from a [[java.sql.ResultSet ResultSet]].
	  */
	def eval[T :SQLWriteForm](name :String)(value: => T) :SQLForm[T] =
		combine(name)(SQLReadForm.eval(name)(SQLWriteForm[T].columnCount)(value), SQLWriteForm.eval(name)(value))

	/** Creates a dummy form which ignores its input and always reads and writes the value of the ''by-name'' argument
	  * `value`. The column width o the form is the same as the width of the implicit `SQLWriteForm[T]` used to set
	  * statement parameters.
	  * @param value the expression used as the argument to [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]]
	  *              and returned from its [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method.
	  * @param text  an optional textual representation of the form, used by its `toString` method.
	  */
	def evalOpt[T :SQLWriteForm :NullValue](value: => Opt[T], text :String = null) :SQLForm[T] =
		combine(SQLReadForm.evalOpt(SQLWriteForm[T].columnCount, text)(value), SQLWriteForm.evalOpt[T](value, text))

	/** Creates a dummy form which ignores its input and always reads and writes the value of the ''by-name'' argument
	  * `value`. The column width o the form is the same as the width of the implicit `SQLWriteForm[T]` used to set
	  * statement parameters.
	  * @param name  a name for this form, used as its identifier: two forms created by this method which share
	  *              the same name are equal.
	  * @param value the value used as the argument to [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]]
	  *              and returned from its [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method.
	  */
	def evalOpt[T :SQLWriteForm :NullValue](name :String)(value: => Opt[T]) :SQLForm[T] =
		combine(name)(SQLReadForm.evalOpt(name)(SQLWriteForm[T].columnCount)(value), SQLWriteForm.evalOpt[T](name)(value))


	/** Creates a form which always returns and writes the value provided by the implicit type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` instead of the actual values
	  * in the `ResultSet` or the arguments.
	  * The type class is used as [[net.noresttherein.oldsql.schema.SQLReadForm!.nulls nulls]] property of the form,
	  * with its [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]] and, by extension,
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] method returning `nulls.value`.
	  * The result of [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] is defined as `Got(value)`.
	  * Similarly, its [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method delegates to the implicit
	  * form's set, passing as the argument `nulls.`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.value value]]
	  * instead of its argument.
	  *
	  * The form is functionally equivalent to the one created by
	  * `SQLReadForm.`[[net.noresttherein.oldsql.schema.SQLReadForm.eval eval]], but implements structural equality.
	  * The difference from similar [[net.noresttherein.oldsql.schema.SQLReadForm.defaults defaults]]
	  * is that the latter's `opt` method returns `Lack` rather than the 'null' value.
	  */
	def nullValue[T :SQLWriteForm :NullValue] :SQLForm[T] =
		combine(SQLReadForm.nullValue[T](SQLWriteForm[T].columnCount), SQLWriteForm.nullValue[T])

	/** Creates a form which always returns and writes the value provided by the implicit type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` instead of the actual values
	  * in the `ResultSet` or the arguments.
	  * The type class is used as [[net.noresttherein.oldsql.schema.SQLReadForm!.nulls nulls]] property of the form,
	  * with its [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]] and, by extension,
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] method returning `nulls.value`.
	  * The result of [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] is defined as `Got(value)`.
	  * Similarly, its [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method delegates to the implicit
	  * form's set, passing as the argument `nulls.`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.value value]]
	  * instead of its argument.
	  *
	  * The form is functionally equivalent to the one created by
	  * `SQLReadForm.`[[net.noresttherein.oldsql.schema.SQLReadForm.eval eval]], but implements structural equality.
	  * The difference from similar [[net.noresttherein.oldsql.schema.SQLReadForm.defaults defaults]]
	  * is that the latter's `opt` method returns `Lack` rather than the 'null' value.
	  * @param name  a name for this form, used as its identifier: two forms created by this method which share
	  *              the same name are equal.
	  */
	def nullValue[T :SQLWriteForm :NullValue](name :String) :SQLForm[T] =
		combine(name)(SQLReadForm.nullValue[T](name)(SQLWriteForm[T].columnCount), SQLWriteForm.nullValue[T](name))


	/** Creates a dummy form which always writes the null value as defined by the implicit type class `NullValue`,
	  * and returns `Lack`/`nulls.value` when reading.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.defaults]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.nullValue]]
	  */
	def defaults[T :SQLWriteForm :NullValue] :SQLForm[T] =
		combine(SQLReadForm.defaults[T](SQLWriteForm[T].columnCount), SQLWriteForm.nullValue[T])

	/** Creates a dummy form which always writes the null value as defined by the implicit `NullValue` type class,
	  * and returns `Lack`/`nulls.value` when reading.
	  * @param name  a name for this form, used as its identifier: two forms created by this method which share
	  *              the same name are equal.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.defaults]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.nullValue]]
	  */
	def defaults[T :SQLWriteForm :NullValue](name :String) :SQLForm[T] =
		combine(name)(SQLReadForm.defaults[T](name)(SQLWriteForm[T].columnCount), SQLWriteForm.nullValue[T](name))


	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
	  * and returns `Lack` when reading. All calls to `apply` will result
	  * in a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  */
	def none[T :SQLWriteForm] :SQLForm[T] =
		combine(SQLReadForm.none[T](SQLWriteForm[T].columnCount), SQLWriteForm.none[T])
//
//	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
//	  * and returns `Lack` when reading. All calls to `apply` will result
//	  * in a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
//	  */
//	def none[T](name :String)(implicit write :SQLWriteForm[T]) :SQLForm[T] =
//		combine(name)(SQLWriteForm.none[T], SQLReadForm.none[T](write.columnCount))

	/** Creates a dummy form which always reads and writes `null` values, except from its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] method, which returns `Lack`.
	  */
	def nulls[T >: Null](columns :Int) :SQLForm[T] =
		combine(SQLReadForm.nulls(columns), SQLWriteForm.nulls(columns))


	private val notNull = new EmptySQLForm[Nothing]()(NotNull)

	/** An empty (zero-width) whose methods [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]],
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.register register]],
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]],
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] are no-ops.
	  * Its [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]] is
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]] and throws
	  * a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]] on each attempt to read
	  * with method [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]].
	  *
	  * All SQL literals of empty forms are empty strings, and all empty forms are comparable with each other.
	  * All instances returned by this method are equal.
	  */
	def empty[T] :SQLForm[T] = notNull.asInstanceOf[SQLForm[T]]

//	def empty[T](name :String) :SQLForm[T] =
//		new EmptySQLForm[T](Got(name))(NotNull) with NamedForm[T]

	/** A form of zero columns, which does not read or write any columns but always returns the given constant `value`
	  * from [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]],
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]]
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]] methods.
	  * All write attempts set no parameters and all its literals are empty strings.
	  * It is equal to other empty forms for the same value (implements structural equality).
	  */
	def empty[T](value :T) :SQLForm[T] = new EmptySQLForm[T]()(NullValue(value))
//
//	/** A form of zero columns, which does not read or write any columns but always returns the given constant `value`
//	  * from [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]],
//	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]]
//	  * and [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]] methods.
//	  * All write attempts set no parameters and all its literals are empty strings.
//	  * All forms created by this method which share the same name are equal.
//	  */
//	def empty[T](name :String, value :T) :SQLForm[T] =
//		new EmptySQLForm[T](Got(name))(NullValue(value)) with NamedForm[T]

	/** A form of zero columns, which does not read or write any columns but always returns the result of
	  * reevaluating by-name expression `value`
	  * from [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]],
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]]
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]] methods.
	  * All write attempts set no parameters and all its literals are empty strings.
	  *
	  * All forms created by this method with the same name are equal, and comparable to all forms created
	  * by any other `empty` method.
	  */
	def empty[T](name :String)(value: => T) :SQLForm[T] =
		new EmptySQLForm[T](Got(name))(NullValue.eval(name, value)) with NamedForm[T]



	/** A form which throws an exception o the specified class on the invocation of any of its methods.
	  * This applies not only to methods [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]], but also
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]]. The form will be equal to other forms created
	  * by this method as long as their column counts and the class of the thrown exception are the same.
	  * @tparam E the class of the thrown exception, which specifies at least one of the following constructors:
	  *           `()`, `(String)`, `(Throwable)`, `(String, Throwable)`.
	  * @param columns the width of this form in columns (the number of 'read' and 'written' columns)
	  *                used to position it among other forms.
	  */
	def error[E <: Throwable :ClassTag, T](columns :Int) :SQLForm[T] = {
		val nulls = NullValue.error[E]
		val name = "ERROR" + columns + "[" + localNameOf[E] + "]"
		val read = SQLReadForm.nullValue[T](columns, name)(nulls)
		val write = SQLWriteForm.error(columns, Got(name), nulls)
		new CombinedForms[T](read, write, Got(name))
	}

	/** A form which throws an exception using the given by-name expression on the invocation of any of its methods.
	  * This applies not only to methods [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]], but also
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  * @param columns the width of this form in columns (the number of 'read' and 'written' columns)
	  *                used to position it among other forms.
	  * @param text    an optional textual representation of this form for its `toString` method.
	  */
	def error[T](columns :Int, text :String = null)(raise: => Nothing) :SQLForm[T] = {
		val name = if (text != null) text else "ERROR" + columns + "@" + doubleToLongBits(math.random()).shortHashString
		val write = SQLWriteForm.error(columns, name)(raise)
		val read = SQLReadForm.error(columns, name)(raise)
		new CombinedForms[T](read, write, Got(name))
	}

	/** A form which throws an exception using the given by-name expression on the invocation of any of its methods.
	  * This applies not only to methods [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]], but also
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  * The form will equal other forms created by this method of the same name.
	  * @param name    the name of this form, used as its identifier and in its textual `toString` representation.
	  * @param columns the width of this form in columns (the number of 'read' and 'written' columns)
	  *                used to position it among other forms.
	  */
	def error[T](name :String)(columns :Int)(raise: => Nothing) :SQLForm[T] =
		combine(name)(SQLReadForm.error(name)(columns)(raise), SQLWriteForm.error(name)(columns)(raise))






	/** A type class providing a 'null' value for type `T`. Its primary purpose is to define
	  * how null column values read from a `ResultSet` should be represented as values of type `T`.
	  * This may be any default value, not only `null` or 'zero'; for example, a collection type can return
	  * an empty collection. As a less obvious example, primary key type [[net.noresttherein.oldsql.model.PK PK]]
	  * represents the 'no value' state, which is treated as the 'null case' by forms and components,
	  * as [[net.noresttherein.oldsql.model.PK$.TransientPK transient]] subtype instances.
	  * The reverse is also possible, although less common: saving `null` references as non-null values in the database.
	  * The concept of nullity is extended also to multi column types and, in particular, a `null` Scala
	  * component - or, more often, `None`, which is the default `NullValue[Option[X]]` - can translate to multiple
	  * null columns, including when used as SQL literals or in the ''select'' clause. Last but not least,
	  * it can also be used to enforce not-null semantics by throwing an exception as in `NullValue.NotNull`.
	  * Ultimately, the exact semantics of handling `null` values - both in Scala and SQL - will depend on the form.
	  *
	  * `NullValue` comes to play primarily in [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]] method
	  * of [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]], returned when
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] would return `Lack`; it is not used by the latter,
	  * so `NullValue` should not be used to provide business logic defaults, as this would result in a discrepancy
	  * between the two methods. This use case can be implemented instead in
	  * [[net.noresttherein.oldsql.schema.Mapping Mapping]], either manually, or by the use of
	  * [[net.noresttherein.oldsql.schema.Buff.SelectDefault SelectDefault]] buff.
	  *
	  * With very few exceptions (mentioned `Option` case is one), this type class doesn't have any implicit definitions
	  * for the base types (not derived from some other type class instance). This is to allow the applications
	  * to make conscious decisions about whether they want to deal with `null` values and how. The default approach,
	  * is to treat all types as non-nullable; this is reflected in how many
	  * [[net.noresttherein.oldsql.schema.SQLReadForm form]] factory methods use
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]] as their default `null`
	  * representation, which throws [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]]
	  * (a subclass of [[NullPointerException]].
	  *
	  * Note that this is a ''SAM'' type, requiring only [[net.noresttherein.oldsql.schema.SQLForm.NullValue.value value]]
	  * method to be implemented, and as such any literal `Function0[T]` expression `() => T` can be converted
	  * to a `NullValue` implementation when used as an argument to a method expecting `NullValue[T]`.
	  * All implementaations must be thread safe.
	  */ //todo: move to schema; Mapping uses it, too
	@implicitNotFound("I do not know how to handle null values for ${T}: missing implicit NullValue[${T}].\n" +
	                  "You can use one of the values provided in the companion object for a syntactically scoped " +
	                  "implicit definition or import the default values from SQLForm.NotNull.Defaults.")
	trait NullValue[+T] extends Serializable {

		/** Value to which null columns in the `ResultSet` are mapped.
		  * @throws NullValueException if the particular form for type `T` prohibits null values.
		  * @throws NoSuchElementException if this is [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NoSuchElement NoSuchElement]].
		  */
		def value :T

		/** Value to which null columns in the `ResultSet` are mapped.
		  * This variant should be overloaded by implementations which throw an exception from
		  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.value value]] in order to use the given message string
		  * in the exception for better debugging.
		  * @throws NullValueException if the particular form for type `T` prohibits null values.
		  * @throws NoSuchElementException if this is [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NoSuchElement NoSuchElement]].
		  */
		def value(errorMessage :String) :T = value

		/** Returns `this.`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.value value]]
		  * if all calls would return the same value and would not throw an exception.
		  * @see [[net.noresttherein.oldsql.schema.SQLForm.NullValue.option]]
		  */
		def toOption :Option[T] = toOpt

		/** Returns `this.value` inside a `Some` unless it would throw an exception, in which case `None` is returned.
		  * This call may produce different values with different calls.
		  * @see [[net.noresttherein.oldsql.schema.SQLForm.NullValue.toOption]]
		  */
		def option :Option[T] = opt //try { Some(value) } catch { case _ :Exception => None }

		/** Returns `this.`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.value value]]
		  * if all calls would return the same value and would not throw an exception.
		  * @see [[net.noresttherein.oldsql.schema.SQLForm.NullValue.opt]]
		  */
		def toOpt :Opt[T] = Opt.Lack

		/** Returns `this.value` inside a `Got` unless it would throw an exception, in which case `Lack` is returned.
		  * This call may produce different values with different calls.
		  * @see [[net.noresttherein.oldsql.schema.SQLForm.NullValue.toOpt]]
		  */
		def opt :Opt[T] = try { Got(value) } catch { case _ :Exception => Opt.Lack }

		/** Adapt this null value to some other type `U`. In most cases, this will simply apply the function to
		  * the wrapped value, but special instances may propagate themselves instead. If the function throws
		  * an exception for the `value`, it will be swallowed and a `NullValue` instance which reevaluates
		  * `map(this.value)` at each access will be returned.
		  */
		def map[U](f :T => U) :NullValue[U] = new MappedNullEval(this, f)

		/** Adapt this null value to some other type `U`. In most cases, this will simply apply the function to
		  * the wrapped value, but special instances may propagate themselves instead. If the function throws
		  * an exception for the `value`, it will be swallowed and a `NullValue` instance which reevaluates
		  * `map(this.value)` at each access will be returned. Returning `None` by the function has the same
		  * effect as throwing a [[NoSuchElementException]], which will be thrown by the returned `NullValue` instead
		  * of letting it out of this method.
		  */
		def optMap[U](f :T => Option[U]) :NullValue[U] =
			map(tnull => f(tnull) getOrElse { //consider: shouldn't it throw NullValueException?
			 	throw new NoSuchElementException("No corresponding null value for " + tnull + " of " + this + ".")
			})

		/** Adapt this null value to some other type `U`. In most cases, this will simply apply the function to
		  * the wrapped value, but special instances may propagate themselves instead. If the function throws
		  * an exception for the `value`, it will be swallowed and a `NullValue` instance which reevaluates
		  * `map(this.value)` at each access will be returned. Returning `Lack` by the function has the same
		  * effect as throwing a [[NoSuchElementException]], which will be throw by the returned `NullValue` instead
		  * of letting it out of this method.
		  */
		def andThen[U](f :T =?> U) :NullValue[U] = f match {
			case _ :EmptyExtractor[_, _] => NullValue.NotNull
			case _ :OptionalExtractor[_, _] => optMap(f.optional)
			case _ :IdentityExtractor[_] => this.asInstanceOf[NullValue[U]]
			case const :ConstantExtractor[_, U @unchecked] => NullValue(const.constant)
			case _ :RequisiteExtractor[_, _] => map(f.requisite.get)
			case _ => optMap(f.optional)
		}

		def canEqual(that :Any) :Boolean = that.isInstanceOf[NullValue[_]]
	}



	/** Factory of [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` type class, providing values
	  * representing `null` in the underlying column(s). No default implicit values are defined in the implicit search
	  * scope except of `None` for `Option` types, but implicit values wrapping default values of all standard value
	  * types (as well as `null` for reference types) are defined in the member object
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.Defaults Defaults]]. This is to avoid accidental conversion
	  * from a database `null` to a zero value, which can easily be made persistent by a future update.
	  * You can import `NullValue.Defaults._` if you wish them brought to scope, but the recommended default approach
	  * is to use [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]], which always
	  * throws a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  * Provides implicit values for reference types and all built-in value types (using their default 'zero' values).
	  */
	object NullValue {
		/** A type alias for [[net.noresttherein.oldsql.morsels.witness.Maybe Maybe]]`[NullValue[T]]`
		  * for use as a view bound.
		  */
		type Maybe[T] = witness.Maybe[NullValue[T]]

		/** Summon the 'null' value for `T` from an implicit `NullValue[T]`. */
		@inline def value[T :NullValue] :T = implicitly[NullValue[T]].value

		/** Summon an implicit `NullValue[T]`. */
		@inline def apply[T :NullValue] :NullValue[T] = implicitly[NullValue[T]]

		/** Guards against a `null` value argument, returning
		  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]] unless the implicit
		  * `NullValue[T]` is not null, in which case it is returned instead.
		  * @return `if (nulls == null) NullValue.NotNull else nulls`.
		  */
		def orNotNull[T](implicit nulls :witness.Maybe[NullValue[T]]) :NullValue[T] = nulls.opt match {
			case Got(tc) if tc != null => tc
			case _ => NotNull
		}

		/** Guards against a `null` value argument, returning
		  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.Null NullValue.Null]] unless the implicit
		  * `NullValue[T]` is not null, in which case it is returned instead.
		  * @return `if (nulls == null) NullValue.Null else nulls`.
		  */
		def orNull[T >: Null](implicit nulls :witness.Maybe[NullValue[T]]) :NullValue[T] = nulls.opt match {
			case Got(tc) if tc != null => tc
			case _ => Null
		}

		/** Attempts to summon an implicit `NullValue[T]` and, if it is not available, returns the alternative
		  * provided as the argument.
		  */
		@inline def orElse[T](alternative: => NullValue[T])(implicit nulls :witness.Maybe[NullValue[T]]) :NullValue[T] =
			nulls.opt match {
				case Got(tc) if tc != null => tc
				case _ => alternative
			}


		/** Create a new instance wrapping the given value of `T` as the 'null' value. */
		def apply[T](sqlNull :T) :NullValue[T] = new ArbitraryNullValue(sqlNull)

		/** Create a new instance which evaluates the given expression each time its `value` method is called.
		  * While returning different values for different calls is strongly discouraged, this allows to provide
		  * an expression which throws an exception or references a yet not initialized value.
		  */
		def eval[T](whenNull: => T) :NullValue[T] = new NullEval[T](() => whenNull)

		/** Create a new instance which evaluates the given expression each time its `value` method is called.
		  * While returning different values for different calls is strongly discouraged, this allows to provide
		  * an expression which throws an exception or references a not initialized as of yet value.
		  * The provided name is used as an identifier, and the created instance will compare equal to any other
		  * instance created by this method with the same name. It is also used as its `toString` representation.
		  */
		def eval[T](name :String, whenNull: => T) :NullValue[T] = new NullEval(() => whenNull) {
			override def canEqual(that :Any) :Boolean = that.getClass == getClass
			override def equals(that :Any) :Boolean = that match {
				case other :NullEval[_] => (this eq other) || getClass == other.getClass && toString == other.toString
				case _ => false
			}
			override def hashCode :Int = name.hashCode
			override def toString :String = name
		}

		/** A `NullValue` implementation which throws the given exception type. It is similar in effect to
		  * `NullValue.`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.eval eval]], but somewhat more concise
		  * and has equality defined in terms of the class of the thrown exception, which helps with `equals`
		  * implementations in [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]] subclasses.
		  * @tparam E a `Throwable` class which must provide one of the standard constructors:
		  *           `(String, Throwable)`, `(String)`, `(Throwable)` or `()`.
		  */
		def error[E <: Throwable :ClassTag] :NullValue[Nothing] = new NullError[E]

		/** A `NullValue` implementation which throws the given exception type. It is similar in effect to
		  * `NullValue.`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.eval eval]], but somewhat more concise
		  * and has equality defined in terms of the class of the thrown exception, which helps with `equals`
		  * implementations in [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]] subclasses.
		  * The given error string is used as the exception message, or prepended to the provided one,
		  * but not taken into an account when comparing two `NullValue` instances. All values created
		  * by this method compare equal to `NullValue.error[E]` with no error message.
		  * @tparam E a `Throwable` class which must provide one of the standard constructors:
		  *           `(String, Throwable)`, `(String)`.
		  */
        def error[E <: Throwable :ClassTag](msg :String) :NullValue[Nothing] = new NullErrorMsg[E](msg)

		/** A `NullValue` instance
		  * which always throws a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
		  * Used with forms for types which don't accept null values or simply wish to forbid them.
		  * The difference from `NullValue.eval` is that
		  * its [[net.noresttherein.oldsql.schema.SQLForm.NullValue.option toOption]] method is fixed to return `None`
		  * (rather than trying to wrap `value` in an `Option`, which would still throw the exception).
		  */
		final val NotNull :NullValue[Nothing] = new NotNull

		/** A `NullValue` instance which always throws a [[NoSuchElementException]]. Used with forms for types which
		  * don't accept null values or simply wish to forbid them. It is very similar to
		  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]], only the type of exception
		  * thrown differs. This can be helpful if the application intends to catch the exception and recover
		  * or the form is used by a method whose contract mentions throwing a standard `NoSuchElementException`.
		  */
		final val NoSuchElement :NullValue[Nothing] = new NullError[NoSuchElementException] {
			override def value :Nothing = throw new NoSuchElementException(DefaultErrorMsg)
			override def value(errorMessage :String) :Nothing = throw new NoSuchElementException(errorMessage)
			override def toString = "Null(NoSuch)"
		}

		/** A `NullValue` instance which always throws a [[NoSuchElementException]] with the given message.
		  * Used with forms for types which don't accept null values or simply wish to forbid them. It is very similar
		  * to [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]], only the type
		  * of the exception thrown differs. This can be helpful if the application intends to catch the exception
		  * and recover or the form is used by a method whose contract mentions
		  * throwing a standard `NoSuchElementException`.
		  */
		def NoSuchElement(message :String) :NullValue[Nothing] = error[NoSuchElementException](message)

		/** A `NullValue` instance which always throws an [[UnsupportedOperationException]]. It is very similar to
		  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]], only the type
		  * of the exception thrown differs. This is a special purpose type class introduced primarily for dummy forms
		  * (in particular, read forms which are required by the API but are guaranteed not to be used),
		  * as well as for the benefit of code which completely controls access to a created
		  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]
		  * or [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]] instance and must throw this exception type.
		  *
		  * Multipurpose forms will almost always opt to use either
		  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]]
		  * or [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NoSuchElement NoSuchElement]].
		  */
		final val Unsupported :NullValue[Nothing] = new NullError[UnsupportedOperationException] {
			override def value :Nothing = throw new UnsupportedOperationException(DefaultErrorMsg)
			override def value(errorMessage :String) :Nothing = throw new UnsupportedOperationException(errorMessage)
			override def toString = "Null(Unsupported)"
		}

		/** A `NullValue` instance which always throws an [[UnsupportedOperationException]] with the given message.
		  *
		  * It is very similar to
		  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]], only the type of exception
		  * thrown differs. This is a special purpose type class introduced primarily for dummy forms
		  * (in particular, read forms which are required by the API but are guaranteed not to be used),
		  * as well as for the benefit of code which completely controls access to a created
		  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]
		  * or [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]] instance and must throw this exception type.
		  *
		  * Multipurpose forms will almost always opt to use either
		  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]]
		  * or [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NoSuchElement NoSuchElement]].
		  */
		def Unsupported(message :String) :NullValue[Nothing] = error[UnsupportedOperationException](message)

		/** `null` itself as the value used by nullable types `T >: scala.Null`. */
		implicit final val Null :NullValue[Null] = new ArbitraryNullValue(null) //this is made an implicit in anticipation of Scala 3 X | Null types

		/** Unit itself as its 'null' value. */
		implicit final val Unit = NullValue(())

		/** Scala `None` as the null value for `Option[T]`. */
		implicit final val None :NullValue[Option[Nothing]] = NullValue(scala.None)

		/** [[net.noresttherein.oldsql.collection.Opt.Lack Opt.Lack]] as a null value
		  * for [[net.noresttherein.oldsql.collection.Opt Opt]]`[T]`.
		  */
		implicit final val Lack :NullValue[Opt[Nothing]] = NullValue(Opt.Lack)

		implicit def NullOptional[T] :NullValue[Optional[T]] = NullValue(Optional.empty[T])

		/** Implicit default values of standard value and reference types as per Java and Scala language specifications.
		  * These values are not in the default implicit search scopes so that form declarations make informed, explicit
		  * decisions about null handling, preventing the common bug of lifting a database `null` to a zero value.
		  * @see [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]]
		  */
		object Defaults {
//			/** Specifies `null` itself as the 'null' value used by nullable types `T >: scala.Null`. */
//			implicit final val NullRef :NullValue[Null] = NullValue(null)
			/** Specifies zero as the 'null' value. */
			implicit final val NullInt = NullValue(0)
			/** Specifies zero as the 'null' value. */
			implicit final val NullLong = NullValue(0L)
			/** Specifies zero as the 'null' value. */
			implicit final val NullShort = NullValue(0.toShort)
			/** Specifies zero as the 'null' value. */
			implicit final val NullByte = NullValue(0.toByte)
			/** Specifies `false` as the 'null' value. */
			implicit final val NullBoolean = NullValue[Boolean](false)
			/** Specifies zero as the 'null' value. */
			implicit final val NullChar = NullValue(0.toChar)
			/** Specifies zero as the 'null' value. */
			implicit final val NullFloat = NullValue(0.0f)
			/** Specifies zero as the 'null' value. */
			implicit final val NullDouble = NullValue(0.0)

			/** Specifies [[net.noresttherein.oldsql.model.Kin Kin]]`.`[[net.noresttherein.oldsql.model.Kin.Nonexistent Nonexistent]]
			  * as the 'null' value.
			  */
			implicit final val NullKin = NullValue(Kin.Nonexistent())
		}

		object Collections { //consider: constants. Lots of mini classes to change precedence
			implicit def nullIterable[C <: Iterable[Any]](implicit factory :Factory[_, C]) :C =
				factory.newBuilder.result()

			implicit val NullChain :NullValue[@~] = NullValue(@~)
//			implicit final val EmptyIterable = NullValue(Iterable.empty)
//			implicit final val EmptySeq = NullValue(Seq.empty)
//			implicit final val EmptyIndexedSeq = NullValue(IndexedSeq.empty)
//			implicit final val EmptyLinearSeq = NullValue(LinearSeq.empty)
//			implicit final val EmptyVector = NullValue(Vector.empty)
//			implicit final val EmptyList = NullValue(Nil)
//			implicit final val EmptySet = NullValue(Set.empty)
//			implicit final val EmptyHashSet = NullValue(HashSet.empty)
//			implicit final val EmptySortedSet = NullValue(SortedSet.empty)
//			implicit final val EmptyTreeSet = NullValue(TreeSet.empty)
		}

		private final val DefaultErrorMsg = "This type does not allow null values."


		private class ArbitraryNullValue[T](override val value :T) extends NullValue[T] {
			override val opt      = Got(value)
			override def toOpt    = opt
			override val option   = Some(value)
			override def toOption = option

			override def map[U](f :T => U) :NullValue[U] = try {
				NullValue(f(value))
			} catch {
				case _ :Exception => new MappedNullEval(this, f)
			}

			override def equals(that :Any) :Boolean = that match {
				case nulls :ArbitraryNullValue[_] => (nulls eq this) || nulls.value == value
				case _ => false
			}
			override def hashCode :Int = if (value == null) 0 else value.hashCode

			override def toString :String = if (value == null) "Null" else "Null(" + value + ")"
		}


		private class NullEval[T](expr :() => T) extends NullValue[T] {
			override def value = expr()
			override def value(errorMessage :String) :T = rethrow { expr() } (errorMessage)
			private def res = expr
			override def equals(that :Any) :Boolean = that match {
				//If expr has an empty closure, it will likely be always the same instance
				// and the equality will mean 'created by the same code'.
				case other :NullEval[_] => (other eq this) || other.canEqual(this) && other.res == res
				case _ => false
			}
			override def hashCode :Int = expr.hashCode
			override def toString :String = "Null{}"
		}


		private class NullError[E <: Throwable :ClassTag] extends NullValue[Nothing] {
			private[this] val infoConstructor :Option[String => Throwable] = (Try {
					classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance(_:String).asInstanceOf[Throwable]
				} orElse Try {
					classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance(_:String, null).asInstanceOf[Throwable]
				}).toOption

			private[this] val constructor :() => Throwable =
				infoConstructor.map(cons => () => cons(DefaultErrorMsg)) getOrElse {
					() => classTag[E].runtimeClass.getConstructor().newInstance().asInstanceOf[Throwable]
				}

			override def value :Nothing = throw constructor()
			override def value(msg :String) = infoConstructor match {
				case Some(cons) => throw cons(msg)
				case _ =>
					val e = constructor()
					e.addSuppressed(new UnsupportedOperationException(
						"Cannot create a " + classTag[E].runtimeClass.getName + " instance with a message (" + msg + ")."
					))
					throw e
			}
			override def opt = Opt.Lack

			override def map[U](f :Nothing => U) :NullValue[U] = this
			override def optMap[U](f :Nothing => Option[U]) :NullValue[U] = this
			override def andThen[U](f :Nothing =?> U) :NullValue[U] = this

			private val exceptionClass = classTag[E].runtimeClass
			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if this eq self => true
				case other :NullError[_] if other canEqual this => exceptionClass == other.exceptionClass
				case _ => false
			}
			override def hashCode :Int = exceptionClass.hashCode
			override def toString :String = "Null(" + newThrowable[E].localClassName + ")"
		}


		private class NullErrorMsg[E <: Throwable :ClassTag](message :String) extends NullError[E] {
			override def value :Nothing = super.value(message)
			override def value(msg :String) :Nothing = super.value(message + ". " + msg)
		}


		private class NotNull extends NullError[NullValueException] with Stateless {
			override def value :Nothing = throw new NullValueException(DefaultErrorMsg)
			override def value(errorMessage :String) :Nothing = throw new NullValueException(errorMessage)
			override def toString = "NotNull"
		}


		private class MappedNullEval[X, Y](nulls :NullValue[X], f :X => Y) extends NullValue[Y] {
			override def value = f(nulls.value)
			override def value(errorMessage :String) :Y = f(nulls.value(errorMessage))
			override def toOpt  = nulls.toOpt.map(f)
			override def opt    = nulls.opt.map(f)
			override def map[U](f :Y => U) = new MappedNullEval(this, f)

			private def nullValue = nulls
			private def fun = f
			override def equals(that :Any) :Boolean = that match {
				case other :MappedNullEval[_, _] => (other eq this) || nullValue == other.nullValue && fun == other.fun
				case _ => false
			}
			override def hashCode :Int = nullValue.hashCode * 31 + fun.hashCode
			override def toString :String = f.toString + "(" + nulls + ")"
		}

	}






	/** Base type for factories of some types `M[X]` and `S[X] <: M[X]`, which take as implicit arguments
	  * `SQLForm` and `ColumnForm` instances (or some higher type parameterized with these form types), respectively
	  * See [[net.noresttherein.oldsql.morsels.SpecializingFactory SpecializingFactory]] for more information
	  * about this framework type.
	  */
	type FormBasedFactory[A[_], M[_], S[X] <: M[X]] = SpecializingFactory[A, A, SQLForm, ColumnForm, M, S]






	/** A Convenience base `SQLReadForm[T]` class which implements `nullValue` based on an implicit `NullValue[T]`
	  * (overriding also `nulls` in the process). */
	abstract class AbstractSQLForm[T](protected override val text :Opt[String] = Lack)
	                                 (implicit override val nulls :NullValue[T])
		extends SQLForm[T] with ReadFormNullValue[T]
	{
		private[schema] lazy val cachedString = if (text.isDefined) "<" + text.get + ">" else super.toString
		override def toString :String = cachedString
	}


	/** A convenience base `SQLForm` class relying on implicit `NullValue[T]` as well as `ClassTag[T]`
	  * for its `toString` implementation.
	  */
	abstract class ReflectedSQLForm[T](protected override val text :Opt[String] = Lack)
	                                  (implicit override val nulls :NullValue[T], clazz :ClassTag[T])
		extends SQLForm[T] with ReadFormNullValue[T]
	{
		def runtimeClass :Class[_] = clazz.runtimeClass

		override def equals(that :Any) :Boolean = that match {
			case same :ReflectedSQLForm[_] => (same eq this) || (same canEqual this) && same.runtimeClass == runtimeClass
			case _ => false
		}
		override def hashCode :Int = clazz.runtimeClass.hashCode

		private[schema] lazy val cachedString :String = if (text.isDefined) "<" + text.get + ">" else super.toString
		override def toString :String = cachedString
	}



	/** Base trait for forms which construct and deconstruct their value to a lower level type handled
	  * by an underlying [[net.noresttherein.oldsql.schema.SQLForm.AbstractMappedForm.form form]].
	  * The `equals` method assumes that the extending class defines uniquely the mapping,
	  * that is it is not generic and does not accept the functions as arguments.
	  * @tparam T the type of values handled by this form.
	  * @tparam S the underlying intermediate value type handled by the form to which this class delegates.
	  */
	abstract class AbstractMappedForm[S, T](protected override val text :Opt[String] = Lack)
	                                       (implicit underlying :SQLForm[S])
		extends SQLForm[T] with ReadFormAdapter[T] with WriteFormAdapter[T] with ReadFormNullValue[T]
	{
		override val nulls :NullValue[T] = underlying.nulls.map(map)
		protected override def form :SQLForm[S] = underlying
		protected def map(s :S) :T
		protected def unmap(t :T) :S

		override def opt(res :ResultSet, position :Int) :Opt[T] =
			underlying.opt(res, position) match {
				case Got(s) => Got(map(s))
				case _ => Lack
			}

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			underlying.set(statement, position, unmap(value))

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			underlying.setNull(statement, position)

		override def literal(value :T, inline :Boolean) :String =
			if (value == null) nullLiteral(inline) else underlying.literal(unmap(value), inline)

		override def columnLiterals(value :T) :Seq[String] =
			if (value == null) underlying.nullColumnLiterals else underlying.columnLiterals(unmap(value))

		override def split :Seq[ColumnWriteForm[T]] = underlying.split.map(_.unmap(unmap))

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :AbstractMappedForm[_, _] => getClass == other.getClass && form == other.form
			case _ => false
		}
		override def hashCode :Int = getClass.hashCode * 31 + form.hashCode

		protected lazy val cachedString :String =
			if (text.isDefined) "<" + text.get + ">" else "<=[" + underlying + "]=>"
		override def toString :String = cachedString
	}



	/** Base trait for forms which construct and deconstruct their value to a lower level type handled
	  * by an underlying [[net.noresttherein.oldsql.schema.SQLForm.AbstractOptMappedForm.form form]].
	  * It is similar to [[net.noresttherein.oldsql.schema.SQLForm.AbstractMappedForm AbstractMappedForm]],
	  * but some values - both those read/written by this form as well as the underlying, intermediate values -
	  * may not have a counterpart in the other type, that is the mapping functions in both directions may not return
	  * a value for some arguments.
	  *
	  * The `equals` method assumes that the extending class defines uniquely the mapping,
	  * that is it is not generic and does not accept the functions as arguments.
	  * @tparam T the type of values handled by this form.
	  * @tparam S the underlying intermediate value type handled by the form to which this class delegates.
	  */
	abstract class AbstractOptMappedForm[S, T](protected override val text :Opt[String] = Lack)
	                                          (implicit underlying :SQLForm[S])
		extends SQLForm[T] with ReadFormAdapter[T] with WriteFormAdapter[T] with ReadFormNullValue[T]
	{
		override val nulls :NullValue[T] = underlying.nulls.optMap(map)
		protected def form :SQLForm[S] = underlying
		protected def map(s :S) :Opt[T]
		protected def unmap(t :T) :Opt[S]

		override def opt(res :ResultSet, position :Int) :Opt[T] = underlying.opt(res, position) match {
			case Got(s) => map(s)
			case _ => Lack
		}

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			underlying.setOpt(statement, position, unmap(value))

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			underlying.setNull(statement, position)

		override def literal(value :T, inline :Boolean) :String = unmap(value) match {
			case Got(s) => underlying.literal(s, inline)
			case _ => underlying.nullLiteral(inline)
		}
		override def columnLiterals(value :T) :Seq[String] = unmap(value) match {
			case Got(s) => underlying.columnLiterals(s)
			case _ => underlying.nullColumnLiterals
		}

		override def split :Seq[ColumnWriteForm[T]] = underlying.split.map(_.optUnmap(unmap))

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :AbstractOptMappedForm[_, _] =>
				getClass == other.getClass && form == other.form && nulls == other.nulls
			case _ => false
		}
		override def hashCode :Int = getClass.hashCode * 31 + underlying.hashCode

		private[schema] lazy val cachedString :String =
			if (text.isDefined) "<" + text.get + ">" else "<[" + underlying + "]>"
		override def toString :String = cachedString
	}


	/** A convenience mixin trait for forms of types which don't have SQL literal representations. These typically
	  * include `Blob` and similar large data types. All literal-related methods of `SQLWriteForm`
	  * throw an `UnsupportedOperationException`.
	  */ //todo: review its usages and remove them where a literal representation can exist.
	trait NonLiteralForm[T] extends SQLForm[T] with NonLiteralWriteForm[T]


	/** Base trait for forms which always
	  * throw a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]] on encountering
	  * a `null` value. It will neither read nor write a `null`.
	  */
	trait NotNullForm[T] extends SQLForm[T] with NotNullWriteForm[T] with NotNullReadForm[T]



	/** A form with equality defined as `name` equality only.
	  * Resets the implementations of form proxy factory methods which any class mixing in this trait might
	  * have overriden with dedicated implementations back to their default implementations from `SQLForm`
	  * in order to preserve a reference to this form and thus name-based, rather than property-based equality
	  * of the created forms.
	  */
	private[schema] trait NamedForm[T] extends NamedReadForm[T] with NamedWriteForm[T] with SQLForm[T] {
		override def nullSafe :SQLForm[T] = super[SQLForm].nullSafe
		override def notNull  :SQLForm[T] = super[SQLForm].notNull
		override def withNull(implicit nulls :NullValue[T]) :SQLForm[T] = super[SQLForm].withNull
	}



	/** A base class for forms which do not read or write any columns. Read methods always return `nullValue`,
	  * implementation of which is left to the subclass.
	  */
	private[schema] trait EmptyForm[T] extends SQLForm[T] with IgnoringReadForm[T] with EmptyWriteForm[T] {
		override def nullSafe :SQLForm[T] = this
		override def notNull :SQLForm[T] = super[SQLForm].notNull

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[EmptyForm[_]]

		override def equals(that :Any) :Boolean = that match {
			case other :EmptyForm[_] =>
				(other eq this) || other.canEqual(this) && (nulls == other.nulls)
			case _ => false
		}
		override def hashCode :Int = nulls.hashCode

		override def toString = "EMPTY"
	}



	private[schema] class EmptySQLForm[T :NullValue](protected override val text :Opt[String] = Got("EMPTY"))
		extends AbstractSQLForm[T](text) with EmptyForm[T]
	{
		override def equals(that :Any) :Boolean = that match {
			case other :EmptySQLForm[_] => (other eq this) || (other canEqual this) && other.nulls == nulls
			case _ => false
		}
		override def hashCode :Int = nulls.hashCode

		private[schema] override lazy val cachedString = text match {
			case Got(string) => "<" + string + ">"
			case _ => "<" + nulls + ">"
		}
	}



	private[schema] class CombinedForms[T](read :SQLReadForm[T], write :SQLWriteForm[T],
	                                       protected override val text :Opt[String] = Lack)
		extends SQLForm[T] with WriteFormLiterals[T]
	{
		if (read.columnCount != write.columnCount)
			throw new IllegalArgumentException(
				"Cannot combine forms with different numbers of read/written columns: " + read
					+ " (#" + read.columnCount + ") <> " + write + " (#" + write.columnCount + ")."
			)

		try { //consider: moving this check to columnTypes definition, partially because it is faster in CombinedColumnForms
			if (read.columnTypes != write.columnTypes)
				throw new IllegalArgumentException(
					"Cannot combine forms with different SQL column types: "
						+ read + read.columnTypes.mkString(" (", ",", ") and ")
						+ write + write.columnTypes.mkString(" (", ",", ").")
				)
		} catch {
			case _ :UnsupportedOperationException =>
		}

		override def reader :SQLReadForm[T]  = read
		override def writer :SQLWriteForm[T] = write

		private lazy val types :Seq[JDBCType] =
			try { read.columnTypes } catch {
				case _ :UnsupportedOperationException => write.columnTypes
			}
		override def columnTypes :Seq[JDBCType] = types
		override def columnCount: Int = read.columnCount
		override def isUniversal :Boolean = read.isUniversal && write.isUniversal

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			write.set(statement, position, value)

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			write.setNull(statement, position)

		override def opt(res :ResultSet, position :Int): Opt[T] = read.opt(res, position)

		override def nullValue: T = read.nullValue
		override def nulls :NullValue[T] = read.nulls

		override def literal(value: T, inline :Boolean): String = write.literal(value, inline)
		override def nullLiteral(inline :Boolean): String = write.nullLiteral(inline)
		override def columnLiterals(value: T): Seq[String] = write.columnLiterals(value)
		override def nullColumnLiterals: Seq[String] = write.nullColumnLiterals

		override def split = write.split

		override def nullSafe = write.nullSafe match {
			case this.write => this
			case w => new CombinedForms[T](read, w, Got(toString + ".nullSafe"))
		}

		override def notNull :SQLForm[T] = (read.notNull, write.notNull) match {
			case (r, w) if (r eq read) && (w eq write) => this
			case (r, w) => new CombinedForms[T](r, w, Got(toString + ".notNull"))
		}

		override def withNull(implicit nulls :NullValue[T]) = SQLForm.combine(read, write.withNull)

		override def comparable(other :SQLReadForm[_]) :Boolean = reader comparable other
		override def comparable(other :SQLWriteForm[_]) :Boolean = writer comparable other

		override def equals(that :Any) :Boolean = that match {
			case combine :CombinedForms[_] =>
				(this eq combine) || (combine canEqual this) && combine.reader == reader && combine.writer == writer
			case _ => false
		}
		override def hashCode :Int = read.hashCode * 31 + write.hashCode

		private[this] lazy val cachedString =
			if (text.isDefined)
				"<" + text.get + ">"
			else
				"<" + read + "<>" + write + ">"

		override def toString :String = cachedString
	}

}


