package net.noresttherein.oldsql.schema

import java.sql.{PreparedStatement, ResultSet}
import java.util.Optional

import scala.annotation.implicitNotFound
import scala.collection.Factory
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.{Chain, Listing, Opt}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.model.Kin
import net.noresttherein.oldsql.morsels.{ColumnBasedFactory, Extractor, Stateless}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.schema.SQLForm.{NotNullSQLForm, NullValue}
import net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull
import net.noresttherein.oldsql.schema.SQLReadForm.{FlatMappedSQLReadForm, LazySQLReadForm, MappedSQLReadForm, NotNullReadForm, ProxyReadForm, ReadFormNullGuard, ReadFormNullValue, ReadFormSeq, SeqReadForm}
import net.noresttherein.oldsql.schema.SQLWriteForm.{CustomNullSQLWriteForm, EmptyWriteForm, EvalOrNullSQLWriteForm, FlatMappedWriteForm, LazyWriteForm, MappedWriteForm, NonLiteralWriteForm, NotNullWriteForm, ProxyWriteForm, SeqWriteForm, SQLWriteFormSeq, WriteFormNullGuard}
import net.noresttherein.oldsql.schema.forms.SQLForms
import net.noresttherein.oldsql.slang.innerNameOf






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
  */ //todo: specialization! (or migration to Opt instead of Option).
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
	  */ //todo: rename optMap
	def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
		SQLForm.flatMap(map)(unmap)(this, NullValue[X])

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
		as(map)(unmap)(nulls.extract(map))


	/** Lifts this form to represent `Option[T]`. The created form maps all values returned by this form using
	  * `Option(...)`. This means that `null` values (actual JVM nulls, not the somewhat arbitrary value provided
	  * by this form) are mapped to `Some(None)`, while a returned `None` indicates that this form returned `None`.
	  * Basically this means that the returned form uses this form's `opt` method as its `apply` implementation.
	  */
	override def toOpt :SQLForm[Option[T]] = SQLForms.OptionForm(this)


	override def nullSafe :SQLForm[T] = //SQLForm.join(super.nullSafe, this)
		new ProxyReadForm[T] with ProxyWriteForm[T] with WriteFormNullGuard[T] with SQLForm[T] {
			override protected def form = outer
			override def nullSafe :SQLForm[T] = this
		}

	/** An adapter or modification of this form which throws a `NullPointerException` whenever it would output
	  * a `null`. This includes both setting statement parameters and reading values from a `ResultSet`.
	  */
	override def notNull :SQLForm[T] = new NotNullSQLForm()(this)

	override def withNull(implicit nullVal :NullValue[T]) :SQLForm[T] =
		new CustomNullSQLWriteForm[T](this) with ProxyReadForm[T] with SQLForm[T] {
			override val form = outer
			override val nulls = nullVal
			override def nullValue = nulls.value
			override def toString :String = super[CustomNullSQLWriteForm].toString
		}

	override def withNull(nullValue :T) :SQLForm[T] = withNull(NullValue(nullValue))


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



	def compatible(other :SQLForm[_]) :Boolean = this == other

}






object SQLForm {

	/** Summon an implicit instance of `SQLForm[T]`. */
	def apply[T :SQLForm] :SQLForm[T] = implicitly[SQLForm[T]]



	/** Creates an `SQLForm` delegating all calls to the implicitly provided read and write forms. */
	def join[T](implicit write :SQLWriteForm[T], read :SQLReadForm[T]) :SQLForm[T] =
		new JoinedForms[T](read, write)

	/** Creates an `SQLForm` delegating all calls to the implicitly provided read and write forms.
	  * @param name the name of the created form returned from its `toString` method.
	  */
	def join[T](name :String)(implicit write :SQLWriteForm[T], read :SQLReadForm[T]) :SQLForm[T] =
		new JoinedForms[T](read, write, name)



	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression.
	  */
	def eval[T](expr: => T, name :String = null)(implicit write :SQLWriteForm[T]) :SQLForm[T] =
		join(name)(SQLWriteForm.eval(expr), SQLReadForm.eval(expr, write.writtenColumns))

	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression.
	  */
	def evalopt[T](expr: => Opt[T], name :String = null)
	              (implicit write :SQLWriteForm[T], nulls :NullValue[T]) :SQLForm[T] =
		join(name)(
			new EvalOrNullSQLWriteForm[T](expr)(write, nulls), SQLReadForm.evalopt(expr, write.writtenColumns)
		)



	/** Creates a dummy form which always returns and writes the same value. */
	def const[T](value :T, name :String = null)(implicit write :SQLWriteForm[T]) :SQLForm[T] =
		join(name)(SQLWriteForm.const(value), SQLReadForm.const(value, write.writtenColumns))

	/** Creates a dummy form which always returns and writes the same value. */
	def constopt[T](value :Opt[T], name :String = null)
	               (implicit write :SQLWriteForm[T], nulls :NullValue[T]) :SQLForm[T] =
		join(name)(SQLWriteForm.constopt(value), SQLReadForm.constopt(value, write.writtenColumns))



	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
	  * and returns `None`/`nulls.value` when reading.
	  */
	def nulls[T](implicit write :SQLWriteForm[T], nulls :NullValue[T]) :SQLForm[T] =
		join(SQLWriteForm.none[T], SQLReadForm.nulls[T](write.writtenColumns))

	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
	  * and returns `None`/`nulls.value` when reading.
	  */
	def nulls[T](name :String)(implicit write :SQLWriteForm[T], nulls :NullValue[T]) :SQLForm[T] =
		join(name)(SQLWriteForm.none[T], SQLReadForm.nulls[T](write.writtenColumns))

	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
	  * and returns `None` when reading. All calls to `apply` will result in a `NullPointerException`.
	  */
	def none[T](implicit write :SQLWriteForm[T]) :SQLForm[T] =
		join(SQLWriteForm.none[T], SQLReadForm.none[T](write.writtenColumns))

	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
	  * and returns `None` when reading. All calls to `apply` will result in a `NullPointerException`.
	  */
	def none[T](name :String)(implicit write :SQLWriteForm[T]) :SQLForm[T] =
		join(SQLWriteForm.none[T](name), SQLReadForm.none[T](write.writtenColumns, name))

	/** A form which does not read or write any columns but always returns the given constant `value`
	  * from `apply`, `opt` and `nullValue` methods.
	  */
	def empty[T](value :T, name :String = null) :SQLForm[T] =
		new EmptyConstForm(value)(name)

	/** A form which does not read or write any columns but always returns the result of evaluating `expr`
	  * from `apply`, `opt` and `nullValue` methods.
	  */
	def emptyEval[T](expr: => T, name :String = "EMPTY") :SQLForm[T] =
		new EmptyEvalForm(expr, name)



	/** Creates a new `SQLForm` for type `T` based on an implicit form for type `S` and an optional `NullValue[T]`.
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm.nulls one present in the source form]] will be mapped
	  * with `map`.
	  */
	def apply[S, T](map :S =?> T)(unmap :T =?> S)(implicit source :SQLForm[S], nulls :Maybe[NullValue[T]]) :SQLForm[T] =
		(map, unmap) match {
			case (_ :IdentityExtractor[_], _:IdentityExtractor[_]) => source.asInstanceOf[SQLForm[T]]
			case (Extractor.Requisite(there), Extractor.Requisite(back)) => SQLForm.map(there)(back)
			case _ => flatMap(map.optional)(unmap.optional)
		}

	/** Creates a new `SQLForm` for type `T` based on an implicit form for type `S` and an optional `NullValue[T]`.
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm.nulls one present in the source form]] will be mapped
	  * with `map`.
	  */
	def apply[S, T](name :String)(map :S =?> T)(unmap :T =?> S)
	               (implicit source :SQLForm[S], nulls :Maybe[NullValue[T]]) :SQLForm[T] =
		(map, unmap) match {
//			case (_ :IdentityExtractor[_], _:IdentityExtractor[_]) => source.asInstanceOf[SQLForm[T]]
			case (Extractor.Requisite(there), Extractor.Requisite(back)) => SQLForm.map(name)(there)(back)
			case _ => flatMap(name)(map.optional)(unmap.optional)
		}



	/** Creates a new `SQLForm` for type `T` based on an implicit form for type `S` and an optional `NullValue[T]`.
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm.nulls one present in the source form]] will be mapped
	  * with `map`.
	  */
	def map[S, T](map :S => T)(unmap :T => S)(implicit source :SQLForm[S], nulls :Maybe[NullValue[T]]) :SQLForm[T] =
		SQLForm.map("<=" + source + "=>")(map)(unmap)

	/** Creates a new `SQLForm` for type `T` based on an implicit form for type `S` and an optional `NullValue[T]`.
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm.nulls one present in the source form]] will be mapped
	  * with `map`.
	  */
	def map[S, T](name :String)(map :S => T)(unmap :T => S)
	             (implicit source :SQLForm[S], nulls :Maybe[NullValue[T]]) :SQLForm[T] =
	{
		val nullValue = nulls.opt getOrElse NullValue.NotNull
		new MappedSQLForm[S, T](map, unmap, name)(source, nullValue)
	}



	/** Creates a new `SQLForm` for type `T` based on an implicit form for type `S` and an optional `NullValue[T]`.
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm.nulls one present in the source form]] will be flat mapped
	  * with `map`.
	  */
	def flatMap[S, T](map :S => Option[T])(unmap :T => Option[S])
	                 (implicit source :SQLForm[S], nulls :Maybe[NullValue[T]]) :SQLForm[T] =
		SQLForm.flatMap("<=" + source + "=>")(map)(unmap)

	/** Creates a new `SQLForm` for type `T` based on an implicit form for type `S` and an optional `NullValue[T]`.
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm.nulls one present in the source form]] will be flat mapped
	  * with `map`.
	  */
	def flatMap[S, T](name :String)(map :S => Option[T])(unmap :T => Option[S])
	                 (implicit source :SQLForm[S], nulls :Maybe[NullValue[T]]) :SQLForm[T] =
	{
		val nullValue = nulls.opt getOrElse NullValue.NotNull
		new FlatMappedSQLForm[S, T](map, unmap, name)(source, nullValue)
	}



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
	def delayed[T](init: => SQLForm[T]) :SQLForm[T] = new LazyForm[T](() => init)



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
	def seq[T :SQLForm](repeats :Int) :SQLForm[Seq[T]] = new SeqSQLForm[T](repeats)

	/** A form mapping sequences of constant length, each element with the corresponding form given as the argument.
	  * @see [[[net.noresttherein.oldsql.schema.SQLReadForm$.seq[T](items:Seq[SQLReadForm[T]])]]]
	  * @see [[[net.noresttherein.oldsql.schema.SQLWriteForm$.seq[T](items:Seq[SQLWriteForm[T]])]]]
	  */
	def seq[T](items :Seq[SQLForm[T]]) :SQLForm[Seq[T]] =
		new SQLFormSeq(items)




	implicit class ChainFormConstructor[I <: Chain](private val self :SQLForm[I]) extends AnyVal {
		def ~[L](implicit next :SQLForm[L]) :SQLForm[I ~ L] = SQLForms.ChainForm(self, next)
	}
	implicit class ListingFormConstructor[I <: Listing](private val self :SQLForm[I]) extends AnyVal {
		def |~[K :ValueOf, V](entry :K :~ SQLForm[V]) :SQLForm[I |~ (K :~ V)] =
			SQLForms.ListingForm(self, implicitly[ValueOf[K]], entry.value)
	}






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
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]] would return `None`; it is not used by the latter,
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
	  * representation, which throws `NullPointerException`.
	  */
	@implicitNotFound("I do not know how to handle null values for ${T}: missing implicit NullValue[${T}].\n" +
	                  "You can use one of the values provided in the companion object for a syntactically scoped " +
	                  "implicit definition or import the default values from SQLForm.NotNull.Defaults.")
	trait NullValue[+T] extends Serializable {

		/** Value to which null columns in the `ResultSet` are mapped.
		  * @throws NullPointerException if the particular form for type `T` prohibits null values.
		  * @throws NoSuchElementException if this is [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NoSuchElement NoSuchElement]]
		  */
		def value :T

		/** Value to which null columns in the `ResultSet` are mapped.
		  * This variant should be overloaded by implementations which throw an exception from
		  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.value value]] in order to use the given message string
		  * in the exception for better debugging.
		  * @throws NullPointerException if the particular form for type `T` prohibits null values.
		  * @throws NoSuchElementException if this is [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NoSuchElement NoSuchElement]]
		  */
		def value(errorMessage :String) :T = value

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
			map(tnull => f(tnull) getOrElse { //consider: shouldn't it throw NullPointerException?
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



	/** Factory of [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValule]]`[T]` type class, providing values
	  * representing `null` in the underlying column(s). No default implicit values are defined in the implicit search
	  * scope except of `None` for `Option` types, but implicit values wrapping default values of all standard value
	  * types (as well as `null` for reference types) are defined in the member object
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.Defaults Defaults]]. This is to avoid accidental conversion
	  * from a database `null` to a zero value, which can easily be made persistent by a future update.
	  * You can import `NullValue.Defaults._` if you wish them brought to scope, but the recommended default approach
	  * is to use [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]], which always
	  * throws a `NullPointerException`
	  * Provides implicit values for reference types and all built-in value types (using their default 'zero' values).
	  */
	object NullValue {
		/** Summon the 'null' value for `T` from an implicit `NullValue[T]`. */
		@inline def value[T :NullValue] :T = implicitly[NullValue[T]].value

		/** Summon an implicit `NullValue[T]`. */
		@inline def apply[T :NullValue] :NullValue[T] = implicitly[NullValue[T]]


		/** Create a new instance wrapping the given value of `T` as the 'null' value. */
		def apply[T](sqlNull :T) :NullValue[T] = new ArbitraryNullValue(sqlNull)

		/** Guards against a `null` value argument, returning
		  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]] unless the implicit
		  * `NullValue[T]` is not null, in which case it is returned instead.
		  * @return `if (nulls == null) NullValue.NotNull else nulls`.
		  */
		def orNotNull[T](implicit nulls :NullValue[T]) :NullValue[T] =
			if (nulls == null) NullValue.NotNull else nulls

		/** Guards against a `null` value argument, returning
		  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.Null NullValue.Null]] unless the implicit
		  * `NullValue[T]` is not null, in which case it is returned instead.
		  * @return `if (nulls == null) NullValue.Null else nulls`.
		  */
		def orNull[T >: Null](implicit nulls :NullValue[T]) :NullValue[T] =
			if (nulls == null) NullValue.Null else nulls


		/** Create a new instance which evaluates the given expression each time its `value` method is called.
		  * While returning different values for different calls is strongly discouraged, this allows to provide
		  * an expression which throws an exception or references a not initialized as of yet value.
		  */
		def eval[T](whenNull: =>T, name :String = null) :NullValue[T] = new NullValue[T] {
			override def value :T = whenNull
			override def map[U](f :T => U) = eval(f(whenNull))
			override def toString = if (name == null) "Null(?)" else name
		}

		/** A `NullValue` instance which always throws a `NullPointerException`. Used with forms for types which
		  * don't accept null values or simply wish to forbid them. The difference from `ŃullValue.eval` is that
		  * its [[net.noresttherein.oldsql.schema.SQLForm.NullValue.toOption toOption]] method is fixed to return `None`
		  * (rather than trying to wrap `value` in an `Option`, which would still throw the exception).
		  */
		final val NotNull :NullValue[Nothing] = new NotNull

		/** A `NullValue` instance which always throws a `NoSuchElementException`. Used with forms for types which
		  * don't accept null values or simply wish to forbid them. It is very similar to
		  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]], only the type of exception
		  * thrown differs. This can be helpful if the application intends to catch the exception and recover.
		  */
		final val NoSuchElement :NullValue[Nothing] = new NotNull {
			override def value(errorMessage :String) :Nothing =
				throw new NoSuchElementException(errorMessage)
		}

		/** `null` itself as the value used by nullable types `T >: scala.Null`. */
		final val Null :NullValue[Null] = Defaults.NullRef

		/** Unit itself as its 'null' value. */
		implicit final val Unit = NullValue(())

		/** Scala `None` as the null value for `Option[T]`. */
		implicit final val None :NullValue[Option[Nothing]] = NullValue(scala.None)

		implicit final val Miss :NullValue[Opt[Nothing]] = NullValue(Opt.Lack)

		implicit def NullOptional[T] :NullValue[Optional[T]] = NullValue(Optional.empty[T])

		/** Implicit default values of standard value and reference types as per Java and Scala language specifications.
		  * These values are not in the default implicit search scopes so that form declarations make informed, explicit
		  * decisions about null handling, preventing the common bug of lifting a database `null` to a zero value.
		  * @see [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]]
		  */
		object Defaults {
			/** Specifies `null` itself as the 'null' value used by nullable types `T >: scala.Null`. */
			implicit final val NullRef :NullValue[Null] = NullValue(null)
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


		private class NotNull extends NullValue[Nothing] with Stateless {
			override def value :Nothing = value("This type does not allow null values.")
			override def value(errorMessage :String) = throw new NullPointerException(errorMessage)
			override def toOption = scala.None
			override def map[U](f :Nothing => U) :NullValue[U] = this

			override def toString = "NotNull"
		}

	}






	/** A convenience mixin trait for forms of types which don't have SQL literal representations. These typically
	  * include `Blob` and similar large data types. All literal-related methods of `SQLWriteForm`
	  * throw an `UnsupportedOperationException`.
	  */ //todo: review its usages and remove them where a literal representation can exist.
	trait NonLiteralForm[T] extends SQLForm[T] with NonLiteralWriteForm[T]



	/** A Convenience base `SQLReadForm[T]` class which implements `nullValue` based on an implicit `NullValue[T]`
	  * (overriding also `nulls` in the process). */
	abstract class AbstractSQLForm[T](implicit override val nulls :NullValue[T])
		extends SQLForm[T] with ReadFormNullValue[T]



	/** A convenience base `SQLForm` class relying on implicit `NullValue[T]` as well as `ClassTag[T]`
	  * for its `toString` implementation.
	  */
	abstract class ReifiedSQLForm[T](implicit override val nulls :NullValue[T], clazz :ClassTag[T])
		extends SQLForm[T] with ReadFormNullValue[T]
	{
		def runtimeClass :Class[_] = clazz.runtimeClass

		override def equals(that :Any) :Boolean = that match {
			case same :ReifiedSQLForm[_] => (same eq this) || (same canEqual this) && same.runtimeClass == runtimeClass
			case _ => false
		}

		override def hashCode :Int = clazz.runtimeClass.hashCode

		override val toString :String = innerNameOf[T]
	}



	abstract class AbstractMappedForm[S, T](implicit form :SQLForm[S], override val nulls :NullValue[T])
		extends SQLForm[T] with ReadFormNullValue[T]
	{
		protected def map(s :S) :T
		protected def unmap(t :T) :S

		override def opt(res :ResultSet, position :Int) :Opt[T] = form.opt(res, position) match {
			case Got(s) => Got(map(s))
			case _ => Lack
		}

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			form.set(statement, position, unmap(value))

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			form.setNull(statement, position)

		override def literal(value :T) :String = form.literal(unmap(value))
		override def inlineLiteral(value :T) :String = form.inlineLiteral(unmap(value))
		override def nullLiteral :String = form.nullLiteral
		override def inlineNullLiteral :String = form.inlineNullLiteral

		override def writtenColumns :Int = form.writtenColumns
		override def readColumns :Int = form.readColumns

		override def toString :String = "<=" + form + "=>"
	}



	/** A base class for forms which do not read or write any columns. Read methods always return `nullValue`,
	  * implementation of which is left to the subclass.
	  */
	trait EmptyForm[T] extends SQLForm[T] with EmptyWriteForm[T] {
		override def apply(res: ResultSet, position: Int): T = nullValue

		override def opt(rs :ResultSet, position :Int) :Opt[T] = Lack

		override def nullSafe :SQLForm[T] = this
		override def notNull :SQLForm[T] = super[SQLForm].notNull

		override def readColumns = 0

		override def toString = "EMPTY"
	}



	trait NotNullForm[T] extends SQLForm[T] with NotNullWriteForm[T] with NotNullReadForm[T] {
		override def notNull :this.type = this
	}






	/** Base type for factories of some types `M[X]` and `S[X] <: M[X]`, which take as arguments
	  * `SQLForm` and `ColumnForm` instances (or some higher type parameterized with these form types), respectively
	  * See [[net.noresttherein.oldsql.morsels.ColumnBasedFactory ColumnBasedFactory]] for more information
	  * about this framework type.
	  */
	type FormFunction[A[_], M[_], S[X] <: M[X]] = ColumnBasedFactory[A, A, SQLForm, ColumnForm, M, S]




	private[schema] class NotNullSQLForm[T](implicit protected override val form :SQLForm[T])
		extends ProxyReadForm[T] with ProxyWriteForm[T] with NotNullForm[T] with  ReadFormNullGuard[T]
	{
		override def apply(res :ResultSet, position :Int) :T = {
			val t = form(res, position)
			if (t == null) throw new NullPointerException("Cannot return null from " + this + ".")
			t
		}

		override def notNull  = this

		override def toString = form.toString + ".notNull"
	}



	private[schema] class EmptyEvalForm[T](value: => T, override val toString :String = "EMPTY")
		extends EmptyForm[T]
	{
		override def nullValue = value

		override def notNull :SQLForm[T] =
			new EmptyEvalForm[T](value, toString + ".notNull") with NotNullForm[T] with ReadFormNullGuard[T]
	}

	private[schema] case class EmptyConstForm[T](override val nullValue :T)
	                                            (override val toString :String = "EMPTY=" + nullValue)
		extends EmptyForm[T]
	{
		override def notNull :SQLForm[T] =
			if (nullValue == null)
				new EmptyEvalForm(
					throw new NullPointerException("Cannot return null from " + this + ".notNull."), toString + ".notNull"
				)
			else this
	}



	private[schema] class JoinedForms[T](read :SQLReadForm[T], write :SQLWriteForm[T], name :String = null)
		extends SQLForm[T]
	{
		protected def r :SQLReadForm[T] = read
		protected def w :SQLWriteForm[T] = write

		override def writtenColumns: Int = write.writtenColumns
		override def readColumns: Int = read.readColumns

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			write.set(statement, position, value)

		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			write.setNull(statement, position)

		override def opt(res :ResultSet, position :Int): Opt[T] = read.opt(res, position)

		override def nullValue: T = read.nullValue
		override def nulls :NullValue[T] = read.nulls

		override def literal(value: T): String = write.literal(value)
		override def nullLiteral: String = write.nullLiteral
		override def inlineLiteral(value: T): String = write.inlineLiteral(value)
		override def inlineNullLiteral: String = write.inlineNullLiteral


		override def nullSafe = SQLForm.join(write.nullSafe, read)

		override def notNull :SQLForm[T] = new JoinedForms[T](read.notNull, write.notNull, toString + ".notNull")

		override def withNull(implicit nulls :NullValue[T]) = SQLForm.join(write.withNull, read)

		override def withNull(nullValue :T) = withNull(NullValue(nullValue))

		override def equals(that :Any) :Boolean = that match {
			case combine :JoinedForms[_] =>
				(this eq combine) || (combine canEqual this) && combine.r == r && combine.w == w
			case _ => false
		}

		override def hashCode :Int = read.hashCode * 31 + write.hashCode

		override val toString =
			if (name != null) name
			else {
				val rs = read.toString; val ws = write.toString
				if (rs == ws) rs
				else if ("<" + rs == ws + ">") "<" + rs
				else if ("<=" + rs == ws + "=>") "<=" + rs
				else "(" + ws + "<>" + rs + ")"
			}
	}






	private[schema] class FlatMappedSQLForm[S, T](fmap :S => Option[T], protected override val unmap :T => Option[S], name :String = null)
	                                             (implicit override val form :SQLForm[S], nulls :NullValue[T])
		extends FlatMappedSQLReadForm[S, T](fmap) with FlatMappedWriteForm[S, T] with SQLForm[T]
	{   //map/flatMap not overriden to preserve potentially custom name.
		override def notNull :SQLForm[T] =
			new FlatMappedSQLForm[S, T](map, unmap, toString + ".notNull")(form, NotNull)
				with NotNullForm[T] with ReadFormNullGuard[T]

		override val toString :String = if (name != null) name else "<=" + form + "=>"
	}



	private[schema] class MappedSQLForm[S, T](f :S => T, protected override val unmap :T => S, name :String = null)
	                                         (implicit override val form :SQLForm[S], nulls :NullValue[T])
		extends MappedSQLReadForm[S, T](f) with MappedWriteForm[S, T] with SQLForm[T]
	{ //map/flatMap not overriden to preserve potentially custom name.
		override def notNull :SQLForm[T] =
			new MappedSQLForm[S, T](map, unmap, toString + ".notNull")(form, NotNull)
				with NotNullForm[T] with ReadFormNullGuard[T]

		override val toString :String = if (name != null) name else "<=" + form + "=>"
	}



	private[schema] class LazyForm[T](delayed: () => SQLForm[T])
		extends LazySQLReadForm[T](delayed) with LazyWriteForm[T] with SQLForm[T]
	{
		protected[this] override var init :() => SQLWriteForm[T] = delayed

		override protected def form :SQLForm[T] = {
			val read = super[LazySQLReadForm].form.asInstanceOf[SQLForm[T]]
			if (fastAccess == null) {
				fastAccess = read
				if (initialized == null)
					initialized = read
				init = null
			}
			read
		}

		override def isInitialized :Boolean = super[LazySQLReadForm].isInitialized

		override def bimap[X :NullValue](map :T => X)(unmap :X => T) :SQLForm[X] =
			form.bimap(map)(unmap)

		override def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
			form.biflatMap(map)(unmap)

		override def toOpt = form.toOpt
		override def nullSafe = form.nullSafe
		override def notNull = form.notNull
		override def withNull(implicit nulls :NullValue[T]) = form.withNull
		override def withNull(nullValue :T) = form.withNull(nullValue)
		override def *(repeat :Int) = form.*(repeat)

		override def *[O](other :SQLForm[O]) :SQLForm[(T, O)] = form * other

		override def toString :String = if (isInitialized) form.toString else "<Lazy>"
	}






	private case class SeqSQLForm[T](form :SQLForm[T], repeats :Int)
		extends SQLForm[Seq[T]] with SeqWriteForm[T] with SeqReadForm[T]
	{
		def this(count :Int)(implicit form :SQLForm[T]) = this(form, count)
		override def notNull :SQLForm[Seq[T]] = new SeqSQLForm(form.notNull, repeats)

		override val readColumns = super.readColumns
		override val writtenColumns = super.writtenColumns
		override val toString :String = "<" + repeats + "*" + form + ">"
	}


	private class SQLFormSeq[T](override val forms :Seq[SQLForm[T]])
		extends SQLWriteFormSeq[T](forms) with ReadFormSeq[T] with SQLForm[Seq[T]]
	{
		override def notNull :SQLForm[Seq[T]] = new SQLFormSeq[T](forms.map(_.notNull))
		override def canEqual(that :Any) :Boolean = getClass == that.getClass
		override def toString = super[SQLWriteFormSeq].toString
	}


}


