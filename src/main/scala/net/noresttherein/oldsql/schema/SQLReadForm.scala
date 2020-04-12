package net.noresttherein.oldsql.schema

import java.sql.{ResultSet, SQLException}

import net.noresttherein.oldsql
import net.noresttherein.oldsql.collection.{Chain, LiteralIndex, Record}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.LiteralIndex.&~
import net.noresttherein.oldsql.collection.Record.|#
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.ColumnReadForm.FallbackColumnReadForm
import net.noresttherein.oldsql.schema.SQLForm.{JDBCSQLType, NullValue}
import net.noresttherein.oldsql.schema.SQLReadForm.{FallbackReadForm, Tuple2ReadForm}
import net.noresttherein.oldsql.slang._

import scala.annotation.tailrec
import scala.collection.immutable.Seq




/** Encapsulates the logic of reading and building the value of `T` from a number of consecutive columns
  * in a `ResultSet`. Both the construction process and, in particular, the number of read columns should be fixed,
  * providing a pure contract. This makes it a lower-level counterpart of [[net.noresttherein.oldsql.schema.Mapping]],
  * which can be represented by possibly many forms, depending on the column list included in the selection.
  * All implementations must be thread safe.
  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm]]
  * @see [[net.noresttherein.oldsql.schema.SQLForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnReadForm]]
  */
trait SQLReadForm[+T] {

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
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]]
	  */
	def apply(position :Int)(res :ResultSet) :T = opt(position)(res) match {
		case Some(x) => x
		case _ => nullValue
	}

	/** Attempts to read the column values from columns `&lt;position..position + this.readColumns` of the passed
	  * `ResultSet` and create an instance of `T`. If the values are unavailable (required columns carry `null` values),
	  * `None` is returned. It is a recommended practice to have the returned option reflect only the availability
	  * of the input values and not their validity. It is allowed for the form to return `Some(null)`
	  * (as long as `T >: Null`), but this results in propagation of `null` values to any forms derived from it
	  * and to the application. As such, it is strongly discouraged. While not strictly required, the form should
	  * throw an exception if the values do not conform to expected constraints or the assembly process fails
	  * for any other reason. Similarly, all thrown `SQLException`s are propagated.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]]
	  */
	def opt(position :Int)(res :ResultSet) :Option[T]

	/** The value a `null` column (or all `null` columns) should be mapped to. It is used in particular by `apply`
	  * when the value is unavailable, for example as a result of an outer join. Extending classes are allowed
	  * to throw an exception here (either a `NoSuchElementException` or a `NullPointerException`) if a concept
	  * of null does not exist for `T` or `null`s are not acceptable values. Note however that default implementations
	  * for scala built-in value types will return some form of `0` here.
	  */
	def nullValue :T

	/** Null representation for type `T`. Wrapped value is used by the form when the mapped column(s) is null.
	  * It should be consistent with `this.nullValue` and the default implementation simply delegates to it at each call.
	  * This wrapper can be implicitly passed as a type class, even if the form does not support `null` values
	  * (i.e. `nullValue` throws an exception), which would not be possible by simply using `nullValue`.
	  */
	implicit def nulls :NullValue[T] = NullValue.eval(nullValue)

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
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.mapNull]]
	  */
	def map[X :NullValue](fun :T => X) :SQLReadForm[X] = SQLReadForm.map(fun)(this, NullValue[X])

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. If the underlying columns
	  * carry null values, passed `nullValue` is used instead of mapping. This guarantees that the given function will
	  * not be called for `null` arguments unless this form returns `Some(null)` from its `opt` method in a non-standard
	  * practice and allows handling of `null` values also when `T` is a value type without a natural `null` value.
	  * Note that `this.nullValue` is never called, directly or indirectly, by the created form.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.mapNull]]
	  */
	def map[X](fun :T => X, nullValue :X) :SQLReadForm[X] = map(fun)(NullValue(nullValue))

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. Note that the given
	  * function may be called for `null` arguments, even if the underlying columns have a ''not null'' constraint
	  * in case of outer join queries.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  */
	def mapNull[X](fun :T => X) :SQLReadForm[X] = map(fun)(nulls.map(fun))




	/** Attempts to map the value of `T` read by this form to type `X` in order to produce a form for `X`.
	  * Unlike `map`, not all values of `T` may have an associated counterpart in `X`, in which case the given function
	  * returns `None` and the new form defaults to its `nullValue`. This method variant relies on implicit
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] to provide the value of `X` which should be used
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
	def flatMapNull[X](fun :T => Option[X]) :SQLReadForm[X] = flatMap(fun)(nulls.flatMap(fun))



	def optMap[X :NullValue](fun :Option[T] => Option[X]) :SQLReadForm[X] =
		SQLReadForm.optMap(fun)(this, NullValue[X])

	def optMap[X](fun :Option[T] => Option[X], nullValue :X) :SQLReadForm[X] =
		optMap(fun)(NullValue(nullValue))



	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`.
	  * This will call `mapNull` or `flatMapNull` based on whether the extractor is a `RequisiteExtractor`.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#mapNull]]
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm#flatMapNull]]
	  */
	def andThen[X](extractor :T =?> X) :SQLReadForm[X] = extractor match {
		case _ :IdentityExtractor[_] => this.asInstanceOf[SQLReadForm[X]]
//		case const :ConstantExtractor[_, _] => SQLReadForm.const(const.constant.asInstanceOf[X])
		case req :RequisiteExtractor[_, _] => mapNull(req.getter.asInstanceOf[T => X])
//		case _ :EmptyExtractor[_, _] => SQLReadForm.none(readColumns)
		case _ => flatMapNull(extractor.optional)
	}

	/** Lifts this form to represent `Option[T]`. The created form maps all values returned by this form using
	  * `Option(...)`. This means that `null` values (actual JVM nulls, not the somewhat arbitrary value provided
	  * by this form) are mapped to `Some(None)`, while a returned `None` indicates that this form returned `None`.
	  * Basically this means that the returned form uses this form's `opt` method as its `apply` implementation.
	  */
	def asOpt :SQLReadForm[Option[T]] = SQLReadForm.OptionReadForm(this)



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
	def *[O](other :SQLReadForm[O]) :SQLReadForm[(T, O)] = Tuple2ReadForm(this, other)

	/** Combines this form with a `SQLWriteForm` to create a read/write `SQLForm[O]`. */
	def &&[O>:T](write :SQLWriteForm[O]) :SQLForm[O] = SQLForm.combine[O](this, write)



	def compatible(other :SQLReadForm[_]) :Boolean = this == other


	def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def toString :String = this.innerClassName
}






object SQLReadForm extends ScalaReadForms {
	/** Summons an implicit `SQLReadForm[T].` */
	def apply[T :SQLReadForm] :SQLReadForm[T] = implicitly[SQLReadForm[T]]



	/** Creates a form reading zero columns and always returning the provided value from its `opt` method.
	  * If `value` is `None`, the implicit `NullValue[T]` is used by the form's `apply` method to stand in.
	  */
	def const[T :NullValue](value :Option[T]) :SQLReadForm[T] = new ConstReadForm(value)

//	/** Creates a form reading zero columns and always returning the provided value from its `opt` method.
//	  * If `value` is `None`, the implicit `NullValue[T]` is used by the form's `apply` method to stand in.
//	  * This method variant allows specifying the number of 'read' columns returned by `readColumns`.
//	  */
//	def const[T :NullValue](value :Option[T], readColumns :Int) :SQLReadForm[T] = new ConstReadForm(value, readColumns)

	/** Creates a form reading zero columns and always returning the provided value from its `opt` method.
	  * If `value` is `None`, the `apply` method will return the provided `nullValue` instead.
	  */
	def const[T](value :Option[T], nullValue :T) :SQLReadForm[T] = new ConstReadForm(value)(NullValue(nullValue))

	/** Creates a form reading zero columns and always returning `Some(value)` from its `opt` method.
	  * The `nullValue` of the created form, while largely irrelevant, is likewise defined as `value`.
	  */
	def const[T](value :T) :SQLReadForm[T] = new ConstReadForm(Some(value))(NullValue(value))

	/** Creates a form reading zero columns and always returning from its `opt` method the value obtained
	  * by reevaluating the given expression. An implicitly provided null value is used by its `nullValue` method,
	  * to which the `apply` method delegates when the former yields `None`. The expression must be thread safe.
	  */
	def eval[T :NullValue](value : =>Option[T]) :SQLReadForm[T] = new EvalReadForm(value)

	/** Creates a form reading zero columns and always returning from its `opt` method the value obtained
	  * by reevaluating the given expression. An explicitly provided null value is used by its `nullValue` method,
	  * to which the `apply` method delegates when the former yields `None`. The expression must be thread safe.
	  */
	def eval[T](value: =>Option[T], nullValue :T) :SQLReadForm[T] = new EvalReadForm(value)(NullValue(nullValue))

	/** Creates a form reading zero columns and always returning from its `apply` method the value obtained
	  * by reevaluating the given expression. The result of `opt` is defined as `Some(value)`.
	  * The expression must be thread safe.
	  */
	def eval[T](value: =>T) :SQLReadForm[T] = new EvalReadForm(Some(value))(NullValue(value))

	/** Creates a proxy form which will delegate all methods to another form, returned by the given by-name argument.
	  * The expression is not evaluated until the form is actually needed. All mapping methods map this instance
	  * if the backing form expression has not been evaluated, and defer to the backing form it has been computed
	  * (essentially shedding the lazy proxy layer). The expression may be evaluated more than once if several
	  * threads trigger the its initialization, but the created form is thread safe.
	  */
	def Lazy[T](form : =>SQLReadForm[T]) :SQLReadForm[T] = new LazyReadForm(() => form)



	/** Maps the result of reading `S` with an implicit form to `T`. */
	def map[S, T](map :S => T)(implicit source :SQLReadForm[S], nulls :NullValue[T] = null) :SQLReadForm[T] =
		source match {
			case col :ColumnReadForm[_] =>
				ColumnReadForm.map(map)(col.asInstanceOf[ColumnReadForm[S]], nulls)
			case _ =>
				new MappedSQLReadForm(map)(source, if (nulls == null) source.nulls.map(map) else nulls)
		}

	def flatMap[S :SQLReadForm, T :NullValue](map :S => Option[T]) :SQLReadForm[T] =
		SQLReadForm[S] match {
			case col :ColumnReadForm[_] =>
				ColumnReadForm.flatMap(map)(col.asInstanceOf[ColumnReadForm[S]], NullValue[T])
		}

	def optMap[S, T](map :Option[S] => Option[T])(implicit source :SQLReadForm[S], nulls :NullValue[T]) :SQLReadForm[T] =
		source match {
			case col :ColumnReadForm[_] =>
				ColumnReadForm.optMap(map)(col.asInstanceOf[ColumnReadForm[S]], nulls)
			case _ =>
				new OptionMappedSQLReadForm[S, T](map)
		}

	def optMap[S :SQLReadForm, T](map :Option[S] => Option[T], nullValue : =>T) :SQLReadForm[T] =
		optMap(map)(SQLReadForm[S], NullValue.eval(nullValue))



	def seq[T](forms :Seq[SQLReadForm[T]]) :SQLReadForm[Seq[T]] = new SeqReadFormImpl[T](forms)






	/** An implicit value for the empty chain `@~`, which reads zero columns and simply returns `@~`. */
	implicit val EmptyChainReadForm :SQLReadForm[@~] = SQLForm.EmptyChainForm

	/** Provides an implicit form for the heterogeneous list (`Chain`) `I ~ L` as long as implicit forms for both
	  * `I` and `L` are available. */
	implicit def ChainReadForm[I <: Chain, L](implicit i :SQLReadForm[I], l :SQLReadForm[L]) :SQLReadForm[I ~ L] =
		new ChainReadForm[I, L] {
			override protected val init = i
			override protected val last = l
		}

	/** Provides an implicit form for the heterogeneous map indexed by literal types (`LiteralIndex`) `I &~ L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def LiteralIndexReadForm[I <: LiteralIndex :SQLReadForm, K <: Singleton :ValueOf, V :SQLReadForm]
			:SQLReadForm[I &~ (K, V)] =
		new LiteralIndexReadForm[&~, I, K, V] {
			override protected val init = SQLReadForm[I]
			override protected val value = SQLReadForm[V]
			override protected val key = valueOf[K]

			override protected[this] def cons(init :I, value :V) = init &~ (key -> value)
			override protected def symbol = "&~"
		}

	/** Provides an implicit form for the heterogeneous map indexed by string literals (`Record`) `I |# L`
	  * as long as implicit forms bor both `L` and `I` and `ValueOf[K]` are available.
	  */
	implicit def RecordReadForm[I <: Record :SQLReadForm, K <: String with Singleton :ValueOf, V :SQLReadForm]
			:SQLReadForm[I |# (K, V)] =
		new LiteralIndexReadForm[|#, I, K, V] {
			override protected val init = SQLReadForm[I]
			override protected val value = SQLReadForm[V]
			override protected val key = valueOf[K]

			override protected[this] def cons(init :I, value :V) = init |# (key -> value)
			override protected def symbol = "|#"
		}






	/** Implements `nullValue` to throw `NoSuchElementException`. */
	trait NotNullReadForm[T] extends SQLReadForm[T] {
		override def nullValue :T = throw new NoSuchElementException("No null value allowed for " + this)
	}






	private class ConstReadForm[+T :NullValue](value :Option[T], val readColumns :Int = 0) extends SQLReadForm[T] {
		private def get = value

		override def opt(position: Int)(res: ResultSet): Option[T] = value

		override def nullValue: T = NullValue.value[T]

		override def equals(that :Any) :Boolean = that match {
			case const :ConstReadForm[_] => (const eq this) || const.get == value && const.readColumns == readColumns
			case _ => false
		}

		override def hashCode :Int = value.hashCode

		override def toString :String = "<" + value
	}



	private class EvalReadForm[+T :NullValue](value: =>Option[T], val readColumns :Int = 0) extends SQLReadForm[T] {
		override def opt(position: Int)(res: ResultSet): Option[T] = value

		override def nullValue: T = NullValue.value[T]

		override def toString :String = "<=?"
	}






	private[schema] class OptionMappedSQLReadForm[S, +T](protected[this] final val map :Option[S] => Option[T])
	                                                    (implicit protected val source :SQLReadForm[S],
	                                                     implicit final override val nulls :NullValue[T])
		extends SQLReadForm[T]
	{
		override def readColumns: Int = source.readColumns

		override def nullValue :T = nulls.value

		override def opt(position: Int)(res: ResultSet): Option[T] =
			map(source.opt(position)(res))

		override def map[X :NullValue](fun :T => X) :SQLReadForm[X] = source.optMap(map(_).map(fun))

		override def flatMap[X :NullValue](fun :T => Option[X]) :SQLReadForm[X] = source.optMap(map(_).flatMap(fun))

		override def optMap[X :NullValue](fun :Option[T] => Option[X]) :SQLReadForm[X] = source.optMap(map andThen fun)

		override def toString :String = "<=" + source
	}



	class FlatMappedSQLReadForm[S, +T](protected[this] final val map :S => Option[T])
	                                  (implicit protected[this] val source :SQLReadForm[S],
	                                   final override val nulls :NullValue[T])
		extends SQLReadForm[T]
	{
		override def readColumns :Int = source.readColumns

		override def opt(position :Int)(res :ResultSet) :Option[T] = source.opt(position)(res).flatMap(map)

		override def nullValue :T = nulls.value

		override def map[X :NullValue](fun :T => X) :SQLReadForm[X] = source.flatMap(map.apply(_).map(fun))

		override def flatMap[X :NullValue](fun :T => Option[X]) :SQLReadForm[X] =
			source.flatMap(map.apply(_).flatMap(fun))

		override def optMap[X :NullValue](fun :Option[T] => Option[X]) :SQLReadForm[X] =
			source.flatMap(map andThen fun)

		override def toString :String = "<=" + source

	}



	class MappedSQLReadForm[S, +T](protected[this] final val map :S => T)
	                                              (implicit protected[this] val source :SQLReadForm[S],
	                                               implicit final override val nulls :NullValue[T])
		extends SQLReadForm[T]
	{
		override def readColumns :Int = source.readColumns

		override def opt(position :Int)(res :ResultSet) :Option[T] = source.opt(position)(res).map(map)

		override def nullValue :T = nulls.value

		override def map[X :NullValue](fun :T => X) :SQLReadForm[X] = source.map(map andThen fun)

		override def flatMap[X :NullValue](fun :T => Option[X]) :SQLReadForm[X] = source.flatMap(map andThen fun)

		override def optMap[X :NullValue](fun :Option[T] => Option[X]) :SQLReadForm[X] =
			source.flatMap((s :S) => fun(Option(map(s))))

		override def toString :String = "<=" + source
	}






	private[schema] class FallbackReadForm[+T](overrides :SQLReadForm[T], fallback :SQLReadForm[T]) extends SQLReadForm[T] {
		protected def first :SQLReadForm[T] = overrides
		protected def second :SQLReadForm[T] = fallback

		override def opt(position :Int)(res :ResultSet) :Option[T] =
			first.opt(position)(res) orElse second.opt(position)(res)

		override def nullValue :T = first.nullValue

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
		override def optMap[X :NullValue](fun :Option[T] => Option[X]) :SQLReadForm[X] = form.optMap(fun)
		override def asOpt :SQLReadForm[Option[T]] = form.asOpt


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

		override protected def form :SQLReadForm[T] = {
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

		override def map[X :NullValue](fun :T => X) :SQLReadForm[X] =
			if (fastAccess == null && initialized == null) new LazyReadForm(() => form.map(fun))
			else form.map(fun)

		override def flatMap[X :NullValue](fun :T => Option[X]) :SQLReadForm[X] =
			if (fastAccess == null && initialized == null) new LazyReadForm(() => form.flatMap(fun))
			else form.flatMap(fun)

		override def optMap[X :NullValue](fun :Option[T] => Option[X]) :SQLReadForm[X] =
			if (fastAccess == null && initialized == null) new LazyReadForm(() => form.optMap(fun))
			else form.optMap(fun)

		override def asOpt :SQLReadForm[Option[T]] =
			if (fastAccess == null && initialized == null) new LazyReadForm(() => form.asOpt)
			else form.asOpt

		override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] =
			if  (fastAccess == null && initialized == null) new LazyReadForm(() => super.orElse(fallback))
			else form orElse fallback

		override def *[O](other :SQLReadForm[O]) :SQLReadForm[(T, O)] =
			if (fastAccess == null && initialized == null) new LazyReadForm(() => super.*(other))
			else form * other

		override def &&[O >: T](write :SQLWriteForm[O]) :SQLForm[O] =
			if (fastAccess == null && initialized == null) super.&&(write)
			else form && write


		override def canEqual(that :Any) :Boolean =
			(that.asInstanceOf[AnyRef] eq this) || that.getClass == getClass && isInitialized

		override def toString :String =
			if (fastAccess == null && initialized == null) "<Lazy"
			else form.toString
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
			else Some(result.result)
		}


		override def apply(position: Int)(res: ResultSet): Seq[T] = {
			var i = position
			val result = List.newBuilder[T]
			for (form <- forms) {
				form.opt(i)(res) foreach { result += _  }
				i += form.readColumns
			}
			result.result
		}

		override def nullValue: Seq[T] = forms.map(_.nullValue)



		override def equals(that :Any) :Boolean = that match {
			case seq :SeqReadForm[_] =>
				(seq eq this) || seq.canEqual(this) && forms == seq.forms
			case _ => false
		}

		override def hashCode() :Int = forms.hashCode

		override def toString :String = forms.mkString("<Seq(",",",")")
	}



	private class SeqReadFormImpl[+T](val forms :Seq[SQLReadForm[T]]) extends SeqReadForm[T] {
		override val readColumns = super.readColumns
		override def toString = super.toString
	}





	private[schema] trait ChainReadForm[+T <: Chain, +H] extends SQLReadForm[T ~ H] {
		protected val init :SQLReadForm[T]
		protected def last :SQLReadForm[H]

		override val readColumns :Int = init.readColumns + last.readColumns

		override def opt(position :Int)(res :ResultSet) :Option[T ~ H] =
			for (t <- init.opt(position)(res); h <- last.opt(position + init.readColumns)(res)) yield t ~ h

		override val nullValue :T ~ H = init.nullValue ~ last.nullValue



		override def equals(that :Any) :Boolean = that match {
			case chain :ChainReadForm[_, _] =>
				(chain eq this) || (chain canEqual this) && chain.last == last && chain.init == init
			case _ => false
		}

		override def hashCode :Int = init.hashCode * 31 + last.hashCode

		override def toString :String = last match {
			case _ :ChainReadForm[_, _] => init.toString + "~(" + last + ")"
			case _ => init.toString + "~" + last
		}
	}



	private[schema] trait LiteralIndexReadForm[C[+A <: I, +B <: (K, V)] <: A &~ B, I <: LiteralIndex, K <: Singleton, V]
		extends SQLReadForm[I C (K, V)]
	{
		protected val init :SQLReadForm[I]
		protected val value :SQLReadForm[V]
		protected def key :K

		override val readColumns :Int = init.readColumns + value.readColumns

		protected[this] def cons(init :I, value :V) :I C (K, V)

		override def opt(position :Int)(res :ResultSet) :Option[I C (K, V)] =
			for (i <- init.opt(position)(res); v <- value.opt(position + init.readColumns)(res))
				yield cons(i, v)

		override val nullValue :I C (K, V) = cons(init.nullValue, value.nullValue)


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case index :LiteralIndexReadForm[_, _, _, _] if index canEqual this =>
				index.key == key && index.value == value && index.init == init
			case _ => false
		}

		override def hashCode :Int = (init.hashCode * 31 + key.hashCode) * 31 + value.hashCode

		protected def symbol :String

		override def toString :String = init.toString + symbol + "(" + key + "->" + value + ")"
	}







}

