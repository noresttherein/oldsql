package net.noresttherein.oldsql.schema

import java.sql.{ResultSet, SQLException}

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.{FallbackColumnReadForm, FallbackReadForm, MappedSQLReadForm, Tuple2ReadForm}
import net.noresttherein.oldsql.slang._

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
	  * if no value is available, but the exact handling is completely up to the implementation. Note `null` column
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
	def apply(position :Int)(res :ResultSet) :T = opt(position)(res) getOrElse nullValue

	/** Attempts to read the column values from columns `&lt;position..position + this.readColumns` of the passed
	  * `ResultSet` and create an instance of `T`. If the values are unavailable (required columns carry `null` values),
	  * `None` is returned. It is a recommended practice to have the returned option reflect only the availability
	  * of the input values and not their validity. While not strictly required, the form should throw an exception
	  * if the values do not conform to expected constraints or the assembly process fails for any other reason.
	  * Similarly, all thrown `SQLException`s are propagated.
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

	/** Number of columns read by this form. This must be a constant as it is typically is used to calculate offsets
	  * for various forms once per `ResultSet` rather than per row. Naturally, there is no requirement for actual
	  * reading of all columns if the form can determine based on some indicator (such as a `null` primary key) that
	  * the value cannot be assembled from the given column set for the row.
	  */
	def readColumns :Int


	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. This method variant
	  * depends on implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] to provide the value of `X`
	  * to be used when `null` value(s) are read from the `ResultSet`. This guarantees that the given function will
	  * not be called for `null` arguments and allows handling of `null` values also when `T` is a value type without
	  * a natural `null` value.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.map]]
	  */
	def nullMap[X :NullValue](fun :T => X) :SQLReadForm[X] = map(fun, NullValue.Null[X])

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. If the underlying columns
	  * carry null values, passed `nullValue` is used instead of mapping. This guarantees that the given function will
	  * not be called for `null` arguments and allows handling of `null` values also when `T` is a value type without
	  * a natural `null` value.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullMap]]
	  */
	def map[X](fun :T => X, nullValue :X) :SQLReadForm[X] = MappedSQLReadForm((t :T) => Some(fun(t)), nullValue)(this)

	/** Maps the value of `T` read by this form to `X` in order to obtain a form for `X`. Note that the given
	  * function may be called for `null` arguments, even if the underlying columns have a ''not null'' constraint
	  * in case of outer join queries.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullMap]]
	  */
	def map[X](fun :T => X) :SQLReadForm[X] = MappedSQLReadForm((t :T) => Some(fun(t)), fun(nullValue))(this)



	/** Attempts to map the value of `T` read by this form to type `X` in order to produce a form for `X`.
	  * Unlike `map`, not all values of `T` may have an associated counterpart in `X`, in which the given function
	  * returns `None` and the new form defaults to its `nullValue`. This method variant relies on implicit
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] to provide the value of `X` which should be used
	  * when the underlying columns carry null values or a value of `X` cannot be assembled due to anticipated reasons.
	  * If the value cannot be assembled due to unforeseen circumstances or data errors, the form should
	  * throw an exception. The function is guaranteed not to be called for `null` arguments, with the created form
	  * using the implicit null value instead.
	  */
	def flatMap[X :NullValue](fun :T => Option[X]) :SQLReadForm[X] = flatMap(fun, NullValue.Null[X])

	/** Attempts to map the value of `T` read by this form to type `X` in order to produce a form for `X`.
	  * Unlike `map`, not all values of `T` may have an associated counterpart in `X`, in which the given function
	  * returns `None` and the new form defaults to the given `nullValue`.  The function is guaranteed not to be called
	  * for `null` arguments, with the created form using `nullValue` instead.
	  */
	def flatMap[X](fun :T => Option[X], nullValue :X) :SQLReadForm[X] = MappedSQLReadForm(fun, nullValue)(this)

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
	def *[O](other :SQLReadForm[O]) :SQLReadForm[(T, O)] = new Tuple2ReadForm()(this, other)

	/** Combines this form with a `SQLWriteForm` to create a read/write `SQLForm[O]`. */
	def &&[O>:T](write :SQLWriteForm[O]) :SQLForm[O] = SQLForm.combine[O](this, write)



	def compatible(other :SQLReadForm[_]) :Boolean = this == other


	override def toString :String = this.innerClassName
}






/** An `SQLReadForm` describing the format of a simple type `T`, mapping to a single database column.
  * Aside from fixing the `readColumns` method to return `1` and overloaded `apply` and `opt` for reading
  * the value from the column of the provided name, it enables static checks that the type `T` is a valid type
  * for a single mapped column.
  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnForm]]
  */
trait ColumnReadForm[+T] extends SQLReadForm[T] with BaseColumnForm {
	override final def readColumns = 1

	def apply(column :String)(res :ResultSet) :T = apply(res.findColumn(column))(res)

	override def opt(position :Int)(res :ResultSet) :Option[T] = {
		val t = apply(position)(res)
		if (res.wasNull) None else Option(t)
	}

	def opt(column :String)(res :ResultSet) :Option[T] = opt(res.findColumn(column))(res)


	override def nullMap[X :NullValue](fun :T => X) :ColumnReadForm[X] =
		map(fun, NullValue.Null[X])

	override def map[X](fun :T => X) :ColumnReadForm[X] =
		MappedSQLReadForm.column((t :T) => Some(fun(t)), fun(this.nullValue))(this)

	override def map[X](fun :T => X, nullValue :X) :ColumnReadForm[X] =
		MappedSQLReadForm.column((t :T) => Some(fun(t)), nullValue)(this)

	override def flatMap[X :NullValue](fun :T => Option[X]) :ColumnReadForm[X] =
		flatMap(fun, NullValue.Null[X])

	override def flatMap[X](fun :T => Option[X], nullValue :X) :ColumnReadForm[X] =
		MappedSQLReadForm.column(fun, nullValue)(this)


	override def asOpt :SQLReadForm[Option[T]] = SQLReadForm.OptionColumnReadForm(this)


	override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] = fallback match {
		case atom :ColumnReadForm[S] => this orElse atom
		case _ => super.orElse(fallback)
	}

	def orElse[S >: T](fallback :ColumnReadForm[S]) :ColumnReadForm[S] =
		if (fallback.sqlType != sqlType)
			throw new IllegalArgumentException(s"$this orElse $fallback: different sqlType ($sqlType vs ${fallback.sqlType}).")
		else
			new FallbackColumnReadForm[S](this, fallback)



	override def &&[O >: T](write :SQLWriteForm[O]) :SQLForm[O] = write match {
		case atom :ColumnWriteForm[O] => SQLForm.combine(this, atom)
		case _ => SQLForm.combine(this, write)
	}

	def &&[O >: T](write :ColumnWriteForm[O]) :ColumnForm[O] = SQLForm.combine(this, write)



	override def compatible(other: SQLReadForm[_]): Boolean = other match {
		case a :ColumnReadForm[_] => a.sqlType == sqlType
		case _ => false
	}

}





object SQLReadForm {
	/** Summons an implicit `SQLReadForm[T].` */
	def apply[T :SQLReadForm] :SQLReadForm[T] = implicitly[SQLReadForm[T]]

	/** Summons an implicit `ColumnReadForm[T]`. */
	def column[T :ColumnReadForm] :ColumnReadForm[T] = implicitly[ColumnReadForm[T]]



	/** Creates a form reading zero columns and always returning the provided value from its `opt` method.
	  * If `value` is `None`, the implicit `NullValue[T]` is used by the form's `apply` method to stand in.
	  */
	def const[T :NullValue](value :Option[T]) :SQLReadForm[T] = new ConstReadForm(value)

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


	def seq[T](forms :Seq[SQLReadForm[T]]) :SQLReadForm[Seq[T]] = new SeqReadFormImpl[T](forms)



	implicit def OptionReadForm[T :SQLReadForm] :SQLReadForm[Option[T]] =
		SQLReadForm[T].map(Option.apply, None)

	implicit def SomeReadForm[T :SQLReadForm] :SQLReadForm[Some[T]] =
		SQLReadForm[T].map(Some.apply)

	implicit def OptionColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Option[T]] =
		SQLReadForm.column[T].map(Option.apply, None)

	implicit def SomeColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Some[T]] =
		SQLReadForm.column[T].map(Some.apply)



	/** Provides an implicit form for the heterogeneous list (`Chain`) `T ~ H` as long as implicit forms for both
	  * `T` and `H` are available. */
	implicit def ChainReadForm[T <: Chain, H](implicit t :SQLReadForm[T], h :SQLReadForm[H]) :SQLReadForm[T ~ H] =
		new ChainReadForm[T, H] {
			override protected[this] val tail = t
			override protected[this] val head = h
		}

	/** An implicit value for the empty chain `@~`, which reads zero columns and simply returns `@~`. */
	implicit val EmptyChainReadForm :SQLReadForm[@~] = const(@~)






	/** Implements `nullValue` to throw `NoSuchElementException`. */
	trait NotNullReadForm[T] extends SQLReadForm[T] {
		override def nullValue :T = throw new NoSuchElementException("No null value allowed for " + this)
	}






	case class ConstReadForm[+T :NullValue](value :Option[T]) extends SQLReadForm[T] {
		override def opt(position: Int)(res: ResultSet): Option[T] = value

		override def readColumns: Int = 0

		override def nullValue: T = NullValue.Null[T]

		override def toString :String = "<" + value
	}



	private class EvalReadForm[+T :NullValue](value: =>Option[T]) extends SQLReadForm[T] {
		override def opt(position: Int)(res: ResultSet): Option[T] = value

		override def readColumns: Int = 0

		override def nullValue: T = NullValue.Null[T]

		override def toString :String = "<=?"
	}



	class MappedSQLReadForm[+T, S](val map :S=>Option[T], nullExpr : =>T)(implicit val source :SQLReadForm[S])
		extends SQLReadForm[T]
	{
		override def nullValue :T = nullExpr

		override def opt(position: Int)(res: ResultSet): Option[T] =
			source.opt(position)(res).flatMap(map)

		override def readColumns: Int = source.readColumns

		override def toString = s"<=$source"
	}



	object MappedSQLReadForm {
		def apply[T :NullValue, S :SQLReadForm](map :S=>Option[T]) :MappedSQLReadForm[T, S] =
			apply(map, NullValue.Null[T])

		def apply[T, S :SQLReadForm](map :S=>Option[T], nullValue : =>T) :MappedSQLReadForm[T, S] =
			implicitly[SQLReadForm[S]] match {
				case a :ColumnReadForm[_] =>
					column(map, nullValue)(a.asInstanceOf[ColumnReadForm[S]])
				case _ =>
					new MappedSQLReadForm[T, S](map, nullValue)
			}


		def column[T :NullValue, S :ColumnReadForm](map :S=>Option[T]) :MappedSQLReadForm[T, S] with ColumnReadForm[T] =
			column(map, NullValue.Null[T])

		def column[T, S :ColumnReadForm](map :S=>Option[T], nullValue : =>T) :MappedSQLReadForm[T, S] with ColumnReadForm[T] =
			new MappedSQLReadForm[T, S](map, nullValue) with ColumnReadForm[T] {
				override val source = implicitly[ColumnReadForm[S]]
				override def sqlType: Int = source.sqlType

				override def opt(position: Int)(res: ResultSet): Option[T] =
					super[MappedSQLReadForm].opt(position)(res)

			}
	}



	private[schema] class FallbackReadForm[+T](first :SQLReadForm[T], second :SQLReadForm[T]) extends SQLReadForm[T] {
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
	}



	private[schema] class FallbackColumnReadForm[+T](first :ColumnReadForm[T], second :ColumnReadForm[T])
		extends FallbackReadForm[T](first, second) with ColumnReadForm[T]
	{
		override val sqlType :Int = first.sqlType

		override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] = fallback match {
			case atom :ColumnReadForm[S] => orElse(atom)
			case _ => super.orElse(fallback)
		}

		override def orElse[S >: T](fallback :ColumnReadForm[S]) :ColumnReadForm[S] =
			if (sqlType != fallback.sqlType)
				throw new IllegalArgumentException(
					s"($this) orElse $fallback: different sqlType ($sqlType vs ${fallback.sqlType})."
				)
			else new FallbackColumnReadForm(first, second orElse fallback)
	}






	trait ProxyReadForm[+T] extends SQLReadForm[T] {
		protected def form :SQLReadForm[T]

		override def opt(position :Int)(res :ResultSet) :Option[T] = form.opt(position)(res)
		override def apply(position :Int)(res :ResultSet) :T = form(position)(res)

		override def nullMap[X :NullValue](fun :T => X) :SQLReadForm[X] = form.nullMap(fun)
		override def map[X](fun :T => X, nullValue :X) :SQLReadForm[X] = form.map(fun, nullValue)
		override def map[X](fun :T => X) :SQLReadForm[X] = form.map(fun)
		override def flatMap[X :NullValue](fun :T => Option[X]) :SQLReadForm[X] = form.flatMap(fun)
		override def flatMap[X](fun :T => Option[X], nullValue :X) :SQLReadForm[X] = form.flatMap(fun, nullValue)
		override def asOpt :SQLReadForm[Option[T]] = form.asOpt


		override def nullValue :T = form.nullValue
		override def readColumns :Int = form.readColumns

		def canEqual(that :Any) :Boolean = that.getClass == getClass

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

		override def nullMap[X :NullValue](fun :T => X) :SQLReadForm[X] =
			if (fastAccess == null && initialized == null) new LazyReadForm(() => form.nullMap(fun))
			else form.nullMap(fun)

		override def map[X](fun :T => X, nullValue :X) :SQLReadForm[X] =
			if (fastAccess == null && initialized == null) new LazyReadForm(() => form.map(fun, nullValue))
			else form.map(fun, nullValue)

		override def map[X](fun :T => X) :SQLReadForm[X] =
			if (fastAccess == null && initialized == null) new LazyReadForm(() => form.map(fun))
			else form.map(fun)

		override def flatMap[X :NullValue](fun :T => Option[X]) :SQLReadForm[X] =
			if (fastAccess == null && initialized == null) new LazyReadForm(() => form.flatMap(fun))
			else form.flatMap(fun)

		override def flatMap[X](fun :T => Option[X], nullValue :X) :SQLReadForm[X] =
			if (fastAccess == null && initialized == null) new LazyReadForm(() => form.flatMap(fun, nullValue))
			else form.flatMap(fun, nullValue)

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



		override def canEqual(that :Any) :Boolean = this eq that.asInstanceOf[AnyRef]

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

		override def toString :String = forms.mkString("<Seq(",",",")")
	}

	private case class SeqReadFormImpl[+T](forms :Seq[SQLReadForm[T]]) extends SeqReadForm[T] {
		override val readColumns = super.readColumns
		override def toString = super.toString
	}



	private[schema] trait AbstractTuple2ReadForm[L, R] extends SQLReadForm[(L, R)] {
		val _1  :SQLReadForm[L]
		val _2  :SQLReadForm[R]

		override def opt(position: Int)(res: ResultSet): Option[(L, R)] = {
			val l = _1.opt(position)(res)
			val r = _2.opt(position + _1.readColumns)(res)
			for (v1<-l; v2<-r) yield (v1, v2)
		}

		override def nullValue: (L, R) = (_1.nullValue, _2.nullValue)

		override def readColumns: Int = _1.readColumns + _2.readColumns

		override def toString = s"<(${_1},${_2})"
	}

	private[schema] class Tuple2ReadForm[L, R](implicit val _1  :SQLReadForm[L], val _2 :SQLReadForm[R]) extends AbstractTuple2ReadForm[L, R]



	private[schema] trait ChainReadForm[+T <: Chain, +H] extends SQLReadForm[T ~ H] {
		protected[this] val tail :SQLReadForm[T]
		protected[this] val head :SQLReadForm[H]

		override def opt(position :Int)(res :ResultSet) :Option[T ~ H] =
			for (t <- tail.opt(position)(res); h <- head.opt(position + tail.readColumns)(res)) yield t ~ h

		override def nullValue :T ~ H = tail.nullValue ~ head.nullValue

		override val readColumns :Int = tail.readColumns + head.readColumns

		override def toString :String = head match {
			case _ :ChainReadForm[_, _] => tail.toString + "~(" + head + ")"
			case _ => tail.toString + "~" + head
		}
	}








/*
		trait AbstractHListReadForm[+T >:Null <:HList] extends SQLReadForm[T] { self =>
			def ::[H](form :SQLReadForm[H]) :HListReadForm[H, T] = new HListReadFormImpl(form, this)
		}


		trait HListReadForm[+H, +T<:HList] extends AbstractHListReadForm[H::T] {
			val head :SQLReadForm[H]
			val tail :SQLReadForm[T]

			override def opt(position: Int)(res: ResultSet): Option[H::T] =
				for (h<-head.opt(position)(res); t<-tail.opt(position+head.readColumns)(res))
					yield h::t

			override def readColumns: Int = head.readColumns + tail.readColumns

			override def opt(res: PositionedResult): Option[H::T] = {
				val (first, rest) = (head.opt(res), tail.opt(res))
				for (h<-first; t<-rest) yield h::t
			}

			override def nullValue: H::T = head.nullValue::tail.nullValue

			override def toString = s"^$head::" + tail.toString
		}

		case class HListReadFormImpl[+H, +T<:HList](head :SQLReadForm[H], tail :SQLReadForm[T]) extends HListReadForm[H, T] {

			override def readColumns: Int = head.readColumns + tail.readColumns

			override def toString = super.toString
		}
	*/

}

