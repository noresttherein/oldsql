package net.noresttherein.oldsql.schema

import java.sql.{ResultSet, Types}

import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.ColumnForm.JDBCSQLType
import net.noresttherein.oldsql.schema.ColumnReadForm.{FallbackColumnReadForm, FlatMappedColumnReadForm, MappedColumnReadForm}
import net.noresttherein.oldsql.schema.ScalaReadForms.OptionReadForm
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.{ConstReadForm, EvalReadForm, FallbackReadForm, FlatMappedSQLReadForm, LazyReadForm, MappedSQLReadForm, NullReadForm, ProxyReadForm}



/** An `SQLReadForm` describing the format of a simple type `T`, mapping to a single database column.
  * Aside from fixing the `readColumns` method to return `1` and overloaded `apply` and `opt` for reading
  * the value from the column of the provided name, it enables static checks that the type `T` is a valid type
  * for a single mapped column.
  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnForm]]
  */
trait ColumnReadForm[+T] extends SQLReadForm[T] with SuperColumnForm {

	override final def readColumns = 1

	/** Target method of `apply` and `opt` which reads the value of the column at the given index in the ResultSet
	  * and returns it as-is, without any handling of `null` values. This method should ''not'' throw null-related
	  * exceptions: value types should return any value of their type, while reference types should return `null`.
	  * It is a low-level method exposed for the purpose of form implementations, and applications should use
	  * `apply` instead.
	  */
	protected def read(position :Int)(res :ResultSet) :T

	@inline final private[schema] def friendRead(position :Int)(res :ResultSet) :T = read(position)(res)

	override def apply(position :Int)(res :ResultSet) :T = {
		val t = read(position)(res)
		if (res.wasNull) nullValue else t
	}

	def apply(column :String)(res :ResultSet) :T = apply(res.findColumn(column))(res)

	override def opt(position :Int)(res :ResultSet) :Option[T] = {
		val t = read(position)(res)
		if (res.wasNull) None else Some(t)
	}

	def opt(column :String)(res :ResultSet) :Option[T] = opt(res.findColumn(column))(res)


	override def map[X :NullValue](fun :T => X) :ColumnReadForm[X] = {
		val nullValue = NullValue[X] match {
			case null => nulls.map(fun)
			case nulls => nulls
		}
		new MappedSQLReadForm[T, X](fun)(this, nullValue) with MappedColumnReadForm[T, X]
	}

	override def map[X](fun :T => X, nullValue :X) :ColumnReadForm[X] = map(fun)(NullValue(nullValue))

	override def nullMap[X](fun :T => X) :ColumnReadForm[X] = map(fun)(nulls.map(fun))



	override def flatMap[X :NullValue](fun :T => Option[X]) :ColumnReadForm[X] = {
		val nullValue = NullValue[X] match {
			case null => nulls.flatMap(fun)
			case nulls => nulls
		}
		new FlatMappedSQLReadForm[T, X](fun)(this, nullValue) with FlatMappedColumnReadForm[T, X]
	}

	override def flatMap[X](fun :T => Option[X], nullValue :X) :ColumnReadForm[X] = flatMap(fun)(NullValue(nullValue))

	override def nullFlatMap[X](fun :T => Option[X]) :ColumnReadForm[X] = flatMap(fun)(nulls.flatMap(fun))



	override def to[X :NullValue](f :T =?> X) :ColumnReadForm[X] = f match {
		case _ :EmptyExtractor[_, _] => ColumnReadForm.nulls(sqlType)
		case _ :OptionalExtractor[_, _] => flatMap(f.optional)
		case _ :IdentityExtractor[_] => this.asInstanceOf[ColumnReadForm[X]]
		case const :ConstantExtractor[_, X @unchecked] => ColumnReadForm.const(sqlType, const.constant)
		case _ :RequisiteExtractor[_, _] => map(f.requisite.get)
		case _ => flatMap(f.optional)
	}

	override def to[X](f :T =?> X, nullValue :X) :ColumnReadForm[X] = to(f)(NullValue(nullValue))

	override def nullTo[X](f :T =?> X) :ColumnReadForm[X] = to(f)(nulls.extract(f))



	override def toOpt :ColumnReadForm[Option[T]] = ColumnReadForm.OptionColumnReadForm(this)


	override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] = fallback match {
		case atom :ColumnReadForm[S] => this orElse atom
		case _ => super.orElse(fallback)
	}

	def orElse[S >: T](fallback :ColumnReadForm[S]) :ColumnReadForm[S] =
		if (fallback.sqlType != sqlType)
			throw new IllegalArgumentException(s"$this orElse $fallback: different sqlType ($sqlType vs ${fallback.sqlType}).")
		else
			new FallbackColumnReadForm[S](this, fallback)



	override def <>[O >: T](write :SQLWriteForm[O]) :SQLForm[O] = write match {
		case atom :ColumnWriteForm[O] => this <> atom
		case _ => super.<>(write)
	}

	def <>[O >: T](write :ColumnWriteForm[O]) :ColumnForm[O] = ColumnForm.combine(this, write)



	override def compatible(other: SQLReadForm[_]): Boolean = other match {
		case a :ColumnReadForm[_] => a.sqlType == sqlType
		case _ => false
	}



}






object ColumnReadForm {

	/** Summons an implicit `ColumnReadForm[T]`. */
	def apply[T :ColumnReadForm] :ColumnReadForm[T] = implicitly[ColumnReadForm[T]]



	/** Creates a new `ColumnReadForm` using the given function to read the value from the result set.
	  * If the column value is null as defined by `ResultSet.wasNull`, the implicitly available `NullValue[T]`
	  * will be used as the result rather than the return value of the function.
	  * @param columnType JDBC code for the SQL type of the read column
	  * @param reader a function taking an SQL `ResultSet`, a column index, and reads the column as a value of `T`.
	  */
	def apply[T :NullValue](columnType :JDBCSQLType)(reader :(ResultSet, Int) => T) :ColumnReadForm[T] =
		new ColumnReadForm[T] {
			protected override def read(position :Int)(res :ResultSet) = reader(res, position)
			override val nulls = NullValue[T]
			override def nullValue = nulls.value
			override val sqlType = columnType
		}



	/** Creates a dummy form which produces no values. Every call to `opt` will return `None`, while `apply`
	  * will always return the implicitly available `NullValue[T]`.
	  */
	def nulls[T :NullValue](jdbcType :JDBCSQLType) :ColumnReadForm[T] =
		new NullReadForm[T](1) with ColumnReadForm[T] {
			override def opt(position :Int)(res :ResultSet) = None
			override def apply(position :Int)(res :ResultSet) = nulls.value
			override def read(position :Int)(res :ResultSet) = nulls.value
			override val sqlType = jdbcType
			override def toString = "NULL:" + sqlType + ">"
		}

	/** Creates a dummy form which produces no values. Every call to `opt` will return `None`, while `apply`
	  * will always throw a `NullPointerException`.
	  */
	def none[T](jdbcType :JDBCSQLType) :ColumnReadForm[T] = nulls(jdbcType)(NullValue.NotNull)



	/** Creates a dummy form which always produces the same value, never reading from the `ResultSet`.
	  * If `value` is `None`, implicit `NullValue[T]` will be used by `apply`.
	  */
	def opt[T :NullValue](jdbcType :JDBCSQLType, value :Option[T]) :ColumnReadForm[T] =
		new ConstReadForm[T](value, 1) with ColumnReadForm[T] {
			override def opt(position :Int)(res :ResultSet) = get
			override def apply(position :Int)(res :ResultSet) = super[ConstReadForm].apply(position)(res)
			override def read(position :Int)(res :ResultSet) :T = super[ConstReadForm].apply(position)(res)
			override val sqlType = jdbcType
			override def toString = value.toString + ":" + sqlType + ">"
		}

	/** Creates a dummy form which always produces the same value, never reading from the `ResultSet`.
	  * The `value` will be returned by `apply`, by `opt` as `Some(value)`, and as the form's `nullValue`.
	  * Note that if `value` is `null`, it will be treated as a valid return value rather than 'no value'
	  * and may result in `NullPointerException`s later if the handling code is not null-safe.
	  */
	def const[T](jdbcType :JDBCSQLType, value :T) :ColumnReadForm[T] = opt(jdbcType, Option(value))(NullValue(value))

	/** Creates a dummy form which always returns from its `opt` method the value obtained
	  * by reevaluating the given expression. An implicitly provided null value is used by its `nullValue` method,
	  * to which the `apply` method delegates when the former yields `None`. The expression must be thread safe.
	  */
	def evalopt[T :NullValue](jdbcType :JDBCSQLType, value: => Option[T]) :ColumnReadForm[T] =
		new EvalReadForm[T](value, 1) with ColumnReadForm[T] {
			override def opt(position :Int)(res :ResultSet) = super[EvalReadForm].opt(position)(res)
			override def apply(position :Int)(res :ResultSet) = super[EvalReadForm].apply(position)(res)
			override def read(position :Int)(res :ResultSet) = super[EvalReadForm].apply(position)(res)
			override val sqlType = jdbcType
			override def toString = sqlType.toString + "?=>"
		}

	/** Creates a form which always returns from its `apply` method the value
	  * obtained by reevaluating the given expression. The result of `opt` is defined as `Some(value)`.
	  * The expression must be thread safe.
	  */
	def eval[T](jdbcType :JDBCSQLType, value: => T) :ColumnReadForm[T] =
		evalopt(jdbcType, Some(value))(NullValue.eval(value))


	/** A form always throwing the given exception. This functions the same as `eval`, but can more clearly define intent. */
	def error(raise : => Nothing, jdbcType :JDBCSQLType = JDBCSQLType.fromInt(Types.OTHER)) :ColumnReadForm[Nothing] =
		eval(jdbcType, raise)

	/** A  form which throws an `UnsupportedOperationException` with the given message with every read attempt. */
	def unsupported(message :String, jdbcType :JDBCSQLType = JDBCSQLType.fromInt(Types.OTHER)) :ColumnReadForm[Nothing] =
		error(throw new UnsupportedOperationException(message), jdbcType)



	/** Creates a proxy form which will delegate all methods to another form, returned by the given by-name argument.
	  * The expression is not evaluated until the form is actually needed. All mapping methods map this instance
	  * if the backing form expression has not been evaluated, and defer to the backing form it has been computed
	  * (essentially shedding the lazy proxy layer). The expression may be evaluated more than once if several
	  * threads trigger the its initialization, but the created form is thread safe.
	  */
	def Lazy[T](init: =>ColumnReadForm[T]) :ColumnReadForm[T] =
		new LazyReadForm[T](() => init) with LazyColumnReadForm[T]



	def apply[S, T](map :S =?> T)(implicit source :ColumnReadForm[S], nulls :NullValue[T] = null) :ColumnReadForm[T] =
		source.to(map)

	def apply[S :ColumnReadForm, T](map :S =?> T, nullValue: => T) :ColumnReadForm[T] =
		apply(map)(ColumnReadForm[S], NullValue.eval(nullValue))

	def map[S, T](map :S => T)(implicit source :ColumnReadForm[S], nulls :NullValue[T] = null) :ColumnReadForm[T] =
		source.map(map)

	def map[S :ColumnReadForm, T](map :S => T, nullValue: => T) :ColumnReadForm[T] =
		this.map(map)(ColumnReadForm[S], NullValue.eval(nullValue))

	def flatMap[S, T](map :S => Option[T])(implicit source :ColumnReadForm[S], nulls :NullValue[T] = null) :ColumnReadForm[T] =
		source.flatMap(map)

	def flatMap[S :ColumnReadForm, T](map :S => Option[T], nullValue: => T) :ColumnReadForm[T] =
		flatMap(map)(ColumnReadForm[S], NullValue.eval(nullValue))



	implicit def OptionColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Option[T]] =
		new OptionColumnReadForm[T] { override val form = ColumnReadForm[T] }

	implicit def SomeColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Some[T]] =
		ColumnReadForm[T].nullMap(Some.apply)






	trait ProxyColumnReadForm[+T] extends ProxyReadForm[T] with ColumnReadForm[T] {
		protected override def form :ColumnReadForm[T]

		override def sqlType :JDBCSQLType = form.sqlType

		protected override def read(position :Int)(res :ResultSet) :T = form.read(position)(res)

		override def map[X :NullValue](fun :T => X) :ColumnReadForm[X] = form.map(fun)
		override def flatMap[X :NullValue](fun :T => Option[X]) :ColumnReadForm[X] = form.flatMap(fun)
		override def toOpt :ColumnReadForm[Option[T]] = form.toOpt
		override def orElse[S >: T](fallback :ColumnReadForm[S]) :ColumnReadForm[S] = form orElse fallback
		override def <>[O >: T](write :ColumnWriteForm[O]) :ColumnForm[O] = form <> write
	}



	private[schema] trait LazyColumnReadForm[T] extends LazyReadForm[T] with ProxyColumnReadForm[T] {
		protected override def form :ColumnReadForm[T] = super.form.asInstanceOf[ColumnReadForm[T]]

		override def map[X :NullValue](fun :T => X) :ColumnReadForm[X] =
			if (isInitialized) form.map(fun)
			else Lazy(form.map(fun))

		override def flatMap[X :NullValue](fun :T => Option[X]) :ColumnReadForm[X] =
			if (isInitialized) form.flatMap(fun)
			else Lazy(form.flatMap(fun))

		override def toOpt :ColumnReadForm[Option[T]] = if (isInitialized) form.toOpt else Lazy(form.toOpt)

		override def orElse[S >: T](fallback :ColumnReadForm[S]) :ColumnReadForm[S] =
			if (isInitialized) form orElse fallback
			else Lazy(form orElse fallback)

		override def <>[O >: T](write :ColumnWriteForm[O]) :ColumnForm[O] =
			if (isInitialized) form <> write
			else ColumnForm.Lazy(form <> write)

		override def toString :String =
			if (isInitialized) "Lazy(" + form + ")" else "Lazy>"
	}






	trait FlatMappedColumnReadForm[S, +T] extends FlatMappedSQLReadForm[S, T] with ColumnReadForm[T] {
		private def form :ColumnReadForm[S] = source.asInstanceOf[ColumnReadForm[S]]

		override def sqlType :JDBCSQLType = form.sqlType

		override protected def read(position :Int)(res :ResultSet) :T = {
			val s = form.friendRead(position)(res)
			if (res.wasNull)
				null.asInstanceOf[T]
			else
				map(s) match {
					case Some(x) => x
					case _ => null.asInstanceOf[T]
				}
		}

		override def opt(position :Int)(res :ResultSet) :Option[T] = source.opt(position)(res).flatMap(map)

		override def map[X :NullValue](fun :T => X) :ColumnReadForm[X] = form.flatMap(map(_).map(fun))

		override def flatMap[X :NullValue](fun :T => Option[X]) :ColumnReadForm[X] = form.flatMap(map(_).flatMap(fun))

		override def toString :String = source.toString + "=>"
	}



	trait MappedColumnReadForm[S, +T] extends MappedSQLReadForm[S, T] with ColumnReadForm[T] {
		private def form :ColumnReadForm[S] = source.asInstanceOf[ColumnReadForm[S]]

		override def sqlType :JDBCSQLType = form.sqlType

		override protected def read(position :Int)(res :ResultSet) :T = {
			val s = form.friendRead(position)(res)
			if (res.wasNull) null.asInstanceOf[T]
			else map(s)
		}

		override def map[X :NullValue](fun :T => X) :ColumnReadForm[X] =
			form.map((map :S => T) andThen fun)

		override def flatMap[X :NullValue](fun :T => Option[X]) :ColumnReadForm[X] =
			form.flatMap((map :S => T) andThen fun)

		override def toString :String = source.toString + "=>"
	}






	private[schema] class FallbackColumnReadForm[+T](first :ColumnReadForm[T], second :ColumnReadForm[T])
		extends FallbackReadForm[T](first, second) with ColumnReadForm[T]
	{
		override val sqlType :JDBCSQLType = first.sqlType

		protected override def read(position :Int)(res :ResultSet) :T = {
			var t = super.first.asInstanceOf[ColumnReadForm[T]].friendRead(position)(res)
			if (res.wasNull)
				t = super.second.asInstanceOf[ColumnReadForm[T]].friendRead(position)(res)
			t
		}

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

		override def toString :String = super[FallbackReadForm].toString
	}



	private[schema] trait OptionColumnReadForm[T] extends OptionReadForm[T] with ColumnReadForm[Option[T]] {
		override def form :ColumnReadForm[T]
		override def sqlType = form.sqlType

		protected override def read(position :Int)(res :ResultSet) :Option[T] = form.opt(position)(res)

		override def toString = super[OptionReadForm].toString
	}

}

