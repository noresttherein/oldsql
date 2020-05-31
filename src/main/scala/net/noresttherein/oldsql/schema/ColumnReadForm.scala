package net.noresttherein.oldsql.schema

import java.sql.ResultSet

import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.ColumnForm.JDBCSQLType
import net.noresttherein.oldsql.schema.ColumnReadForm.FallbackColumnReadForm
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.{ConstReadForm, EvalReadForm, FallbackReadForm, FlatMappedSQLReadForm, LazyReadForm, MappedSQLReadForm, NullReadForm, OptionMappedSQLReadForm}



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
		if (res.wasNull) None else Option(t)
	}

	def opt(column :String)(res :ResultSet) :Option[T] = opt(res.findColumn(column))(res)


	override def map[X :NullValue](fun :T => X) :ColumnReadForm[X] =
		ColumnReadForm.map(fun)(this, NullValue[X])

	override def map[X](fun :T => X, nullValue :X) :ColumnReadForm[X] = map(fun)(NullValue(nullValue))

	override def mapNull[X](fun :T => X) :ColumnReadForm[X] = map(fun)(nulls.map(fun))



	override def flatMap[X :NullValue](fun :T => Option[X]) :ColumnReadForm[X] =
		ColumnReadForm.flatMap(fun)(this, NullValue[X])

	override def flatMap[X](fun :T => Option[X], nullValue :X) :ColumnReadForm[X] = flatMap(fun)(NullValue(nullValue))

	override def flatMapNull[X](fun :T => Option[X]) :ColumnReadForm[X] = flatMap(fun)(nulls.flatMap(fun))



	override def optMap[X :NullValue](fun :Option[T] => Option[X]) :ColumnReadForm[X] =
		ColumnReadForm.optMap(fun)(this, NullValue[X])

	override def optMap[X](fun :Option[T] => Option[X], nullValue :X) :SQLReadForm[X] = optMap(fun)(NullValue(nullValue))

	override def andThen[X](extractor :T =?> X) :SQLReadForm[X] = extractor match {
		case _ :IdentityExtractor[_] => this.asInstanceOf[SQLReadForm[X]]
		case const :ConstantExtractor[_, _] => ColumnReadForm.const(sqlType, const.constant.asInstanceOf[X])
		case req :RequisiteExtractor[_, _] => mapNull(req.getter.asInstanceOf[T => X])
		case _ :EmptyExtractor[_, _] => ColumnReadForm.none(sqlType)
		case _ => flatMapNull(extractor.optional)
	}



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



	/** Creates a proxy form which will delegate all methods to another form, returned by the given by-name argument.
	  * The expression is not evaluated until the form is actually needed. All mapping methods map this instance
	  * if the backing form expression has not been evaluated, and defer to the backing form it has been computed
	  * (essentially shedding the lazy proxy layer). The expression may be evaluated more than once if several
	  * threads trigger the its initialization, but the created form is thread safe.
	  */
	def Lazy[T](init: =>ColumnReadForm[T]) :ColumnReadForm[T] =
		new LazyReadForm[T](() => init) with LazyColumnReadForm[T]



	def map[S, T](map :S => T)(implicit source :ColumnReadForm[S], nulls :NullValue[T] = null) :ColumnReadForm[T] =
		new MappedSQLReadForm[S, T](map) with MappedColumnReadForm[S, T]

	def flatMap[S :ColumnReadForm, T :NullValue](map :S => Option[T]) :ColumnReadForm[T] =
		new FlatMappedSQLReadForm[S, T](map) with FlatMappedColumnReadForm[S, T]

	def optMap[S :ColumnReadForm, T :NullValue](map :Option[S] => Option[T]) :ColumnReadForm[T] =
		new OptionMappedSQLReadForm[S, T](map) with OptionMappedColumnReadForm[S, T]

	def optMap[S :ColumnReadForm, T](map :Option[S] =>Option[T], nullValue : =>T) :ColumnReadForm[T] =
		optMap(map)(implicitly[ColumnReadForm[S]], NullValue.eval(nullValue))




	implicit def OptionColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Option[T]] =
		ColumnReadForm[T].map(Option.apply, None)

	implicit def SomeColumnReadForm[T :ColumnReadForm] :ColumnReadForm[Some[T]] =
		ColumnReadForm[T].mapNull(Some.apply)






	private[schema] trait LazyColumnReadForm[T] extends LazyReadForm[T] with ColumnReadForm[T] {
		protected override def form :ColumnReadForm[T] = super.form.asInstanceOf[ColumnReadForm[T]]

		override def sqlType :JDBCSQLType = form.sqlType

		protected override def read(position :Int)(res :ResultSet) :T = form.read(position)(res)

		override def map[X :NullValue](fun :T => X) :ColumnReadForm[X] =
			if (isInitialized) form.map(fun)
			else Lazy(form.map(fun))

		override def flatMap[X :NullValue](fun :T => Option[X]) :ColumnReadForm[X] =
			if (isInitialized) form.flatMap(fun)
			else Lazy(form.flatMap(fun))

		override def optMap[X :NullValue](fun :Option[T] => Option[X]) :ColumnReadForm[X] =
			if (isInitialized) form.optMap(fun)
			else Lazy(form.optMap(fun))

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






	private[schema] trait OptionMappedColumnReadForm[S, +T]
		extends OptionMappedSQLReadForm[S, T] with SQLReadForm[T] with ColumnReadForm[T]
	{
		private def form :ColumnReadForm[S] = source.asInstanceOf[ColumnReadForm[S]]

		override def sqlType: JDBCSQLType = form.sqlType

		override def read(position :Int)(res :ResultSet) :T = {
			val s = form.friendRead(position)(res)
			if (res.wasNull)
				null.asInstanceOf[T] //safe cast, because erased and won't be passed out of apply/opt
			else
				map(Some(s)) match {
					case Some(x) => x
					case _ => null.asInstanceOf[T]
				}
		}

		override def apply(column: Int)(res: ResultSet): T = super[SQLReadForm].apply(column)(res)

		override def opt(position: Int)(res: ResultSet): Option[T] = map(source.opt(position)(res))

		override def map[X :NullValue](fun :T => X) :ColumnReadForm[X] = form.optMap(map(_).map(fun))

		override def flatMap[X :NullValue](fun :T => Option[X]) :ColumnReadForm[X] = form.optMap(map(_).flatMap(fun))

		override def optMap[X :NullValue](fun :Option[T] => Option[X]) :ColumnReadForm[X] =
			form.optMap((map :Option[S] => Option[T]) andThen fun)

		override def toString :String = source.toString + "=>"
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

		override def optMap[X :NullValue](fun :Option[T] => Option[X]) :ColumnReadForm[X] =
			form.flatMap((map :S => Option[T]) andThen fun)

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

		override def optMap[X :NullValue](fun :Option[T] => Option[X]) :ColumnReadForm[X] =
			form.flatMap((s :S) => fun(Option(map(s))))

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


}

