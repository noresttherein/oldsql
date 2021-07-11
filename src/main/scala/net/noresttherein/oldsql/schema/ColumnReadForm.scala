package net.noresttherein.oldsql.schema

import java.sql.{CallableStatement, JDBCType, ResultSet}

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.schema.ColumnReadForm.{FallbackColumnReadForm, FlatMappedColumnReadForm, MappedColumnReadForm}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull
import net.noresttherein.oldsql.schema.SQLReadForm.{ConstSQLReadForm, CustomOptSQLReadForm, CustomSQLReadForm, DerivedFlatMappedSQLReadForm, DerivedMappedSQLReadForm, DerivedReadForm, EvalSQLReadForm, FallbackSQLReadForm, FlatMappedSQLReadForm, LazySQLReadForm, MappedSQLReadForm, NotNullReadForm, NotNullSQLReadForm, NullValueSQLReadForm, ReadFormAdapter, ReadFormNullGuard}
import net.noresttherein.oldsql.schema.forms.{SQLForms, SuperColumnForm}
import net.noresttherein.oldsql.schema.forms.SQLForms.SuperAdapterColumnForm






/** An `SQLReadForm` describing the format of a simple type `T`, mapping to a single database column.
  * Aside from fixing the `readColumns` method to return `1` and overloaded `apply` and `opt` for reading
  * the value from the column of the provided name, it enables static checks that type `T` is a valid type
  * for a single mapped column.
  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnForm]]
  */
@implicitNotFound("I do not know how to read a column of type ${T} from a ResultSet: " +
                  "missing implicit ColumnReadForm[${T}].")
trait ColumnReadForm[+T] extends SQLReadForm[T] with SuperColumnForm {

	override final def readColumns = 1

	def apply(column :String)(res :ResultSet) :T = apply(res, res.findColumn(column))

	def opt(column :String)(res :ResultSet) :Opt[T] = opt(res, res.findColumn(column))

	protected override def errorMessage(position :Int, res :ResultSet) :String =
		"Null values not allowed for column " + res.getMetaData.getColumnName(position) + ":" + this + "."

	override def register(call :CallableStatement, position :Int) :Unit =
		call.registerOutParameter(position, sqlType)


	override def map[X :NullValue](fun :T => X) :ColumnReadForm[X] =
		ColumnReadForm.map(fun)(this, NullValue[X])

	override def map[X](fun :T => X, nullValue :X) :ColumnReadForm[X] = map(fun)(NullValue(nullValue))

	override def nullMap[X](fun :T => X) :ColumnReadForm[X] = map(fun)(nulls.map(fun))



	override def optMap[X :NullValue](fun :T => Option[X]) :ColumnReadForm[X] =
		ColumnReadForm.optMap(fun)(this, NullValue[X])

	override def optMap[X](fun :T => Option[X], nullValue :X) :ColumnReadForm[X] = optMap(fun)(NullValue(nullValue))

	override def nullOptMap[X](fun :T => Option[X]) :ColumnReadForm[X] = optMap(fun)(nulls.optMap(fun))



	override def to[X :NullValue](f :T =?> X) :ColumnReadForm[X] = ColumnReadForm(f)(this, NullValue[X])

	override def to[X](f :T =?> X, nullValue :X) :ColumnReadForm[X] = to(f)(NullValue(nullValue))

	override def nullTo[X](f :T =?> X) :ColumnReadForm[X] = to(f)(nulls.extract(f))



	override def toOpt :ColumnReadForm[Option[T]] = SQLForms.OptionColumnReadForm(this)

	override def notNull :ColumnReadForm[T] =
		new NotNullSQLReadForm[T]()(this) with ColumnReadForm[T] {
			override def notNull :this.type = this
			override def sqlType = form.asInstanceOf[ColumnReadForm[_]].sqlType
		}

	override def orElse[S >: T](fallback :SQLReadForm[S]) :SQLReadForm[S] = fallback match {
		case atom :ColumnReadForm[S] => this orElse atom
		case _ => super.orElse(fallback)
	}

	def orElse[S >: T](fallback :ColumnReadForm[S]) :ColumnReadForm[S] =
		if (fallback.sqlType != sqlType)
			throw new IllegalArgumentException(
				s"$this orElse $fallback: different sqlType ($sqlType vs ${fallback.sqlType})."
			)
		else
			new FallbackColumnReadForm[S](this, fallback)



	override def <>[O >: T](write :SQLWriteForm[O]) :SQLForm[O] = write match {
		case atom :ColumnWriteForm[O] => this <> atom
		case _ => super.<>(write)
	}

	def <>[O >: T](write :ColumnWriteForm[O]) :ColumnForm[O] = ColumnForm.combine(write, this)



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
	def apply[T :NullValue](columnType :JDBCType)(reader :(ResultSet, Int) => T) :ColumnReadForm[T] =
		ColumnReadForm[T](columnType, null)(reader)

	/** Creates a new `ColumnReadForm` using the given function to read the value from the result set.
	  * If the column value is null as defined by `ResultSet.wasNull`, the implicitly available `NullValue[T]`
	  * will be used as the result rather than the return value of the function.
	  * @param columnType JDBC code for the SQL type of the read column.
	  * @param name the name of the form returned from its `toString` method.
	  * @param reader a function taking an SQL `ResultSet`, a column index, and reads the column as a value of `T`.
	  */
	def apply[T :NullValue](columnType :JDBCType, name :String)(reader :(ResultSet, Int) => T) :ColumnReadForm[T] =
		new CustomColumnReadForm[T](columnType, name)(reader)

	/** Creates a new `ColumnReadForm` using the given function to read an optional value from the result set.
	  * @param columnType JDBC code for the SQL type of the read column.
	  * @param name the name of the form returned from its `toString` method.
	  * @param read a function taking an SQL `ResultSet`, a column position, and reads the value of the column.
	  */
	def opt[T :NullValue](columnType :JDBCType, name :String = null)
	                     (read :(ResultSet, Int) => Opt[T]) :ColumnReadForm[T] =
		new CustomOptColumnReadForm[T](columnType, name)(read)



	/** Creates a form which always returns from its `apply` method the value
	  * obtained by reevaluating the given expression. The result of `opt` is defined as `Some(value)`.
	  * The expression must be thread safe.
	  */
	def eval[T](jdbcType :JDBCType, value: => T, name :String = "=_>") :ColumnReadForm[T] =
		evalopt(jdbcType, Some(value), name)(NullValue.eval(value))

	/** Creates a dummy form which always returns from its `opt` method the value obtained
	  * by reevaluating the given expression. An implicitly provided null value is used by its `nullValue` method,
	  * to which the `apply` method delegates when the former yields `None`. The expression must be thread safe.
	  */
	def evalopt[T :NullValue](jdbcType :JDBCType, value: => Opt[T], name :String = "=_>") :ColumnReadForm[T] =
		new EvalSQLReadForm[T](value, 1, name) with ColumnReadForm[T] { outer =>
			override def notNull :ColumnReadForm[T] =
				new EvalSQLReadForm[T](value, 1, name)(NotNull)
					with ColumnReadForm[T] with ReadFormNullGuard[T]
				{
					override val sqlType = outer.sqlType
				}
			override val sqlType = jdbcType
		}



	/** Creates a dummy form which always produces the same value, never reading from the `ResultSet`.
	  * The `value` will be returned by `apply`, by `opt` as `Some(value)`, and as the form's `nullValue`.
	  * Note that if `value` is `null`, it will be treated as a valid return value rather than 'no value'
	  * and may result in `NullPointerException`s later if the handling code is not null-safe.
	  */
	def const[T](jdbcType :JDBCType, value :T, name :String = null) :ColumnReadForm[T] =
		constopt(jdbcType, Option(value), name)(NullValue(value))

	/** Creates a dummy form which always produces the same value, never reading from the `ResultSet`.
	  * If `value` is `None`, implicit `NullValue[T]` will be used by `apply`.
	  */
	def constopt[T :NullValue](jdbcType :JDBCType, value :Opt[T], name :String = null) :ColumnReadForm[T] =
		new ConstSQLReadForm[T](value, 1, name) with ColumnReadForm[T] {
			override def notNull :ColumnReadForm[T] = value match {
				case Got(null) => error(sqlType, toString + ".notNull") {
					throw new NullPointerException("Cannot return a null value from " + this + ".opt.")
				}
				case _ if nulls == NotNull => this
				case _ => constopt[T](sqlType, value, name)(NotNull)
			}
			override val sqlType = jdbcType
		}




	/** A form always throwing the given exception instead of producing a value. Note that this applies also to
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm!.opt opt]] method.
	  * See [[net.noresttherein.oldsql.schema.ColumnReadForm.none none]] and
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm.defaults defaults]] if you wish the form to simply return `None`.
	  * This functions the same way as `eval`, but can more clearly define intent.
	  */
	def error(raise: => Nothing) :ColumnReadForm[Nothing] = error(JDBCType.OTHER, "ERROR>")(raise)

	/** A form always throwing the given exception instead of producing a value. Note that this applies also to
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm!.opt opt]] method.
	  * See [[net.noresttherein.oldsql.schema.ColumnReadForm.none none]] and
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm.defaults defaults]] if you wish the form to simply return `None`.
	  * This functions the same way as `eval`, but can more clearly define intent.
	  */
	def error(jdbcType :JDBCType, name :String)(raise: => Nothing)
			:ColumnReadForm[Nothing] =
		eval(jdbcType, raise, if (name != null) name else "ERROR>")

	/** A  form which throws an `UnsupportedOperationException` with the given message with every read attempt.
	  * This is a simple shorthand for [[net.noresttherein.oldsql.schema.ColumnReadForm.error error]].
	  */
	def unsupported(jdbcType :JDBCType, name :String = null)(message :String) :ColumnReadForm[Nothing] =
		error(jdbcType, if (name != null) name else "UNSUPPORTED[" + jdbcType + "]>") {
			throw new UnsupportedOperationException(message)
		}

	/** A  form which throws an `UnsupportedOperationException` with the given message with every read attempt.
	  * This is a simple shorthand for [[net.noresttherein.oldsql.schema.ColumnReadForm.error error]].
	  */
	def unsupported(message :String) :ColumnReadForm[Nothing] =
		unsupported(JDBCType.OTHER, "UNSUPPORTED>")(message)



	/** Creates a dummy form which produces no values. Every call to `opt` will return `None`, while `apply`
	  * will always return the implicitly available `NullValue[T]`. Note that it, being a column form, still represents
	  * a single column in the result set, unlike [[net.noresttherein.oldsql.schema.SQLReadForm.defaults SQLReadForm.defaults]]
	  * which, by default, consists of zero columns. As such, it only suppresses a present value rather than
	  * excludes the column from the schema.
	  */
	def defaults[T](jdbcType :JDBCType, name :String = null)(implicit nulls :NullValue[T]) :ColumnReadForm[T] =
		new NullValueSQLReadForm[T](1)(if (nulls == null) NullValue.NotNull else nulls) with ColumnReadForm[T] {

			protected override def errorMessage(position :Int, res :ResultSet) :String =
				res.getMetaData.getColumnName(position) + ":" + this + " does not allow null values."

			override def notNull :ColumnReadForm[T] = try {
				if (nullValue == null) error(sqlType, toString + ".notNull") {
					throw new NullPointerException("Cannot return null from " + this + ".notNull.")
				} else this
			} catch { case _ :Exception => this }

			override val sqlType = jdbcType
			override val toString = if (name != null) name else "NULL:" + sqlType + ">"
		}

	/** Creates a dummy form which produces no values. Every call to `opt` will return `None`, while `apply`
	  * will always throw a `NullPointerException`. Note that it, being a column form, still represents
	  * a single column in the result set, unlike [[net.noresttherein.oldsql.schema.SQLReadForm.none SQLReadForm.none]]
	  * which, by default, consists of zero columns. As such, it only suppresses a present value rather than
	  * excludes the column from the schema.
	  */
	def none[T](jdbcType :JDBCType = JDBCType.NULL, name :String = null) :ColumnReadForm[T] =
		defaults(jdbcType, name)(NullValue.NotNull)

	val nulls :ColumnReadForm[Null] = SQLForms.NullForm



	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * the [[net.noresttherein.oldsql.schema.ColumnReadForm.defaults one in the source form]] will be
	  * mapped/flat mapped, depending on the mapping extractor.
	  */
	def apply[S, T](extract :S =?> T)(implicit base :ColumnReadForm[S], nulls :Maybe[NullValue[T]]) :ColumnReadForm[T] =
		extract match {
			case _ :EmptyExtractor[_, _] => ColumnReadForm.defaults(base.sqlType)(nulls.opt getOrElse NullValue.NotNull)
			case _ :OptionalExtractor[_, _] => optMap(extract.optional)
			case _ :IdentityExtractor[_] => base.asInstanceOf[ColumnReadForm[T]]
			case const :ConstantExtractor[_, T @unchecked] => ColumnReadForm.const(base.sqlType, const.constant)
			case _ :RequisiteExtractor[_, _] => map(extract.requisite.get)
			case _ => optMap(extract.optional)
		}

	/** Creates a new `ColumnReadForm` of a 'soft type' given as `name` argument by applying the given extractor
	  * to values read by an implicit base `ColumnReadForm[S]`. This is equivalent to
	  * `ColumnReadForm`[[net.noresttherein.oldsql.schema.ColumnReadForm.apply[S,T](extract* (extract)]],
	  * but the created form will equal any other `ColumnReadForm` created by this method if they have the same name
	  * and their base forms and [[net.noresttherein.oldsql.schema.ColumnReadForm.nulls null values]] are equal.
	  */
	def apply[S, T](name :String)(extract :S =?> T)
	               (implicit base :ColumnReadForm[S], nulls :Maybe[NullValue[T]]) :ColumnReadForm[T] =
		extract match {
			case _ :EmptyExtractor[_, _] =>
				defaults(base.sqlType, name)(nulls.opt getOrElse NullValue.NotNull)
			case _ :OptionalExtractor[_, _] => optMap(name)(extract.optional)
			case const :ConstantExtractor[_, T @unchecked] => ColumnReadForm.const(base.sqlType, const.constant, name)
			case req :RequisiteExtractor[S @unchecked, T @unchecked] => map(name)(req.getter)
			case _ => optMap(name)(extract.optional)
		}

	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and a by-name expression as its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  */
	def apply[S :ColumnReadForm, T](map :S =?> T, nullValue: => T) :ColumnReadForm[T] =
		apply(map)(ColumnReadForm[S], NullValue.eval(nullValue))

	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and a by-name expression as its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  */
	def apply[S :ColumnReadForm, T](name :String, map :S =?> T, nullValue: => T) :ColumnReadForm[T] =
		apply(name)(map)(ColumnReadForm[S], NullValue.eval(nullValue))



	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * the [[net.noresttherein.oldsql.schema.ColumnReadForm.defaults one in the source form]] will be
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.map mapped]].
	  */
	def map[S, T](f :S => T)(implicit base :ColumnReadForm[S], nulls :Maybe[NullValue[T]]) :ColumnReadForm[T] = {
		val nullValue = nulls.opt getOrElse base.nulls.map(f)
		new MappedSQLReadForm[S, T](f)(base, nullValue) with MappedColumnReadForm[S, T]
	}

	/** Creates a new `ColumnReadForm` of a 'soft type' given as `name` argument by applying the given function
	  * to values read by an implicit base `ColumnReadForm[S]`. This is equivalent to
	  * `ColumnReadForm`[[net.noresttherein.oldsql.schema.ColumnReadForm.map[S,T](f:S=>T)* .map(f)]],
	  * but the created form will equal any other `ColumnReadForm` created by this method if they have the same name
	  * and their base forms and [[net.noresttherein.oldsql.schema.ColumnReadForm.nulls null values]] are equal.
	  */
	def map[S, T](name :String)(f :S => T)
	             (implicit base :ColumnReadForm[S], nulls :Maybe[NullValue[T]]) :ColumnReadForm[T] =
	{
		val nullValue = nulls.opt getOrElse base.nulls.map(f)
		new DerivedMappedSQLReadForm[S, T](name, f)(base, nullValue) with DerivedMappedColumnReadForm[S, T]
	}

	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and a by-name expression as its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  */
	def map[S :ColumnReadForm, T](f :S => T, nullValue: => T) :ColumnReadForm[T] =
		ColumnReadForm.map(f)(ColumnReadForm[S], NullValue.eval(nullValue))

	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and a by-name expression as its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  */
	def map[S :ColumnReadForm, T](name :String, f :S => T, nullValue: => T) :ColumnReadForm[T] =
		ColumnReadForm.map(name)(f)(ColumnReadForm[S], NullValue.eval(nullValue))



	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * the [[net.noresttherein.oldsql.schema.ColumnReadForm.defaults one in the source form]] will be
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.optMap flat mapped]].
	  */
	def optMap[S, T](f :S => Option[T])
	                (implicit base :ColumnReadForm[S], nulls :Maybe[NullValue[T]]) :ColumnReadForm[T] =
	{
		val nullValue = nulls.opt getOrElse base.nulls.optMap(f)
		new FlatMappedSQLReadForm[S, T](f)(base, nullValue) with FlatMappedColumnReadForm[S, T]
	}

	/** Creates a new `ColumnReadForm` of a 'soft type' given as `name` argument by applying the given function
	  * (returning the value as an `Option`) to values read by an implicit base `ColumnReadForm[S]`. This is equivalent
	  * to `ColumnReadForm`[[net.noresttherein.oldsql.schema.ColumnReadForm.optMap[S,T](f:S=>Option[T])* .optMap(f)]],
	  * but the created form will equal any other `ColumnReadForm` created by this method if they have the same name
	  * and their base forms and [[net.noresttherein.oldsql.schema.ColumnReadForm.nulls null values]] are equal.
	  */
	def optMap[S, T](name :String)(f :S => Option[T])
	                (implicit base :ColumnReadForm[S], nulls :Maybe[NullValue[T]]) :ColumnReadForm[T] =
	{
		val nullValue = nulls.opt getOrElse base.nulls.optMap(f)
		new DerivedFlatMappedSQLReadForm[S, T](name, f)(base, nullValue) with DerivedFlatMappedColumnReadForm[S, T]
	}

	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and a by-name expression as its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  */
	def optMap[S :ColumnReadForm, T](f :S => Option[T], nullValue: => T) :ColumnReadForm[T] =
		optMap(f)(ColumnReadForm[S], NullValue.eval(nullValue))

	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and a by-name expression as its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  */
	def optMap[S :ColumnReadForm, T](name :String, f :S => Option[T], nullValue: => T) :ColumnReadForm[T] =
		optMap(name)(f)(ColumnReadForm[S], NullValue.eval(nullValue))



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
	def delayed[T](init: =>ColumnReadForm[T]) :ColumnReadForm[T] =
		new LazySQLReadForm[T](() => init) with LazyColumnReadForm[T] with SuperAdapterColumnForm






	/** Base trait for column forms which read data directly from the `ResultSet`. All implementations are assumed
	  * to return `nullValue` ''iff'' the underlying column was `null`. It is intended in particular for forms
	  * of value types or wrappers over single value types.
	  */
	trait DirectColumnReadForm[+T] extends ColumnReadForm[T] {

		/** Target method of `apply` and `opt` which reads the value of the column at the given index in the ResultSet
		  * and returns it as-is, without any handling of `null` values. This method should ''not'' throw null-related
		  * exceptions, unless `apply` and `opt` are also overriden: value types should return any value of their type,
		  * while reference types should return `null`. It is a low-level method exposed for the purpose
		  * of form implementations, and applications should use `apply` instead.
		  */
		protected def read(res :ResultSet, position :Int) :T

		override def apply(res :ResultSet, position :Int) :T = {
			val t = read(res, position)
			if (res.wasNull) try {
				nullValue
			} catch {
				case e :NullPointerException =>
					throw new NullPointerException(errorMessage(position, res)).initCause(e)
				case e :NoSuchElementException =>
					throw new NoSuchElementException(errorMessage(position, res)).initCause(e)
				case e :ClassCastException =>
					throw new ClassCastException(errorMessage(position, res)).initCause(e)
			} else t
		}

		override def opt(res :ResultSet, position :Int) :Opt[T] = {
			val t = read(res, position)
			if (res.wasNull) Lack else Got(t)
		}
	}



	/** A Convenience base `ColumnReadForm[T]` class which implements `nullValue` based on an implicit `NullValue[T]`
	  * (overriding also `nulls` in the process). */
	abstract class AbstractColumnReadForm[+T](implicit override val nulls :NullValue[T])
		extends ColumnReadForm[T]
	{
		override def nullValue :T = nulls.value
	}


	private[schema] trait ColumnReadFormAdapter[S, +T] extends ReadFormAdapter[T] with ColumnReadForm[T] {
		protected abstract override def form :ColumnReadForm[S] = super.form.asInstanceOf[ColumnReadForm[S]]
		override def sqlType :JDBCType = form.sqlType
	}
	
	
	private[schema] trait FlatMappedColumnReadForm[S, +T] 
		extends FlatMappedSQLReadForm[S, T] with ColumnReadFormAdapter[S, T] 
	{
		override def notNull :ColumnReadForm[T] =
			new FlatMappedSQLReadForm[S, T](map, toString + ".notNull")(form, NotNull)
				with ColumnReadFormAdapter[S, T] with ReadFormNullGuard[T]
	}
	
	private[schema] trait DerivedFlatMappedColumnReadForm[S, +T] 
		extends FlatMappedColumnReadForm[S, T] with DerivedReadForm[T] 
	{
		override def notNull :ColumnReadForm[T] =
			new DerivedFlatMappedSQLReadForm[S, T](name + ".notNull", map)(form, NotNull)
				with ColumnReadFormAdapter[S, T] with ReadFormNullGuard[T]
	}

	private[schema] trait MappedColumnReadForm[S, +T] 
		extends MappedSQLReadForm[S, T] with ColumnReadFormAdapter[S, T] 
	{
		override def notNull :ColumnReadForm[T] =
			new MappedSQLReadForm[S, T](map, toString + ".notNull")(form, NotNull)
				with ColumnReadFormAdapter[S, T] with ReadFormNullGuard[T]
	}
	
	private[schema] trait DerivedMappedColumnReadForm[S, +T] 
		extends MappedColumnReadForm[S, T] with DerivedReadForm[T]
	{
		override def notNull :ColumnReadForm[T] =
			new DerivedMappedSQLReadForm[S, T](name + ".notNull", map)(form, NotNull)
				with ColumnReadFormAdapter[S, T] with ReadFormNullGuard[T]
	}






	private[schema] class CustomColumnReadForm[+T :NullValue](override val sqlType :JDBCType, name :String = null)
	                                                         (reader :(ResultSet, Int) => T)
		extends CustomSQLReadForm[T](1, name)(reader) with DirectColumnReadForm[T]
	{
		protected override def read(res :ResultSet, position :Int) = reader(res, position)

		override def notNull :ColumnReadForm[T] =
			new CustomColumnReadForm[T](sqlType, toString + ".notNull")(reader)(NotNull) with NotNullReadForm[T] {
				override val nulls = NotNull
				override def apply(res :ResultSet, position :Int) :T = {
					val t = reader(res, position)
					if (res.wasNull) throw new NullPointerException("Cannot return null from " + this + ".")
					else t
				}
			}

		override val toString =
			if (name != null) name
			else "ColumnReadForm[" + sqlType + "]@" + hashCode
	}


	private[schema] class CustomOptColumnReadForm[+T :NullValue](override val sqlType :JDBCType, name :String = null)
	                                                            (reader :(ResultSet, Int) => Opt[T])
		extends CustomOptSQLReadForm[T](1, name)(reader) with ColumnReadForm[T]
	{
		override def notNull :ColumnReadForm[T] =
			new CustomOptColumnReadForm[T](sqlType, toString + ".notNull")(reader)(NotNull) with ReadFormNullGuard[T]

		override val toString =
			if (name != null) name
			else "ColumnReadForm[" + sqlType + "]@" + hashCode
	}



	private class FallbackColumnReadForm[+T](first :ColumnReadForm[T], second :ColumnReadForm[T])
		extends FallbackSQLReadForm[T](first, second) with ColumnReadForm[T]
	{
		override val sqlType :JDBCType = first.sqlType

		override def notNull :ColumnReadForm[T] = first.notNull orElse second.notNull

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


	private[schema] trait LazyColumnReadForm[T] extends LazySQLReadForm[T] with ColumnReadForm[T] {
		protected override def form :ColumnReadForm[T] = super.form.asInstanceOf[ColumnReadForm[T]]

		override def map[X :NullValue](fun :T => X) :ColumnReadForm[X] = form.map(fun)
		override def optMap[X :NullValue](fun :T => Option[X]) :ColumnReadForm[X] = form.optMap(fun)

		override def toOpt :ColumnReadForm[Option[T]] = form.toOpt
		override def notNull :ColumnReadForm[T] = form.notNull
		override def orElse[S >: T](fallback :ColumnReadForm[S]) :ColumnReadForm[S] = form orElse fallback
		override def <>[O >: T](write :ColumnWriteForm[O]) :ColumnForm[O] = form <> write

		override def toString :String =
			if (isInitialized) "Lazy(" + form + ")" else "Lazy>"
	}

}

