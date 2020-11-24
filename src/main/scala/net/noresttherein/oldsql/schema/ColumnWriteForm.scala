package net.noresttherein.oldsql.schema

import java.sql.{JDBCType, PreparedStatement}

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLWriteForm.{ConstSQLWriteForm, CustomNullSQLWriteForm, ErrorSQLWriteForm, EvalSQLWriteForm, FlatMappedWriteForm, LazyWriteForm, MappedWriteForm, NonLiteralWriteForm, NullSQLWriteForm, NullValueSQLWriteForm, ProxyWriteForm, WriteFormNullGuard}
import net.noresttherein.oldsql.schema.forms.{SQLForms, SuperColumnForm}
import net.noresttherein.oldsql.schema.forms.SQLForms.SuperAdapterColumnForm






/** An `SQLReadForm` describing the write format of a simple type `T`, mapping to a single database column.
  * Aside from fixing the `writtenColumns` method to return `1` and introducing a property for the code
  * of the underlying SQL type, it enables static checks that the type `T` is a valid type for a single mapped column.
  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnForm]]
  */
@implicitNotFound("I do not know how to set a PreparedStatement parameter of type ${T}: " +
                  "missing implicit ColumnWriteForm[${T}].")
trait ColumnWriteForm[-T] extends SQLWriteForm[T] with SuperColumnForm { outer =>

	final override def writtenColumns = 1


	override def unmap[X](fun :X => T) :ColumnWriteForm[X] = ColumnWriteForm.map(fun)(this)

	override def flatUnmap[X](fun :X => Option[T]) :ColumnWriteForm[X] = ColumnWriteForm.flatMap(fun)(this)

	override def from[X](extractor :X =?> T) :ColumnWriteForm[X] = compose(extractor)

	override def compose[X](extractor :X =?> T) :ColumnWriteForm[X] = ColumnWriteForm(extractor)(this)


	override def toOpt :ColumnWriteForm[Option[T]] = SQLForms.OptionColumnWriteForm(this)

	override def nullSafe :ColumnWriteForm[T] =
		new ProxyWriteForm[T] with WriteFormNullGuard[T] with ColumnWriteForm[T] with SuperAdapterColumnForm {
			override def form = outer
			override def nullSafe :ColumnWriteForm[T] = this
		}

	override def withNull(implicit nulls :NullValue[T]) :ColumnWriteForm[T] =
		new CustomNullSQLWriteForm[T](this) with ColumnWriteForm[T] {
			override val sqlType = outer.sqlType
		}

	override def withNull(nullValue :T) :ColumnWriteForm[T] = withNull(NullValue(nullValue))


	override def <>[O <: T](read :SQLReadForm[O]) :SQLForm[O] = read match {
		case atom :ColumnReadForm[O] => this <> atom
		case _ => super.<>(read)
	}

	def <>[O <: T](read :ColumnReadForm[O]) :ColumnForm[O] = ColumnForm.join(this, read)



	override def compatible(other: SQLWriteForm[_]): Boolean = other match {
		case a :ColumnWriteForm[_] => a.sqlType == sqlType
		case _ => false
	}


}






object ColumnWriteForm {

	/** Summon an implicitly available `SQLWriteForm[T]`. */
	@inline def apply[X :ColumnWriteForm] :ColumnWriteForm[X] = implicitly[ColumnWriteForm[X]]



	/** Creates a non-literal `ColumnWriteForm` using the given function to set statement parameters
	  * based on a value of `T`.
	  * @param columnType the JDBC code for the SQL type of the parameter.
	  * @param write a function taking a statement, index of the parameter to set, and a value of the parameter.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	def apply[T](columnType :JDBCType)(write :(PreparedStatement, Int, T) => Unit) :ColumnWriteForm[T] =
		ColumnWriteForm(columnType, null)(write)

	/** Creates a non-literal `ColumnWriteForm` using the given function to set statement parameters
	  * based on a value of `T`.
	  * @param name the name of the form, as returned by its `toString` method.
	  * @param columnType the JDBC code for the SQL type of the parameter.
	  * @param write a function taking a statement, index of the parameter to set, and a value of the parameter.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	def apply[T](columnType :JDBCType, name :String)(write :(PreparedStatement, Int, T) => Unit) :ColumnWriteForm[T] =
		new DirectColumnWriteForm[T] with NonLiteralWriteForm[T] {
			override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
				write(statement, position, value)

			override val sqlType = columnType

			override val toString =
				if (name != null) name else "ColumnWriteForm(" + sqlType + ")@" + System.identityHashCode(this)
		}



	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `ColumnWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form.
	  */
	def eval[T :ColumnWriteForm](expr: => T, name :String = null) :ColumnWriteForm[Any] =
		evalopt(Some(expr), name)

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `ColumnWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form. If it yields
	  * `None`, the value carried by an implicitly available `NullValue` will be written instead, using the backing
	  * form's `set` method.
	  */
	def evalopt[T :ColumnWriteForm](value: => Option[T], name :String = null) :ColumnWriteForm[Any] =
		new EvalSQLWriteForm[T](value, name) with ColumnWriteForm[Any] {
			override val sqlType = ColumnWriteForm[T].sqlType
		}



	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `ColumnWriteForm[T]`.
	  */
	def const[T :ColumnWriteForm](value :T, name :String = null) :ColumnWriteForm[Any] =
		if (value == null)
			none[T]
		else
            new ConstSQLWriteForm[T](value, name) with ColumnWriteForm[Any] {
	            override val sqlType = ColumnWriteForm[T].sqlType
			}

	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `ColumnWriteForm[T]`.
	  */
	def constopt[T :ColumnWriteForm](value :Option[T], name :String = null) :ColumnWriteForm[Any] =
		if (value.isEmpty) none[T](name)
		else const(value.get, name)



	/** An `SQLWriteForm` ignoring its input and always writing `null` values using the implicitly given
	  * `SQLWriteForm[T]`. Null is defined here by the [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]
	  * type class, which must be also present for type `T`. This form is similar to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.none SQLWriteForm.none]], but passes the written value
	  * explicitly to the backing form's [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method,
	  * rather than delegate null handling to its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]],
	  * like the latter.
	  */ //consider: renaming to something like default
	def nulls[T :ColumnWriteForm :NullValue] :ColumnWriteForm[Any] = nulls[T](null)

	/** An `SQLWriteForm` ignoring its input and always writing `null` values using the implicitly given
	  * `SQLWriteForm[T]`. Null is defined here by the [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]
	  * type class, which must be also present for type `T`. This form is similar to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.none SQLWriteForm.none]], but passes the written value
	  * explicitly to the backing form's [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method,
	  * rather than delegate null handling to its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]],
	  * like the latter.
	  * @param name the name of the form, used in its `toString` implementation (and thrown exceptions).
	  */
	def nulls[T :ColumnWriteForm :NullValue](name :String) :ColumnWriteForm[Any] =
		new NullValueSQLWriteForm[T](name) with ColumnWriteForm[Any] {
			override val sqlType = ColumnWriteForm[T].sqlType
		}

	/** A `ColumnWriteForm` which will always write the 'null' value of type `T` using the implicitly available form.
	  * How `null` values are handled depends on the base form implementation of null-specific methods of `SQLWriteForm`
	  * interface, such as [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]]. The returned form
	  * will delegate all calls accepting `T` as an argument to their counterpart dedicated to `null` representation.
	  */
	def none[T :ColumnWriteForm] :ColumnWriteForm[Any] = ColumnWriteForm.none[T](null :String)

	/** A `ColumnWriteForm` which will always write the 'null' value of type `T` using the implicitly available form.
	  * How `null` values are handled depends on the base form implementation of null-specific methods of `SQLWriteForm`
	  * interface, such as [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]]. The returned form
	  * will delegate all calls accepting `T` as an argument to their counterpart dedicated to `null` representation.
	  */
	def none[T :ColumnWriteForm](name :String) :ColumnWriteForm[Any] =
		new NullSQLWriteForm[T](name) with ColumnWriteForm[Any] {
			override val sqlType = ColumnWriteForm[T].sqlType
		}



	/** A write form which will throw the given exception at every write attempt. */
	def error(raise: => Nothing, columnType :JDBCType = JDBCType.OTHER, name :String = null)
			:ColumnWriteForm[Any] =
		new ErrorSQLWriteForm[Any](1, raise, name) with ColumnWriteForm[Any] {
			override val sqlType = columnType
		}

	/** A dummy column form which throws an `UnsupportedOperationException` at each write attempt.
	  * Used as part of `ColumnForm.join` to convert a `ColumnReadForm` into a `ColumnForm` for situations
	  * where its write functionality is known not to be used. Be careful!
	  */
	def unsupported(columnType :JDBCType, name :String = null)(message :String) :ColumnWriteForm[Any] =
		error(throw new UnsupportedOperationException(message),
			columnType, if (name != null) name else "<UNSUPPORTED[" + columnType + "]"
		)

	/** A dummy column form which throws an `UnsupportedOperationException` at each write attempt.
	  * Used as part of `ColumnForm.join` to convert a `ColumnReadForm` into a `ColumnForm` for situations
	  * where its write functionality is known not to be used. Be careful!
	  */
	def unsupported(message :String) :ColumnWriteForm[Any] =
		unsupported(JDBCType.OTHER, "<UNSUPPORTED")(message)



	/** Composes an implicitly available write form `ColumnWriteForm[S]` with a given extractor to create
	  * a `ColumnWriteForm[T]`. This has the effect of calling either `map` or `flatMap`, depending on the type
	  * of the extractor.
	  */
	def apply[S :ColumnWriteForm, T](property :T =?> S) :ColumnWriteForm[T] =
		property match {
			case _ :IdentityExtractor[_] => this.asInstanceOf[ColumnWriteForm[T]]
			case const :ConstantExtractor[_, S @unchecked] => ColumnWriteForm.const(const.constant)
			case req :RequisiteExtractor[T @unchecked, S @unchecked] => map(req.getter)
			case _ :EmptyExtractor[_, _] => ColumnWriteForm.none[S]
			case _ => flatMap(property.optional)
		}

	/** Composes an implicitly available write form `ColumnWriteForm[S]` with a given extractor to create
	  * an `ColumnWriteForm[T]`. This has the effect of calling either `map` or `flatMap`, depending on the type
	  * of the extractor.
	  */
	def apply[S :ColumnWriteForm, T](name :String)(property :T =?> S) :ColumnWriteForm[T] =
		property match {
			case _ :IdentityExtractor[_] => this.asInstanceOf[ColumnWriteForm[T]]
			case const :ConstantExtractor[_, S @unchecked] => ColumnWriteForm.const(const.constant, name)
			case req :RequisiteExtractor[T @unchecked, S @unchecked] => map(name)(req.getter)
			case _ :EmptyExtractor[_, _] => ColumnWriteForm.none[S](name)
			case _ => flatMap(name)(property.optional)
		}

	/** Composes an implicitly available write form `ColumnWriteForm[S]` with a given getter function to create
	  * an `ColumnWriteForm[T]`. This function can in particular be used to present a `ColumnWriteForm` for the type
	  * of a property of some entity type as a write form for said entity type. Such forms can be later combined
	  * together with [[net.noresttherein.oldsql.schema.SQLWriteForm.combine SQLWriteForm.combine(forms)]] to create
	  * an `SQLWriteForm` for the whole entity.
	  */
	def map[S :ColumnWriteForm, T](map :T => S) :ColumnWriteForm[T] =
		ColumnWriteForm.map(null :String)(map)

	/** Composes an implicitly available write form `ColumnWriteForm[S]` with a given getter function to create
	  * a `ColumnWriteForm[T]`. This function can in particular be used to present a `ColumnWriteForm` for the type
	  * of a property of some entity type as a write form for said entity type. Such forms can be later combined
	  * together with [[net.noresttherein.oldsql.schema.SQLWriteForm.combine SQLWriteForm.combine(forms)]] to create
	  * an `SQLWriteForm` for the whole entity.
	  */
	def map[S :ColumnWriteForm, T](name :String)(map :T => S) :ColumnWriteForm[T] =
		new MappedWriteForm[S, T] with ColumnWriteForm[T] with SuperAdapterColumnForm {
			override val form = ColumnWriteForm[S]
			override val unmap = map
			override val toString = if (name != null) name else super[MappedWriteForm].toString
		}

	/** Composes an implicitly available write form `ColumnWriteForm[S]` with a given getter to create
	  * a `ColumnWriteForm[T]`. If the function returns `None`, the created form will use
	  * [[net.noresttherein.oldsql.schema.ColumnWriteForm.setNull setNull]] method of the base form instead of
	  * [[net.noresttherein.oldsql.schema.ColumnWriteForm.set set]].
	  */
	def flatMap[S :ColumnWriteForm, T](map :T => Option[S]) :ColumnWriteForm[T] =
		flatMap(null :String)(map)

	/** Composes an implicitly available write form `ColumnWriteForm[S]` with a given getter to create
	  * a `ColumnWriteForm[T]`. If the function returns `None`, the created form will use
	  * [[net.noresttherein.oldsql.schema.ColumnWriteForm.setNull setNull]] method of the base form instead of
	  * [[net.noresttherein.oldsql.schema.ColumnWriteForm.set set]].
	  */
	def flatMap[S :ColumnWriteForm, T](name :String)(map :T => Option[S]) :ColumnWriteForm[T] =
		new FlatMappedWriteForm[S, T] with ColumnWriteForm[T] with SuperAdapterColumnForm {
			override val form = ColumnWriteForm[S]
			override val unmap = map
			override val toString = if (name != null) name else super[FlatMappedWriteForm].toString
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
	def delayed[T](delayed: => ColumnWriteForm[T]) :ColumnWriteForm[T] =
		new LazyWriteForm[T] with LazyColumnWriteForm[T] {
			override protected[this] var init: () => SQLWriteForm[T] = () => delayed
		}






	/** Base trait for column forms which do not rely on other forms in implementations, but
	  * set parameters directly on the `PreparedStatement`. Returns "null" as the `null` literal from all
	  * null-specific literal methods and delegates all non-null literal methods to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.literal(value:T) literal]]`(value)`
	  * (essentially equating literals and inline literals).
	  */
	trait DirectColumnWriteForm[-T] extends ColumnWriteForm[T] {
		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			statement.setNull(position, sqlType.getVendorTypeNumber)

		override def literal(value :T, inline :Boolean) :String = literal(value)
		override def inlineLiteral(value: T): String = literal(value)

		override def nullLiteral(inline :Boolean) :String = nullLiteral
		override def nullLiteral: String = "null"
		override def inlineNullLiteral: String = nullLiteral
	}


	trait NonLiteralColumnWriteForm[-T] extends NonLiteralWriteForm[T] with ColumnWriteForm[T] {
		override def nullLiteral :String = "null"
		override def inlineNullLiteral :String = nullLiteral
	}






	private[schema] trait LazyColumnWriteForm[-T]
		extends LazyWriteForm[T] with ColumnWriteForm[T] with SuperAdapterColumnForm
	{
		override def form :ColumnWriteForm[T] = super[LazyWriteForm].form.asInstanceOf[ColumnWriteForm[T]]

		override def unmap[X](fun :X => T) :ColumnWriteForm[X] = form.unmap(fun)
		override def flatUnmap[X](fun :X => Option[T]) :ColumnWriteForm[X] = form.flatUnmap(fun)

		override def toOpt = form.toOpt
		override def nullSafe = form.nullSafe
		override def withNull(implicit nulls :NullValue[T]) = form.withNull
		override def withNull(nullValue :T) = form.withNull(nullValue)

		override def <>[O <: T](read :ColumnReadForm[O]) = read <> this

		override def toString :String = if (isInitialized) "Lazy(" + form + ")" else "<Lazy"
	}

}

