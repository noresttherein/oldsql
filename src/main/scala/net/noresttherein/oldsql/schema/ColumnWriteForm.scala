package net.noresttherein.oldsql.schema

import java.sql.{JDBCType, PreparedStatement}

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.{Opt, ReversedList}
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.ColumnWriteForm.SingletonColumnWriteForm
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLWriteForm.{ConstSQLWriteForm, CustomNullSQLWriteForm, CustomSQLWriteForm, ErrorSQLWriteForm, EvalSQLWriteForm, FlatMappedSQLWriteForm, GapSQLWriteForm, IgnoringWriteForm, LazyWriteForm, MappedSQLWriteForm, NonLiteralWriteForm, NotNullSQLWriteForm, NotNullWriteForm, NullifiedSQLWriteForm, NullValueSQLWriteForm, ProxyWriteForm, WriteFormNullGuard}
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

	override def param = "?"
	override def inlineParam = "?"

	override def split :Seq[ColumnWriteForm[T]] = this::Nil

	override def unmap[X](fun :X => T) :ColumnWriteForm[X] = ColumnWriteForm.map(fun)(this)

	override def flatUnmap[X](fun :X => Option[T]) :ColumnWriteForm[X] = ColumnWriteForm.flatMap(fun)(this)

	override def from[X](extractor :X =?> T) :ColumnWriteForm[X] = compose(extractor)

	override def compose[X](extractor :X =?> T) :ColumnWriteForm[X] = ColumnWriteForm(extractor)(this)

	override def toOpt :ColumnWriteForm[Option[T]] = SQLForms.OptionColumnWriteForm(this)

	override def nullSafe :ColumnWriteForm[T] =
		new SuperAdapterColumnForm with ProxyWriteForm[T] with SingletonColumnWriteForm[T] with WriteFormNullGuard[T] {
			override def form = outer
			override def nullSafe :ColumnWriteForm[T] = this
		}

	override def notNull :ColumnWriteForm[T] =
		new NotNullSQLWriteForm[T](this) with SingletonColumnWriteForm[T] {
			override def notNull :this.type = this
			override def sqlType = form.asInstanceOf[ColumnWriteForm[_]].sqlType
		}

	override def withNull(implicit nulls :NullValue[T]) :ColumnWriteForm[T] =
		new CustomNullSQLWriteForm[T](this) with SingletonColumnWriteForm[T] {
			override val sqlType = outer.sqlType
		}

	override def withNull(nullValue :T) :ColumnWriteForm[T] = withNull(NullValue(nullValue))


	override def <>[O <: T](read :SQLReadForm[O]) :SQLForm[O] = read match {
		case atom :ColumnReadForm[O] => this <> atom
		case _ => super.<>(read)
	}

	def <>[O <: T](read :ColumnReadForm[O]) :ColumnForm[O] = ColumnForm.combine(this, read)



	override def compatible(other: SQLWriteForm[_]): Boolean = other match {
		case a :ColumnWriteForm[_] => a.sqlType == sqlType
		case _ => false
	}


	private[schema] def superToString :String = super.toString
	private[schema] def superEquals(that :Any) :Boolean = super.equals(that)
	private[schema] def superHashCode :Int = super.hashCode
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
		new CustomColumnWriteForm[T](columnType, name)(write)



	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `ColumnWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form.
	  */
	def eval[T :ColumnWriteForm](expr: => T, name :String = null) :ColumnWriteForm[Any] =
		evalopt(Got(expr), name)

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `ColumnWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form. If it yields
	  * `None`, the value carried by an implicitly available `NullValue` will be written instead, using the backing
	  * form's `set` method.
	  */
	def evalopt[T :ColumnWriteForm](value: => Opt[T], name :String = null) :ColumnWriteForm[Any] =
		new EvalSQLWriteForm[T](value, name) with IgnoringColumnWriteForm { outer =>
			override def notNull :ColumnWriteForm[Any] =
				new EvalSQLWriteForm[T](value, name) with IgnoringColumnWriteForm with NotNullWriteForm[Any] {
					override val sqlType = outer.sqlType
				}
			override val sqlType = ColumnWriteForm[T].sqlType
		}



	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `ColumnWriteForm[T]`.
	  */
	def const[T :ColumnWriteForm](value :T, name :String = null) :ColumnWriteForm[Any] =
		if (value == null)
			none[T]
		else
            new ConstSQLWriteForm[T](value, name) with IgnoringColumnWriteForm { outer =>
	            override def notNull :ColumnWriteForm[Any] =
		            new ConstSQLWriteForm[T](value, toString + ".notNull")
			            with IgnoringColumnWriteForm with NotNullWriteForm[Any]
		            {
			            override val sqlType = outer.sqlType
		            }
	            override val sqlType = ColumnWriteForm[T].sqlType
			}

	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `ColumnWriteForm[T]`.
	  */
	def constopt[T :ColumnWriteForm](value :Opt[T], name :String = null) :ColumnWriteForm[Any] =
		if (value.isEmpty) none[T](name)
		else const(value.get, name)



	/** An `SQLWriteForm` ignoring its input and always writing `null` values using the implicitly given
	  * `SQLWriteForm[T]`. Null is defined here by the [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]
	  * type class, which must be also present for type `T`. This form is similar to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.none SQLWriteForm.none]], but passes the written value
	  * explicitly to the backing form's [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method,
	  * rather than delegate null handling to its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]],
	  * like the latter.
	  */
	def defaults[T :ColumnWriteForm :NullValue] :ColumnWriteForm[Any] = defaults[T](null)

	/** An `SQLWriteForm` ignoring its input and always writing `null` values using the implicitly given
	  * `SQLWriteForm[T]`. Null is defined here by the [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]
	  * type class, which must be also present for type `T`. This form is similar to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.none SQLWriteForm.none]], but passes the written value
	  * explicitly to the backing form's [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method,
	  * rather than delegate null handling to its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]],
	  * like the latter.
	  * @param name the name of the form, used in its `toString` implementation (and thrown exceptions).
	  */
	def defaults[T :ColumnWriteForm :NullValue](name :String) :ColumnWriteForm[Any] =
		new NullValueSQLWriteForm[T](name) with SingletonColumnWriteForm[Any] { outer =>
			override def notNull :ColumnWriteForm[Any] =
				new NullValueSQLWriteForm[T](toString + ".notNull")(ColumnWriteForm[T].notNull, NullValue[T])
					with IgnoringColumnWriteForm with NotNullWriteForm[Any]
				{
					override val sqlType = outer.sqlType
				}
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
		new NullifiedSQLWriteForm[T](name) with IgnoringColumnWriteForm { outer =>
			override def notNull :ColumnWriteForm[Any] =
				new NullifiedSQLWriteForm[T](name)(ColumnWriteForm[T].notNull)
					with IgnoringColumnWriteForm with NotNullWriteForm[Any]
				{
					override val sqlType = outer.sqlType
				}
			override val sqlType = ColumnWriteForm[T].sqlType
		}

	/** A column form which always writes `null` values. */
	def nulls[T] :ColumnWriteForm[T] = ColumnForm.nulls

	val gap :ColumnWriteForm[Any] =
		new GapSQLWriteForm(1) with IgnoringColumnWriteForm {
			override def sqlType = JDBCType.NULL
		}

	/** A write form which will throw the given exception at every write attempt. */
	def error(raise: => Nothing) :ColumnWriteForm[Any] = error(JDBCType.OTHER, "<ERROR")(raise)

	/** A write form which will throw the given exception at every write attempt. */
	def error(columnType :JDBCType, name :String)(raise: => Nothing) :ColumnWriteForm[Any] =
		new ErrorSQLWriteForm[Any](1, raise, name) with SingletonColumnWriteForm[Any] {
			override def notNull :this.type = this
			override val sqlType = columnType
		}

	/** A dummy column form which throws an `UnsupportedOperationException` at each write attempt.
	  * Used as part of `ColumnForm.combine` to convert a `ColumnReadForm` into a `ColumnForm` for situations
	  * where its write functionality is known not to be used. Be careful!
	  */
	def unsupported(columnType :JDBCType, name :String = null)(message :String) :ColumnWriteForm[Any] =
		error(columnType, if (name != null) name else "<UNSUPPORTED[" + columnType + "]") {
			throw new UnsupportedOperationException(message)
		}

	/** A dummy column form which throws an `UnsupportedOperationException` at each write attempt.
	  * Used as part of `ColumnForm.combine` to convert a `ColumnReadForm` into a `ColumnForm` for situations
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
	  * together with [[net.noresttherein.oldsql.schema.SQLWriteForm.join SQLWriteForm.join(forms)]] to create
	  * an `SQLWriteForm` for the whole entity.
	  */
	def map[S :ColumnWriteForm, T](f :T => S) :ColumnWriteForm[T] =
		ColumnWriteForm.map(null :String)(f)

	/** Composes an implicitly available write form `ColumnWriteForm[S]` with a given getter function to create
	  * a `ColumnWriteForm[T]`. This function can in particular be used to present a `ColumnWriteForm` for the type
	  * of a property of some entity type as a write form for said entity type. Such forms can be later combined
	  * together with [[net.noresttherein.oldsql.schema.SQLWriteForm.join SQLWriteForm.join(forms)]] to create
	  * an `SQLWriteForm` for the whole entity.
	  */
	def map[S :ColumnWriteForm, T](name :String)(f :T => S) :ColumnWriteForm[T] =
		new MappedSQLWriteForm[S, T](f, name) with SingletonColumnWriteForm[T] with SuperAdapterColumnForm { outer =>
			override val form = ColumnWriteForm[S]
			override def notNull :ColumnWriteForm[T] =
				new MappedSQLWriteForm[S, T](f, name)(form.notNull)
					with SuperAdapterColumnForm with SingletonColumnWriteForm[T] with NotNullWriteForm[T]
				{
					override val form = outer.form
				}
		}

	/** Composes an implicitly available write form `ColumnWriteForm[S]` with a given getter to create
	  * a `ColumnWriteForm[T]`. If the function returns `None`, the created form will use
	  * [[net.noresttherein.oldsql.schema.ColumnWriteForm.setNull setNull]] method of the base form instead of
	  * [[net.noresttherein.oldsql.schema.ColumnWriteForm.set set]].
	  */
	def flatMap[S :ColumnWriteForm, T](f :T => Option[S]) :ColumnWriteForm[T] =
		flatMap(null :String)(f)

	/** Composes an implicitly available write form `ColumnWriteForm[S]` with a given getter to create
	  * a `ColumnWriteForm[T]`. If the function returns `None`, the created form will use
	  * [[net.noresttherein.oldsql.schema.ColumnWriteForm.setNull setNull]] method of the base form instead of
	  * [[net.noresttherein.oldsql.schema.ColumnWriteForm.set set]].
	  */
	def flatMap[S :ColumnWriteForm, T](name :String)(f :T => Option[S]) :ColumnWriteForm[T] =
		new FlatMappedSQLWriteForm[S, T](f, name) with SingletonColumnWriteForm[T] with SuperAdapterColumnForm { outer =>
			override val form = ColumnWriteForm[S]
			override def notNull :ColumnWriteForm[T] =
				new FlatMappedSQLWriteForm[S, T](f, name)(form.notNull)
					with SingletonColumnWriteForm[T] with SuperAdapterColumnForm with NotNullWriteForm[T]
				{
					override val form = outer.form
				}
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
		new LazyColumnWriteForm[T] {
			protected[this] override var initializer: () => SQLWriteForm[T] = () => delayed
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




	protected[schema] trait NonLiteralColumnWriteForm[-T] extends NonLiteralWriteForm[T] with ColumnWriteForm[T] {
		override def nullLiteral :String = "null"
		override def inlineNullLiteral :String = nullLiteral
	}


	/** An implementation mixin for column write forms which optimizes
	  * [[net.noresttherein.oldsql.schema.ColumnWriteForm.SingletonColumnWriteForm.split split]] by extending `Seq`
	  * and returning itself, without a need for creating a (likely temporary) singleton wrapper object.
	  */
	private[oldsql] trait SingletonColumnWriteForm[-T] extends ColumnWriteForm[T] with Seq[ColumnWriteForm[T]] {
		override def split :Seq[ColumnWriteForm[T]] = this

		override def apply(i :Int) :ColumnWriteForm[T] =
			if (i == 0) this else throw new IndexOutOfBoundsException(i)

		override def length :Int = 1
		override def iterator :Iterator[ColumnWriteForm[T]] = Iterator.single(this)

		override def map[B](f :ColumnWriteForm[T] => B) = f(this) match {
			//s.type <:< B with SingletonColumnWriteForm[x] => s.type <:< B with Seq[s.type] => s <:< Seq[B]
			case singleton :SingletonColumnWriteForm[_] => singleton.asInstanceOf[Seq[B]]
			case res => ReversedList :+ res
		}

		override def flatMap[B](f :ColumnWriteForm[T] => IterableOnce[B]) = f(this) match {
			case empty :Iterable[B] if empty.isEmpty => ReversedList.empty
			case one :Iterable[B] if one.sizeIs == 1 && one.head.isInstanceOf[SingletonColumnWriteForm[_]] =>
				one.head.asInstanceOf[Seq[B]]
			case iter :Iterable[B] => iter.toSeq
			case iter :Iterator[B] => if (iter.hasNext) iter.toSeq else ReversedList.empty
			case other => other.iterator.toSeq
		}

		override def canEqual(that :Any) = super[ColumnWriteForm].canEqual(that)
		override def equals(that :Any) = superEquals(that)
		override def hashCode :Int = superHashCode
		//skip the `toString` of Seq which would cause infinite recursion and use whatever was before we were mixed in
		override def toString :String = superToString
	}


	/** A simple mix-in trait for write forms which ignore passed arguments when setting parameters.
	  * It directs [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method
	  * to [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  */
	private[schema] trait IgnoringColumnWriteForm extends IgnoringWriteForm with SingletonColumnWriteForm[Any] {
		override def unmap[X](fun :X => Any) :ColumnWriteForm[X] = this
		override def flatUnmap[X](fun :X => Option[Any]) :ColumnWriteForm[X] = this
		override def compose[X](extractor :X =?> Any) :ColumnWriteForm[X] = this
		override def toOpt :ColumnWriteForm[Option[Any]] = this

		override def nullSafe :ColumnWriteForm[Any] = this
	}


	private[schema] class CustomColumnWriteForm[-T](override val sqlType :JDBCType, name :String = null)
	                                               (write :(PreparedStatement, Int, T) => Unit)
		extends CustomSQLWriteForm[T](1, name)(write)(NullValue.NotNull)
		   with DirectColumnWriteForm[T] with NonLiteralColumnWriteForm[T] with SingletonColumnWriteForm[T]
	{
		override def notNull :ColumnWriteForm[T] =
			new CustomColumnWriteForm[T](sqlType, toString + ".notNull")(write) with NotNullWriteForm[T]

		override val toString :String =
			if (name != null) name else "ColumnWriteForm(" + sqlType + ")@" + System.identityHashCode(this)
	}


	private[schema] trait LazyColumnWriteForm[-T]
		extends LazyWriteForm[T] with SingletonColumnWriteForm[T] with SuperAdapterColumnForm
	{
		override def form :ColumnWriteForm[T] = super[LazyWriteForm].form.asInstanceOf[ColumnWriteForm[T]]

		override def unmap[X](fun :X => T) :ColumnWriteForm[X] = form.unmap(fun)
		override def flatUnmap[X](fun :X => Option[T]) :ColumnWriteForm[X] = form.flatUnmap(fun)

		override def toOpt = form.toOpt
		override def nullSafe = form.nullSafe
		override def notNull = form.notNull
		override def withNull(implicit nulls :NullValue[T]) = form.withNull
		override def withNull(nullValue :T) = form.withNull(nullValue)

		override def <>[O <: T](read :ColumnReadForm[O]) = read <> this

		override def toString :String = if (isInitialized) "Lazy(" + form + ")" else "<Lazy"
	}

}

