package net.noresttherein.oldsql.schema

import java.sql.{JDBCType, PreparedStatement, ResultSet}

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

import net.noresttherein.oldsql.morsels.Extractor.{=?>, IdentityExtractor}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.schema.ColumnReadForm.{DirectColumnReadForm, FlatMappedColumnReadForm, LazyColumnReadForm, MappedColumnReadForm}
import net.noresttherein.oldsql.schema.ColumnWriteForm.{DirectColumnWriteForm, LazyColumnWriteForm, SingletonColumnWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.{CombinedForms, FlatMappedSQLForm, LazyForm, MappedSQLForm, NotNullForm, NotNullSQLForm, NullValue, SQLMetaForm}
import net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull
import net.noresttherein.oldsql.schema.SQLReadForm.{NullableReadForm, ProxyReadForm, ReadFormNullGuard, ReadFormNullValue}
import net.noresttherein.oldsql.schema.SQLWriteForm.{CustomNullSQLWriteForm, NullableWriteForm, ProxyWriteForm, WriteFormNullGuard}
import net.noresttherein.oldsql.schema.forms.SQLForms
import net.noresttherein.oldsql.slang

//here be implicits
import slang._






/** An [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] responsible for reading and writing a single column
  * from a `ResultSet`/to a `PreparedStatement`. All [[net.noresttherein.oldsql.schema.ColumnMapping columns]]
  * of a mapping must have a `ColumnForm` for the mapped type. Implementations exist for all standard JDBC types,
  * with default implicit (and non-implicit for alternatives definitions) values grouped in
  * [[net.noresttherein.oldsql.schema.forms.SQLForms SQLForms]].
  *
  * As all [[net.noresttherein.oldsql.schema.SQLReadForm read forms]], `ColumnForm` relies on
  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] type class which defines how null columns should
  * be represented as a particular scala type. While the default (and recommended in general) value for many factory
  * methods is [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]], which simply throws
  * a `NullPointerException` with debugging information, enforcing that all types other than `Option` must map
  * to not-null columns, this decision should be consciously made by each application and, for this reason,
  * with few exception, no implicit values of the type class exist, but must be introduced into lexical scope
  * (or implicit search scope for application classes). Logical implicit defaults for importing exist in
  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.Defaults NullValue.Defaults]]. If an implicit `NullValue`
  * for a type exists, implicit form definitions will use it over `NotNull`.
  *
  * While the [[net.noresttherein.oldsql.schema.ColumnForm$ companion object]] contains several base classes/traits
  * which can be useful when implementing new forms (and companions to super types of `ColumnForm` include more),
  * the simplest way of introducing a form for a new type, is either
  * [[net.noresttherein.oldsql.schema.ColumnForm.jdbc jdbc]] factory method - for types directly supported
  * by the JDBC driver - or to map an existing form. The latter will be most common for application classes
  * and can be done either using the methods defined in this class, or those in the companion object, using
  * an implicit form as the base:
  * {{{
  *     case class URL(url :String)
  *     implicit val URLForm = ColumnForm.map(URL.apply)(_.url)
  * }}}
  */
@implicitNotFound("I do not know how to map type ${T} into a database column: missing implicit ColumnForm[${T}].")
trait ColumnForm[T] extends SQLForm[T] with ColumnReadForm[T] with ColumnWriteForm[T] { outer =>

	override def bimap[X :NullValue](map :T => X)(unmap :X => T) :ColumnForm[X] =
		ColumnForm.map(map)(unmap)(this, NullValue[X])

	override def bimap[X](map :T => X, nullValue :X)(unmap :X => T) :ColumnForm[X] =
		bimap(map)(unmap)(NullValue(nullValue))

	override def nullBimap[X](map :T => X)(unmap :X => T) :ColumnForm[X] =
		bimap(map)(unmap)(nulls.map(map))


	override def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :ColumnForm[X] =
		ColumnForm.flatMap(map)(unmap)(this, NullValue[X])

	override def biflatMap[X](map :T => Option[X], nullValue :X)(unmap :X => Option[T]) :ColumnForm[X] =
		biflatMap(map)(unmap)(NullValue(nullValue))

	override def nullBiflatMap[X](map :T => Option[X])(unmap :X => Option[T]) :ColumnForm[X] =
		biflatMap(map)(unmap)(nulls.flatMap(map))


	override def as[X :NullValue](map :T =?> X)(unmap :X =?> T) :ColumnForm[X] =
		ColumnForm(map)(unmap)(this, NullValue[X])

	override def as[X](map :T =?> X, nullValue :X)(unmap :X =?> T) :ColumnForm[X] =
		as(map)(unmap)(NullValue(nullValue))

	override def nullAs[X](map :T =?> X)(unmap :X =?> T) :ColumnForm[X] =
		as(map)(unmap)(nulls.extract(map))


	override def toOpt :ColumnForm[Option[T]] = SQLForms.OptionColumnForm(this)


	override def nullSafe :ColumnForm[T] =
		new ProxyReadForm[T] with ProxyWriteForm[T] with ColumnForm[T]
			with SingletonColumnWriteForm[T] with WriteFormNullGuard[T]
		{
			override def form = outer
			override def sqlType = outer.sqlType
			override def nullSafe = this
		}

	override def notNull :ColumnForm[T] =
		new NotNullSQLForm[T]()(this) with ColumnForm[T] {
			override def notNull :this.type = this
			override val sqlType = outer.sqlType
		}

	override def withNull(implicit nullVal :NullValue[T]) :ColumnForm[T] =
		new CustomNullSQLWriteForm[T](this)
			with ProxyReadForm[T] with ColumnForm[T] with SingletonColumnWriteForm[T]
		{
			override val form = outer
			override val sqlType = outer.sqlType
			override val nulls = nullVal
			override def nullValue = nulls.value
			override def toString :String = super[CustomNullSQLWriteForm].toString
		}

	override def withNull(nullValue :T) :ColumnForm[T] = withNull(NullValue(nullValue))

	override def writer :ColumnWriteForm[T] = this
	override def reader :ColumnReadForm[T] = this


	override def compatible(other: SQLForm[_]): Boolean = other match {
		case a :ColumnForm[_] => a.sqlType == sqlType
		case _ => false
	}

}






object ColumnForm {

	/** Summons an implicitly available instance of `ColumnForm[T]`. */
	@inline def apply[T :ColumnForm] :ColumnForm[T] = implicitly[ColumnForm[T]]


	/** Factory method for column forms of classes supported directly by the JDBC driver.
	  * The form will use [[java.sql.ResultSet.getObject getObject]]/[[java.sql.PreparedStatement.setObject setObject]]
	  * to read and write the value, respectively, providing the class from the implicit `ClassTag[T]` as the argument.
	  * The `sqlType` argument is currently used only in [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]]
	  * method and in most cases can be omitted.
	  */
	def jdbc[T :NullValue :ClassTag](sqlType :JDBCType) :ColumnForm[T] =
		new JDBCObjectForm[T](sqlType)

	/** Equivalent to [[net.noresttherein.oldsql.schema.ColumnForm.jdbc(sqlType:JDBCType) jdbc]]`(`[[java.sql.JDBCType.JAVA_OBJECT JDBCType.JAVA_OBJECT]]`)`. */
	def jdbc[T :NullValue :ClassTag] :ColumnForm[T] = new JDBCObjectForm[T]


	/** Creates a `ColumnForm` delegating all calls to the implicitly provided read and write forms. */
	def combine[T](implicit write :ColumnWriteForm[T], read :ColumnReadForm[T]) :ColumnForm[T] =
		combine(null)

	/** Creates a `ColumnForm` delegating all calls to the implicitly provided read and write forms. */
	def combine[T](name :String)(implicit write :ColumnWriteForm[T], read :ColumnReadForm[T]) :ColumnForm[T] =
		if (read.sqlType != write.sqlType)
			throw new IllegalArgumentException(
				s"Can't combine column forms $read and $write with different underlying sql types: ${read.sqlType} != ${write.sqlType}."
			)
		else
			new CombinedForms[T](read, write, name) with ColumnForm[T] with SingletonColumnWriteForm[T] {
				override val reader = read
				override val writer = write
				override def split = writer.split
				override def notNull = combine(toString + ".notNull")(writer.notNull, reader.notNull)
				override val sqlType = reader.asInstanceOf[ColumnReadForm[T]].sqlType
			}



	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression.
	  */
	def eval[T :ColumnWriteForm](expr: => T, name :String = null) :ColumnForm[T] =
		combine(name)(ColumnWriteForm.eval(expr), ColumnReadForm.eval(ColumnWriteForm[T].sqlType, expr))

	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression.
	  */
	def evalopt[T :ColumnWriteForm :NullValue](expr: => Option[T], name :String = null) :ColumnForm[T] =
		combine(name)(ColumnWriteForm.evalopt(expr), ColumnReadForm.evalopt(ColumnWriteForm[T].sqlType, expr))



	/** Creates a dummy form which always returns and writes the same value. */
	def const[T :ColumnWriteForm](value :T, name :String = null) :ColumnForm[T] =
		combine(name)(ColumnWriteForm.const(value), ColumnReadForm.const(ColumnWriteForm[T].sqlType, value))

	/** Creates a dummy form which always returns and writes the same value. */
	def constopt[T :ColumnWriteForm :NullValue](value :Option[T], name :String = null) :ColumnForm[T] =
		combine(name)(ColumnWriteForm.constopt(value), ColumnReadForm.constopt(ColumnWriteForm[T].sqlType, value))



	/** Creates a dummy form which always writes the null value as defined by the implicit `NullValue`,
	  * and returns `None`/`nulls.value` when reading.
	  */
	def defaults[T :ColumnWriteForm :NullValue] :ColumnForm[T] =
		combine(ColumnWriteForm.defaults[T], ColumnReadForm.defaults[T](ColumnWriteForm[T].sqlType))

	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
	  * and returns `None` when reading. All calls to `apply` will result in a `NullPointerException`.
	  */
	def none[T :ColumnWriteForm] :ColumnForm[T] =
		combine(ColumnWriteForm.none[T], ColumnReadForm.none[T](ColumnWriteForm[T].sqlType))

//	/** Creates a dummy form which always writes `null` and returns `None` (and throws `NullPointerException` in `apply`). */
//	def nulls[T] :ColumnForm[T] = SQLForms.NotNullNull.asInstanceOf[ColumnForm[T]]

	/** Creates a dummy form which always reads and writes `null` values, except from the `opt` method, which returns `None`. */
	def nulls[T >: Null] :ColumnForm[T] = SQLForms.NullForm

	/** Creates a `ColumnForm[T]` based on implicit `ColumnForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]] is used and `null` values
	  * will never be returned to the application.
	  */
	def apply[S, T](map :S =?> T)(unmap :T =?> S)
	               (implicit source :ColumnForm[S], nulls :Maybe[NullValue[T]]) :ColumnForm[T] =
		(map, unmap) match {
			case (_ :IdentityExtractor[_], _ :IdentityExtractor[_]) => source.asInstanceOf[ColumnForm[T]]
			case (Extractor.Requisite(there), Extractor.Requisite(back)) => ColumnForm.map(there)(back)
			case _ => ColumnForm.flatMap(map.optional)(unmap.optional)
		}

	/** Creates a `ColumnForm[T]` based on implicit `ColumnForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]] is used and `null` values
	  * will never be returned to the application.
	  */
	def apply[S, T](name :String)(map :S =?> T)(unmap :T =?> S)
	               (implicit source :ColumnForm[S], nulls :Maybe[NullValue[T]]) :ColumnForm[T] =
		(map, unmap) match {
//			case (_ :IdentityExtractor[_], _ :IdentityExtractor[_]) => this.asInstanceOf[ColumnForm[X]]
			case (Extractor.Requisite(there), Extractor.Requisite(back)) => ColumnForm.map(name)(there)(back)
			case _ => ColumnForm.flatMap(name)(map.optional)(unmap.optional)
		}



	/** Creates a `ColumnForm[T]` based on implicit `ColumnForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]] is used and `null` values
	  * will never be returned to the application.
	  */
	def map[S, T](map :S => T)(unmap :T => S)
	             (implicit source :ColumnForm[S], nulls :Maybe[NullValue[T]]) :ColumnForm[T] =
		this.map(null :String)(map)(unmap)

	/** Creates a `ColumnForm[T]` based on implicit `ColumnForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]] is used and `null` values
	  * will never be returned to the application.
	  */
	def map[S, T](name :String)(map :S => T)(unmap :T => S)
	             (implicit source :ColumnForm[S], nulls :Maybe[NullValue[T]]) :ColumnForm[T] =
	{
		val nullValue = nulls.opt getOrElse source.nulls.map(map)
		new MappedColumnForm(map, unmap, name)(source, nullValue)
	}



	/** Creates a `ColumnForm[T]` based on implicit `ColumnForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]] is used and `null` values
	  * will never be returned to the application.
	  */
	def flatMap[S, T](map :S => Option[T])(unmap :T => Option[S])
	                 (implicit source :ColumnForm[S], nulls :Maybe[NullValue[T]]) :ColumnForm[T] =
		flatMap(null :String)(map)(unmap)

	/** Creates a `ColumnForm[T]` based on implicit `ColumnForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]] is used and `null` values
	  * will never be returned to the application.
	  */
	def flatMap[S, T](name :String)(map :S => Option[T])(unmap :T => Option[S])
	                 (implicit source :ColumnForm[S], nulls :Maybe[NullValue[T]]) :ColumnForm[T] =
	{
		val nullValue = nulls.opt getOrElse source.nulls.flatMap(map)
		new FlatMappedColumnForm(map, unmap, name)(source, nullValue)
	}



	/** A proxy form which will delegate all calls to the form returned by the given expression. The expression
	  * will be evaluated only if/when needed, but must be thread safe and may be executed more than once
	  * if several threads trigger the initialization at the same time, but the returned form is thread safe.
	  * The proxy is shallow: all methods returning another form are delegated to the backing form, evaluating them.
	  * If you wish them to remain lazy, invoke them directly on the backing form in the evaluation expression instead.
	  * This is useful when the initialization expression cannot be successfully evaluated at this time,
	  * but the form must be passed by-reference to some method. In other cases a `lazy val` or
	  * [[net.noresttherein.oldsql.morsels.Lazy Delayed]] wrapper are preferable, as they do not incur the penalty
	  * of checking for initialization and delegation at every call.
	  */
	def delayed[T](delayed: => ColumnForm[T]) :ColumnForm[T] =
		new LazyForm[T](() => delayed)
			with LazyColumnReadForm[T] with LazyColumnWriteForm[T] with ColumnForm[T] //with SingletonColumnWriteForm[T]
		{
			override def form :ColumnForm[T] = super.form.asInstanceOf[ColumnForm[T]]

			override def bimap[X :NullValue](map :T => X)(unmap :X => T) =
				form.bimap[X](map)(unmap)

			override def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) =
				form.biflatMap(map)(unmap)

			override def toOpt = form.toOpt
			override def nullSafe = form.nullSafe
			override def notNull = form.notNull
			override def withNull(implicit nulls :NullValue[T]) = form.withNull
			override def withNull(nullValue :T) = form.withNull(nullValue)
			override def *(repeat :Int) = form.*(repeat)

			override def toString :String = super[LazyForm].toString
		}






	/** A convenience mixin trait for forms of reference types using `null` as their `nullValue`.
	  * Implements also the null literal methods to return "null".
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue]]
	  */
	trait NullableColumnForm[T >: Null] extends NullableReadForm[T] with NullableWriteForm[T] with ColumnForm[T] {
		override def nullLiteral :String = "null"
		override def inlineNullLiteral :String = "null"
	}

	trait NullSafeColumnForm[T] extends ColumnForm[T] {
		override def nullSafe :ColumnForm[T] = this
	}



	/** A convenience base class for `ColumnForm` implementations using an implicit
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] for their
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue null value]] and the provided JDBC code.
	  * @see [[net.noresttherein.oldsql.schema.ColumnReadForm.DirectColumnReadForm]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm.DirectColumnWriteForm]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiteralsBackFeed]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	abstract class AbstractColumnForm[T](override val sqlType :JDBCType)(implicit override val nulls :NullValue[T])
		extends ColumnForm[T] with ReadFormNullValue[T]


	/** A convenience base class for `ColumnForm` implementations using implicit
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] for their
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue null value]] and having a predefined name.
	  * Only the latter differentiates it from
	  * [[net.noresttherein.oldsql.schema.ColumnForm.AbstractColumnForm AbstractColumnForm]], which uses
	  * inner class name in its `toString` method instead.
	  * @see [[net.noresttherein.oldsql.schema.ColumnReadForm.DirectColumnReadForm]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm.DirectColumnWriteForm]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiteralsBackFeed]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	abstract class NamedColumnForm[T](name :String, override val sqlType :JDBCType)
	                                 (implicit override val nulls :NullValue[T])
		extends ColumnForm[T] with ReadFormNullValue[T]
	{
		override def toString :String = name
	}

	/** A convenience base class for `ColumnForm` implementations using an implicit
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] for their
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue null value]] and the provided JDBC code.
	  * The only difference from `AbstractColumnForm[T]` lies in the `toString` implementation:
	  * this class uses the local class name of type `T` rather than the form class name.
	  * @see [[net.noresttherein.oldsql.schema.ColumnReadForm.DirectColumnReadForm]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm.DirectColumnWriteForm]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiteralsBackFeed]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	abstract class ColumnMetaForm[T :NullValue :ClassTag](override val sqlType :JDBCType)
		extends SQLMetaForm[T] with ColumnForm[T] with ReadFormNullValue[T]



	/** A base class for `ColumnForm` implementations for standard JDBC types.
	  * It uses the provided `JDBCType` as its `toString` implementation and, in addition to
	  * other base types, implements [[net.noresttherein.oldsql.schema.ColumnForm.JDBCForm.literal literal]]
	  * as `String.valueOf(value)`, which is suitable for most, but not all, types which actually have a literal
	  * representation in SQL.
	  * @see [[net.noresttherein.oldsql.schema.ColumnForm.AbstractColumnForm]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnForm.NamedColumnForm]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnForm.ColumnMetaForm]]
	  */
	abstract class JDBCForm[T](override val sqlType :JDBCType)(implicit override val nulls :NullValue[T])
		extends ColumnForm[T] with ReadFormNullValue[T] with DirectColumnReadForm[T] with DirectColumnWriteForm[T]
	{
		override def literal(value :T) :String = String.valueOf(value)

		override def toString :String = sqlType.toString
	}



	/** A `ColumnForm` for classes supported directly by JDBC drivers.
	  * The form will use [[java.sql.ResultSet.getObject getObject]]/[[java.sql.PreparedStatement.setObject setObject]]
	  * to read and write the value, respectively, providing the class from the implicit `ClassTag[T]` as the argument.
	  * The `sqlType` argument is currently used only in [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]]
	  * method and in most cases can be omitted.
	  */
	class JDBCObjectForm[T](private val cls :Class[T], override val sqlType :JDBCType = JDBCType.JAVA_OBJECT)
	                       (implicit override val nulls :NullValue[T])
		extends JDBCForm[T](sqlType) with NullSafeColumnForm[T]
	{
		def this(sqlType :JDBCType)(implicit nulls :NullValue[T], tag :ClassTag[T]) =
			this(tag.runtimeClass.asInstanceOf[Class[T]], sqlType)

		def this()(implicit nulls :NullValue[T], tag :ClassTag[T]) =
			this(tag.runtimeClass.asInstanceOf[Class[T]])

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			statement.setObject(position, value, sqlType)

		protected override def read(res :ResultSet, position :Int) :T =
			res.getObject(position, cls)

		override def notNull :ColumnForm[T] =
			new JDBCObjectFormSingleton[T](cls, sqlType)(NotNull) {
				override def apply(res :ResultSet, position :Int) :T = {
					val t = super.apply(res, position)
					if (res.wasNull) throw new NullPointerException("Cannot return null from " + this + ".")
					t
				}

				override val toString :String = innerNameOf(cls) + ".notNull"
			}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case form :JDBCObjectForm[_] if form canEqual this =>
				cls == form.cls && sqlType == form.sqlType && nulls == form.nulls
			case _ => false
		}

		override def hashCode :Int = cls.hashCode * 31 + sqlType.hashCode

		override def toString :String = string
		private[this] val string = innerNameOf(cls)
	}

	private[schema] class JDBCObjectFormSingleton[T](cls :Class[T], sqlType :JDBCType = JDBCType.JAVA_OBJECT)
	                                                (implicit override val nulls :NullValue[T])
		extends JDBCObjectForm[T](cls, sqlType) with SingletonColumnWriteForm[T]
	{
		def this(sqlType :JDBCType)(implicit nulls :NullValue[T], tag :ClassTag[T]) =
			this(tag.runtimeClass.asInstanceOf[Class[T]], sqlType)

		def this()(implicit nulls :NullValue[T], tag :ClassTag[T]) = this(tag.runtimeClass.asInstanceOf[Class[T]])
	}







	private[schema] class MappedColumnForm[S, T](f :S => T, g :T => S, name :String = null)
	                                            (implicit override val form :ColumnForm[S], nulls :NullValue[T])
		extends MappedSQLForm(f, g, name)
		   with MappedColumnReadForm[S, T] with ColumnForm[T] with SingletonColumnWriteForm[T]
	{
		override def split :Seq[ColumnWriteForm[T]] = form.compose(unmap)::Nil

		override def notNull :ColumnForm[T] =
			new MappedColumnForm[S, T](map, unmap, toString + ".notNull")(form.asInstanceOf[ColumnForm[S]], NotNull)
				with NotNullForm[T] with ReadFormNullGuard[T]

		override val toString :String = if (name != null) name else "<=" + ColumnForm[S] + "=>"
	}

	private[schema] class FlatMappedColumnForm[S :ColumnForm, T :NullValue]
	                                          (f :S => Option[T], g :T => Option[S], name :String = null)
		extends FlatMappedSQLForm(f, g, name)
			with FlatMappedColumnReadForm[S, T] with ColumnForm[T] with SingletonColumnWriteForm[T]
	{
		override def notNull :ColumnForm[T] =
			new FlatMappedColumnForm[S, T](map, unmap, toString + ".notNull")(form.asInstanceOf[ColumnForm[S]], NotNull)
				with NotNullForm[T] with ReadFormNullGuard[T]

		override val toString :String = if (name != null) name else "<=" + ColumnForm[S] + "=>"
	}

}
