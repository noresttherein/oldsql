package net.noresttherein.oldsql.schema

import java.sql.JDBCType

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

import net.noresttherein.oldsql.morsels.Extractor.{=?>, IdentityExtractor}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.schema.ColumnReadForm.{DirectColumnReadForm, FlatMappedColumnReadForm, LazyColumnReadForm, MappedColumnReadForm}
import net.noresttherein.oldsql.schema.SQLForm.{FlatMappedSQLForm, JoinedForms, LazyForm, MappedSQLForm, NullableForm, NullValue, ReifiedSQLForm}
import net.noresttherein.oldsql.schema.forms.SQLForms
import net.noresttherein.oldsql.schema.ColumnWriteForm.{DirectColumnWriteForm, LazyColumnWriteForm}
import net.noresttherein.oldsql.schema.SQLReadForm.ProxyReadForm
import net.noresttherein.oldsql.schema.SQLWriteForm.{CustomNullSQLWriteForm, ProxyWriteForm, WriteFormNullGuard}







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
		new ProxyReadForm[T] with ProxyWriteForm[T] with WriteFormNullGuard[T] with ColumnForm[T] {
			override def form = outer
			override def sqlType = outer.sqlType
			override def nullSafe = this
		}

	override def withNull(implicit nullVal :NullValue[T]) :ColumnForm[T] =
		new CustomNullSQLWriteForm[T](this) with ProxyReadForm[T] with ColumnForm[T] {
			override val form = outer
			override val sqlType = outer.sqlType
			override val nulls = nullVal
			override def nullValue = nulls.value
			override def toString :String = super[CustomNullSQLWriteForm].toString
		}

	override def withNull(nullValue :T) :ColumnForm[T] = withNull(NullValue(nullValue))

	override def compatible(other: SQLForm[_]): Boolean = other match {
		case a :ColumnForm[_] => a.sqlType == sqlType
		case _ => false
	}


}






object ColumnForm {

	@inline def apply[X :ColumnForm] :ColumnForm[X] = implicitly[ColumnForm[X]]


	/** Creates a `ColumnForm` delegating all calls to the implicitly provided read and write forms. */
	def join[T](implicit write :ColumnWriteForm[T], read :ColumnReadForm[T]) :ColumnForm[T] =
		join(null)

	/** Creates a `ColumnForm` delegating all calls to the implicitly provided read and write forms. */
	def join[T](name :String)(implicit write :ColumnWriteForm[T], read :ColumnReadForm[T]) :ColumnForm[T] =
		if (read.sqlType != write.sqlType)
			throw new IllegalArgumentException(
				s"Can't combine column forms $read and $write with different underlying sql types: ${read.sqlType} != ${write.sqlType}."
			)
		else
			new JoinedForms[T](read, write, name) with ColumnForm[T] {
				override val sqlType = r.asInstanceOf[ColumnReadForm[T]].sqlType
			}



	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression.
	  */
	def eval[T :ColumnWriteForm](expr: => T, name :String = null) :ColumnForm[T] =
		join(name)(ColumnWriteForm.eval(expr), ColumnReadForm.eval(ColumnWriteForm[T].sqlType, expr))

	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression.
	  */
	def evalopt[T :ColumnWriteForm :NullValue](expr: => Option[T], name :String = null) :ColumnForm[T] =
		join(name)(ColumnWriteForm.evalopt(expr), ColumnReadForm.evalopt(ColumnWriteForm[T].sqlType, expr))



	/** Creates a dummy form which always returns and writes the same value. */
	def const[T :ColumnWriteForm](value :T, name :String = null) :ColumnForm[T] =
		join(name)(ColumnWriteForm.const(value), ColumnReadForm.const(ColumnWriteForm[T].sqlType, value))

	/** Creates a dummy form which always returns and writes the same value. */
	def constopt[T :ColumnWriteForm :NullValue](value :Option[T], name :String = null) :ColumnForm[T] =
		join(name)(ColumnWriteForm.constopt(value), ColumnReadForm.constopt(ColumnWriteForm[T].sqlType, value))



	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
	  * and returns `None`/`nulls.value` when reading.
	  */
	def nulls[T :ColumnWriteForm :NullValue] :ColumnForm[T] =
		join(ColumnWriteForm.none[T], ColumnReadForm.nulls[T](ColumnWriteForm[T].sqlType))

	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
	  * and returns `None` when reading. All calls to `apply` will result in a `NullPointerException`.
	  */
	def none[T :ColumnWriteForm] :ColumnForm[T] =
		join(ColumnWriteForm.none[T], ColumnReadForm.none[T](ColumnWriteForm[T].sqlType))



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
	  * [[net.noresttherein.oldsql.morsels.Lazy Lazy]] wrapper are preferable, as they do not incur the penalty
	  * of checking for initialization and delegation at every call.
	  */
	def delayed[T](delayed: => ColumnForm[T]) :ColumnForm[T] =
		new LazyForm[T](() => delayed) with LazyColumnReadForm[T] with LazyColumnWriteForm[T] with ColumnForm[T] {
			override def form :ColumnForm[T] = super.form.asInstanceOf[ColumnForm[T]]

			override def bimap[X :NullValue](map :T => X)(unmap :X => T) =
				form.bimap[X](map)(unmap)

			override def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) =
				form.biflatMap(map)(unmap)

			override def toOpt = form.toOpt
			override def nullSafe = form.nullSafe
			override def withNull(implicit nulls :NullValue[T]) = form.withNull
			override def withNull(nullValue :T) = form.withNull(nullValue)
			override def *(repeat :Int) = form.*(repeat)

			override def toString :String = super[LazyForm].toString
		}






	/** A convenience mixin trait for forms of reference types using `null` as their `nullValue`.
	  * Implements also the null literal methods to return "null".
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue]]
	  */
	trait NullableColumnForm[T >: Null] extends NullableForm[T] with ColumnForm[T] {
		override def nullLiteral :String = "null"
		override def inlineNullLiteral :String = "null"
	}

	trait NullSafeColumnForm[T] extends ColumnForm[T] {
		override def nullSafe :ColumnForm[T] = this
	}



	/** A convenience base class for `ColumnForm` implementations using an implicit `NullValue[T]` and the provided
	  * JDBC code.
	  */
	abstract class AbstractColumnForm[T](override val sqlType :JDBCType)(implicit override val nulls :NullValue[T])
		extends ColumnForm[T]
	{
		override def nullValue :T = nulls.value
	}



	/** A convenience base class for `ColumnForm` implementations using an implicit `NullValue[T]` and the provided
	  * JDBC code. The only difference from `AbstractColumnForm[T]` lies in the `toString` implementation:
	  * this class uses the unqualified class name of type `T` rather than the form class name.
	  */
	abstract class ReifiedColumnForm[T](override val sqlType :JDBCType)
	                                   (implicit nulls :NullValue[T], clazz :ClassTag[T])
		extends ReifiedSQLForm[T] with ColumnForm[T] with DirectColumnWriteForm[T]



	/** A base class for `ColumnForm` implementations for standard JDBC types. The only difference from
	  * `AbstractColumnForm[T]` lies in the `toString` implementation: this class uses the name of the underlying
	  * SQL type rather than unqualified form class name.
	  */
	abstract class JDBCForm[T](val sqlType :JDBCType)(implicit override val nulls :NullValue[T])
		extends ColumnForm[T] with DirectColumnReadForm[T] with DirectColumnWriteForm[T] //with NullSafeColumnForm[T]
	{
		override def nullValue :T = nulls.value

		override def toString :String = sqlType.toString
	}




	abstract class NullableJDBCForm[T >: Null](sqlType :JDBCType)
		extends JDBCForm[T](sqlType)(NullValue.Null) with NullableColumnForm[T]
	{
		override val nulls :NullValue[T] = NullValue.Null
	}






	private[schema] class MappedColumnForm[S :ColumnForm, T :NullValue](map :S => T, unmap :T => S, name :String = null)
		extends MappedSQLForm(map, unmap, name) with MappedColumnReadForm[S, T] with ColumnForm[T]
	{
		override val toString :String = if (name != null) name else "<=" + ColumnForm[S] + "=>"
	}

	private[schema] class FlatMappedColumnForm[S :ColumnForm, T :NullValue]
	                                          (map :S => Option[T], unmap :T => Option[S], name :String = null)
		extends FlatMappedSQLForm(map, unmap, name) with FlatMappedColumnReadForm[S, T] with ColumnForm[T]
	{
		override val toString :String = if (name != null) name else "<=" + ColumnForm[S] + "=>"
	}

}
