package net.noresttherein.oldsql.schema

import java.lang.Double.doubleToLongBits
import java.sql.{JDBCType, PreparedStatement, ResultSet}

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.{Opt, PassedArray, ReversedList}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.NullValueException
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, IdentityExtractor}
import net.noresttherein.oldsql.schema.ColumnReadForm.DirectColumnReadForm
import net.noresttherein.oldsql.schema.ColumnWriteForm.{DirectColumnWriteForm, SingletonColumnWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.{CombinedForms, NamedForm, NullValue, ReflectedSQLForm}
import net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull
import net.noresttherein.oldsql.schema.SQLReadForm.{NotNullReadFormProxy, NullableReadForm, ReadFormNullValue, ReadFormProxy}
import net.noresttherein.oldsql.schema.SQLWriteForm.{NotNullWriteFormProxy, NullSafeWriteForm, NullSafeWriteFormProxy, WriteFormWithNull}
import net.noresttherein.oldsql.schema.forms.{CustomColumnForm, DerivedMappedColumnForm, DerivedOptMappedColumnForm, LazyColumnForm, MappedColumnForm, OffsetColumnForm, OptMappedColumnForm, SQLForms}
import net.noresttherein.oldsql.schema.forms.UnspecifiedForm.{BaseFormAdapter, UnspecifiedColumnFormAdapter}
import net.noresttherein.oldsql.slang

//here be implicits
import slang._






/** An [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] responsible for reading and writing a single column
  * from a `ResultSet`/to a `PreparedStatement`. All [[net.noresttherein.oldsql.schema.ColumnMapping columns]]
  * of a mapping must have a `ColumnForm` for the mapped type. Implementations exist for all standard JDBC types,
  * with default implicit (and non-implicit for alternative definitions) values grouped in
  * [[net.noresttherein.oldsql.schema.forms.SQLForms SQLForms]].
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
  *
  * All column forms should implement meaningful `equals` and `hashCode`, which, ideally, should recognize as equal
  * all instances whose read and write methods are functionally equivalent. This is true for all built in forms,
  * and should also be true for mapped forms which use pure functions without closures, although this depends
  * on the compiler implementation. For this reason, all factory and mapping methods from the companion object
  * have a variant accepting a ''name'':
  * {{{
  *     case class Parcel[X](content :X)
  *     implicit def parcelForm[X :ColumnForm] = ColumnForm.map("Parcel")(Parcel.apply)(_.content)
  * }}}
  * All forms created by the above method are equal if their `content` forms are equal. Inherited `comparable` methods
  * check if the underlying [[net.noresttherein.oldsql.schema.ColumnForm.sqlType sqlType]]s are equal on both forms.
  *
  * As all [[net.noresttherein.oldsql.schema.SQLReadForm read forms]], `ColumnForm` relies on
  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] type class which defines how null columns should
  * be represented as a particular scala type. While the default (and recommended in general) value for many factory
  * methods is [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NullValue.NotNull]], which simply throws
  * a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]] with debugging information,
  * enforcing that all types other than `Option` must map to not-null columns, this decision should be consciously made
  * by each application and, for this reason, with few exceptions, no implicit values of the type class exist,
  * but must be introduced into lexical scope (or implicit search scope for application classes).
  * Sensible implicit defaults for importing exist in
  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.Defaults NullValue.Defaults]]. If an implicit `NullValue`
  * for a type exists, implicit form definitions will use it over `NotNull`.
  */
@implicitNotFound("I do not know how to map type ${T} into a database column: missing implicit ColumnForm[${T}].")
trait ColumnForm[T] extends SQLForm[T] with ColumnReadForm[T] with ColumnWriteForm[T] { outer =>

	override def bimap[X :NullValue](map :T => X)(unmap :X => T) :ColumnForm[X] =
		ColumnForm.map(map)(unmap)(this, NullValue[X])

	override def bimap[X](map :T => X, nullValue :X)(unmap :X => T) :ColumnForm[X] =
		bimap(map)(unmap)(NullValue(nullValue))

	override def nullBimap[X](map :T => X)(unmap :X => T) :ColumnForm[X] =
		bimap(map)(unmap)(nulls.map(map))


	override def optBimap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :ColumnForm[X] =
		ColumnForm.optMap(map)(unmap)(this, NullValue[X])

	override def optBimap[X](map :T => Option[X], nullValue :X)(unmap :X => Option[T]) :ColumnForm[X] =
		optBimap(map)(unmap)(NullValue(nullValue))

	override def nullOptBimap[X](map :T => Option[X])(unmap :X => Option[T]) :ColumnForm[X] =
		optBimap(map)(unmap)(nulls.optMap(map))


	override def as[X :NullValue](map :T =?> X)(unmap :X =?> T) :ColumnForm[X] =
		ColumnForm(map)(unmap)(this, NullValue[X])

	override def as[X](map :T =?> X, nullValue :X)(unmap :X =?> T) :ColumnForm[X] =
		as(map)(unmap)(NullValue(nullValue))

	override def nullAs[X](map :T =?> X)(unmap :X =?> T) :ColumnForm[X] =
		as(map)(unmap)(nulls.andThen(map))


	override def toOpt :ColumnForm[Option[T]] = SQLForms.OptionColumnForm(this)


	override def nullSafe :ColumnForm[T] =
		new BaseFormAdapter(this)
			with UnspecifiedColumnFormAdapter with ColumnForm[T] with SingletonColumnWriteForm[T]
			with ReadFormProxy[T] with NullSafeWriteFormProxy[T]

	override def notNull :ColumnForm[T] =
		new BaseFormAdapter(this)
			with UnspecifiedColumnFormAdapter with ColumnForm[T] with SingletonColumnWriteForm[T]
			with NotNullReadFormProxy[T] with NotNullWriteFormProxy[T]

	override def withNull(implicit nulls :NullValue[T]) :ColumnForm[T] = {
		val nullVal = nulls
		new BaseFormAdapter(this) with UnspecifiedColumnFormAdapter with ColumnForm[T]
			with ReadFormProxy[T] with WriteFormWithNull[T] with SingletonColumnWriteForm[T]
		{
			override def nulls = nullVal
			override def nullValue = nullVal.value
			override def withNull(implicit nulls :NullValue[T]) :ColumnForm[T] = form.withNull
		}
	}

	override def withNull(nullValue :T) :ColumnForm[T] = withNull(NullValue(nullValue))

	override def reorder(permutation :IndexedSeq[Int]) :ColumnForm[T] =
		if (permutation.length != 1 || permutation.head != 0)
			throw new IllegalArgumentException(
				"The only valid permutation of a single column is Seq(0), but got : " + permutation + "."
			)
		else
			this

	override def >>(shift :Int) :ColumnForm[T] =
		if (shift == 0) this else new OffsetColumnForm[T](this, shift)

	override def <<(shift :Int) :ColumnForm[T] = this >> -shift


	override def <>[O >: T](write :ColumnWriteForm[O]) :ColumnForm[O] =
		if (write == writer || write == this) this.castParam[O]
		else ColumnForm.combine[O](reader, write)

	override def <>[O <: T](read :ColumnReadForm[O]) :ColumnForm[O] =
		if (read == reader || read == this) this.castParam[O]
		else ColumnForm.combine[O](read, writer)

	override def writer :ColumnWriteForm[T] = this
	override def reader :ColumnReadForm[T] = this


	override def comparable(other: SQLForm[_]): Boolean = other match {
		case a :ColumnForm[_] => a.sqlType == sqlType
		case _ if other.columnCount != 1 || other.columnCount != 1 => false
		case _ => super.comparable(other)
	}

}






object ColumnForm {

	/** Summons an implicitly available instance of `ColumnForm[T]`. */
	@inline def apply[T](implicit form :ColumnForm[T]) :ColumnForm[T] = form



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

	/** Creates a [[net.noresttherein.oldsql.schema.SQLForm.NamedForm named]] column form which
	  * reads values using [[java.sql.ResultSet.getObject getObject]]
	  * and writes using [[java.sql.PreparedStatement.setObject setObject]],
	  * with the specified `sqlType` as an argument. The name itself determines if the form
	  * will equal another `ColumnForm`.
	  */
	def jdbc[T :NullValue :ClassTag](name :String, sqlType :JDBCType = JDBCType.JAVA_OBJECT) :ColumnForm[T] = {
		val n = name
		new JDBCObjectForm[T](sqlType) with NamedForm[T] {
			override val name = Got(n)
			override def text = name
			override def toString = string
			private[this] val string = if (sqlType == JDBCType.JAVA_OBJECT) text.get else text.get + "(" + sqlType + ")"
		}
	}


	/** Creates a `ColumnForm` delegating all calls to the implicitly provided read and write forms.
	  * Returned form implements structural equality.
	  */
	def combine[T :ColumnReadForm :ColumnWriteForm] :ColumnForm[T] = ColumnForm(ColumnReadForm[T], ColumnWriteForm[T])

	/** Creates a `ColumnForm` delegating all calls to the implicitly provided read and write forms. */
	def combine[T :ColumnReadForm :ColumnWriteForm](name :String) :ColumnForm[T] =
		ColumnForm(name, ColumnReadForm[T], ColumnWriteForm[T])


	/** Creates a `ColumnForm` delegating all calls to the provided read and write forms.
	  * Returned form implements structural equality.
	  */
	def apply[T](read :ColumnReadForm[T], write :ColumnWriteForm[T]) :ColumnForm[T] =
	//sound, because read is covariant and write is contravariant, so T is the exact type parameter of the form
		if ((write eq read) && write.isInstanceOf[ColumnForm[_]])
			write.asInstanceOf[ColumnForm[T]]
		else
			new CombinedColumnForms[T](read, write)

	/** Creates a `ColumnForm` delegating all calls to the provided read and write forms. */ //todo: separate param lists
	def apply[T](name :String, read :ColumnReadForm[T], write :ColumnWriteForm[T]) :ColumnForm[T] =
		new CombinedColumnForms[T](read, write, Got(name)) with NamedColumnForm[T]



	/** An `SQLForm` using the specified functions to read column values from a [[java.sql.ResultSet ResultSet]]
	  * and set parameters of a [[java.sql.PreparedStatement PreparedStatement]]. The form is equivalent
	  * to a pair of [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]]
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]] forms wrapping these functions.
	  * In particular, `reader` is responsible for handling `null` values in the `ResultSet`
	  * and `null` statement parameters are set directly using the JDBC API.
	  * @param columnType the sql type of the read columns and set parameters.
	  * @param writer     a function accepting a statement and parameter position and setting the following `columns`
	  *                   parameters.
	  * @param reader     a function, accepting a result set and a column index, and returning a value of `T` constructed
	  *                   from the following `columns` column values.
	  */
	def apply[T](columnType :JDBCType)
	            (reader :(ResultSet, Int) => T)(writer :(PreparedStatement, Int, T) => Unit) :ColumnForm[T] =
		new CustomColumnForm[T](columnType)(reader, writer)
//		combine(ColumnReadForm(columnType)(reader), ColumnWriteForm(columnType)(writer))

	/** An `SQLForm` using the specified functions to read column values from a [[java.sql.ResultSet ResultSet]]
	  * and set parameters of a [[java.sql.PreparedStatement PreparedStatement]].  The form is equivalent
	  * to a pair of [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]]
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]] forms wrapping these functions.
	  * In particular, `reader` is responsible for handling `null` values in the `ResultSet`
	  * and `null` statement parameters are set directly using the JDBC API. Two forms created with the same
	  * name are equal regardless of the values of other parameters
	  * @param columnType the sql type of the read columns and set parameters.
	  * @param name       a name for this form, used as its identifier, and by its `toString` method.
	  * @param writer     a function accepting a statement and parameter position and setting the following `columns`
	  *                   parameters. It must handle `null` values.
	  * @param reader     a function, accepting a result set and a column index, and returning a value of `T` constructed
	  *                   from the following `columns` column values.
	  */
	def apply[T](name :String, columnType :JDBCType)
	            (reader :(ResultSet, Int) => T)(writer :(PreparedStatement, Int, T) => Unit) :ColumnForm[T] =
		new CustomColumnForm[T](columnType, Got(name))(reader, writer) with NamedColumnForm[T]
//		combine(name)(ColumnReadForm(name, columnType)(reader), ColumnWriteForm(name, columnType)(writer))



	/** Creates a `ColumnForm[T]` based on implicit `ColumnForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * the [[net.noresttherein.oldsql.schema.ColumnReadForm!.nullValue one in the source form]] will be
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.andThen mapped]].
	  */
	def apply[S :ColumnForm, T :NullValue.Maybe](map :S =?> T)(unmap :T =?> S) :ColumnForm[T] =
		(map, unmap) match {
			case (_ :IdentityExtractor[_], _ :IdentityExtractor[_]) => ColumnForm[S].asInstanceOf[ColumnForm[T]]
			case (Extractor.Requisite(there), Extractor.Requisite(back)) => ColumnForm.map(there)(back)
			case _ => ColumnForm.optMap(map.optional)(unmap.optional)
		}

	/** Creates a new `ColumnForm[T]` of 'soft type' given as `name` argument, based on an implicit form for type `S`
	  * and an optional `NullValue[T]`. This is equivalent in function to
	  * `ColumnForm`[[net.noresttherein.oldsql.schema.ColumnForm.apply[S,T](map* (map)(unmap)]],
	  * but the created form will equal any other `ColumnForm` created by this method if they have the same name
	  * and their base forms and [[net.noresttherein.oldsql.schema.SQLReadForm.nulls null values]] are equal.
	  *
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * the [[net.noresttherein.oldsql.schema.ColumnReadForm!.nullValue one in the source form]] will be
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.andThen mapped]].
	  */
	def apply[S :ColumnForm, T :NullValue.Maybe](name :String)(map :S =?> T)(unmap :T =?> S) :ColumnForm[T] =
		(map, unmap) match {
			case (Extractor.Requisite(there), Extractor.Requisite(back)) => ColumnForm.map(name)(there)(back)
			case _ => ColumnForm.optMap(name)(map.optional)(unmap.optional)
		}


	/** Creates a `ColumnForm[T]` based on implicit `ColumnForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * the [[net.noresttherein.oldsql.schema.ColumnReadForm!.nullValue one in the source form]] will be
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.map mapped]].
	  */
	def map[S :ColumnForm, T :NullValue.Maybe](map :S => T)(unmap :T => S) :ColumnForm[T] = {
		implicit val nullValue = NullValue.orElse(ColumnForm[S].nulls.map(map))
		new MappedColumnForm(map, unmap)
	}

	/** Creates a new `ColumnForm[T]` of 'soft type' given as `name` argument, based on an implicit form for type `S`
	  * and an optional `NullValue[T]`. This is equivalent in function to
	  * `ColumnForm`[[net.noresttherein.oldsql.schema.ColumnForm.map[S,T](map:S=>T)* .map(map)(unmap)]],
	  * but the created form will equal any other `ColumnForm` created by this method if they have the same name
	  * and their base forms and [[net.noresttherein.oldsql.schema.SQLReadForm.nulls null values]] are equal.
	  *
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue one present in the source form]] will be mapped
	  * with `map`.
	  */
	def map[S :ColumnForm, T :NullValue.Maybe](name :String)(map :S => T)(unmap :T => S) :ColumnForm[T] = {
		implicit val nullValue = NullValue.orElse(ColumnForm[S].nulls.map(map))
		new DerivedMappedColumnForm(name, map, unmap)
	}


	/** Creates a `ColumnForm[T]` based on implicit `ColumnForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * the [[net.noresttherein.oldsql.schema.ColumnReadForm!.nullValue one in the source form]] will be
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.optMap mapped]].
	  */
	def optMap[S :ColumnForm, T :NullValue.Maybe](map :S => Option[T])(unmap :T => Option[S]) :ColumnForm[T] = {
		implicit val nullValue = NullValue.orElse(ColumnForm[S].nulls.optMap(map))
		new OptMappedColumnForm(map, unmap)
	}

	/** Creates a new `ColumnForm[T]` of 'soft type' given as `name` argument, based on an implicit form for type `S`
	  * and an optional `NullValue[T]`. This is equivalent in function to
	  * `ColumnForm`[[net.noresttherein.oldsql.schema.ColumnForm.optMap[S,T](map:S=>Option[T])(unmap :T=>Option[S])* .optMap(map)(unmap)]],
	  * but the created form will equal any other `ColumnForm` created by this method if they have the same name
	  * and their base forms and [[net.noresttherein.oldsql.schema.SQLReadForm.nulls null values]] are equal.
	  *
	  * If no implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue]]`[T]` is present,
	  * the [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue one present in the source form]] will be mapped
	  * with `optMap`.
	  */
	def optMap[S :ColumnForm, T :NullValue.Maybe](name :String)
	                                             (map :S => Option[T])(unmap :T => Option[S]) :ColumnForm[T] =
	{
		implicit val nullValue = NullValue.orElse(ColumnForm[S].nulls.optMap(map))
		new DerivedOptMappedColumnForm(name, map, unmap)
	}



	/** A proxy form which will delegate all calls to the form returned by the given expression. The expression
	  * will be evaluated only if/when needed, but must be thread safe and may be executed more than once
	  * if several threads trigger the initialization at the same time, although the returned form is thread safe.
	  * The proxy is shallow: all methods returning another form are delegated to the backing form, evaluating them.
	  * If you wish them to remain lazy, invoke them directly on the backing form in the evaluation expression instead.
	  * This is useful when the initialization expression cannot be successfully evaluated at this time,
	  * but the form must be passed by-reference to some method. In other cases a `lazy val` or
	  * [[net.noresttherein.oldsql.morsels.Lazy Lazy]] wrapper are preferable, as they do not incur the penalty
	  * of checking for initialization and delegation at every call.
	  */
	def delayed[T](delayed: => ColumnForm[T]) :ColumnForm[T] = new LazyColumnForm(() => delayed)



	/** Creates a dummy form which always returns and writes the same value.
	  * @param value the value returned from the form's [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]],
	  *              [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] methods and set as the parameter
	  *              with [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] instead of the argument/actual values
	  *              in the `ResultSet`.
	  * @param text  an optional textual representation of the form for its `toString` method.
	  */
	def const[T :ColumnWriteForm](value :T, text :String = null) :ColumnForm[T] =
		new CombinedColumnForms[T](
			ColumnReadForm.const(ColumnWriteForm[T].sqlType, text)(value),
			ColumnWriteForm.const[T](value, text),
			Opt(text)
		)

	/** Creates a dummy form which always returns and writes the same value.
	  * All forms created with this method with the same name are equal
	  * @param name  the name of the form, used as its identifier and by its `toString` method.
	  * @param value the value returned from the form's [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]],
	  *              [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] methods and set as the parameter
	  *              with [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] instead of the argument/actual values
	  *              in the `ResultSet`.
	  */
	def const[T :ColumnWriteForm](name :String)(value :T) :ColumnForm[T] =
		combine(name)(ColumnReadForm.const(name, ColumnWriteForm[T].sqlType)(value), ColumnWriteForm.const(name)(value))

	/** Creates a dummy form which always returns and writes the same value.
	  * @param value the value returned from the form's [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method
	  *              and set as the parameter with [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
	  *              instead of the argument/actual values in the `ResultSet`.
	  * @param text  an optional textual representation of the form for its `toString` method.
	  */
	def constOpt[T :ColumnWriteForm :NullValue](value :Option[T], text :String = null) :ColumnForm[T] =
		new CombinedColumnForms[T](
			ColumnReadForm.constOpt(ColumnWriteForm[T].sqlType, text)(value),
			ColumnWriteForm.constOpt(value, text),
			Opt(text)
		)

	/** Creates a dummy form which always returns and writes the same value.
	  * Two forms created with the same name are equal, regardless of their values.
	  * @param name  the name of the form, used as its identifier and by its `toString` method.
	  * @param value the value returned from the form's [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method
	  *              and set as the parameter with [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
	  *              instead of the argument/actual values in the `ResultSet`.
	  */
	def constOpt[T :ColumnWriteForm :NullValue](name :String)(value :Option[T]) :ColumnForm[T] =
		combine(name)(
			ColumnReadForm.constOpt[T](name, ColumnWriteForm[T].sqlType)(value),
			ColumnWriteForm.constOpt[T](name)(value)
		)


	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression.
	  * @param value a by-name expression returned from the form's
	  *              [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] and
	  *              [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] methods,
	  *              and set as the parameter with [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
	  *              instead of the argument/actual values in the `ResultSet`.
	  * @param text  an optional textual representation of the form for its `toString` method.
	  */
	def eval[T :ColumnWriteForm](value: => T, text :String = null) :ColumnForm[T] =
		new CombinedColumnForms[T](
			ColumnReadForm.eval(ColumnWriteForm[T].sqlType, text)(value), ColumnWriteForm.eval(value, text), Opt(text)
		)

	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression. All forms with the same name created by this method are equal.
	  * @param name  the name of the form, used as its identifier and by its `toString` method.
	  * @param value a by-name expression returned from the form's
	  *              [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] and
	  *              [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] methods,
	  *              and set as the parameter with [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
	  *              instead of the argument/actual values in the `ResultSet`.
	  */
	def eval[T :ColumnWriteForm](name :String)(value: => T) :ColumnForm[T] =
		combine[T](name)(ColumnReadForm.eval(name, ColumnWriteForm[T].sqlType)(value), ColumnWriteForm.eval(name)(value))

	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression.
	  * @param value a by-name expression returned from the form's
	  *              [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method,
	  *              and set as the parameter with [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
	  *              instead of the argument/actual values in the `ResultSet`.
	  * @param text  an optional textual representation of the form for its `toString` method.
	  */
	def evalOpt[T :ColumnWriteForm :NullValue](value: => Option[T], text :String = null) :ColumnForm[T] =
		new CombinedColumnForms[T](
			ColumnReadForm.evalOpt(ColumnWriteForm[T].sqlType, text)(value),
			ColumnWriteForm.evalOpt(value, text),
			Opt(text)
		)

	/** Creates a dummy form which ignores its input and always reads and writes the value resulting from
	  * reevaluating the given expression. The form will equal all other forms created by this method with the same name.
	  * @param name  the name of the form, used as its identifier and by its `toString` method.
	  * @param value a by-name expression returned from the form's
	  *              [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] method,
	  *              and set as the parameter with [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]]
	  *              instead of the argument/actual values in the `ResultSet`.
	  */
	def evalOpt[T :ColumnWriteForm :NullValue](name :String)(value: => Option[T]) :ColumnForm[T] =
		combine(name)(
			ColumnReadForm.evalOpt(name, ColumnWriteForm[T].sqlType)(value), ColumnWriteForm.evalOpt(name)(value)
		)



	/** Creates a form which always returns and writes the value provided by the implicit type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` instead of the actual values
	  * in the `ResultSet` or the arguments.
	  * The type class is used as [[net.noresttherein.oldsql.schema.SQLReadForm!.nulls nulls]] property of the form,
	  * with its [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]] and, by extension,
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] method returning `nulls.value`.
	  * The result of [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] is defined as `Got(value)`.
	  * Similarly, its [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method delegates to the implicit
	  * form's set, passing as the argument `nulls.`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.value value]]
	  * instead of its argument.
	  *
	  * The form is functionally equivalent to the one created by
	  * `SQLReadForm.`[[net.noresttherein.oldsql.schema.SQLReadForm.eval eval]], but implements structural equality.
	  * The difference from similar [[net.noresttherein.oldsql.schema.SQLReadForm.defaults defaults]]
	  * is that the latter's `opt` method returns `Lack` rather than the 'null' value.
	  */
	def nullValue[T :ColumnWriteForm :NullValue] :ColumnForm[T] =
		new CombinedColumnForms[T](ColumnReadForm.nullValue[T](ColumnWriteForm[T].sqlType), ColumnWriteForm.nullValue[T])

	/** Creates a form which always returns and writes the value provided by the implicit type class
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` instead of the actual values
	  * in the `ResultSet` or the arguments.
	  * The type class is used as [[net.noresttherein.oldsql.schema.SQLReadForm!.nulls nulls]] property of the form,
	  * with its [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]] and, by extension,
	  * from its [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] method returning `nulls.value`.
	  * The result of [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] is defined as `Got(value)`.
	  * Similarly, its [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method delegates to the implicit
	  * form's set, passing as the argument `nulls.`[[net.noresttherein.oldsql.schema.SQLForm.NullValue.value value]]
	  * instead of its argument.
	  *
	  * The form is functionally equivalent to the one created by
	  * `SQLReadForm.`[[net.noresttherein.oldsql.schema.SQLReadForm.eval eval]].
	  * The difference from similar [[net.noresttherein.oldsql.schema.SQLReadForm.defaults defaults]]
	  * is that the latter's `opt` method returns `Lack` rather than the 'null' value.
	  *
	  * All forms with the same name created by this method are equal.
	  * @param name  a name for this form, used as its identifier: two forms created by this method which share
	  *              the same name are equal.
	  */
	def nullValue[T :ColumnWriteForm :NullValue](name :String) :ColumnForm[T] =
		combine(name)(ColumnReadForm.nullValue[T](name, ColumnWriteForm[T].sqlType), ColumnWriteForm.nullValue[T](name))


	//consider: none or defaults could forward setNull to setNull to further differentiate from `nullValue`
	/** Creates a dummy form which always writes the null value as defined by the implicit `NullValue`,
	  * and returns `Lack`/`nulls.value` when reading.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.defaults]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.nullValue]]
	  */
	def defaults[T :ColumnWriteForm :NullValue] :ColumnForm[T] =
		combine(ColumnReadForm.defaults[T](ColumnWriteForm[T].sqlType), ColumnWriteForm.nullValue[T])

	/** Creates a dummy form which always writes the null value as defined by the implicit `NullValue` type class,
	  * and returns `Lack`/`nulls.value` when reading.
	  * @param name  a name for this form, used as its identifier: two forms created by this method which share
	  *              the same name are equal.
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.defaults]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.nullValue]]
	  */
	def defaults[T :ColumnWriteForm :NullValue](name :String) :ColumnForm[T] =
		combine(name)(ColumnReadForm.defaults[T](ColumnWriteForm[T].sqlType), ColumnWriteForm.nullValue[T])


	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
	  * and returns `None` when reading. All calls to `apply` and `set`
	  * will result in a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  */
	def none[T :ColumnWriteForm] :ColumnForm[T] =
		combine(ColumnReadForm.none[T](ColumnWriteForm[T].sqlType), ColumnWriteForm.none[T])
//
//	/** Creates a dummy form which always writes the null value as defined by the implicit `write` form,
//	  * and returns `None` when reading. All calls to `apply` will result in a `NullPointerException`.
//	  */
//	def none[T](name :String)(implicit write :ColumnWriteForm[T]) :ColumnForm[T] =
//		combine(name)(ColumnWriteForm.none[T], ColumnReadForm.none[T](write.sqlType))

//	/** Creates a dummy form which always writes `null` and returns `None` (and throws `NullPointerException` in `apply`). */
//	def nulls[T] :ColumnForm[T] = SQLForms.NotNullNull.asInstanceOf[ColumnForm[T]]

	/** Creates a dummy form which always reads and writes `null` values, except from the `opt` method, which returns `Lack`. */
	def nulls[T >: Null](columnType :JDBCType) :ColumnForm[T] = SQLForms.NullForm(columnType)

	/** Creates a dummy form which always reads and writes `null` values, except from the `opt` method, which returns `Lack`. */
	def nulls[T >: Null] :ColumnForm[T] = SQLForms.NullForm



	/** A form which throws an exception o the specified class on the invocation of any of its methods.
	  * This applies not only to methods [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]], but also
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]]. The form will be equal to other forms created
	  * by this method as long as their column counts and the class of the thrown exception are the same.
	  * @tparam E the class of the thrown exception, which specifies at least one of the following constructors:
	  *           `()`, `(String)`, `(Throwable)`, `(String, Throwable)`.
	  */
	def error[E <: Throwable :ClassTag, T](columnType :JDBCType) :ColumnForm[T] = {
		val nulls = NullValue.error[E]
		val name = "ERROR:" + columnType + "[" + localNameOf[E] + "]"
		val read = ColumnReadForm.nullValue[T](columnType, name)(nulls)
		val write = ColumnWriteForm.error(columnType, Got(name), nulls)
		new CombinedColumnForms[T](read, write, Got(name))
	}

	/** A form which throws an exception using the given by-name expression on the invocation of any of its methods.
	  * This applies not only to methods [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]], but also
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  * @param columnType the SQL type of the mapped column, for compatibility checks with other forms.
	  * @param text       an optional textual representation of this form for its `toString` method.
	  */
	def error[T](columnType :JDBCType, text :String = null)(raise: => Nothing) :ColumnForm[T] = {
		val name =
			if (text != null) text
			else "ERROR:" + columnType + "@" + doubleToLongBits(math.random()).shortHashString
		val write = ColumnWriteForm.error(columnType, name)(raise)
		val read = ColumnReadForm.error(columnType, name)(raise)
		new CombinedColumnForms[T](read, write, Got(name))
	}

	/** A form which throws an exception using the given by-name expression on the invocation of any of its methods.
	  * This applies not only to methods [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]], but also
	  * [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]].
	  * The form will equal other forms created by this method of the same name.
	  * @param name       the name of this form, used as its identifier and in its textual `toString` representation.
	  * @param columnType the SQL type of the mapped column, for compatibility checks with other forms.
	  */
	def error[T](name :String, columnType :JDBCType)(raise: => Nothing) :ColumnForm[T] =
		combine(name)(ColumnReadForm.error(name, columnType)(raise), ColumnWriteForm.error(name, columnType)(raise))






	/** A convenience mixin trait for forms of reference types using `null` as their `nullValue`.
	  * Implements also the null literal methods to return "null".
	  * @see [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue]]
	  */
	trait NullableColumnForm[T >: Null] extends ColumnForm[T] with NullableReadForm[T] with NullSafeWriteForm[T] {
		override def nullLiteral :String = "null"
		override def inlineNullLiteral :String = nullLiteral
		override def nullColumnLiterals :Seq[String] = SingletonColumnLiteral
		abstract override def split :Seq[ColumnWriteForm[T]] = this::Nil
	}
	private[this] val SingletonColumnLiteral = Seq("null")



	trait NullSafeColumnForm[T] extends ColumnForm[T] {
		override def nullSafe :ColumnForm[T] = this
	}



	/** A convenience base class for `ColumnForm` implementations using an implicit
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] for their
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue null value]] and the provided JDBC code.
	  * @see [[net.noresttherein.oldsql.schema.ColumnReadForm.DirectColumnReadForm]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm.DirectColumnWriteForm]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	abstract class AbstractColumnForm[T](override val sqlType :JDBCType, override val text :Opt[String] = Lack)
	                                    (implicit override val nulls :NullValue[T])
		extends ColumnForm[T] with ReadFormNullValue[T]


	/** A convenience base class for `ColumnForm` implementations using an implicit
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] for their
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue null value]] and the provided JDBC code.
	  * The only difference from `AbstractColumnForm[T]` lies in the `toString` implementation:
	  * this class uses the local class name of type `T` rather than the form class name.
	  * @see [[net.noresttherein.oldsql.schema.ColumnReadForm.DirectColumnReadForm]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm.DirectColumnWriteForm]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	abstract class ReflectedColumnForm[T :NullValue :ClassTag](override val sqlType :JDBCType,
	                                                           override val text :Opt[String] = Lack)
		extends ReflectedSQLForm[T] with ColumnForm[T]


	/** A base class for `ColumnForm` implementations for standard JDBC types.
	  * It uses the provided `JDBCType` as its `toString` implementation and, in addition to
	  * other base types, implements [[net.noresttherein.oldsql.schema.ColumnForm.JDBCForm.literal literal]]
	  * as `String.valueOf(value)`, which is suitable for most, but not all, types which actually have a literal
	  * representation in SQL.
	  * @see [[net.noresttherein.oldsql.schema.ColumnForm.AbstractColumnForm]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnForm.NamedColumnForm]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnForm.ReflectedColumnForm]]
	  */
	abstract class JDBCForm[T](override val sqlType :JDBCType)(implicit override val nulls :NullValue[T])
		extends ColumnForm[T] with ReadFormNullValue[T] with DirectColumnReadForm[T] with DirectColumnWriteForm[T]
	{
		override val columnTypes :Seq[JDBCType] = PassedArray.single(sqlType)

		override def literal(value :T) :String = String.valueOf(value)

		override def notNull :ColumnForm[T] =
			new BaseFormAdapter(this)
				with UnspecifiedColumnFormAdapter with ColumnForm[T] with SingletonColumnWriteForm[T]
				with NotNullReadFormProxy[T] with NotNullWriteFormProxy[T]
			{
				override def apply(res :ResultSet, position :Int) :T = {
					val t = form.get(res, position)
					if (res.wasNull)
						throw new NullValueException("Cannot return null from " + this + ".")
					else t
				}
				override def sqlType = form.sqlType
			}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :JDBCForm[_] if other canEqual this => sqlType == other.sqlType && nulls == other.nulls
			case _  => false
		}
		override def hashCode :Int = sqlType.hashCode * 31 + nulls.hashCode
		override def toString :String = sqlType.toString
	}


	/** A `ColumnForm` for classes supported directly by JDBC drivers which do not have their own getters and setters.
	  * The form uses [[java.sql.ResultSet.getObject getObject]]/[[java.sql.PreparedStatement.setObject setObject]]
	  * to read and write the value, respectively, providing the class from the implicit `ClassTag[T]` as the argument.
	  * The `sqlType` argument is currently used only in [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]]
	  * method and in most cases can be omitted.
	  */
	class JDBCObjectForm[T](private val runtimeClass :Class[T], override val sqlType :JDBCType = JDBCType.JAVA_OBJECT)
	                       (implicit override val nulls :NullValue[T])
		extends JDBCForm[T](sqlType) with NullSafeColumnForm[T]
	{
		def this(sqlType :JDBCType)(implicit nulls :NullValue[T], tag :ClassTag[T]) =
			this(tag.runtimeClass.asInstanceOf[Class[T]], sqlType)

		def this()(implicit nulls :NullValue[T], tag :ClassTag[T]) =
			this(tag.runtimeClass.asInstanceOf[Class[T]])

		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			statement.setObject(position, value, sqlType)

		protected override def get(res :ResultSet, position :Int) :T =
			res.getObject(position, runtimeClass)

		override def notNull :ColumnForm[T] =
			new JDBCObjectFormSingleton[T](runtimeClass, sqlType)(NotNull) {
				override def apply(res :ResultSet, position :Int) :T = {
					val t = super.apply(res, position)
					if (res.wasNull)
						throw new NullValueException("Cannot return null from " + this + ".")
					t
				}

				override val toString :String = innerNameOf(runtimeClass) + ".notNull"
			}

		override def comparable(other :SQLReadForm[_]) :Boolean = other match {
			case _ if this eq other => true
			case form :JDBCObjectForm[_] => getClass == form.getClass || sqlType == form.sqlType
			case _ => super.comparable(other)
		}
		override def comparable(other :SQLWriteForm[_]) :Boolean = other match {
			case _ if this eq other => true
			case form :JDBCObjectForm[_] => getClass == form.getClass || sqlType == form.sqlType
			case _ => super.comparable(other)
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case form :JDBCObjectForm[_] if form canEqual this =>
				runtimeClass == form.runtimeClass && sqlType == form.sqlType && nulls == form.nulls
			case _ => false
		}
		override def hashCode :Int = runtimeClass.hashCode * 31 + sqlType.hashCode

		override def toString :String = string
		private[this] val string = innerNameOf(runtimeClass)
	}

	private[schema] class JDBCObjectFormSingleton[T](cls :Class[T], sqlType :JDBCType = JDBCType.JAVA_OBJECT)
	                                                (implicit override val nulls :NullValue[T])
		extends JDBCObjectForm[T](cls, sqlType) with SingletonColumnWriteForm[T]
	{
		def this(sqlType :JDBCType)(implicit nulls :NullValue[T], tag :ClassTag[T]) =
			this(tag.runtimeClass.asInstanceOf[Class[T]], sqlType)

		def this()(implicit nulls :NullValue[T], tag :ClassTag[T]) = this(tag.runtimeClass.asInstanceOf[Class[T]])
	}



	/** A convenience base class for `ColumnForm` implementations using implicit
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] for their
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue null value]] and having a predefined name.
	  * Only the latter differentiates it from
	  * [[net.noresttherein.oldsql.schema.ColumnForm.AbstractColumnForm AbstractColumnForm]], which uses
	  * inner class name in its `toString` method instead.
	  *
	  * Note that this form does not implement `equals`, in particular two forms of the same may be unequal.
	  * @see [[net.noresttherein.oldsql.schema.ColumnReadForm.DirectColumnReadForm]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm.DirectColumnWriteForm]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiterals]]
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	private trait NamedColumnForm[T] extends ColumnForm[T] with NamedForm[T] {
		override def nullSafe :ColumnForm[T] = super[ColumnForm].nullSafe
		override def notNull  :ColumnForm[T] = super[ColumnForm].notNull
		override def withNull(implicit nulls :NullValue[T]) :ColumnForm[T] = super[ColumnForm].withNull
	}



	private class CombinedColumnForms[T](override val reader :ColumnReadForm[T], override val writer :ColumnWriteForm[T],
	                                     protected override val text :Opt[String] = Lack)
		extends CombinedForms[T](reader, writer, text) with ColumnForm[T] with SingletonColumnWriteForm[T]
	{
		if (reader.sqlType != writer.sqlType)
			throw new IllegalArgumentException(
				"Cannot combine forms " + writer + " and " + reader + " as they have different underlying column types: " +
					writer.sqlType + " vs. " + reader.sqlType + "."
			)
		override val sqlType = reader.sqlType

		override def split = writer.split

		override def notNull = (reader.notNull, writer.notNull) match {
			case (this.reader, this.writer) => this
			case (r, w) => new CombinedColumnForms(r, w, Got(toString + ".notNull"))
		}
	}

}
