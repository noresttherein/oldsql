package net.noresttherein.oldsql.schema

import java.lang.Double.doubleToLongBits
import java.sql.{CallableStatement, JDBCType, ResultSet}

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{NullValueException, OldSQLException}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.ColumnReadForm.FallbackColumnReadForm
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull
import net.noresttherein.oldsql.schema.SQLReadForm.{ConstSQLReadForm, DefaultsSQLReadForm, EvalNullValueSQLReadForm, EvalOptSQLReadForm, FallbackSQLReadForm, NotNullReadFormProxy, ReadFormAdapter, ReadFormNullGuard}
import net.noresttherein.oldsql.schema.forms.{CustomColumnReadForm, CustomOptColumnReadForm, DerivedMappedColumnReadForm, DerivedMappedSQLReadForm, DerivedOptMappedColumnReadForm, DerivedOptMappedSQLReadForm, LazyColumnReadForm, LazyReadForm, LazySQLReadForm, MappedColumnReadForm, MappedSQLReadForm, NotNullCustomReadForm, OffsetColumnReadForm, OptMappedColumnReadForm, OptMappedSQLReadForm, SQLForms, UnspecifiedColumnForm}
import net.noresttherein.oldsql.schema.forms.UnspecifiedForm.{BaseFormAdapter, UnspecifiedColumnFormAdapter, UnspecifiedNamedForm}
import net.noresttherein.oldsql.slang.{classNameMethods, localNameOf}






/** An `SQLReadForm` describing the format of a simple type `T`, mapping to a single database column.
  * Aside from fixing the `columnCount` method to return `1` and overloaded `apply` and `opt` for reading
  * the value from the column of the provided name, it enables static checks that type `T` is a valid type
  * for a single mapped column.
  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnForm]]
  */
@implicitNotFound("I do not know how to read a column of type ${T} from a ResultSet: " +
                  "missing implicit ColumnReadForm[${T}].")
trait ColumnReadForm[+T] extends SQLReadForm[T] with UnspecifiedColumnForm {

	def apply(column :String)(res :ResultSet) :T = apply(res, res.findColumn(column))

	def opt(column :String)(res :ResultSet) :Opt[T] = opt(res, res.findColumn(column))

	protected override def errorMessage(res :ResultSet, position :Int) :String =
		"Null values not allowed for column " + res.getMetaData.getColumnName(position) + ":" + this + "."

	override def register(call :CallableStatement, position :Int) :Unit =
		call.registerOutParameter(position, sqlType)


	override def map[X :NullValue](f :T => X) :ColumnReadForm[X] =
		ColumnReadForm.map(f)(this, NullValue[X])

	override def map[X](f :T => X, nullValue :X) :ColumnReadForm[X] = map(f)(NullValue(nullValue))

	override def nullMap[X](f :T => X) :ColumnReadForm[X] = map(f)(nulls.map(f))


	override def optMap[X :NullValue](f :T => Option[X]) :ColumnReadForm[X] =
		ColumnReadForm.optMap(f)(this, NullValue[X])

	override def optMap[X](f :T => Option[X], nullValue :X) :ColumnReadForm[X] = optMap(f)(NullValue(nullValue))

	override def nullOptMap[X](f :T => Option[X]) :ColumnReadForm[X] = optMap(f)(nulls.optMap(f))


	override def to[X :NullValue](f :T =?> X) :ColumnReadForm[X] = ColumnReadForm(f)(this, NullValue[X])

	override def to[X](f :T =?> X, nullValue :X) :ColumnReadForm[X] = to(f)(NullValue(nullValue))

	override def nullTo[X](f :T =?> X) :ColumnReadForm[X] = to(f)(nulls.andThen(f))

	override def andThen[X :NullValue](f :T =?> X) :ColumnReadForm[X] = to(f)

	override def andThen[X](f :T =?> X, nullValue :X) :ColumnReadForm[X] = to(f)(NullValue(nullValue))

	override def andThenNull[X](f :T =?> X) :ColumnReadForm[X] = to(f)(nulls.andThen(f))


	override def toOpt :ColumnReadForm[Option[T]] = SQLForms.OptionColumnReadForm(this)

	override def notNull :ColumnReadForm[T] =
		new BaseFormAdapter(this) with UnspecifiedColumnFormAdapter with ColumnReadForm[T] with NotNullReadFormProxy[T]

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

	override def reorder(permutation :IndexedSeq[Int]) :ColumnReadForm[T] =
		if (permutation.length != 1 || permutation.head != 0)
			throw new IllegalArgumentException(
				"The only valid permutation of a single column is Seq(0), but got : " + permutation + "."
			)
		else
			this


	override def >>(shift :Int) :ColumnReadForm[T] =
		if (shift == 0) this else new OffsetColumnReadForm[T](this, shift)

	override def <<(shift :Int) :ColumnReadForm[T] = this >> -shift


	override def <>[O >: T](write :SQLWriteForm[O]) :SQLForm[O] = write match {
		case atom :ColumnWriteForm[O] => this <> atom
		case _ => super.<>(write)
	}

	def <>[O >: T](write :ColumnWriteForm[O]) :ColumnForm[O] = ColumnForm.combine(this, write)


	override def comparable(other: SQLReadForm[_]): Boolean = other match {
		case a :ColumnReadForm[_] => a.sqlType == sqlType
		case _ => super.comparable(other)
	}

}






object ColumnReadForm {

	/** Summons an implicit `ColumnReadForm[T]`. */
	@inline def apply[T](implicit form :ColumnReadForm[T]) :ColumnReadForm[T] = form


	/** Creates a new `ColumnReadForm` using the given function to read the value from the result set.
	  * If the column value is null as defined by `ResultSet.wasNull`, the implicitly available `NullValue[T]`
	  * will be used as the result rather than the return value of the function.
	  * @param columnType a JDBC code for the SQL type of the read column
	  * @param reader     a function taking an SQL `ResultSet`, a column index, and reads the column as a value of `T`.
	  */
	def apply[T](columnType :JDBCType)(reader :(ResultSet, Int) => T) :ColumnReadForm[T] =
		new CustomColumnReadForm[T](columnType)(reader)

	/** Creates a new `ColumnReadForm` using the given function to read the value from the result set.
	  * If the column value is null as defined by `ResultSet.wasNull`, the implicitly available `NullValue[T]`
	  * will be returned by [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] rather than the return value
	  * of the function, while [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] will return `Lack`.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name       the name of the form, identifying it and returned by its `toString` method.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param reader     a function taking an SQL `ResultSet`, a column index, and reads the column as a value of `T`.
	  */
	def apply[T](name :String, columnType :JDBCType)(reader :(ResultSet, Int) => T) :ColumnReadForm[T] =
		new CustomColumnReadForm[T](columnType, Got(name))(reader) with UnspecifiedNamedForm


	/** Creates a new `ColumnReadForm` using the given function to read an optional value from the result set.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param reader     a function taking an SQL `ResultSet`, a column position, and reads the value of the column.
	  */
	def opt[T :NullValue](columnType :JDBCType)(reader :(ResultSet, Int) => Opt[T]) :ColumnReadForm[T] =
		new CustomOptColumnReadForm[T](columnType)(reader) { outer =>
			override def notNull :ColumnReadForm[T] =
				new CustomOptColumnReadForm[T](sqlType, Got(toString + ".notNull"))(reader)(NotNull)
					with ReadFormNullGuard[T]
		}

	/** Creates a new `ColumnReadForm` using the given function to read an optional value from the result set.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name       the name of the form, identifying it and returned by its `toString` method.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param reader     a function taking an SQL `ResultSet`, a column position, and reads the value of the column.
	  */
	def opt[T :NullValue](name :String, columnType :JDBCType)
	                     (reader :(ResultSet, Int) => Opt[T]) :ColumnReadForm[T] =
		new CustomOptColumnReadForm[T](columnType, Got(name))(reader) with UnspecifiedNamedForm



	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * the [[net.noresttherein.oldsql.schema.ColumnReadForm!.nullValue one in the source form]] will be
	  * mapped/flat mapped, depending on the mapping extractor.
	  *
	  * This method is equivalent to [[net.noresttherein.oldsql.schema.ColumnReadForm$.map map]],
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm$.optMap optMap]],
	  * or [[net.noresttherein.oldsql.schema.ColumnReadForm$.const const]], depending on the extractor type.
	  */
	def apply[S :ColumnReadForm, T :NullValue.Maybe](extract :S =?> T) :ColumnReadForm[T] =
		extract match {
			case _ :EmptyExtractor[_, _] =>
				ColumnReadForm.defaults(ColumnReadForm[S].sqlType)(NullValue.orNotNull[T])
			case _ :OptionalExtractor[_, _] =>
				optMap(extract.optional)
			case _ :IdentityExtractor[_] =>
				ColumnReadForm[S].asInstanceOf[ColumnReadForm[T]]
			case const :ConstantExtractor[_, T @unchecked] =>
				ColumnReadForm.const(ColumnReadForm[S].sqlType)(const.constant)
			case _ :RequisiteExtractor[_, _] =>
				map(extract.requisite.get)
			case _ => optMap(extract.optional)
		}

	/** Creates a new `ColumnReadForm` of a 'soft type' given as `name` argument by applying the given extractor
	  * to values read by an implicit base `ColumnReadForm[S]`. This is equivalent to
	  * `ColumnReadForm`[[net.noresttherein.oldsql.schema.ColumnReadForm.apply[S,T](extract* (extract)]],
	  * but the created form will equal any other `ColumnReadForm` created by this method if they have the same name
	  * and their base forms and [[net.noresttherein.oldsql.schema.ColumnReadForm.nulls null values]] are equal.
	  *
	  * If an implicit [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]` exists,
	  * database `NULL`s will be returned as its [[net.noresttherein.oldsql.schema.SQLForm.NullValue!.value value]].
	  * In absence of this type class, the form will instead use the given extractor to map the values
	  * returned by the mapped form's [[net.noresttherein.oldsql.schema.SQLReadForm!.nullValue nullValue]].
	  * If the extractor does not return a result when applied to `SQLReadForm[S].nullValue`,
	  * a [[NoSuchElementException]] will be thrown from the form's `nullValue`
	  * and [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] methods.
	  */
	def apply[S :ColumnReadForm, T :NullValue.Maybe](name :String)(extract :S =?> T) :ColumnReadForm[T] =
		extract match {
			case _ :EmptyExtractor[_, _] =>
				defaults(name, ColumnReadForm[S].sqlType)(NullValue.orNotNull[T])
			case _ :OptionalExtractor[_, _] =>
				optMap(name)(extract.optional)
			case const :ConstantExtractor[_, T @unchecked] =>
				ColumnReadForm.const(name, ColumnReadForm[S].sqlType)(const.constant)
			case req :RequisiteExtractor[S @unchecked, T @unchecked] =>
				map(name)(req.getter)
			case _ => optMap(name)(extract.optional)
		}

	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and a by-name expression as its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  */
	def apply[S :ColumnReadForm, T](map :S =?> T, nullValue: => T) :ColumnReadForm[T] =
		apply(map)(ColumnReadForm[S], NullValue.eval(nullValue))

	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and a by-name expression as its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  */ //todo: in Scala3 make name a separate parameter group
	def apply[S :ColumnReadForm, T](name :String, map :S =?> T, nullValue: => T) :ColumnReadForm[T] =
		apply(name)(map)(ColumnReadForm[S], NullValue.eval(nullValue))


	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * the [[net.noresttherein.oldsql.schema.ColumnReadForm!.nullValue one in the source form]] will be
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.map mapped]].
	  */
	def map[S :ColumnReadForm, T :NullValue.Maybe](f :S => T) :ColumnReadForm[T] = {
		implicit val nullValue = NullValue.orElse(ColumnReadForm[S].nulls.map(f))
		new MappedColumnReadForm[S, T](f)
	}

	/** Creates a new `ColumnReadForm` of a 'soft type' given as `name` argument by applying the given function
	  * to values read by an implicit base `ColumnReadForm[S]`. This is equivalent to
	  * `ColumnReadForm`[[net.noresttherein.oldsql.schema.ColumnReadForm.map[S,T](f:S=>T)* .map(f)]],
	  * but the created form will equal all other `ColumnReadForm`s created by this method if they have the same name
	  * and their base forms and [[net.noresttherein.oldsql.schema.ColumnReadForm.nulls null values]] are equal.
	  */
	def map[S :ColumnReadForm, T :NullValue.Maybe](name :String)(f :S => T) :ColumnReadForm[T] = {
		implicit val nullValue = NullValue.orElse(ColumnReadForm[S].nulls.map(f))
		new DerivedMappedColumnReadForm[S, T](name, f)
	}

	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and a by-name expression as its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  */
	def map[S :ColumnReadForm, T](f :S => T, nullValue: => T) :ColumnReadForm[T] =
		ColumnReadForm.map(f)(ColumnReadForm[S], NullValue.eval(nullValue))

	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and a by-name expression as its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  */ //todo: in Scala3 make name a separate parameter group
	def map[S :ColumnReadForm, T](name :String, f :S => T, nullValue: => T) :ColumnReadForm[T] =
		ColumnReadForm.map(name)(f)(ColumnReadForm[S], NullValue.eval(nullValue))


	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and, optionally, `NullValue[T]`.
	  * If no [[net.noresttherein.oldsql.schema.SQLForm.NullValue]] type class is present for the target type,
	  * the [[net.noresttherein.oldsql.schema.ColumnReadForm.defaults one in the source form]] will be
	  * [[net.noresttherein.oldsql.schema.SQLForm.NullValue.optMap mapped]].
	  */
	def optMap[S :ColumnReadForm, T :NullValue.Maybe](f :S => Option[T]) :ColumnReadForm[T] = {
		implicit val nullValue = NullValue.orElse(ColumnReadForm[S].nulls.optMap(f))
		new OptMappedColumnReadForm[S, T](f)
	}

	/** Creates a new `ColumnReadForm` of a 'soft type' given as `name` argument by applying the given function
	  * (returning the value as an `Option`) to values read by an implicit base `ColumnReadForm[S]`. This is equivalent
	  * to `ColumnReadForm`[[net.noresttherein.oldsql.schema.ColumnReadForm.optMap[S,T](f:S=>Option[T])* .optMap(f)]],
	  * but the created form will equal any other `ColumnReadForm` created by this method if they have the same name
	  * and their base forms and [[net.noresttherein.oldsql.schema.ColumnReadForm.nulls null values]] are equal.
	  */
	def optMap[S :ColumnReadForm, T :NullValue.Maybe](name :String)(f :S => Option[T]) :ColumnReadForm[T] = {
		implicit val nullValue = NullValue.orElse(ColumnReadForm[S].nulls.optMap(f))
		new DerivedOptMappedColumnReadForm[S, T](name, f)
	}

	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and a by-name expression as its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  */
	def optMap[S :ColumnReadForm, T](f :S => Option[T], nullValue: => T) :ColumnReadForm[T] =
		optMap(f)(ColumnReadForm[S], NullValue.eval(nullValue))

	/** Creates a `ColumnReadForm[T]` based on implicit `ColumnReadForm[S]` and a by-name expression as its
	  * [[net.noresttherein.oldsql.schema.SQLReadForm.nullValue nullValue]].
	  */ //todo: in Scala3 make name a separate parameter group
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
	def delayed[T](init: => ColumnReadForm[T]) :ColumnReadForm[T] =
		new LazyColumnReadForm[T] {
			@volatile protected[this] override var initializer = () => init
		}



	/** Creates a dummy form which always produces the same value, never reading from a `ResultSet`.
	  * The `value` will be returned by `apply`, by `opt` as `Got(value)`, and as the form's `nullValue`.
	  * Note that if `value` is `null`, it will be treated as a valid return value rather than 'no value'
	  * and may result in `NullPointerException`s later if the handling code is not null-safe.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param text       `toString` representation of this form.
	  * @param value      the value returned from the form's [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]]
	  *                   method.
	  */
	def const[T](columnType :JDBCType, text :String = null)(value :T) :ColumnReadForm[T] =
		constOpt(columnType, text)(Got(value))(NullValue(value))

	/** Creates a dummy form which always produces the same value, never reading from a `ResultSet`.
	  * The `value` will be returned by `apply`, by `opt` as `Got(value)`, and as the form's `nullValue`.
	  * Note that if `value` is `null`, it will be treated as a valid return value rather than 'no value'
	  * and may result in `NullPointerException`s later if the handling code is not null-safe.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name       the name of the form, identifying it and returned by its `toString` method.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param value      the value returned from the form's [[net.noresttherein.oldsql.schema.SQLReadForm.apply apply]]
	  *                   method.
	  */
	def const[T](name :String, columnType :JDBCType)(value :T) :ColumnReadForm[T] =
		constOpt(name, columnType)(Got(value))(NullValue(value))

	/** Creates a dummy form which always produces the same value, never reading from a `ResultSet`.
	  * If `value` is `Lack`, implicit `NullValue[T]` will be used by `apply`.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param text       `toString` representation of this form.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param value      the value returned from the form's [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]]
	  *                   method.
	  */
	def constOpt[T :NullValue](columnType :JDBCType, text :String = null)(value :Opt[T]) :ColumnReadForm[T] =
		new ConstSQLReadForm[T](value, 1, Opt(text)) with ColumnReadForm[T] {
			override def notNull :ColumnReadForm[T] = value match {
				case Got(null) => error(sqlType, toString + ".notNull") {
					throw new NullValueException("Cannot return a null value from " + this + ".opt.")
				}
				case _ if nulls == NotNull => this
				case _ => constOpt[T](sqlType, text.orNull)(value)(NotNull)
			}
			override val sqlType = columnType
		}

	/** Creates a dummy form which always produces the same value, never reading from a `ResultSet`.
	  * If `value` is `Lack`, implicit `NullValue[T]` will be used by `apply`.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name       the name of the form, identifying it and returned by its `toString` method.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param value      the value returned from the form's [[net.noresttherein.oldsql.schema.SQLReadForm.opt opt]]
	  *                   method.
	  */
	def constOpt[T :NullValue](name :String, columnType :JDBCType)(value :Opt[T]) :ColumnReadForm[T] =
		//overrides ConstSQLReadForm.notNull with ColumnReadForm.notNull
		new ConstSQLReadForm[T](value, 1, Got(name)) with UnspecifiedNamedForm with ColumnReadForm[T] {
			override val sqlType = columnType
		}


	/** Creates a form which always returns from its `apply` method the value
	  * obtained by reevaluating the given expression. The result of `opt` is defined as `Some(value)`.
	  * The expression must be thread safe.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param text       `toString` representation of this form.
	  * @param value      a ''by-name'' expression evaluated and returned with each call to
	  *                   [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]]
	  *                   and [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]].
	  */
	def eval[T](columnType :JDBCType, text :String = null)(value: => T) :ColumnReadForm[T] =
		evalOpt[T](columnType, text)(Got(value))(NullValue(value))

	/** Creates a form which always returns from its `apply` method the value
	  * obtained by reevaluating the given expression. The result of `opt` is defined as `Some(value)`.
	  * The expression must be thread safe.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name       the name of the form, identifying it and returned by its `toString` method.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param value      a ''by-name'' expression evaluated and returned with each call to
	  *                   [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]]
	  *                   and [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]].
	  */
	def eval[T](name :String, columnType :JDBCType)(value: => T) :ColumnReadForm[T] =
		evalOpt(name, columnType)(Got(value))(NullValue.eval(value))

	/** Creates a dummy form which always returns from its `opt` method the value obtained
	  * by reevaluating the given expression. An implicitly provided null value is used by its `nullValue` method,
	  * to which the `apply` method delegates when the former yields `Lack`. The expression must be thread safe.
	  * @param columnType  JDBC code for the SQL type of the read column.
	  * @param text       `toString` representation of this form.
	  * @param value      a ''by-name'' expression evaluated and returned with each call to
	  *                   [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]].
	  */
	def evalOpt[T :NullValue](columnType :JDBCType, text :String = null)(value: => Opt[T]) :ColumnReadForm[T] =
		new EvalOptSQLReadForm[T](value, 1, Opt(text)) with ColumnReadForm[T] { outer =>
			override def notNull :ColumnReadForm[T] =
				new EvalOptSQLReadForm[T](value, 1, Got(toString + ".notNull"))(NotNull)
					with ColumnReadForm[T] with ReadFormNullGuard[T]
				{
					override val sqlType = outer.sqlType
				}
			override val sqlType = columnType
		}

	/** Creates a dummy form which always returns from its `opt` method the value obtained
	  * by reevaluating the given expression. An implicitly provided null value is used by its `nullValue` method,
	  * to which the `apply` method delegates when the former yields `Lack`. The expression must be thread safe.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name       the name of the form, identifying it and returned by its `toString` method.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param value      a ''by-name'' expression evaluated and returned with each call to
	  *                   [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]].
	  */
	def evalOpt[T :NullValue](name :String, columnType :JDBCType)(value: => Opt[T]) :ColumnReadForm[T] =
		//overrides ConstSQLReadForm.notNull with ColumnReadForm.notNull
		new EvalOptSQLReadForm[T](value, 1, Got(name)) with UnspecifiedNamedForm with ColumnReadForm[T] {
			override val sqlType = columnType
		}


	/** Creates a form whose [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] always returns
	  * the value obtained from [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] type class.
	  * The result of [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] is defined as `Opt(value)`.
	  * The expression must be thread safe.
	  *
	  * The form is functionally equivalent to the one created by
	  * `ColumnReadForm.`[[net.noresttherein.oldsql.schema.ColumnReadForm.eval eval]], but implements `equals`
	  * as equality of the provided type class, making this method preferable when the expression is simple
	  * and can be covered by an existing or application-provided `NullValue` instance. The difference from similar
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm.defaults defaults]] is that the latter's `opt` method
	  * returns `Lack` rather than the 'null' value.
	  *
	  * The expression must be thread safe.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param text       an optional `toString` representation of this form.
	  * @param value      provider of values returned by the form.
	  */
	def nullValue[T](columnType :JDBCType, text :String = null)(implicit value :NullValue[T]) :ColumnReadForm[T] =
		value.toOption match {
			case Some(const) =>
				ColumnReadForm.const(columnType, text)(const)
			case _ =>
				new EvalNullValueSQLReadForm[T](0, Opt(text))(value) with ColumnReadForm[T] { outer =>
					override def notNull :ColumnReadForm[T] =
						new EvalNullValueSQLReadForm[T](0, Got(toString + ".notNull"))(value)
							with ColumnReadForm[T] with ReadFormNullGuard[T]
						{
							override val sqlType = outer.sqlType
						}
					override val sqlType = columnType
				}
		}

	/** Creates a form whose [[net.noresttherein.oldsql.schema.SQLReadForm!.apply apply]] always returns
	  * the value obtained from [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]] type class.
	  * The result of [[net.noresttherein.oldsql.schema.SQLReadForm!.opt opt]] is defined as `Opt(value)`.
	  *
	  * The expression must be thread safe.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name       the name of the form, identifying it and returned by its `toString` method.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param value      provider of values returned by the form.
	  */
	def nullValue[T](name :String, columnType :JDBCType)(implicit value :NullValue[T]) :ColumnReadForm[T] =
		value.toOption match {
			case Some(const) =>
				ColumnReadForm.const(name, columnType)(const)
			case _ =>
				new EvalNullValueSQLReadForm[T](1, Got(name))(value)
					with UnspecifiedNamedForm with ColumnReadForm[T]
				{
					override val sqlType = columnType
				}
		}


	/** Creates a dummy form which produces no values. Every call to `opt` will return `Lack`, while `apply`
	  * will always return the implicitly available [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]`.
	  * Note that it, being a column form, still represents a single column in the result set,
	  * unlike [[net.noresttherein.oldsql.schema.SQLReadForm.defaults SQLReadForm.defaults]]
	  * which, by default, consists of zero columns. As such, it only suppresses a present value rather than
	  * excludes the column from the schema.
	  * The difference from `ColumnReadForm.`[[net.noresttherein.oldsql.schema.ColumnReadForm.nullValue nullValue]]
	  * is that `opt` method will always return `Lack`, where the former would return the same value as in `apply`.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param text       an optional `toString` representation of this form.
	  */
	def defaults[T :NullValue](columnType :JDBCType, text :String = null) :ColumnReadForm[T] =
		new DefaultsSQLReadForm[T](1, Opt(text)) with ColumnReadForm[T] {
			override def notNull :ColumnReadForm[T] =
				if (nulls == NotNull)
					this
				else if (nulls.toOption.contains(null))
					error(sqlType, toString + ".notNull") {
						throw new NullValueException("Cannot return null from " + this + ".notNull.")
					}
				else
					super.notNull
			override val sqlType = columnType
		}

	/** Creates a dummy form which produces no values. Every call to `opt` will return `Lack`, while `apply`
	  * will always return the implicitly available [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]`[T]`.
	  * Note that it, being a column form, still represents a single column in the result set,
	  * unlike [[net.noresttherein.oldsql.schema.SQLReadForm.defaults SQLReadForm.defaults]] which, by default,
	  * consists of zero columns. As such, it only suppresses a present value rather than excludes the column
	  * from the schema.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name       the name of the form, identifying it and returned by its `toString` method.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  */
	def defaults[T :NullValue](name :String, columnType :JDBCType) :ColumnReadForm[T] =
		//overrides NullValueSQLReadForm.notNull with ColumnReadForm.notNull
		new DefaultsSQLReadForm[T](1, Got(name)) with UnspecifiedNamedForm with ColumnReadForm[T] {
			override val sqlType = columnType
		}

	/** Creates a dummy form which produces no values. Every call to `opt` will return `Lack`, while `apply`
	  * will always throw a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  * Note that it, being a column form, still represents a single column in the result set,
	  * unlike [[net.noresttherein.oldsql.schema.SQLReadForm.none SQLReadForm.none]] which, by default, consists
	  * of zero columns. As such, it only suppresses a present value rather than excludes the column from the schema.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  */
	def none[T](columnType :JDBCType) :ColumnReadForm[T] =
		defaults("NONE:" + columnType, columnType)(NullValue.NotNull)

	/** A dummy form which produces no values. Every call to `opt` will return `Lack`, while `apply`
	  * will always throw a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
	  * Note that it, being a column form, still represents a single column in the result set,
	  * unlike [[net.noresttherein.oldsql.schema.SQLReadForm.none SQLReadForm.none]] which, by default, consists
	  * of zero columns. As such, it only suppresses a present value rather than excludes the column from the schema.
	  */ //a method rather than a val because when combining <> with a write form we must provide the type for the compiler
	def none[T] :ColumnReadForm[T] = noneForm

	private[this] val noneForm = defaults("NONE", JDBCType.NULL)(NullValue.NotNull)

//	/** Creates a dummy form which produces no values. Every call to `opt` will return `Lack`, while `apply`
//	  * will always throw a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]].
//	  * Note that it, being a column form, still represents a single column in the result set,
//	  * unlike [[net.noresttherein.oldsql.schema.SQLReadForm.none SQLReadForm.none]] which, by default, consists
//	  * of zero columns. As such, it only suppresses a present value rather than excludes the column from the schema.
//	  *
//	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
//	  * two forms of the same name returned by this method
//	  * or [[net.noresttherein.oldsql.schema.ColumnReadForm.defaults defaults]] will compare equal,
//	  * regardless of their properties. They will not compare equal however to forms with the same name
//	  * created by other methods.
//	  * @param name       the name of the form, identifying it and returned by its `toString` method.
//	  * @param columnType a JDBC code for the SQL type of the read column.
//	  */
//	def none[T](name :String, columnType :JDBCType) :ColumnReadForm[T] =
//		defaults(name, columnType)(NullValue.NotNull)

	/** A single column form which always returns `null`. */
	def nulls[T >: Null] :ColumnReadForm[T] = SQLForms.NullForm


	/** A form always throwing the specified exception instead of producing a value. Note that this applies also to
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm!.opt opt]] method.
	  * See [[net.noresttherein.oldsql.schema.ColumnReadForm.none none]] and
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm.defaults defaults]] if you wish the form to simply return `Lack`.
	  * This functions the same way as `eval`, but can more clearly define intent.
	  * @tparam E a `Throwable` class which defines at least one of the following constructors:
	  *           `(String, Throwable)`, `(String)`, `(Throwable)`, `()`.
	  */
	def error[E <: Throwable :ClassTag] :ColumnReadForm[Nothing] =
		defaults(JDBCType.OTHER, "ERROR[" + localNameOf[E] + "]")(NullValue.error[E])

	/** A form always throwing the specified exception instead of producing a value. Note that this applies also to
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm!.opt opt]] method.
	  * See [[net.noresttherein.oldsql.schema.ColumnReadForm.none none]] and
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm.defaults defaults]] if you wish the form to simply return `Lack`.
	  * @tparam E a `Throwable` class which defines at least one of the following constructors:
	  *           `(String, Throwable)`, `(String)`, `(Throwable)`, `()`.
	  * @param columnType a JDBC code for the SQL type of the read column.*
	  */
	def error[E <: Throwable :ClassTag](columnType :JDBCType) :ColumnReadForm[Nothing] =
		defaults(columnType, "ERROR:" + columnType + "[" + localNameOf[E] + "]")(NullValue.error[E])

	/** A form always throwing the given exception instead of producing a value. Note that this applies also to
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm!.opt opt]] method.
	  * See [[net.noresttherein.oldsql.schema.ColumnReadForm.none none]] and
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm.defaults defaults]] if you wish the form to simply return `Lack`.
	  * This functions the same way as `eval`, but can more clearly define intent.
	  *
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param text       an optional `toString` representation of the form.
	  * @param raise      an expression throwing an exception, evaluated with each call to form's `apply` and `opt`
	  *                   methods.
	  */
	def error(columnType :JDBCType, text :String = null)(raise: => Nothing) :ColumnReadForm[Nothing] = {
		val name =
			if (text != null) text
			else "ERROR:" + columnType + "@" + doubleToLongBits(math.random()).shortHashString
		eval(columnType, name)(raise)
	}

	/** A form always throwing the given exception instead of producing a value. Note that this applies also to
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm!.opt opt]] method.
	  * See [[net.noresttherein.oldsql.schema.ColumnReadForm.none none]] and
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm.defaults defaults]] if you wish the form to simply return `Lack`.
	  * This functions the same way as `eval`, but can more clearly define intent.
	  *
	  * The `name` parameter serves as a kind of pseudo type and identifier of the form, defining its identity:
	  * two forms of the same name returned by this method will compare equal, regardless of their properties.
	  * They will not compare equal however to forms with the same name created by other methods.
	  * @param name       the name for the form, serving as its identifier and used in its `toString` representation.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param raise      an expression throwing an exception, evaluated with each call to form's `apply` and `opt`
	  *                   methods.
	  */
	def error(name :String, columnType :JDBCType)(raise: => Nothing) :ColumnReadForm[Nothing] =
		eval(name, columnType)(raise)

	/** A form always throwing the given exception instead of producing a value. Note that this applies also to
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm!.opt opt]] method.
	  * See [[net.noresttherein.oldsql.schema.ColumnReadForm.none none]] and
	  * [[net.noresttherein.oldsql.schema.ColumnReadForm.defaults defaults]] if you wish the form to simply return `Lack`.
	  * This functions the same way as `eval`, but can more clearly define intent.
	  * @param raise      an expression throwing an exception, evaluated with each call to form's `apply` and `opt`
	  *                   methods.
	  */
	def error(raise: => Nothing) :ColumnReadForm[Nothing] =
		eval(JDBCType.OTHER, "ERROR@" + doubleToLongBits(math.random()).shortHashString)(raise)


	/** A  form which throws an [[UnsupportedOperationException]] with the given message with every read attempt.
	  * @param columnType a JDBC code for the SQL type of the read column.
	  * @param text       an optional `toString` representation of the form.
	  * @param message    the message included in the thrown exception.
	  * @return `nullValue(columnType, text)(NullValue.Unsupported(message))`.
	  */
	def unsupported(columnType :JDBCType, text :String = null)(message :String) :ColumnReadForm[Nothing] =
		nullValue(columnType, text)(NullValue.Unsupported(message))

	/** A  form which throws an [[UnsupportedOperationException]] with the given message with every read attempt.
	  * All forms created with this method are equal and use `JDBCType.`[[java.sql.JDBCType.OTHER OTHER]]
	  * as the column type.
	  * @param message    the message included in the thrown exception.
	  * @return `nullValue(columnType, text)(NullValue.Unsupported(message))`.
	  */
	def unsupported(message :String) :ColumnReadForm[Nothing] =
		nullValue("UNSUPPORTED", JDBCType.OTHER)(NullValue.Unsupported(message))




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
		protected def get(res :ResultSet, position :Int) :T

		override def apply(res :ResultSet, position :Int) :T = {
			val t = get(res, position)
			if (res.wasNull) try {
				nullValue
			} catch {
				case e :OldSQLException =>
					throw e.addInfo(errorMessage(res, position))
				case e :NullPointerException =>
					throw new NullPointerException(errorMessage(res, position)).initCause(e)
				case e :NoSuchElementException =>
					throw new NoSuchElementException(errorMessage(res, position)).initCause(e)
				case e :ClassCastException =>
					throw new ClassCastException(errorMessage(res, position)).initCause(e)
			} else t
		}

		override def opt(res :ResultSet, position :Int) :Opt[T] = {
			val t = get(res, position)
			if (res.wasNull) Lack else Got(t)
		}

		override def notNull :ColumnReadForm[T] =
			new BaseFormAdapter(this) with ColumnReadForm[T] with NotNullReadFormProxy[T] {
				override def apply(res :ResultSet, position :Int) :T = {
					val t = form.get(res, position)
					if (res.wasNull)
						throw new NullValueException("Cannot return null from " + this + ".")
					else t
				}
				override def sqlType = form.sqlType
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

}

