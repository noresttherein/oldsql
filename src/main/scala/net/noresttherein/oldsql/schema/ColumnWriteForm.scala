package net.noresttherein.oldsql.schema

import java.sql.{JDBCType, PreparedStatement}

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.{Opt, PassedArray}
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.ColumnWriteForm.SingletonColumnWriteForm
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLWriteForm.{ConstSQLWriteForm, ErrorSQLWriteForm, EvalSQLWriteForm, GapSQLWriteForm, IgnoringWriteForm, NamedSQLWriteForm, NamedWriteForm, NonLiteralWriteForm, NotNullWriteForm, NotNullWriteFormProxy, NullifiedSQLWriteForm, NullSafeWriteFormProxy, NullValueSQLWriteForm, WriteFormWithNull}
import net.noresttherein.oldsql.schema.forms.{CustomColumnWriteForm, CustomOptColumnWriteForm, DerivedMappedColumnWriteForm, DerivedOptMappedColumnWriteForm, LazyColumnWriteForm, MappedColumnWriteForm, OptMappedColumnWriteForm, SQLForms, UnspecifiedColumnForm}
import net.noresttherein.oldsql.schema.forms.UnspecifiedForm.{BaseFormAdapter, UnspecifiedColumnFormAdapter, UnspecifiedFormDefaults}
import net.noresttherein.oldsql.slang.{classNameMethods, localNameOf}






/** An `SQLReadForm` describing the write format of a simple type `T`, mapping to a single database column.
  * Aside from fixing the `columnCount` method to return `1` and introducing a property for the code
  * of the underlying SQL type, it enables static checks that the type `T` is a valid type for a single mapped column.
  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm.DirectColumnWriteForm]]
  */
@implicitNotFound("I do not know how to set a PreparedStatement parameter of type ${T}: " +
                  "missing implicit ColumnWriteForm[${T}].")
trait ColumnWriteForm[-T] extends SQLWriteForm[T] with UnspecifiedColumnForm { outer =>

	override def param = "?"
	override def inlineParam = "?"

	override def split :Seq[ColumnWriteForm[T]] = this::Nil

	override def unmap[X](f :X => T) :ColumnWriteForm[X] = ColumnWriteForm.map(f)(this)

	override def optUnmap[X](f :X => Option[T]) :ColumnWriteForm[X] = ColumnWriteForm.optMap(f)(this)

	override def from[X](extractor :X =?> T) :ColumnWriteForm[X] = compose(extractor)

	override def compose[X](extractor :X =?> T) :ColumnWriteForm[X] = ColumnWriteForm(extractor)(this)


	override def toOpt :ColumnWriteForm[Option[T]] = SQLForms.OptionColumnWriteForm(this)

	override def nullSafe :ColumnWriteForm[T] =
		new BaseFormAdapter(this) with UnspecifiedColumnFormAdapter with ColumnWriteForm[T]
			with NullSafeWriteFormProxy[T] with SingletonColumnWriteForm[T]

	override def notNull :ColumnWriteForm[T] =
		new BaseFormAdapter(this) with UnspecifiedColumnFormAdapter with ColumnWriteForm[T]
			with NotNullWriteFormProxy[T] with SingletonColumnWriteForm[T]

	override def withNull(implicit nulls :NullValue[T]) :ColumnWriteForm[T] = {
		val nullVal = nulls
		new BaseFormAdapter(this) with UnspecifiedColumnFormAdapter with ColumnWriteForm[T]
			with WriteFormWithNull[T] with SingletonColumnWriteForm[T]
		{
			override def nulls = nullVal
			override def nullValue = nullVal.value
			override def withNull(implicit nulls :NullValue[T]) :ColumnWriteForm[T] = form.withNull
		}
	}


	override def withNull(nullValue :T) :ColumnWriteForm[T] = withNull(NullValue(nullValue))

	override def reorder(permutation :IndexedSeq[Int]) :ColumnWriteForm[T] =
		if (permutation.length != 1 || permutation.head != 0)
			throw new IllegalArgumentException(
				"The only valid permutation of a single column is Seq(0), but got : " + permutation + "."
			)
		else
			this

//	override def >>(shift :Int) :ColumnWriteForm[T] =
//		if (shift == 0) this else new OffsetColumnWriteForm(this, shift)
//
//	override def <<(shift :Int) :ColumnWriteForm[T] = this >> -shift

	override def <>[O <: T](read :SQLReadForm[O]) :SQLForm[O] = read match {
		case atom :ColumnReadForm[O] => this <> atom
		case _ => super.<>(read)
	}

	def <>[O <: T](read :ColumnReadForm[O]) :ColumnForm[O] = ColumnForm.combine(read, this)



	override def comparable(other: SQLWriteForm[_]): Boolean = other match {
		case a :ColumnWriteForm[_] => a.sqlType == sqlType
		case _ => super.comparable(other)
	}

}






object ColumnWriteForm {

	/** Summon an implicitly available `SQLWriteForm[T]`. */
	@inline def apply[T](implicit form :ColumnWriteForm[T]) :ColumnWriteForm[T] = form


	/** Creates a non-literal `ColumnWriteForm` using the given function to set statement parameters
	  * based on a value of `T`. The function becomes the body of the form's
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method. Its
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] simply uses the statement's
	  * [[java.sql.PreparedStatement.setNull setNull]] method with the column type as an argument.
	  * Arguments to `set` are not inspected for nullity, however. This can be achieved by using
	  * the [[net.noresttherein.oldsql.schema.SQLWriteForm.notNull notNull]] variant of the returned form.
	  * @param columnType the JDBC code for the SQL type of the parameter.
	  * @param writer     a function taking a statement, index of the parameter to set, and a value of the parameter.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	def apply[T](columnType :JDBCType)(writer :(PreparedStatement, Int, T) => Unit) :ColumnWriteForm[T] =
		new CustomColumnWriteForm[T](columnType)(writer)

	/** Creates a non-literal `ColumnWriteForm` using the given function to set statement parameters
	  * based on a value of `T`. The function becomes the body of the form's
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method. Its
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] simply uses the statement's
	  * [[java.sql.PreparedStatement.setNull setNull]] method with the column type as an argument.
	  * Arguments to `set` are not inspected for nullity, however. This can be achieved by using
	  * the [[net.noresttherein.oldsql.schema.SQLWriteForm.notNull notNull]] variant of the returned form.
	  *
	  * Two forms created by this method are equal if they have the same name.
	  * @param name       the name of the form, used as its identifier and returned by its `toString` method.
	  * @param columnType the JDBC code for the SQL type of the parameter.
	  * @param writer     a function taking a statement, index of the parameter to set, and a value of the parameter.
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.NonLiteralWriteForm]]
	  */
	def apply[T](name :String, columnType :JDBCType)(writer :(PreparedStatement, Int, T) => Unit) :ColumnWriteForm[T] =
		new CustomColumnWriteForm[T](columnType, Got(name))(writer) with NamedColumnWriteForm[T]


	/** Creates a non-literal `ColumnWriteForm` using the given function to set statement parameters based on a value of `T`.
	  * The function becomes the body of the form's [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]]
	  * method, and methods [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] delegate to it by wrapping the parameter
	  * in `Got` or passing `Lack`, respectively.
	  * @param columnType the sql type of the set parameter.
	  * @param writer     a function taking a statement, index of the parameter to set, an optional value
	  *                   of the parameter.
	  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm.NonLiteralColumnWriteForm]]
	  */
	def opt[T](columnType :JDBCType)(writer :(PreparedStatement, Int, Opt[T]) => Unit) :SQLWriteForm[T] =
		new CustomOptColumnWriteForm[T](columnType)(writer)

	/** Creates a non-literal `ColumnWriteForm` using the given function to set statement parameters based on a value of `T`.
	  * The function becomes the body of the form's [[net.noresttherein.oldsql.schema.SQLWriteForm.setOpt setOpt]]
	  * method, and methods [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] and
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] delegate to it by wrapping the parameter
	  * in `Got` or passing `Lack`, respectively.
	  *
	  * Two forms created by this method are equal if they have the same name.
	  * @param name       the name of the form, used as its identifier and returned by its `toString` method.
	  * @param columnType the sql type of the set parameter.
	  * @param writer     a function taking a statement, index of the parameter to set, an optional value
	  *                   of the parameter.
	  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm.NonLiteralColumnWriteForm]]
	  */
	def opt[T](name :String, columnType :JDBCType)(writer :(PreparedStatement, Int, Opt[T]) => Unit) :SQLWriteForm[T] =
		new CustomOptColumnWriteForm[T](columnType, Got(name))(writer) with NamedColumnWriteForm[T] {
			override def nullSafe :this.type = this
		}



	/** Composes an implicitly available write form `ColumnWriteForm[S]` with a given extractor to create
	  * a `ColumnWriteForm[T]`. This has the effect of calling either `map` or `optMap`, depending on the type
	  * of the extractor.
	  */
	def apply[S :ColumnWriteForm, T](extractor :T =?> S) :ColumnWriteForm[T] =
		extractor match {
			case _ :IdentityExtractor[_] => this.asInstanceOf[ColumnWriteForm[T]]
			case const :ConstantExtractor[_, S @unchecked] => ColumnWriteForm.const(const.constant)
			case req :RequisiteExtractor[T @unchecked, S @unchecked] => map(req.getter)
			case _ :EmptyExtractor[_, _] => ColumnWriteForm.none[S]
			case _ => optMap(extractor.optional)
		}

	/** Creates a new form with a 'soft type constructor' given as `name` argument, by combining
	  * an implicit `ColumnWriteForm[S]` with a given extractor to create an `ColumnWriteForm[T]`. It is equivalent to
	  * `ColumnWriteForm`[[net.noresttherein.oldsql.schema.ColumnWriteForm.apply[S,T](extractor)* (extractor)]],
	  * but the created form will equal any other `ColumnWriteForm` created by this method if they have the same name
	  * and their underlying forms and are equal.
	  * @param name      the name of this form's constructor, used to recognize compatible forms
	  *                  and in its `toString` implementation.
	  * @param extractor producer of values for the implicit base form to write, called for every value written
	  *                  with the produced form.
	  */
	def apply[S :ColumnWriteForm, T](name :String)(extractor :T =?> S) :ColumnWriteForm[T] =
		extractor match {
			case _ :IdentityExtractor[_] =>
				val res = new NamedSQLWriteForm[S](ColumnWriteForm[S], Got(name))
					with ColumnWriteForm[S] with UnspecifiedColumnFormAdapter
				{
					override val form :ColumnWriteForm[S] = ColumnWriteForm[S]
				}
				res.asInstanceOf[ColumnWriteForm[T]]
			case const :ConstantExtractor[_, S @unchecked] => ColumnWriteForm.const(const.constant, name)
			case req :RequisiteExtractor[T @unchecked, S @unchecked] => map(name)(req.getter)
			case _ :EmptyExtractor[_, _] => ColumnWriteForm.none[S](name)
			case _ => optMap(name)(extractor.optional)
		}

	/** Composes an implicitly available write form `ColumnWriteForm[S]` with a given getter function to create
	  * an `ColumnWriteForm[T]`. This function can in particular be used to present a `ColumnWriteForm` for the type
	  * of a property of some entity type as a write form for said entity type. Such forms can be later combined
	  * together with [[net.noresttherein.oldsql.schema.SQLWriteForm.join SQLWriteForm.join(forms)]] to create
	  * an `SQLWriteForm` for the whole entity.
	  */
	def map[S :ColumnWriteForm, T](f :T => S) :ColumnWriteForm[T] =
		new MappedColumnWriteForm[S, T](f)

	/** Creates a new form with a 'soft type constructor ' given as `name` argument, by combining
	  * an implicit `ColumnWriteForm[S]` with a given function to create an `ColumnWriteForm[T]`. It is equivalent to
	  * `ColumnWriteForm`[[net.noresttherein.oldsql.schema.ColumnWriteForm.map[S,T](f:T=>S)* .map(f)]],
	  * but the created form will equal any other `ColumnWriteForm` created by this method if they have the same name
	  * and their underlying forms and are equal.
	  * @param name the name of this form constructor, used to recognize compatible forms and in `toString` implementation.
	  * @param f    the function mapping the arguments of the created form to values accepted by the implicit base form.
	  */
	def map[S :ColumnWriteForm, T](name :String)(f :T => S) :ColumnWriteForm[T] =
		new DerivedMappedColumnWriteForm[S, T](name, f)

	/** Composes an implicitly available write form `ColumnWriteForm[S]` with a given getter to create
	  * a `ColumnWriteForm[T]`. If the function returns `None`, the created form will use
	  * [[net.noresttherein.oldsql.schema.ColumnWriteForm.setNull setNull]] method of the base form instead of
	  * [[net.noresttherein.oldsql.schema.ColumnWriteForm.set set]].
	  */
	def optMap[S :ColumnWriteForm, T](f :T => Option[S]) :ColumnWriteForm[T] =
		new OptMappedColumnWriteForm[S, T](f)

	/** Creates a new form with a 'soft type constructor' given as `name` argument, by combining
	  *  an implicit `ColumnWriteForm[S]` with a given function (returning the result value as an `Option`)
	  *  to create an `ColumnWriteForm[T]`. It is equivalent
	  * to `ColumnWriteForm.`[[net.noresttherein.oldsql.schema.ColumnWriteForm.optMap[S,T](f:T=>Option[S])* .optMap(f)]],
	  * but the created form will equal any other `ColumnWriteForm` created by this method if they have the same name
	  * and their underlying forms and are equal.
	  * @param name the name of this form constructor, used to recognize compatible forms and in `toString` implementation.
	  * @param f    the function mapping the arguments of the created form to values accepted by the implicit base form.
	  */
	def optMap[S :ColumnWriteForm, T](name :String)(f :T => Option[S]) :ColumnWriteForm[T] =
		new DerivedOptMappedColumnWriteForm[S, T](name, f)



	/** A proxy form which will delegate all calls to the form returned by the given expression. The expression
	  * will be evaluated only if/when needed, but must be thread safe and may be executed more than once
	  * if several threads trigger the initialization at the same time, but the returned form is thread safe.
	  * The proxy is shallow: all methods returning another form are delegated to the backing form, evaluating it.
	  * If you wish them to remain lazy, invoke them directly on the backing form in the evaluation expression instead.
	  * This is useful when the initialization expression cannot be successfully evaluated at this time,
	  * but the form must be passed by-reference to some method. In other cases a `lazy val` or
	  * of checking for initialization and delegation at every call.
	  */
	def delayed[T](delayed: => ColumnWriteForm[T]) :ColumnWriteForm[T] =
		new LazyColumnWriteForm[T] {
			@volatile protected[this] override var initializer = () => delayed
		}



	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `ColumnWriteForm[T]`.
	  * @param value the value returned by the form's [[net.noresttherein.oldsql.schema.SQLWriteForm!.set set]] method.
	  * @param text  an optional textual representation of the form used in its `toString` method.
	  */ //In Scala 2, ColumnWriteForm.const("boo") doesn't compile for some reason
	def const[T :ColumnWriteForm](value :T, text :String = null) :ColumnWriteForm[Any] =
		if (value == null)
			none[T](ColumnWriteForm[T], text)
		else
            new ConstSQLWriteForm[T](value, Opt(text)) with IgnoringColumnWriteForm with UnspecifiedColumnFormAdapter {
	            outer =>
	            override def notNull :ColumnWriteForm[Any] =
		            new ConstSQLWriteForm[T](value, Got(toString + ".notNull"))
			            with IgnoringColumnWriteForm with NotNullWriteForm[Any] with UnspecifiedColumnFormAdapter
		            {
			            override val form = outer.form
		            }
	            override val form = ColumnWriteForm[T]
			}

	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `ColumnWriteForm[T]`. Two forms created by this method are equal if they share the same name;
	  * the values or underlying forms are not compared.
	  * @param name  the name of the created form, used as its identifier and by its `toString` method.
	  * @param value the value returned from the form's [[net.noresttherein.oldsql.schema.SQLWriteForm!.set set]] method.
	  */
	def const[T :ColumnWriteForm](name :String)(value :T) :ColumnWriteForm[Any] =
		if (value == null)
			none[T](name)
		else
			new ConstSQLWriteForm[T](value, Got(name)) with NamedColumnWriteForm[Any] with IgnoringColumnWriteForm {
				override val sqlType = ColumnWriteForm[T].sqlType
			}

	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `ColumnWriteForm[T]`.
	  * @param value the value returned by the form's [[net.noresttherein.oldsql.schema.SQLWriteForm!.set set]] method.
	  * @param text  an optional textual representation of the form used in its `toString` method.
	  */
	def constOpt[T :ColumnWriteForm](value :Opt[T], text :String = null) :ColumnWriteForm[Any] =
		if (value.isEmpty) none[T](ColumnWriteForm[T], text)
		else const(value.get, text)

	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `ColumnWriteForm[T]`. If the value is `Lack`, the form will delegate all
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] calls to method
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]] of the underlying form. Two forms
	  * created by this method are equal if they share the same name; the values or underlying forms are not compared.
	  * @param name  the name of the created form, used as its identifier and by its `toString` method.
	  * @param value the value returned by the form's[[net.noresttherein.oldsql.schema.SQLWriteForm!.set set]] method.
	  */
	def constOpt[T :ColumnWriteForm](name :String)(value :Opt[T]) :ColumnWriteForm[Any] =
		if (value.isEmpty) none[T](name)
		else const(name)(value.get)


	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `ColumnWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form.
	  * @param value a ''by name'' parameter evaluated with each call to the form's
	  *              [[net.noresttherein.oldsql.schema.SQLWriteForm!.set set]] method.
	  * @param text  an optional textual representation of the form used in its `toString` method.
	  */
	def eval[T :ColumnWriteForm](value: => T, text :String = null) :ColumnWriteForm[Any] =
		evalOpt(Got(value), text)

	/** A form which will ignore all values provided as arguments and instead write the value resulting from
	  * evaluating the given by-name argument, using the implicit `ColumnWriteForm[T]`. Two forms created by this method
	  * are equal if they share the same name; the values or underlying forms are not compared.
	  * @param name  the name of the created form, used as its identifier and by its `toString` method.
	  * @param value the value returned by the form's[[net.noresttherein.oldsql.schema.SQLWriteForm!.set set]] method.
	  */
	def eval[T :ColumnWriteForm](name :String)(value: => T) :ColumnWriteForm[Any] =
		evalOpt(name)(Got(value))

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `ColumnWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form. If it yields
	  * `Lack`, the value carried by an implicitly available `NullValue` will be written instead, using the backing
	  * form's `set` method.
	  * @param value an expression becoming the body of the form's
	  *              [[net.noresttherein.oldsql.schema.SQLWriteForm!.set set]] method.
	  * @param text  an optional textual representation of the form used in its `toString` method.
	  */
	def evalOpt[T :ColumnWriteForm](value: => Opt[T], text :String = null) :ColumnWriteForm[Any] =
		new EvalSQLWriteForm[T](value, Opt(text)) with IgnoringColumnWriteForm { outer =>
			override def notNull :ColumnWriteForm[Any] =
				new EvalSQLWriteForm[T](value, this.text) with IgnoringColumnWriteForm with NotNullWriteForm[Any] {
					override val sqlType = outer.sqlType
				}
			override val sqlType = ColumnWriteForm[T].sqlType
		}

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `ColumnWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form. If it yields
	  * `Lack`, the value carried by an implicitly available `NullValue` will be written instead, using the backing
	  * form's `set` method. Two forms created by this method are equal if they share the same name; the values
	  * or underlying forms are not compared.
	  * @param name  the name of the created form, used as its identifier and by its `toString` method.
	  * @param value an expression becoming the body of the form's
	  *              [[net.noresttherein.oldsql.schema.SQLWriteForm!.set set]] method.
	  */
	def evalOpt[T :ColumnWriteForm](name :String)(value: => Opt[T]) :ColumnWriteForm[Any] =
		new EvalSQLWriteForm[T](value, Got(name)) with IgnoringColumnWriteForm with NamedColumnWriteForm[Any] {
			override val sqlType = ColumnWriteForm[T].sqlType
		}




	/** An `SQLWriteForm` ignoring its input and always writing `null` values using the implicitly given
	  * `SQLWriteForm[T]`. Null is defined here by the [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]
	  * type class, which must be also present for type `T`. This form is similar to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.none SQLWriteForm.none]], but passes the written value
	  * explicitly to the backing form's [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method,
	  * rather than delegate null handling to its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]],
	  * like the latter.
	  */
	def nullValue[T :ColumnWriteForm :NullValue] :ColumnWriteForm[Any] =
		new NullValueSQLWriteForm[T] with SingletonColumnWriteForm[Any] with IgnoringColumnWriteForm { outer =>
			override def notNull :ColumnWriteForm[Any] =
				new NullValueSQLWriteForm[T](Got(toString + ".notNull"))(ColumnWriteForm[T].notNull, NullValue[T])
					with IgnoringColumnWriteForm with NotNullWriteForm[Any]
				{
					override val sqlType = outer.sqlType
				}
			override val sqlType = ColumnWriteForm[T].sqlType
		}

	/** An `SQLWriteForm` ignoring its input and always writing `null` values using the implicitly given
	  * `SQLWriteForm[T]`. Null is defined here by the [[net.noresttherein.oldsql.schema.SQLForm.NullValue NullValue]]
	  * type class, which must be also present for type `T`. This form is similar to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.none SQLWriteForm.none]], but passes the written value
	  * explicitly to the backing form's [[net.noresttherein.oldsql.schema.SQLWriteForm.set set]] method,
	  * rather than delegate null handling to its [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]],
	  * like the latter. Two name sharing forms created by this method are always equal.
	  * @param name the name of the form, used as its identifier and in its `toString` method (and thrown exceptions).
	  */
	def nullValue[T :ColumnWriteForm :NullValue](name :String) :ColumnWriteForm[Any] =
		new NullValueSQLWriteForm[T](Got(name)) with SingletonColumnWriteForm[Any] with NamedColumnWriteForm[Any] {
			override val sqlType = ColumnWriteForm[T].sqlType
		}

	/** A `ColumnWriteForm` which will always write the 'null' value of type `T` using the implicitly available form.
	  * How `null` values are handled depends on the base form implementation of null-specific methods of `SQLWriteForm`
	  * interface, such as [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]]. The returned form
	  * will delegate all calls accepting `T` as an argument to their counterpart dedicated to `null` representation.
	  */
	def none[T :ColumnWriteForm] :ColumnWriteForm[Any] = none[T](ColumnWriteForm[T], null)

	private def none[T](write :ColumnWriteForm[T], text :String) :ColumnWriteForm[Any] =
		new NullifiedSQLWriteForm[T](Opt(text))(write) with SingletonColumnWriteForm[Any] with IgnoringColumnWriteForm {
			override def notNull :ColumnWriteForm[Any] =
				error(sqlType, Got(toString + ".notNull"), NullValue.NotNull)
			override val sqlType = write.sqlType
		}

	/** A `ColumnWriteForm` which will always write the 'null' value of type `T` using the implicitly available form.
	  * How `null` values are handled depends on the base form implementation of null-specific methods of `SQLWriteForm`
	  * interface, such as [[net.noresttherein.oldsql.schema.SQLWriteForm.setNull setNull]]. The returned form
	  * will delegate all calls accepting `T` as an argument to their counterpart dedicated to `null` representation.
	  * Two name sharing forms created by this method are always equal.
	  * @param name the name of the form, used as its identifier and in its `toString` method (and thrown exceptions).
	  */
	private def none[T :ColumnWriteForm](name :String) :ColumnWriteForm[Any] =
		new NullifiedSQLWriteForm[T](Got(name)) with NamedColumnWriteForm[Any] with IgnoringColumnWriteForm {
			override val sqlType = ColumnWriteForm[T].sqlType
		}


	/** A column form which always writes `null` values, declaring them as `JDBCType.NULL`. */
	def nulls[T] :ColumnWriteForm[T] = ColumnForm.nulls

	/** A column form which always writes `null` values. */
	def nulls[T](columnType :JDBCType) :ColumnWriteForm[T] = ColumnForm.nulls(columnType)

	/** A no-op form serving only as a padding between other forms, shifting their reading and writing starting indices.
	  * Used primarily for the result type of JDBC [[java.sql.CallableStatement CallableStatement]].
	  */
	def gap[T] :ColumnWriteForm[T] = gapForm

	private[this] val gapForm =
		new GapSQLWriteForm(1) with IgnoringColumnWriteForm {
			override def sqlType = JDBCType.NULL
		}


	/** A column form which, instead of setting a parameter, throws the exception specified by type parameter `E`.
	  * All forms created by this method and throwing the exception of the same type are equal.
	  * @tparam E an exception class which defines one of the following constructors:
	  *           `()`, `(String)`, `(Throwable)`, `(String, Throwable)`.
	  */
	def error[E <: Throwable :ClassTag] :ColumnWriteForm[Any] =
		error(JDBCType.OTHER, Got("ERROR[" + localNameOf[E] + ']'), NullValue.error[E])

	/** A column form which, instead of setting a parameter, throws the exception specified by type parameter `E`.
	  * All forms of the same JDBC type, created by this method and throwing the exception of the same type, are equal.
	  * @tparam E an exception class which defines one of the following constructors:
	  *           `()`, `(String)`, `(Throwable)`, `(String, Throwable)`.
	  * @param columnType the JDBC type of the set parameter, for compatibility checks with other forms.
	  */
	def error[E <: Throwable :ClassTag](columnType :JDBCType) :ColumnWriteForm[Any] =
		error(columnType, Got("ERROR:" + columnType + '[' + localNameOf[E] + ']'), NullValue.error[E])

	/** A column form which in its [[net.noresttherein.oldsql.schema.SQLWriteForm!.set set]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm!.setNull setNull]] methods evaluates the given
	  * exception throwing expression, without setting any parameters.
	  * @param columnType the JDBC type of the set parameter, for compatibility checks with other forms.
	  * @param text       an optional textual representation of this form for its `toString` method.
	  * @param raise      an arbitrary expression ending with throwing an exception.
	  */
	def error(columnType :JDBCType, text :String = null)(raise: => Nothing) :ColumnWriteForm[Any] = {
		val nulls = if (text == null) NullValue.eval(raise) else NullValue.eval(text, raise)
		val name = if (text == null) "ERROR:" + columnType + "@" + nulls.shortHashString else text
		error(columnType, Got(name), nulls)
	}

	/** A column form which in its [[net.noresttherein.oldsql.schema.SQLWriteForm!.set set]]
	  * and [[net.noresttherein.oldsql.schema.SQLWriteForm!.setNull setNull]] methods evaluates the given
	  * exception throwing expression, without setting any parameters. All forms created by this method and sharing
	  * the same name are equal.
	  * @param name       the name of the form, used as its identifier and in its `toString` method (and thrown exceptions).
	  * @param columnType the JDBC type of the set parameter, for compatibility checks with other forms.
	  * @param raise      an arbitrary expression ending with throwing an exception.
	  */
	def error(name :String, columnType :JDBCType)(raise: => Nothing) :ColumnWriteForm[Any] =
		error(name, columnType, NullValue.eval(name, raise))

	/** A write form which will throw the given exception at every write attempt.
	  * @param raise an arbitrary expression ending with throwing an exception.
	  */
	def error(raise: => Nothing) :ColumnWriteForm[Any] = {
		val nulls = NullValue.eval(raise)
		error(JDBCType.OTHER, Got("ERROR@" + nulls.shortHashString), nulls)
	}

	private[schema] def error(columnType :JDBCType, text :Opt[String], value :NullValue[Nothing]) :ColumnWriteForm[Any] =
		new ErrorSQLWriteForm(1, value, text) with SingletonColumnWriteForm[Any] with IgnoringColumnWriteForm {
			override val sqlType = columnType
		}

	private[schema] def error(name :String, columnType :JDBCType, value :NullValue[Nothing]) :ColumnWriteForm[Any] =
		new ErrorSQLWriteForm(1, value, Got(name))
			with SingletonColumnWriteForm[Any] with IgnoringColumnWriteForm with NamedColumnWriteForm[Any]
		{
			override val sqlType = columnType
		}


	/** A dummy column form which throws an [[UnsupportedOperationException]] on each write attempt.
	  * Used as part of `ColumnForm.combine` to convert a `ColumnReadForm` into a `ColumnForm` for situations
	  * where its write functionality is known not to be used.
	  * @param columnType the JDBC type of the set parameter, for compatibility checks with other forms.
	  * @param text       an optional textual representation of this form for its `toString` method.
	  * @param message    a message included in all thrown exceptions.
	  */
	def unsupported(columnType :JDBCType, text :String = null)(message :String) :ColumnWriteForm[Any] =
		error(columnType, Got(if (text != null) text else "UNSUPPORTED:" + columnType), NullValue.Unsupported(message))

	/** A dummy column form which throws an [[UnsupportedOperationException]] on each write attempt.
	  * Used as part of `ColumnForm.combine` to convert a `ColumnReadForm` into a `ColumnForm` for situations
	  * where its write functionality is known not to be used.
	  * @param message a message included in all thrown exceptions.
	  */
	def unsupported(message :String) :ColumnWriteForm[Any] =
		error(JDBCType.OTHER, Got("UNSUPPORTED"), NullValue.Unsupported(message))




	/** Base trait for column forms which do not rely on other forms in their implementations, but
	  * set parameters directly on the `PreparedStatement`. Returns "null" as the `null` literal from all
	  * null-specific literal methods and delegates all non-null literal methods to
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm.literal(value:T) literal]]`(value)`
	  * (essentially equating literals and inline literals).
	  */
	trait DirectColumnWriteForm[-T] extends ColumnWriteForm[T] {
		override def setNull(statement :PreparedStatement, position :Int) :Unit =
			statement.setNull(position, sqlType.getVendorTypeNumber)

		override def literal(value :T, inline :Boolean) :String = literal(value)
		override def inlineLiteral(value :T) :String = literal(value)
		override def columnLiterals(value: T): Seq[String] = literal(value)::Nil

		override def nullLiteral(inline :Boolean) :String = nullLiteral
		override def nullLiteral: String = "null"
		override def inlineNullLiteral :String = nullLiteral
		override def nullColumnLiterals: Seq[String] = NullSingletonLiteral
	}
	private val NullSingletonLiteral = Seq("null")


	/** A small mixin trait for column forms for which non-null values do not have a literal representation,
	  * such as various `Blob`s. At the same time,
	  * [[net.noresttherein.oldsql.schema.ColumnWriteForm.nullLiteral nullLiteral]] and other null literal methods
	  * simply return `"null"` without throwing an [[UnsupportedOperationException]].
	  */
	trait NonLiteralColumnWriteForm[-T] extends NonLiteralWriteForm[T] with ColumnWriteForm[T] {
		override def nullLiteral(inline :Boolean) :String = "null"
		override def nullColumnLiterals :Seq[String] = NullSingletonLiteral
	}


	/** A read form with equality defined as `name` equality only (at least by default).
	  * Resets the implementations of form proxy factory methods which any class which mixes this trait in might
	  * have overriden with dedicated implementations back to their default implementations from `SQLWriteForm`
	  * in order to preserve a reference to this form and thus name-based, rather than property-based equality
	  * of the created forms.
	  */
	private[schema] trait NamedColumnWriteForm[T] extends NamedWriteForm[T] with ColumnWriteForm[T] {
		override def nullSafe :ColumnWriteForm[T] = super[ColumnWriteForm].nullSafe
		override def notNull :ColumnWriteForm[T] = super[ColumnWriteForm].notNull
		override def withNull(implicit nulls :NullValue[T]) :ColumnWriteForm[T] = super[ColumnWriteForm].withNull
	}


	/** An implementation mixin for column write forms which optimizes
	  * [[net.noresttherein.oldsql.schema.ColumnWriteForm.SingletonColumnWriteForm.split split]] by extending `Seq`
	  * and returning itself, without a need for creating a (likely temporary) singleton wrapper object.
	  */
	private[oldsql] trait SingletonColumnWriteForm[-T]
		extends ColumnWriteForm[T] with UnspecifiedFormDefaults with Seq[ColumnWriteForm[T]]
	{
		override def split :Seq[ColumnWriteForm[T]] = this

		override def apply(i :Int) :ColumnWriteForm[T] =
			if (i == 0) this else throw new IndexOutOfBoundsException(i)

		override def length :Int = 1
		override def iterator :Iterator[ColumnWriteForm[T]] = Iterator.single(this)

		override def map[B](f :ColumnWriteForm[T] => B) = f(this) match {
			//s.type <:< B with SingletonColumnWriteForm[x] => s.type <:< B with Seq[s.type] => s <:< Seq[B]
			case singleton :SingletonColumnWriteForm[_] => singleton.asInstanceOf[Seq[B]]
			case res => PassedArray.single(res)
		}

		override def flatMap[B](f :ColumnWriteForm[T] => IterableOnce[B]) = f(this) match {
			case empty :Iterable[B] if empty.isEmpty => PassedArray.empty
			case one :Iterable[B] if one.sizeIs == 1 && one.head.isInstanceOf[SingletonColumnWriteForm[_]] =>
				one.head.asInstanceOf[Seq[B]]
			case iter :Iterable[B] => iter.toSeq
			case iter :Iterator[B] => if (iter.hasNext) iter.toSeq else PassedArray.empty
			case other => other.iterator.toSeq
		}

		override def canEqual(that :Any) = superCanEqual(that)
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
		override def unmap[X](f :X => Any) :ColumnWriteForm[X] = this
		override def optUnmap[X](f :X => Option[Any]) :ColumnWriteForm[X] = this
		override def compose[X](extractor :X =?> Any) :ColumnWriteForm[X] = this
		override def toOpt :ColumnWriteForm[Option[Any]] = this

		override def nullSafe :ColumnWriteForm[Any] = this
	}

}

