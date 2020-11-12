package net.noresttherein.oldsql.schema

import java.sql.PreparedStatement

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.ColumnForm.JDBCSQLType
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLWriteForm.{ConstWriteForm, ErrorWriteForm, EvalOrNullWriteForm, EvalWriteForm, FlatMappedSQLWriteForm, LazyWriteForm, MappedSQLWriteForm, NonLiteralWriteForm, NullWriteForm, ProxyWriteForm}
import net.noresttherein.oldsql.schema.forms.ColumnWriteForms
import net.noresttherein.oldsql.schema.forms.ScalaWriteForms.OptionWriteForm






/** An `SQLReadForm` describing the write format of a simple type `T`, mapping to a single database column.
  * Aside from fixing the `writtenColumns` method to return `1` and introducing a property for the code
  * of the underlying SQL type, it enables static checks that the type `T` is a valid type for a single mapped column.
  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnForm]]
  */
@implicitNotFound("I do not know how to set a PreparedStatement parameter of type ${T}: " +
                  "missing implicit ColumnWriteForm[${T}].")
trait ColumnWriteForm[-T] extends SQLWriteForm[T] with SuperColumnForm {

	final override def writtenColumns = 1

	override def setNull(position :Int)(statement :PreparedStatement) :Unit =
		statement.setNull(position, sqlType)

	override def literal(value: T): String = if (value == null) "null" else value.toString
	override def nullLiteral: String = "null"

	override def inlineLiteral(value: T): String = literal(value)
	override def inlineNullLiteral: String = nullLiteral



	override def unmap[X](fun :X => T) :ColumnWriteForm[X] =
		new MappedSQLWriteForm[T, X] with ColumnWriteForm[X] {
			override val source = ColumnWriteForm.this
			override val unmap = fun
			override def sqlType = source.sqlType
			override def toString = super[MappedSQLWriteForm].toString
		}


	override def flatUnmap[X](fun :X => Option[T]) :ColumnWriteForm[X]  =
		new FlatMappedSQLWriteForm[T, X] with ColumnWriteForm[X] {
			override val source = ColumnWriteForm.this
			override val unmap = fun
			override def sqlType: JDBCSQLType = source.sqlType
			override def toString = super[FlatMappedSQLWriteForm].toString
		}

	override def from[X](extractor :X =?> T) :ColumnWriteForm[X] =  extractor match {
		case _ :IdentityExtractor[_] => this.asInstanceOf[ColumnWriteForm[X]]
		case const :ConstantExtractor[_, _] => ColumnWriteForm.const(const.constant.asInstanceOf[T])(this)
		case req :RequisiteExtractor[_, _] => unmap(req.getter.asInstanceOf[X => T])
		case _ :EmptyExtractor[_, _] => ColumnWriteForm.none(this)
		case _ => flatUnmap(extractor.optional)
	}

	override def compose[X](extractor :X =?> T) :ColumnWriteForm[X] =  extractor match {
		case _ :IdentityExtractor[_] => this.asInstanceOf[ColumnWriteForm[X]]
		case const :ConstantExtractor[_, _] => ColumnWriteForm.const(const.constant.asInstanceOf[T])(this)
		case req :RequisiteExtractor[_, _] => unmap(req.getter.asInstanceOf[X => T])
		case _ :EmptyExtractor[_, _] => ColumnWriteForm.none(this)
		case _ => flatUnmap(extractor.optional)
	}


	override def toOpt :ColumnWriteForm[Option[T]] = ColumnWriteForms.OptionColumnWriteForm(this)



	override def <>[O <: T](read :SQLReadForm[O]) :SQLForm[O] = read match {
		case atom :ColumnReadForm[O] => this <> atom
		case _ => super.<>(read)
	}

	def <>[O <: T](read :ColumnReadForm[O]) :ColumnForm[O] = ColumnForm.combine(read, this)



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
	def apply[X](columnType :JDBCSQLType)(write :(PreparedStatement, Int, X) => Unit) :ColumnWriteForm[X] =
		new ColumnWriteForm[X] with NonLiteralWriteForm[X] {
			override def set(position :Int)(statement :PreparedStatement, value :X) :Unit =
				write(statement, position, value)

			override def sqlType = columnType

			override def toString = "ColumnWriteForm(" + columnType + ")@" + System.identityHashCode(this)
		}



	/** A `ColumnWriteForm` which will always write the 'null' value of type `T` using the implicitly available form. */
	def none[X :ColumnWriteForm] :ColumnWriteForm[Any] =
		new NullWriteForm[X] with ColumnWriteForm[Any] {
			override val sqlType = ColumnWriteForm[X].sqlType

			override def setNull(position :Int)(statement :PreparedStatement) :Unit =
				form.setNull(position)(statement)

			override def literal(value :Any) = form.nullLiteral
			override def nullLiteral = form.nullLiteral
			override def inlineLiteral(value :Any) = form.inlineNullLiteral
			override def inlineNullLiteral = form.inlineNullLiteral

			override def toString = super[NullWriteForm].toString
		}



	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `ColumnWriteForm[T]`.
	  */
	def opt[X :ColumnWriteForm](value :Option[X]) :ColumnWriteForm[Any] =
		if (value.isEmpty) none[X]
		else const(value.get)

	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `ColumnWriteForm[T]`.
	  */
	def const[X :ColumnWriteForm](value :X) :ColumnWriteForm[Any] =
		if (value == null)
			none[X]
		else
            new ConstWriteForm[X](value) with ColumnWriteForm[Any] {
	            override val sqlType = ColumnWriteForm[X].sqlType

	            override def setNull(position :Int)(statement :PreparedStatement) :Unit =
		            target.set(position)(statement, const)

	            override def literal(value :Any) = target.literal(const)
	            override def nullLiteral = target.literal(const)
	            override def inlineLiteral(value :Any) = target.inlineLiteral(const)
	            override def inlineNullLiteral = target.inlineLiteral(const)

	            override def toString :String = super[ConstWriteForm].toString
			}



	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `ColumnWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form. If it yields
	  * `None`, the default `orElse` value will be written instead using the backing forms `set` method.
	  */
	def evalopt[T :ColumnWriteForm](value: => Option[T], orElse :T) :ColumnWriteForm[Any] =
		new EvalOrNullWriteForm[T](value)(SQLWriteForm[T], NullValue(orElse)) with ColumnWriteForm[Any] {
			override val sqlType = ColumnWriteForm[T].sqlType

			override def setNull(position :Int)(statement :PreparedStatement) :Unit =
				super[EvalOrNullWriteForm].setNull(position)(statement)

			override def literal(value :Any) = super[EvalOrNullWriteForm].literal(value)
			override def nullLiteral = literal(null.asInstanceOf[T])
			override def inlineLiteral(value :Any) = super[EvalOrNullWriteForm].inlineLiteral(value)
			override def inlineNullLiteral = inlineLiteral(null.asInstanceOf[T])

			override def toString :String = super[EvalOrNullWriteForm].toString
		}

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `ColumnWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form. If it yields
	  * `None`, the value carried by an implicitly available `NullValue` will be written instead, using the backing
	  * form's `set` method.
	  */
	def evalopt[T :ColumnWriteForm](value: => Option[T]) :ColumnWriteForm[Any] =
		new EvalWriteForm[T](value) with ColumnWriteForm[Any] {
			override val sqlType = ColumnWriteForm[T].sqlType

			override def setNull(position :Int)(statement :PreparedStatement) :Unit =
				super[EvalWriteForm].setNull(position)(statement)

			override def literal(value :Any) = super[EvalWriteForm].literal(value)
			override def nullLiteral = literal(null.asInstanceOf[T])
			override def inlineLiteral(value :Any) = super[EvalWriteForm].inlineLiteral(value)
			override def inlineNullLiteral = inlineLiteral(null.asInstanceOf[T])

			override def toString :String = super[EvalWriteForm].toString
		}

	/** A form which will ignore all values provided as arguments and instead write the value resulting from evaluating
	  * the given by-name argument, using the implicit `ColumnWriteForm[T]`. The given expression must be thread safe
	  * and will be evaluated exactly once for every `set` and/or `setNull` call on the returned form.
	  */
	def eval[T :ColumnWriteForm](value: => T) :ColumnWriteForm[Any] =
		evalopt(Some(value))


	/** A write form which will throw the given exception at every write attempt. */
	def error(columnType :JDBCSQLType)(raise: => Nothing) :ColumnWriteForm[Any] =
		new ErrorWriteForm[Any](1, raise) with ColumnWriteForm[Any] {
			override val sqlType = columnType
		}

	/** A dummy column form which throws an `UnsupportedOperationException` at each write attempt.
	  * Used as part of `ColumnForm.combine` to convert a `ColumnReadForm` into a `ColumnForm` for situations
	  * where its write functionality is known not to be used. Be careful!
	  */
	def unsupported(message :String, columnType :JDBCSQLType) :ColumnWriteForm[Any] =
		error(columnType)(throw new UnsupportedOperationException(message))






	/** A proxy form which will delegate all calls to the form returned by the given expression. The expression
	  * will be evaluated only if/when needed, but must be thread safe and may be executed more than once
	  * if several threads trigger the initialization at the same time, but the returned form is thread safe.
	  */
	def Lazy[T](delayed: => ColumnWriteForm[T]) :ColumnWriteForm[T] =
		new LazyWriteForm[T] with LazyColumnWriteForm[T] {
			override protected[this] var init: () => SQLWriteForm[T] = () => delayed
		}



	/** Calls [[net.noresttherein.oldsql.schema.ColumnWriteForm.extracted extracted]] on the implicit form for `S`. */
	def apply[S, T](map :T =?> S)(implicit writer :ColumnWriteForm[S]) :ColumnWriteForm[T] =
		writer.from(map)

	/** Calls [[net.noresttherein.oldsql.schema.ColumnWriteForm.flatUnmap flatUnmap]] on the implicit form for `S`. */
	def flatMap[S :ColumnWriteForm, T](map :T => Option[S]) :ColumnWriteForm[T] =
		ColumnWriteForm[S].flatUnmap(map)

	/** Calls [[net.noresttherein.oldsql.schema.ColumnWriteForm.unmap unmap]] on the implicit form for `S`. */
	def map[S :ColumnWriteForm, T](map :T => S) :ColumnWriteForm[T] = ColumnWriteForm[S].unmap(map)






	private[schema] trait ColumnProxyWriteForm[-T] extends ProxyWriteForm[T] with ColumnWriteForm[T] {
//		abstract override def form :ColumnWriteForm[T] = super[ProxyWriteForm].form.asInstanceOf[ColumnWriteForm[T]]

		override def sqlType :JDBCSQLType = form.asInstanceOf[ColumnWriteForm[T]].sqlType

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = form.setNull(position)(statement)
		override def literal(value :T) :String = form.literal(value)
		override def nullLiteral :String = form.nullLiteral
		override def inlineLiteral(value :T) :String = form.inlineLiteral(value)
		override def inlineNullLiteral :String = form.inlineNullLiteral

		override def toString :String = "~" + form
	}



	private[schema] trait LazyColumnWriteForm[-T] extends LazyWriteForm[T] with ColumnProxyWriteForm[T] {
		override def form :ColumnWriteForm[T] = super[LazyWriteForm].form.asInstanceOf[ColumnWriteForm[T]]

		override def unmap[X](fun :X => T) :ColumnWriteForm[X] =
			if (isInitialized) form.unmap(fun) else Lazy(form.unmap(fun))

		override def flatUnmap[X](fun :X => Option[T]) :ColumnWriteForm[X] =
			if (isInitialized) form.flatUnmap(fun) else Lazy(form.flatUnmap(fun))

		override def toOpt :ColumnWriteForm[Option[T]] = if (isInitialized) form.toOpt else Lazy(form.toOpt)

		override def <>[O <: T](read :ColumnReadForm[O]) :ColumnForm[O] =
			if (isInitialized) read <> this
			else ColumnForm.Lazy(read <> form)
		
		override def toString :String = if (isInitialized) "Lazy(" + form + ")" else "<Lazy"
	}





	private[schema] trait OptionColumnWriteForm[T] extends OptionWriteForm[T] with ColumnWriteForm[Option[T]] {
		protected override def form :ColumnWriteForm[T]
		override def sqlType :JDBCSQLType = form.sqlType
		override def toString :String = super[OptionWriteForm].toString
	}

}

