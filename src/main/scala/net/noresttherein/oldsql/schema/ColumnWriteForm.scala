package net.noresttherein.oldsql.schema

import java.sql.PreparedStatement

import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.ColumnForm.JDBCSQLType
import net.noresttherein.oldsql.schema.ScalaWriteForms.OptionWriteForm
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLWriteForm.{ConstWriteForm, EvalOrNullWriteForm, EvalWriteForm, FlatMappedSQLWriteForm, LazyWriteForm, MappedSQLWriteForm, NonLiteralWriteForm, NullWriteForm, ProxyWriteForm}



/** An `SQLReadForm` describing the write format of a simple type `T`, mapping to a single database column.
  * Aside from fixing the `writtenColumns` method to return `1` and introducing a property for the code
  * of the underlying SQL type, it enables static checks that the type `T` is a valid type for a single mapped column.
  * @see [[net.noresttherein.oldsql.schema.ColumnWriteForm]]
  * @see [[net.noresttherein.oldsql.schema.ColumnForm]]
  */
trait ColumnWriteForm[-T] extends SQLWriteForm[T] with BaseColumnForm {
	final override def writtenColumns = 1

	override def setNull(position :Int)(statement :PreparedStatement) :Unit =
		statement.setNull(position, sqlType)

	override def literal(value: T): String = if (value == null) "null" else value.toString
	override def nullLiteral: String = "null"

	override def inlineLiteral(value: T): String = literal(value)
	override def inlineNullLiteral: String = nullLiteral



	override def unmap[X](fun :X => T) :ColumnWriteForm[X] =
		ColumnWriteForm.map(fun)(this)

	override def flatUnmap[X](fun :X => Option[T]) :ColumnWriteForm[X]  =
		ColumnWriteForm.flatMap(fun)(this)

	override def compose[X](extractor :X =?> T) :ColumnWriteForm[X] =  extractor match {
		case _ :IdentityExtractor[_] => this.asInstanceOf[ColumnWriteForm[X]]
		case const :ConstantExtractor[_, _] => ColumnWriteForm.const(const.constant.asInstanceOf[T])(this)
		case req :RequisiteExtractor[_, _] => unmap(req.getter.asInstanceOf[X => T])
		case _ :EmptyExtractor[_, _] => ColumnWriteForm.none(this)
		case _ => flatUnmap(extractor.optional)
	}


	override def asOpt :ColumnWriteForm[Option[T]] = ColumnWriteForm.OptionColumnWriteForm(this)



	override def <>[O <: T](read :SQLReadForm[O]) :SQLForm[O] = read match {
		case atom :ColumnReadForm[O] => this <> atom
		case _ => super.<>(read)
	}

	def <>[O <: T](read :ColumnReadForm[O]) :ColumnForm[O] = ColumnForm.combine(read, this)



	override def compatible(other: SQLWriteForm[_]): Boolean = other match {
		case a :ColumnWriteForm[_] => a.sqlType == sqlType
		case _ => false
	}



	override def toString :String = "<" + sqlType
}






object ColumnWriteForm {

	/** Summon an implicitly available `SQLWriteForm[T]`. */
	@inline def apply[X :ColumnWriteForm] :ColumnWriteForm[X] = implicitly[ColumnWriteForm[X]]



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
	def evalopt[T :ColumnWriteForm](value: =>Option[T], orElse :T) :ColumnWriteForm[Any] =
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
	def evalopt[T :ColumnWriteForm](value: =>Option[T]) :ColumnWriteForm[Any] =
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
	def eval[T :ColumnWriteForm](value: =>T) :ColumnWriteForm[Any] =
		evalopt(Some(value))



	/** A dummy column form which throws an `UnsupportedOperationException` at each write attempt.
	  * Used as part of `ColumnForm.combine` to convert a `ColumnReadForm` into a `ColumnForm` for situations
	  * where its write functionality is known not to be used. Be careful!
	  */
	private[oldsql] def dummy[T](jdbcType :JDBCSQLType) :ColumnWriteForm[T] =
		new NonLiteralWriteForm[Any] with ColumnWriteForm[Any] {
			override val sqlType = jdbcType

			override def set(position :Int)(statement :PreparedStatement, value :Any) :Unit =
				setNull(position)(statement)

			override def setNull(position :Int)(statement :PreparedStatement) :Unit =
				throw new UnsupportedOperationException("ColumnWriteForm.dummy.set")

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case col :ColumnWriteForm[_] => col.sqlType == sqlType && col.getClass == getClass
				case _ => false
			}

			override def hashCode :Int = sqlType.hashCode //getClass.hashCode

			override def toString = "dummy".toString
		}



	/** A proxy form which will delegate all calls to the form returned by the given expression. The expression
	  * will be evaluated only if/when needed, but must be thread safe and may be executed more than once
	  * if several threads trigger the initialization at the same time, but the returned form is thread safe.
	  */
	def Lazy[T](delayed: => ColumnWriteForm[T]) :ColumnWriteForm[T] =
		new LazyWriteForm[T] with LazyColumnWriteForm[T] {
			override protected[this] var init: () => SQLWriteForm[T] = () => delayed
		}



	/** Calls [[net.noresttherein.oldsql.schema.SQLWriteForm#flatUnmap flatUnmap]] on the implicit form for `S`. */
	def flatMap[S :ColumnWriteForm, T](map :T => Option[S]) :ColumnWriteForm[T] =
		new FlatMappedSQLWriteForm[S, T] with ColumnWriteForm[T] {
			override val source = implicitly[ColumnWriteForm[S]]
			override val unmap = map
			override def sqlType: JDBCSQLType = source.sqlType
			override def toString = super[FlatMappedSQLWriteForm].toString
		}

	/** Calls [[net.noresttherein.oldsql.schema.SQLWriteForm#unmap unmap]] on the implicit form for `S`. */
	def map[S :ColumnWriteForm, T](map :T => S) :ColumnWriteForm[T] =
		new MappedSQLWriteForm[S, T] with ColumnWriteForm[T] {
			override val source = ColumnWriteForm[S]
			override val unmap = map
			override def sqlType = source.sqlType
			override def toString = super[MappedSQLWriteForm].toString
		}




	implicit def OptionColumnWriteForm[T :ColumnWriteForm] :ColumnWriteForm[Option[T]] =
		new OptionColumnWriteForm[T] {
			val form :ColumnWriteForm[T] = ColumnWriteForm[T]
		}

	implicit def SomeColumnWriteForm[T :ColumnWriteForm] :ColumnWriteForm[Some[T]] =
		ColumnWriteForm[T].unmap(_.get)






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

		override def asOpt :ColumnWriteForm[Option[T]] = if (isInitialized) form.asOpt else Lazy(form.asOpt)

		override def <>[O <: T](read :ColumnReadForm[O]) :ColumnForm[O] =
			if (isInitialized) read <> this
			else ColumnForm.Lazy(read <> form)
		
		override def toString :String = if (isInitialized) "Lazy(" + form + ")" else "<Lazy"
	}





	private[schema] trait OptionColumnWriteForm[T] extends OptionWriteForm[T] with ColumnWriteForm[Option[T]] {
		protected override def form :ColumnWriteForm[T]
		override def sqlType :JDBCSQLType = form.sqlType
		override def toString :String = "Option[" + form + "]"
	}

}

