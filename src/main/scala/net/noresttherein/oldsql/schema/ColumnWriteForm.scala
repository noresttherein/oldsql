package net.noresttherein.oldsql.schema

import java.sql.PreparedStatement

import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.ScalaWriteForms.OptionWriteForm
import net.noresttherein.oldsql.schema.SQLForm.JDBCSQLType
import net.noresttherein.oldsql.schema.SQLWriteForm.{ConstWriteForm, FlatMappedSQLWriteForm, LazyWriteForm, MappedSQLWriteForm, NullWriteForm, OptionWriteForm}



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



	override def &&[O <: T](read :SQLReadForm[O]) :SQLForm[O] = read match {
		case atom :ColumnReadForm[O] => this && atom
		case _ => super.&&(read)
	}

	def &&[O <: T](read :ColumnReadForm[O]) :ColumnForm[O] = ColumnForm.combine(read, this)



	override def compatible(other: SQLWriteForm[_]): Boolean = other match {
		case a :ColumnWriteForm[_] => a.sqlType == sqlType
		case _ => false
	}

}






object ColumnWriteForm {

	/** Summon an implicitly available `SQLWriteForm[T]`. */
	@inline def apply[X :ColumnWriteForm] :ColumnWriteForm[X] = implicitly[ColumnWriteForm[X]]



	/** A `ColumnWriteForm` which will always write the 'null' value of type `T` using the implicitly available form. */
	def none[X :ColumnWriteForm] :ColumnWriteForm[Any] =
		new NullWriteForm[X] with ColumnWriteForm[Any] {
			override val sqlType = ColumnWriteForm[X].sqlType
		}

	/** A form which will ignore all values provided as arguments and instead write the value provided here,
	  * using the implicit `ColumnWriteForm[T]`.
	  */
	def const[X :ColumnWriteForm](value :X) :ColumnWriteForm[Any] =
		if (value == null)
			none[X]
		else
            new ConstWriteForm[X](value) with ColumnWriteForm[Any] {
				override val sqlType = ColumnWriteForm[X].sqlType
			}



	/** A proxy form which will delegate all calls to the form returned by the given expression. The expression
	  * will be evaluated only if/when needed, but must be thread safe and may be executed more than once
	  * if several threads trigger the initialization at the same time, but the returned form is thread safe.
	  */
	def Lazy[T](delayed: => ColumnWriteForm[T]) :ColumnWriteForm[T] =
		new LazyWriteForm[T] with LazyColumnWriteForm[T] {
			override protected[this] var init: () => SQLWriteForm[T] = () => delayed

			override def sqlType = form.sqlType
		}



	/** Calls [[net.noresttherein.oldsql.schema.SQLWriteForm#flatUnmap flatUnmap]] on the implicit form for `S`. */
	def flatMap[S :ColumnWriteForm, T](map :T => Option[S]) :ColumnWriteForm[T] =
		new FlatMappedSQLWriteForm[S, T] with ColumnWriteForm[T] {
			override val source = implicitly[ColumnWriteForm[S]]
			override val unmap = map
			override def sqlType: Int = source.sqlType
		}

	/** Calls [[net.noresttherein.oldsql.schema.SQLWriteForm#unmap unmap]] on the implicit form for `S`. */
	def map[S :ColumnWriteForm, T](map :T => S) :ColumnWriteForm[T] =
		new MappedSQLWriteForm[S, T] with ColumnWriteForm[T] {
			override val source = ColumnWriteForm[S]
			override val unmap = map
			override def sqlType = source.sqlType
		}




	implicit def OptionColumnWriteForm[T :ColumnWriteForm] :ColumnWriteForm[Option[T]] =
		new OptionColumnWriteForm[T] { val form :ColumnWriteForm[T] = ColumnWriteForm[T] }

	implicit def SomeColumnWriteForm[T :ColumnWriteForm] :ColumnWriteForm[Some[T]] =
		ColumnWriteForm[T].unmap(_.get)






	private[schema] trait LazyColumnWriteForm[T] extends LazyWriteForm[T] with ColumnWriteForm[T] {
		@inline override def form :ColumnWriteForm[T] = super[LazyWriteForm].form.asInstanceOf[ColumnWriteForm[T]]

		override def unmap[X](fun :X => T) :ColumnWriteForm[X] =
			if (isInitialized) form.unmap(fun) else Lazy(form.unmap(fun))

		override def flatUnmap[X](fun :X => Option[T]) :ColumnWriteForm[X] =
			if (isInitialized) form.flatUnmap(fun) else Lazy(form.flatUnmap(fun))

		override def asOpt :ColumnWriteForm[Option[T]] = if (isInitialized) form.asOpt else Lazy(form.asOpt)

		override def &&[O <: T](read :ColumnReadForm[O]) :ColumnForm[O] =
			if (isInitialized) read && this
			else ColumnForm.Lazy(read && form)
	}





	private[schema] trait OptionColumnWriteForm[T] extends OptionWriteForm[T] with ColumnWriteForm[Option[T]] {
		protected override def form :ColumnWriteForm[T]
		override def sqlType :JDBCSQLType = form.sqlType
	}

}

