package net.noresttherein.oldsql.schema

import java.sql.ResultSet

import net.noresttherein.oldsql.schema.ColumnReadForm.{FlatMappedColumnReadForm, LazyColumnReadForm, MappedColumnReadForm}
import net.noresttherein.oldsql.schema.ColumnWriteForm.{LazyColumnWriteForm, OptionColumnWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.{CombinedForm, FlatMappedSQLForm, JDBCSQLType, LazyForm, MappedSQLForm, NullableForm, NullValue, OptionForm}



trait BaseColumnForm {
	/** The JDBC code for the underlying column type, as defined by constants in `java.sql.Types`. */
	def sqlType :JDBCSQLType
}



trait ColumnForm[T] extends SQLForm[T] with ColumnReadForm[T] with ColumnWriteForm[T] {

	override def bimap[X :NullValue](map :T => X)(unmap :X => T) :ColumnForm[X] =
		ColumnForm.map[T, X](map)(unmap)(this, NullValue[X])

	override def bimap[X](map :T => X, nullValue :X)(unmap :X => T) :ColumnForm[X] =
		bimap(map)(unmap)(NullValue(nullValue))

	override def bimapNull[X](map :T => X)(unmap :X => T) :ColumnForm[X] =
		bimap(map)(unmap)(nulls.map(map))


	override def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :ColumnForm[X] =
		ColumnForm.flatMap(map)(unmap)(this, NullValue[X])

	override def biflatMap[X](map :T => Option[X], nullValue :X)(unmap :X => Option[T]) :ColumnForm[X] =
		biflatMap(map)(unmap)(NullValue(nullValue))

	override def biflatMapNull[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) :ColumnForm[X] =
		biflatMap(map)(unmap)(nulls.flatMap(map))

	override def asOpt :ColumnForm[Option[T]] = ColumnForm.OptionColumnForm(this)



	override def compatible(other: SQLForm[_]): Boolean = other match {
		case a :ColumnForm[_] => a.sqlType == sqlType
		case _ => false
	}


}






object ColumnForm {
	@inline def apply[X :ColumnForm] :ColumnForm[X] = implicitly[ColumnForm[X]]

	def combine[T](read :ColumnReadForm[T], write :ColumnWriteForm[T]) :ColumnForm[T] =
		if (read.sqlType != write.sqlType)
			throw new IllegalArgumentException(
				s"Can't combine column forms $read and $write with different underlying sql types: ${read.sqlType} != ${write.sqlType}."
			)
		else
			new CombinedForm[T](read, write) with ColumnForm[T] {
				override val sqlType = r.asInstanceOf[ColumnReadForm[T]].sqlType

				override def read(position :Int)(res :ResultSet) =
					r.asInstanceOf[ColumnReadForm[T]].friendRead(position)(res)
			}



	def Lazy[T](delayed: => ColumnForm[T]) :ColumnForm[T] =
		new LazyForm[T](() => delayed) with LazyColumnReadForm[T] with LazyColumnWriteForm[T] with ColumnForm[T] {
			override def form :ColumnForm[T] = super[LazyForm].form.asInstanceOf[ColumnForm[T]]

			override def bimap[X :NullValue](map :T => X)(unmap :X => T) =
				if (isInitialized) form.bimap[X](map)(unmap)
				else Lazy(form.bimap[X](map)(unmap))

			override def biflatMap[X :NullValue](map :T => Option[X])(unmap :X => Option[T]) =
				if (isInitialized) form.biflatMap(map)(unmap)
				else Lazy(form.biflatMap[X](map)(unmap))
		}



	def map[S, T](map :S => T)(unmap :T => S)(implicit source :ColumnForm[S], nulls :NullValue[T] = null) :ColumnForm[T] =
		new MappedSQLForm[S, T](map, unmap) with MappedColumnReadForm[S, T] with ColumnForm[T]

	def flatMap[S :ColumnForm, T :NullValue](map :S => Option[T])(unmap :T => Option[S]) :ColumnForm[T] =
		new FlatMappedSQLForm[S, T](map, unmap) with FlatMappedColumnReadForm[S, T] with ColumnForm[T]



	implicit def OptionColumnForm[T :ColumnForm] :ColumnForm[Option[T]] =
		new OptionForm[T] with OptionColumnWriteForm[T] with ColumnForm[Option[T]] {
			override val form = ColumnForm[T]
			override def read(position :Int)(res :ResultSet) :Option[T] = ColumnForm[T].opt(position)(res)
		}

	implicit def SomeColumnForm[T :ColumnForm] :ColumnForm[Some[T]] =
		ColumnForm[T].bimapNull(Some.apply)(_.get)






	trait NullableColumnForm[T >: Null] extends ColumnForm[T] with NullableForm[T] {
		override def apply(position :Int)(res :ResultSet) :T = read(position)(res)
	}



}
