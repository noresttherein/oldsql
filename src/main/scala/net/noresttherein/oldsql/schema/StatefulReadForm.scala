package net.noresttherein.oldsql.schema

import java.sql.ResultSet

import scala.collection.mutable.Builder
import scala.collection.IterableFactory






/**
  * @author Marcin Mościcki
  */
trait StatefulReadForm[+T] extends Serializable {
	def add(result :ResultSet, position :Int) :Unit
	def result() :T
}





object StatefulReadForm {
	class IterableForm[+C[X] <: Iterable[X], E](builder :Builder[E, C[E]])(implicit form :SQLReadForm[E])
		extends StatefulReadForm[C[E]]
	{
		def this(factory :IterableFactory[C])(implicit form :SQLReadForm[E]) =
			this(factory.newBuilder[E])

		override def add(result :ResultSet, position :Int) :Unit = form.opt(result, position) match {
			case Some(item) => builder += item
			case _ =>
		}

		override def result() :C[E] = builder.result()
	}
}
