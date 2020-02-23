package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping


import com.adpilot.cortb.clientapi.util.Matching.Unapply
import com.adpilot.cortb.clientapi.util.{OptionOps, Repeat}

import scala.slick.jdbc.PositionedResult

import scala.collection.breakOut

import OptionOps._
import ColumnValues._



/** A list of columns with their values being a source of data for mappings. Can be created either with explicit values, or for a result of a query */
trait ColumnValues {
	def select(selection :ColumnSelection) :ColumnValues = selection(this)

	def compose(before :ColumnMapping[_, _]=>ColumnMapping[_, _]) :ColumnValues =
		new MappingColumnValues(this, before)

	def filter(remaining :ColumnMapping[_, _]=>Boolean) :ColumnValues =
		new FilteringColumnValues(this, remaining)

	def collector(fun :PartialFunction[ColumnMapping[_, _], ColumnMapping[_, _]]) :ColumnValues =
		new CollectingColumnValues(this, fun)

	def apply[X](column :ColumnMapping[_, X]) :Option[X]

	def value[X](column :ColumnMapping[_, X]) :Option[ColumnValue[X]] = apply(column).map(ColumnValue(column, _))
}


case class ColumnValue[T](column :ColumnMapping[_, T], value :T) extends ColumnValues {
	override def toString = s"$column=$value"


	override def filter(remaining: (ColumnMapping[_, _]) => Boolean): ColumnValues =
		if (remaining(column)) this
		else EmptyValues

	def apply[X](column :ColumnMapping[_, X]) :Option[X] =
		value.asInstanceOf[X].providing(column==this.column)

	override def value[X](column: ColumnMapping[_, X]): Option[ColumnValue[X]] =
		this.providing(column==this.column).asInstanceOf[Option[ColumnValue[X]]]
}

object ColumnValue {
	implicit def apply[T](pair :(ColumnMapping[_, T], T)) :ColumnValue[T] = new ColumnValue(pair._1, pair._2)

	def apply[E, T](entity :E, column :ColumnMapping[E, T]) :ColumnValue[T] = new ColumnValue(column, column.value(entity))

	def apply[T](column :ColumnMapping[_, T], res :PositionedResult) :ColumnValue[T] =
		new ColumnValue(column, column(res))

	def apply[T](column :ColumnMapping[_, T], values :ColumnValues) :ColumnValue[T] =
		values.value(column) getOrElse {
			throw new IllegalArgumentException(s"can't get value for $column from $values")
		}
}


object ColumnValues {
	val EmptyValues = ColumnValues(Seq())


	def apply(columns :ColumnIndex, res :PositionedResult) :ColumnValues =
		new PositionedResultValues(columns, res)

	def apply(columns :Seq[ColumnMapping[_, _]], res :PositionedResult) :ColumnValues =
		apply(ColumnIndex(columns), res)

	def apply(values :Seq[ColumnValue[_]]) :ColumnValues =
		apply(ColumnValueIndex(values))

	def apply(values :ColumnValueIndex) :ColumnValues =
		new ExplicitColumnValues(values)



	def offset(res :PositionedResult, offset :Int) :PositionedResult = res match {
		case view :PositionedResultView => view.offset(offset)
		case _ => new PositionedResultView(res, res.currentPos + offset)
	}

	def view(res :PositionedResult) :PositionedResult = offset(res, 0)


	class PositionedResultView(res :PositionedResult, position :Int) extends PositionedResult(res.rs) {
		def this(res :PositionedResult) = this(res, res.currentPos)

		def close() = ()

		pos = position

		def offset(idx :Int) = new PositionedResultView(res, pos + idx)
	}
	





	private case class MappingColumnValues(values :ColumnValues, decorator :ColumnMapping[_, _]=>ColumnMapping[_, _]) extends ColumnValues {
		override def apply[X](column: ColumnMapping[_, X]): Option[X] =
			values(decorator(column).asInstanceOf[ColumnMapping[_, X]])

		override def compose(before: (ColumnMapping[_, _]) => ColumnMapping[_, _]): ColumnValues =
			values.compose(before andThen decorator)

		override def collector(fun: PartialFunction[ColumnMapping[_, _], ColumnMapping[_, _]]): ColumnValues =
			values.collector(fun andThen decorator)

		override def toString = s"Mapped($values)"
	}

	private case class FilteringColumnValues(values :ColumnValues, filter :ColumnMapping[_, _]=>Boolean) extends ColumnValues {

		override def apply[X](column: ColumnMapping[_, X]): Option[X] =
			if (filter(column)) values(column)
			else None

		override def filter(remaining: (ColumnMapping[_, _]) => Boolean): ColumnValues =
			values.filter(c => remaining(c) && filter(c))

		override def collector(fun: PartialFunction[ColumnMapping[_, _], ColumnMapping[_, _]]): ColumnValues = {
			val matcher = Unapply(fun)
			values.collector({ case matcher(c) if filter(c) => c })
		}

		override def toString = s"Filtered($values)"
	}

	private case class CollectingColumnValues(values :ColumnValues, collect :PartialFunction[ColumnMapping[_, _], ColumnMapping[_, _]]) extends ColumnValues {
		private val Collect = Unapply(collect)

		override def apply[X](column: ColumnMapping[_, X]): Option[X] =
			collect.andThen(values.apply(_)).applyOrElse(column, (_:ColumnMapping[_, _]) => None).asInstanceOf[Option[X]]

		override def filter(remaining: (ColumnMapping[_, _]) => Boolean): ColumnValues =
			values.collector{ case c @ Collect(res) if remaining(c) => res }

		override def collector(fun: PartialFunction[ColumnMapping[_, _], ColumnMapping[_, _]]): ColumnValues = {
			val matcher = Unapply(fun)
			values.collector{ case matcher(Collect(c)) => c }
		}


	}


	private class PositionedResultValues(columns :ColumnIndex, res :PositionedResult) extends ColumnValues {

		override def apply[X](column: ColumnMapping[_, X]): Option[X] =
			columns(column).map(index => column.columnType.Getter(offset(res, index)))

		override def toString = s"ColumnValues($columns), $res)"
	}



	private class ExplicitColumnValues(columns :ColumnValueIndex) extends ColumnValues {

		override def apply[X](column: ColumnMapping[_, X]): Option[X] =
			columns(column).map(columns.value(_).asInstanceOf[X])

		override def value[X](column: ColumnMapping[_, X]): Option[ColumnValue[X]] =
			columns(column).map(columns.values(_).asInstanceOf[ColumnValue[X]])

		override def toString = columns.values.mkString("ColumnValues(",", ", ")")
	}




	trait ColumnIndex {
		def columns :Seq[ColumnMapping[_, _]]
		def apply(column :ColumnMapping[_, _]) :Option[Int]

		override def toString = columns.toString
	}

	trait ColumnValueIndex extends ColumnIndex {
		def values :Seq[ColumnValue[_]]
		def value(idx :Int) = values(idx).value
	}

	private class IndexedColumns(val columns :Seq[ColumnMapping[_, _]], indexOf :ColumnMapping[_, _]=>Option[Int]) extends ColumnIndex {
		override def apply(column: ColumnMapping[_, _]): Option[Int] = indexOf(column)
	}

	private class IndexedValues(val values :Seq[ColumnValue[_]], indexOf :ColumnMapping[_, _]=>Option[Int])
		extends IndexedColumns(values.view.map(_.column), indexOf) with ColumnValueIndex



	object ColumnIndex {
		def apply(columns :Seq[ColumnMapping[_, _]], indexOf :ColumnMapping[_, _]=>Option[Int]) :ColumnIndex =
			new IndexedColumns(columns, indexOf)

		def apply(columns :Seq[ColumnMapping[_, _]]) :ColumnIndex = {
			val indices = columns.view.zipWithIndex.toMap[ColumnMapping[_, _], Int]
			apply(columns, indices.get)
		}

		def NotIndexed(columns :Seq[ColumnMapping[_, _]]) :ColumnIndex =
			new IndexedColumns(columns, columns.indexOf(_).providing(_>=0))

		val Empty :ColumnIndex = new ColumnIndex {
			def columns = Seq()
			def apply(column: ColumnMapping[_, _]): Option[Int] = None
		}
	}

	object ColumnValueIndex {

		def apply(values :Seq[ColumnValue[_]], indexOf :ColumnMapping[_, _]=>Option[Int]) :ColumnValueIndex =
			new IndexedValues(values, indexOf)

		def apply(values :Seq[ColumnValue[_]]) :ColumnValueIndex = {
			val indices = values.view.map(_.column).zipWithIndex.toMap[ColumnMapping[_, _], Int]
			new IndexedValues(values, indices.get)
		}

		def NotIndexed(values :Seq[ColumnValue[_]]) :ColumnValueIndex =
			new IndexedValues(values, c => values.view.indexWhere(_.column==c).providing(_>=0))

		val Empty :ColumnValueIndex = new ColumnValueIndex {
			def values = Seq()
			def columns = Seq()
			def apply(column: ColumnMapping[_, _]): Option[Int] = None
		}
	}




}
