package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping



import scala.annotation.unspecialized
import scala.collection.breakOut

trait ColumnSelection extends (ColumnValues => ColumnValues) {
	import ColumnSelection._
//	def columns :Seq[ColumnMapping[_, _]]

	def apply(values :ColumnValues) :ColumnValues

//	override def toString = columns.toString
	@unspecialized
	override def andThen[A](g: (ColumnValues) => A): (ColumnValues) => A = g match {
		case s:ColumnSelection => (this andThen s).asInstanceOf[ColumnValues=>A]
	}

	def andThen(g :ColumnSelection) :ColumnSelection = Compose(this, g)

	@unspecialized
	override def compose[A](g: (A) => ColumnValues): (A) => ColumnValues = g match {
		case s:ColumnSelection => (this compose s).asInstanceOf[A=>ColumnValues]
	}

	def compose(g :ColumnSelection) :ColumnSelection = Compose(g, this)
}




object ColumnSelection {
	case class Single(column :ColumnMapping[_, _]) extends ColumnSelection {
		override def apply(values: ColumnValues): ColumnValues = values.filter(_==column)

		override def toString = s"Single($column)"
	}


	case class Inverse(columns :Seq[ColumnMapping[_, _]], map :ColumnMapping[_, _]=>ColumnMapping[_, _]) extends ColumnSelection {
		private val inverse = columns.map(c => (map(c), c)).toMap[ColumnMapping[_, _], ColumnMapping[_, _]]

		def apply(values: ColumnValues): ColumnValues = values.compose(inverse)

		override def toString = columns.mkString("Inverse(",", ", ")")
	}


	case class Adapted(selection :Map[ColumnMapping[_, _], ColumnMapping[_, _]]) extends ColumnSelection {
		def this(adapted :ColumnMapping[_, _], adapter :ColumnMapping[_, _]) = this(Map(adapted->adapter))

		override def apply(values: ColumnValues): ColumnValues =
			values.compose(selection)

		override def toString = selection.toString
	}

	object Adapted {
		def apply(adapted :ColumnMapping[_, _], adapter :ColumnMapping[_, _]) :Adapted =
			new Adapted(adapted, adapter)

		def apply(adapters :Seq[(ColumnMapping[_, _], ColumnMapping[_, _])]) :Adapted =
			new Adapted(adapters.toMap[ColumnMapping[_, _], ColumnMapping[_, _]])
	}


	case class Filter(filter :ColumnMapping[_, _]=>Boolean) extends ColumnSelection {
		override def apply(values: ColumnValues): ColumnValues = values.filter(filter)

		override def toString = s"Filter()"
	}


	case class Collect(fun :PartialFunction[ColumnMapping[_, _], ColumnMapping[_, _]]) extends ColumnSelection {
		override def apply(values: ColumnValues): ColumnValues = values.collector(fun)

		override def toString = s"Collect()"
	}

	case class Compose(outer :ColumnSelection, inner :ColumnSelection) extends ColumnSelection {

		override def apply(values: ColumnValues): ColumnValues =
			outer(inner(values))

		override def toString = s"$outer andThen $inner"
	}
}