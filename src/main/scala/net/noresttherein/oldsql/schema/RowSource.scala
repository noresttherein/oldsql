package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.Mapping.AnyComponent


/**
  * @author Marcin Mościcki
  */
trait RowSource[M[O] <: AnyComponent[O]] {
	def apply[O] :M[O]
	def sql :String

	override def toString :String = sql
}



object RowSource {

	def apply[M[O] <: GenericMapping[O, _]](name :String, template :M[_]) :RowSource[M] = new RowSource[M] {
		override def apply[O] = template.asInstanceOf[M[O]]
		override def sql = name
	}



	trait Table[M[O] <: AnyComponent[O]] extends RowSource[M] {
		def name :String
		override def sql :String = name
	}

	object Table {
		def apply[M[O] <: GenericMapping[O, _]](tableName :String, template :M[_]) :Table[M] =
			new Table[M] {
				override def name = tableName
				override def apply[O] = template.asInstanceOf[M[O]]
			}

		def apply[M[O] <: GenericMapping[O, _]](tableName :String)(implicit mapping :String => M[_]) :Table[M] =
			apply(tableName, mapping(tableName))

		def of[S] :TableConstructor[S] = new TableConstructor[S] {}

		trait TableConstructor[S] extends Any {
			def apply[M[O] <: GenericMapping[O, S]](name :String)(implicit mapping :String => M[_]) :Table[M] =
				Table(name, mapping(name))
		}

		of[Entity]("table")
		
		case class Entity(name :String)

		implicit class Entities[O](s :String) extends MappingSupport[O, Entity] {
			val name = column(_.name)

			override protected def construct(implicit pieces :Pieces) :Entity =
				Entity(name)
		}
	}


}
