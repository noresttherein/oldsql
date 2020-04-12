package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.Mapping.AnyComponent


/**
  * @author Marcin Mościcki
  */
trait RowSource[M[O] <: AnyComponent[O]] {
	type Row[O] = M[O]

	def apply[O] :M[O]
	def sql :String

	override def toString :String = sql
}



object RowSource {

	def apply[M[O] <: GenericMapping[_, O]](name :String, template :M[_]) :RowSource[M] = new RowSource[M] {
		override def apply[O] = template.asInstanceOf[M[O]]
		override def sql = name
	}



	trait NamedSource[N <: String with Singleton, M[O] <: AnyComponent[O]] extends RowSource[M] {
		def name :N
		override def sql :String = name
	}



	trait Table[M[O] <: AnyComponent[O]] extends RowSource[M] {
		def name :String
		override def sql :String = name
	}

	object Table {
		def apply[M[O] <: GenericMapping[_, O]](tableName :String, template :M[_]) :Table[M] =
			new Table[M] {
				override def name = tableName
				override def apply[O] = template.asInstanceOf[M[O]]
			}

		def apply[M[O] <: GenericMapping[_, O]](tableName :String)(implicit mapping :String => M[_]) :Table[M] =
			apply(tableName, mapping(tableName))

//		def apply[M[O] <: GenericMapping[O, _], N <: String with Singleton]
//		         (implicit mapping :String => M[_], name :ValueOf[N]) :StaticTable[M, N] =
//			new StaticTable[M, N] {
//				override val name = valueOf[N]
//				override def apply[O] = mapping(name).asInstanceOf[M[O]]
//			}

		def apply[N <: String with Singleton, M[O] <: GenericMapping[_, O]]
		         (implicit tableName :ValueOf[N], mapping :String => M[_]) :StaticTable[N, M] =
			new StaticTable[N, M] {
				override val name = valueOf[N]
				override def apply[O] = mapping(name).asInstanceOf[M[O]]
			}



		def of[S] :TableConstructor[S] = new TableConstructor[S] {}

		trait TableConstructor[S] extends Any {
			def apply[M[O] <: GenericMapping[S, O]](name :String)(implicit mapping :String => M[_]) :Table[M] =
				Table(name, mapping(name))
		}



		trait StaticTable[N <: String with Singleton, M[O] <: AnyComponent[O]] extends Table[M] with NamedSource[N, M]


//		case class Entity(field :String)
//
//		implicit class Entities[O](name :String) extends MappingSupport[O, Entity] {
//			override protected def construct(implicit pieces :Pieces) :Entity = ???
//		}
//
//		val table :StaticTable["entities", Entities] = Table["entities", Entities]
//		val table2 :StaticTable[Entities, "entities"] = Table[Entities, "entities"]
	}


}
