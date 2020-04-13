package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, TypedMapping, MappingAlias}


/**
  * @author Marcin Mościcki
  */
trait RowSource[M[O] <: MappingFrom[O]] {
	type Row[O] = M[O]

	def apply[O] :M[O]
	def sql :String

	override def toString :String = sql
}






object RowSource {

	def apply[M[O] <: MappingFrom[O], A](name :String, template :M[A])
	                                    (implicit alias :MappingAlias[M[A], A, M[Any], Any]) :RowSource[M] =
		new AliasingSource(template, name)


	def apply[M[O] <: MappingFrom[O], A](template :M[A])
	                                    (implicit alias :MappingAlias[M[A], A, M[Any], Any]) :RowSource[M] =
		new AliasingSource(template)






	class AliasingSource[M[O] <: MappingFrom[O], A](protected val template :M[A], override val sql :String)
	                                               (implicit protected val alias :MappingAlias[M[A], A, M[Any], Any])
		extends RowSource[M]
	{
		def this(template :M[A])(implicit alias :MappingAlias[M[A], A, M[Any], Any]) =
			this(template, template.sqlName getOrElse {
				throw new IllegalArgumentException(
					s"Can't create a RowSource with template mapping $template as it has an empty sqlName."
				)
			})

		override def apply[O] :M[O] = alias.asInstanceOf[MappingAlias[M[A], A, M[O], O]](template)
	}



	trait NamedSource[N <: String with Singleton, M[O] <: MappingFrom[O]] extends RowSource[M] {
		def name :N
		override def sql :String = name
	}






	trait Table[M[O] <: MappingFrom[O]] extends RowSource[M] {
		def name :String
		override def sql :String = name
	}

	object Table {

		def apply[M[O] <: MappingFrom[O], A](tableName :String, template :M[A])
		                                    (implicit alias :MappingAlias[M[A], A, M[Any], Any]) :Table[M] =
			new AliasingSource[M, A](template, tableName) with Table[M] {
				override val sql = tableName
				override def name = sql
			}



		def apply[M[O] <: MappingFrom[O]](tableName :String)
		                                 (implicit mapping :String => M[_],
		                                   alias :MappingAlias[M[AnyRef], AnyRef, M[Any], Any]) :Table[M] =
			new Table[M] {
				override val name = tableName
				override def apply[O]  = mapping(name).asInstanceOf[M[O]]
			}



		def apply[N <: String with Singleton, M[O] <: MappingFrom[O]]
		         (implicit tableName :ValueOf[N], mapping :String => M[_],
		          alias :MappingAlias[M[AnyRef], AnyRef, M[Any], M[Any]]) :StaticTable[N, M] =
			new StaticTable[N, M] {
				override val name = valueOf[N]
				override def apply[O] = mapping(name).asInstanceOf[M[O]]
			}



		def of[S] :TableConstructor[S] = new TableConstructor[S] {}

		sealed trait TableConstructor[S] extends Any {
			def apply[M[O] <: TypedMapping[S, O]](name :String)
			                                     (implicit mapping :String => M[_],
			                                   alias :MappingAlias[M[AnyRef], AnyRef, M[Any], Any]) :Table[M] =
				Table(name, mapping(name).asInstanceOf[M[AnyRef]])
		}



		trait StaticTable[N <: String with Singleton, M[O] <: MappingFrom[O]] extends Table[M] with NamedSource[N, M]


	}


}
