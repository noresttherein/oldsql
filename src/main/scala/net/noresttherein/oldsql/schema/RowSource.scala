package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.support.ConstantMapping
import net.noresttherein.oldsql.sql.Join
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject



//todo: Relation?
/**
  * @author Marcin Mościcki
  */
trait RowSource[M[O] <: MappingFrom[O]] {
	type Row[O] = M[O] //consider: RowSource could be covariant if not for this member type. Maybe not needed?

	def apply[O] :M[O]
	def sql :String

	override def toString :String = sql
}






object RowSource {
	type * = RowSource[M] forSome { type M[O] <: MappingFrom[O] }


	def apply[M[O] <: MappingFrom[O], A](name :String, template :M[A])
	                                    (implicit alias :OriginProjection[M[A], A, M[Any], Any]) :RowSource[M] =
		new AliasingSource(template, name)


	def apply[M[O] <: MappingFrom[O], A](template :M[A])
	                                    (implicit alias :OriginProjection[M[A], A, M[Any], Any]) :RowSource[M] =
		new AliasingSource(template)






	implicit def identityCast[J[M[O] <: MappingFrom[O]] <: _ Join M, R[O] <: MappingFrom[O], T[O] <: TypedMapping[_, O]]
	                         (source :RowSource[R])
	                         (implicit cast :JoinedRelationSubject[J, R, T, TypedMapping.AnyFrom]) :RowSource[T] =
		cast(source)






	class AliasingSource[M[O] <: MappingFrom[O], A](protected val template :M[A], override val sql :String)
	                                               (implicit protected val alias :OriginProjection[M[A], A, M[Any], Any])
		extends RowSource[M]
	{
		def this(template :M[A])(implicit alias :OriginProjection[M[A], A, M[Any], Any]) =
			this(template, template.sqlName getOrElse {
				throw new IllegalArgumentException(
					s"Can't create a RowSource with template mapping $template as it has an empty sqlName."
				)
			})

		override def apply[O] :M[O] = alias.asInstanceOf[OriginProjection[M[A], A, M[O], O]](template)
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
		                                    (implicit alias :OriginProjection[M[A], A, M[Any], Any]) :Table[M] =
			new AliasingSource[M, A](template, tableName) with Table[M] {
				override val sql = tableName
				override def name = sql
			}



		def apply[M[O] <: MappingFrom[O]](tableName :String)
		                                 (implicit mapping :String => M[_],
		                                   alias :OriginProjection[M[AnyRef], AnyRef, M[Any], Any]) :Table[M] =
			new Table[M] {
				override val name = tableName
				override def apply[O]  = mapping(name).asInstanceOf[M[O]]
			}



		def apply[N <: String with Singleton, M[O] <: MappingFrom[O]]
		         (implicit tableName :ValueOf[N], mapping :String => M[_],
		          alias :OriginProjection[M[AnyRef], AnyRef, M[Any], M[Any]]) :StaticTable[N, M] =
			new StaticTable[N, M] {
				override val name = valueOf[N]
				override def apply[O] = mapping(name).asInstanceOf[M[O]]
			}



		def of[S] :TableConstructor[S] = new TableConstructor[S] {}

		sealed trait TableConstructor[S] extends Any {
			def apply[M[O] <: RefinedMapping[S, O]](name :String)
			                                       (implicit mapping :String => M[_],
			                                   alias :OriginProjection[M[AnyRef], AnyRef, M[Any], Any]) :Table[M] =
				Table(name, mapping(name).asInstanceOf[M[AnyRef]])
		}



		trait StaticTable[N <: String with Singleton, M[O] <: MappingFrom[O]] extends Table[M] with NamedSource[N, M]


	}



	private[oldsql] val Dummy = RowSource[({ type M[O] = ConstantMapping["Dummy", O] })#M, "Dummy"](
		new ConstantMapping["Dummy", "Dummy"]("Dummy")
	)
}
