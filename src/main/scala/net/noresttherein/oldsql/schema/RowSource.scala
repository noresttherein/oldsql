package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.bits.ConstantMapping
import net.noresttherein.oldsql.schema.Mapping.{FunctorProjection, MappingAt, OriginProjection}
import net.noresttherein.oldsql.sql.Join
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject



//todo: Relation?
/**
  * @author Marcin Mościcki
  */
trait RowSource[M[O] <: MappingAt[O]] {
	type Row[O] = M[O] //consider: RowSource could be covariant if not for this member type. Maybe not needed?

	def apply[O] :M[O]
	def sql :String

	override def toString :String = sql
}






object RowSource {
	type * = RowSource[M] forSome { type M[O] <: MappingAt[O] }


	def apply[M <: Mapping](name :String, template :M)
	                       (implicit alias :OriginProjection[M]) :RowSource[alias.WithOrigin] =
		new ProjectingSource[alias.WithOrigin](alias[Any](template), name)(alias.ismorphism)


	def apply[M <: Mapping](template :M)
	                       (implicit alias :OriginProjection[M]) :RowSource[alias.WithOrigin] =
		new ProjectingSource[alias.WithOrigin](alias[Any](template))(alias.ismorphism)






	implicit def identityCast[J[M[O] <: MappingAt[O]] <: _ Join M, R[O] <: MappingAt[O], T[O] <: TypedMapping[_, O]]
	                         (source :RowSource[R])
	                         (implicit cast :JoinedRelationSubject[J, R, T, TypedMapping.AnyFrom]) :RowSource[T] =
		cast(source)






	private class ProjectingSource[M[O] <: MappingAt[O]]
	                              (protected val template :M[_], override val sql :String)
	                              (implicit protected val alias :FunctorProjection[M])
		extends RowSource[M]
	{
		def this(template :M[_])(implicit alias :FunctorProjection[M]) =
			this(template, template.sqlName getOrElse {
				throw new IllegalArgumentException(
					s"Can't create a RowSource with template mapping $template as it has an empty sqlName."
				)
			})

		override def apply[O] :M[O] = alias(template)
	}



	trait NamedSource[N <: String with Singleton, M[O] <: MappingAt[O]] extends RowSource[M] {
		def name :N
		override def sql :String = name
	}






	trait Table[M[O] <: MappingAt[O]] extends RowSource[M] {
		def name :String
		override def sql :String = name
	}

	object Table {

		def apply[M <: Mapping](tableName :String, template :M)
		                       (implicit alias :OriginProjection[M]) :Table[alias.WithOrigin] =
			new ProjectingSource[alias.WithOrigin](alias[Any](template), tableName)(alias.ismorphism)
				with Table[alias.WithOrigin]
			{
				override val sql = tableName
				override def name = sql
			}



		trait StaticTable[N <: String with Singleton, M[O] <: MappingAt[O]] extends Table[M] with NamedSource[N, M]


	}



	private[oldsql] val Dummy = RowSource(new ConstantMapping["Dummy", "Dummy"]("Dummy"))

}
