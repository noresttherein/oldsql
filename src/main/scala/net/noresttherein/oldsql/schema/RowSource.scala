package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.bits.ConstantMapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, OriginProjection}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.FunctorProjection
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


	def apply[M <: Mapping, S](name :String, template :M)
	                          (implicit projection :OriginProjection[M, S]) :RowSource[projection.WithOrigin] =
		new ProjectingSource[projection.WithOrigin, S](projection[Any](template), name)(projection.isomorphism)


	def apply[M <: Mapping, S](template :M)
	                          (implicit projection :OriginProjection[M, S]) :RowSource[projection.WithOrigin] =
		new ProjectingSource[projection.WithOrigin, S](projection[Any](template))(projection.isomorphism)






	implicit def identityCast[J[M[O] <: MappingAt[O]] <: _ Join M, R[O] <: MappingAt[O], T[O] <: BaseMapping[_, O]]
	                         (source :RowSource[R])
	                         (implicit cast :JoinedRelationSubject[J, R, T, BaseMapping.AnyFrom]) :RowSource[T] =
		cast(source)






	private class ProjectingSource[M[O] <: BaseMapping[S, O], S]
	                              (protected val template :M[Any], override val sql :String)
	                              (implicit protected val projection :FunctorProjection[M, S, Any])
		extends RowSource[M]
	{
		def this(template :M[Any])(implicit projection :FunctorProjection[M, S, Any]) =
			this(template, template.sqlName getOrElse {
				throw new IllegalArgumentException(
					s"Can't create a RowSource with template mapping $template as it has an empty sqlName."
				)
			})

		override def apply[O] :M[O] = projection(template)
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

		def apply[M <: Mapping, S](tableName :String, template :M)
		                          (implicit projection :OriginProjection[M, S]) :Table[projection.WithOrigin] =
			new ProjectingSource[projection.WithOrigin, S](projection[Any](template), tableName)(projection.isomorphism)
				with Table[projection.WithOrigin]
			{
				override val sql = tableName
				override def name = sql
			}



		trait StaticTable[N <: String with Singleton, M[O] <: MappingAt[O]] extends Table[M] with NamedSource[N, M]


	}



	private[oldsql] val Dummy = RowSource(new ConstantMapping["Dummy", "Dummy"]("Dummy"))

}

