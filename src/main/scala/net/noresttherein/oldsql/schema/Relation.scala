package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.bits.ConstantMapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, OriginProjection}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.FunctorProjection
import net.noresttherein.oldsql.sql.Using
import net.noresttherein.oldsql.sql.Using.JoinedRelationSubject



/**
  * @author Marcin Mościcki
  */
trait Relation[+M[O] <: MappingAt[O]] {
	def row[O] :M[O] = apply[O]

	def apply[O] :M[O]

	def sql :String

	override def toString :String = sql
}






object Relation {
	type * = Relation[M] forSome { type M[O] <: MappingAt[O] }


	def apply[M <: Mapping, S](name :String, template :M)
	                          (implicit projection :OriginProjection[M, S]) :Relation[projection.WithOrigin] =
		new ProjectingSource[projection.WithOrigin, S](projection[Any](template), name)(projection.isomorphism)


	def apply[M <: Mapping, S](template :M)
	                          (implicit projection :OriginProjection[M, S]) :Relation[projection.WithOrigin] =
		new ProjectingSource[projection.WithOrigin, S](projection[Any](template))(projection.isomorphism)






	implicit def identityCast[J[M[O] <: MappingAt[O]] <: _ Using M, R[O] <: MappingAt[O], T[O] <: BaseMapping[_, O]]
	                         (source :Relation[R])
	                         (implicit cast :JoinedRelationSubject[J, R, T, BaseMapping.AnyAt]) :Relation[T] =
		cast(source)






	private class ProjectingSource[+M[O] <: BaseMapping[S, O], S]
	                              (protected[this] val template :M[Any], override val sql :String)
	                              (implicit protected[this] val projection :FunctorProjection[M, S, Any])
		extends Relation[M]
	{
		def this(template :M[Any])(implicit projection :FunctorProjection[M, S, Any]) =
			this(template, template.sqlName getOrElse {
				throw new IllegalArgumentException(
					s"Can't create a Relation with template mapping $template as it has an empty sqlName."
				)
			})

		override def apply[O] :M[O] = projection(template)
	}



	trait NamedRelation[N <: String with Singleton, +M[O] <: MappingAt[O]] extends Relation[M] {
		def name :N
		override def sql :String = name
	}






	trait Table[+M[O] <: MappingAt[O]] extends Relation[M] {
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



		trait StaticTable[N <: String with Singleton, M[O] <: MappingAt[O]] extends Table[M] with NamedRelation[N, M]

	}



	private[oldsql] val Dummy = Relation(new ConstantMapping["Dummy", "Dummy"]("Dummy"))

}

