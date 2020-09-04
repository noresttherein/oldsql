package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.bits.ConstantMapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.FunctorProjection
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
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
		new ProjectingRelation[projection.WithOrigin, S](projection[Any](template), name)(projection.isomorphism)


	def apply[M <: Mapping, S](template :M)
	                          (implicit projection :OriginProjection[M, S]) :Relation[projection.WithOrigin] =
		new ProjectingRelation[projection.WithOrigin, S](projection[Any](template))(projection.isomorphism)




	implicit class RelationAliasing[M[O] <: MappingAt[O]](private val self :Relation[M]) extends AnyVal {
		@inline def alias[A <: Label :ValueOf] :M As A = new As[M, A](self, valueOf[A])
		@inline def as[A <: Label](alias :A) :M As A = new As[M, A](self, alias)
	}

	implicit def identityCast[J[M[O] <: MappingAt[O]] <: _ Using M, R[O] <: MappingAt[O], T[O] <: BaseMapping[_, O]]
	                         (source :Relation[R])
	                         (implicit cast :JoinedRelationSubject[J, R, T, BaseMapping.AnyAt]) :Relation[T] =
		cast(source)




	private class ProjectingRelation[+M[O] <: BaseMapping[S, O], S]
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



	/** A `Relation[M]` wrapper annotating it with an alias `L` (a string literal type).
	  * The mapping of the relation is adapted to a [[net.noresttherein.oldsql.schema.bits.LabeledMapping LabeledMapping]]
	  * `L @: M`. It exposes the labeled type as a single-argument type constructor `this.T[O]`
	  * accepting the `Origin` type for use in `FromClause` subclasses and other types accepting such a type constructor:
	  * `Dual Join (Humans As "humans")#T` (where `Humans[O] &lt;: MappingAt[O]`).
	  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping.@:]]
	  */
	class As[M[O] <: MappingAt[O], A <: Label](val relation :Relation[M], val alias :A)
		extends NamedRelation[A, ({ type T[O] = A @: M[O] })#T]
	{
		type T[O] = A @: M[O]

		override def name :A = alias

		override def apply[O] :A @: M[O] =
			(alias @: relation[O].asInstanceOf[RefinedMapping[Any, Any]]).asInstanceOf[A @: M[O]]
	}






	trait Table[+M[O] <: MappingAt[O]] extends Relation[M] {
		def name :String
		override def sql :String = name
	}

	object Table {

		def apply[M <: Mapping, S](tableName :String, template :M)
		                          (implicit projection :OriginProjection[M, S]) :Table[projection.WithOrigin] =
			new ProjectingRelation[projection.WithOrigin, S](projection[Any](template), tableName)(projection.isomorphism)
				with Table[projection.WithOrigin]
			{
				override val sql = tableName
				override def name = sql
			}



		trait StaticTable[N <: String with Singleton, M[O] <: MappingAt[O]] extends Table[M] with NamedRelation[N, M]

	}



	private[oldsql] val Dummy = Relation(new ConstantMapping["Dummy", "Dummy"]("Dummy"))

}

