package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.bits.ConstantMapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.{ExactProjection, IsomorphicProjection, ProjectionBound}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.Relation.As
import net.noresttherein.oldsql.sql.Compound
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject



/**
  * @author Marcin Mościcki
  */
trait Relation[+M[O] <: MappingAt[O]] {
	//we can't have OriginProjection here as M[O] is not a BaseMapping[S, O] and it can't be because it's used in Joins
	def row[O] :M[O] = apply[O]

	def apply[O] :M[O]

	def sql :String

	override def toString :String = sql
}






object Relation {
	type * = Relation[M] forSome { type M[O] <: MappingAt[O] }


	def apply[M <: Mapping, S](name :String, template :M)
	                          (implicit projection :OriginProjection[M, S]) :Relation[projection.WithOrigin] =
		new ProjectingRelation[projection.WithOrigin, S](projection[()](template), name)(projection.isomorphism)


	def apply[M <: Mapping, S](template :M)
	                          (implicit projection :OriginProjection[M, S]) :Relation[projection.WithOrigin] =
		new ProjectingRelation[projection.WithOrigin, S](projection[()](template))(projection.isomorphism)



	/** Extension methods for [[net.noresttherein.oldsql.schema.Relation Relation]], extracted due to its covariance
	  * in the mapping parameter.
	  */
	implicit class RelationAliasing[M[O] <: MappingAt[O]](private val self :Relation[M]) extends AnyVal {
		@inline def alias[A <: Label :ValueOf] :M As A = new As[M, A](self, valueOf[A])
		@inline def as[A <: Label](alias :A) :M As A = new As[M, A](self, alias)
	}

	implicit def identityCast[J[M[O] <: MappingAt[O]] <: _ Compound M, R[O] <: MappingAt[O], T[O] <: BaseMapping[_, O]]
	                         (source :Relation[R])
	                         (implicit cast :JoinedRelationSubject[J, R, T, BaseMapping.AnyAt]) :Relation[T] =
		cast(source)




	private class ProjectingRelation[+M[O] <: BaseMapping[S, O], S]
	                                (protected[this] val template :M[()], override val sql :String)
	                                (implicit projection :IsomorphicProjection[M, S, ()])
		extends Relation[M]
	{
		def this(template :M[()])(implicit projection :IsomorphicProjection[M, S, ()]) =
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



	trait PersistentRelation[+M[O] <: MappingAt[O]] extends Relation[M] {
		def name :String
		override def sql :String = name
	}



	/** A `Relation[M]` wrapper annotating it with an alias `L` (a string literal type).
	  * The mapping of the relation is adapted to a [[net.noresttherein.oldsql.schema.bits.LabeledMapping LabeledMapping]]
	  * `L @: M`. It exposes the labeled type as a single-argument type constructor `this.T[O]`
	  * accepting the `Origin` type for use in `RowProduct` subclasses and other types accepting such a type constructor:
	  * `Dual Join (Humans As "humans")#T` (where `Humans[O] <: MappingAt[O]`).
	  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping.@:]]
	  */ //
	class As[M[O] <: MappingAt[O], A <: Label](val relation :Relation[M], val alias :A)
		extends NamedRelation[A, ({ type T[O] = A @: M[O] })#T]
	{ //todo: the contract of name/sql must be specified here; is alias a part of sql?
		type T[O] = A @: M[O]

		override def name :A = alias
		override def sql :String = relation.sql

		override def apply[O] :A @: M[O] =
			(alias @: relation[O].asInstanceOf[RefinedMapping[Any, Any]]).asInstanceOf[A @: M[O]]
	}






	trait Table[+M[O] <: MappingAt[O]] extends PersistentRelation[M]

	object Table {

		def apply[M <: Mapping, S](tableName :String, template :M)
		                          (implicit project :OriginProjection[M, S]) :Table[project.WithOrigin] =
			new ProjectingRelation[project.WithOrigin, S](project[()](template), tableName)(project.isomorphism)
				with Table[project.WithOrigin]
			{
				override val sql = tableName
				override def name = sql
			}



		trait StaticTable[N <: String with Singleton, M[O] <: MappingAt[O]] extends Table[M] with NamedRelation[N, M]

	}



	private[oldsql] val Dummy = Relation("Dummy", new ConstantMapping["Dummy", "Dummy"]("Dummy"))

}

