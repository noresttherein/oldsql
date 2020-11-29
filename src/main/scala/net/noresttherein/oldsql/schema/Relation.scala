package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.bits.ConstantMapping
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.Relation.AlteredRelation
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.support.AdjustedMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter.Adapted
import net.noresttherein.oldsql.sql.Compound
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject






/**
  * @author Marcin Mościcki
  */
trait Relation[+M[O] <: MappingAt[O]] {
	//we can't have OriginProjection here as M[O] is not a BaseMapping[S, O] and it can't be because it's used in Joins
	def row[O] :M[O] = apply[O]

	def apply[O] :M[O]

	def altered[O] :MappingAt[O]// = apply[O]//Adapted[M[O]]


	def apply(components :M[this.type] => ComponentSelection[_, this.type]*) :Relation[M] = {
		val self = row[this.type]; val alt = altered[this.type]
		val excludes = components.view.map { _(self) }.collect {
			case ExcludedComponent(c) => alt.export(c)
		}.to(Unique)
		val includes = components.view.map { _(self) }.collect {
			case IncludedComponent(c) => alt.export(c)
		}.filterNot(excludes.contains).to(Unique)
		new AlteredRelation[M](this, includes, excludes)
	}

	def include(components :M[this.type] => RefinedMapping[_, this.type]*) :Relation[M] = {
		val self = row[this.type]; val alt = altered[this.type]
		val comps = components.view.map(_(self)).map(alt.export(_)).to(Unique)
		new AlteredRelation[M](this, comps, Unique.empty)
	}

	def exclude(components :M[this.type] => RefinedMapping[_, this.type]*) :Relation[M] = {
		val self = row[this.type]; val alt = altered[this.type]
		val comps = components.view.map(_(self)).map(alt.export(_)).to(Unique)
		new AlteredRelation[M](this, Unique.empty, comps)
	}


	def sql :String

	override def toString :String = sql

}






object Relation {
	type * = Relation[M] forSome { type M[O] <: MappingAt[O] }


	def apply[M <: Mapping, S](name :String, template :M)
	                          (implicit projection :OriginProjection[M, S]) :Relation[projection.WithOrigin] =
		new ProjectingRelation[projection.WithOrigin, S](projection[()](template), name)(projection.isomorphism)



	/** Extension methods for [[net.noresttherein.oldsql.schema.Relation Relation]], extracted due to its covariance
	  * in the mapping parameter.
	  */
	implicit class RelationAliasing[M[O] <: MappingAt[O]](private val self :Relation[M]) extends AnyVal {
		@inline def alias[A <: Label :ValueOf] :M Aliased A = new Aliased[M, A](self, valueOf[A])
		@inline def as[A <: Label](alias :A) :M Aliased A = new Aliased[M, A](self, alias)
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
		override def apply[O] :M[O] = projection(template)
		override def altered[O] :M[O] = projection(template)
	}



	trait SelectableRelation[+M[O] <: MappingAt[O]] extends Relation[M]

	trait PersistentRelation[+M[O] <: MappingAt[O]] extends Relation[M] {
		def name :String
		override def sql :String = name
	}

	trait StaticRelation[N <: String with Singleton, +M[O] <: MappingAt[O]] extends PersistentRelation[M]

	trait PseudoRelation[+M[O] <: MappingAt[O]] extends Relation[M]


	/** A `Relation[M]` wrapper annotating it with an alias `L` (a string literal type).
	  * The mapping of the relation is adapted to a [[net.noresttherein.oldsql.schema.bits.LabeledMapping LabeledMapping]]
	  * `L @: M`. It exposes the labeled type as a single-argument type constructor `this.T[O]`
	  * accepting the `Origin` type for use in `RowProduct` subclasses and other types accepting such a type constructor:
	  * `Dual Join (Humans Aliased "humans")#T` (where `Humans[O] <: MappingAt[O]`).
	  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping.@:]]
	  */ //
	class Aliased[M[O] <: MappingAt[O], A <: Label](val relation :Relation[M], val alias :A)
		extends StaticRelation[A, ({ type T[O] = A @: M[O] })#T]
	{ //todo: the contract of name/sql must be specified here; is alias a part of sql?
		type T[O] = A @: M[O]

		override def name :A = alias
		override def sql :String = relation.sql

		override def apply[O] :A @: M[O] = labeled.asInstanceOf[A @: M[O]]
		override def altered[O] = relation.altered[O]

		private val labeled = alias @: relation[()].refine

	}






	trait Table[+M[O] <: MappingAt[O]] extends PersistentRelation[M] { outer =>

		override def apply(components :M[this.type] => ComponentSelection[_, this.type]*) :Table[M] = {
			val self = row[this.type]; val alt = altered[this.type]
			val excludes = components.view.map { _(self) }.collect {
				case ExcludedComponent(c) => alt.export(c)
			}.to(Unique)
			val includes = components.view.map { _(self) }.collect {
				case IncludedComponent(c) => alt.export(c)
			}.filterNot(excludes.contains).to(Unique)
			alter(includes, excludes)
		}

		override def include(components :M[this.type] => RefinedMapping[_, this.type]*) :Table[M] = {
			val self = row[this.type]; val alt = altered[this.type]
			val comps = components.view.map(_(self)).map(alt.export(_)).to(Unique)
			alter(comps, Unique.empty)
		}

		override def exclude(components :M[this.type] => RefinedMapping[_, this.type]*) :Relation[M] = {
			val self = row[this.type]; val alt = altered[this.type]
			val comps = components.view.map(_(self)).map(alt.export(_)).to(Unique)
			alter(Unique.empty, comps)
		}

		private def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]]) :Table[M]  = {
			new AlteredRelation[M](this, includes, excludes) with Table[M] {
				override def name = outer.name
			}
		}
	}



	object Table {

		def apply[M <: Mapping, S](tableName :String, template :M)
		                          (implicit project :OriginProjection[M, S]) :Table[project.WithOrigin] =
			new ProjectingRelation[project.WithOrigin, S](project[()](template), tableName)(project.isomorphism)
				with Table[project.WithOrigin]
			{
				override val sql = tableName
				override def name = sql
			}



		trait StaticTable[N <: String with Singleton, M[O] <: MappingAt[O]] extends Table[M] with StaticRelation[N, M]

	}





	private class AlteredRelation[M[O] <: MappingAt[O]]
	              (val source :Relation[M], includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
		extends Relation[M]
	{
		private val mapping = {
			val mapping = source[this.type]
			AdjustedMapping[RefinedMapping[mapping.Subject, this.type], mapping.Subject, this.type](
				mapping.refine,
				includes.asInstanceOf[Unique[RefinedMapping[_, this.type]]],
				excludes.asInstanceOf[Unique[RefinedMapping[_, this.type]]]
			)
		}

		override def altered[O] = mapping.asInstanceOf[Adapted[M[O]]]

		override def apply[O] :M[O] = source[O]

		override def apply(components :M[this.type] => ComponentSelection[_, this.type]*) :Relation[M] = {
			val self = row[this.type]; val alt = altered[this.type]
			val newExcludes = components.view.map { _(self) }.collect {
				case ExcludedComponent(c) => alt.export(c)
			}.to(Unique)
			val newIncludes = components.view.map { _(self) }.collect {
				case IncludedComponent(c) => alt.export(c)
			}.filterNot(newExcludes.contains).to(Unique)
			new AlteredRelation[M](this,
				(includes.view.filterNot(newExcludes.contains) ++ newIncludes).to(Unique),
				(excludes.view.filterNot(newIncludes.contains) ++ newExcludes).to(Unique)
			)
		}

		override def include(components :M[this.type] => RefinedMapping[_, this.type]*) :Relation[M] = {
			val newIncludes = components.view.map(_(row[this.type])).to(Unique)
			new AlteredRelation[M](source, includes ++ newIncludes, excludes.filterNot(newIncludes.toSet))
		}

		override def exclude(components :M[this.type] => RefinedMapping[_, this.type]*) :Relation[M] = {
			val newExcludes = components.view.map(_(row[this.type])).to(Unique)
			new AlteredRelation[M](source, includes.filterNot(newExcludes.toSet), excludes ++ newExcludes)
		}


		override def sql :String = source.sql

		override lazy val toString :String =
			(includes.view.map("+" + _) ++ excludes.view.map("-" + _)).mkString(s"($sql){", ", ", "}")

	}



	private[oldsql] val Dummy = Relation("Dummy", new ConstantMapping["Dummy", "Dummy"]("Dummy"))

}

