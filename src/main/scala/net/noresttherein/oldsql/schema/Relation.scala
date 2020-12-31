package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.bits.ConstantMapping
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.Relation.{AlteredRelation, RelationTemplate}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.support.AdjustedMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter.Adapted
import net.noresttherein.oldsql.schema.Relation.BaseTable.TableFactory
import net.noresttherein.oldsql.sql.Adjoin
import net.noresttherein.oldsql.sql.Adjoin.JoinedRelationSubject






/** Supertype of [[net.noresttherein.oldsql.schema.Relation Relation]] introduced for the purpose of using it in
  * collections. In many cases, the compiler cannot abstract over higher kinded type parameters.
  */
sealed trait AbstractRelation extends Serializable { this :Relation[MappingAt] =>
	def generic :Relation[MappingAt] = this
}



/** A representation and mapping of an SQL relation, that is a tuple of SQL data types (columns).
  * This in particular involves tables, views, queries, but can also represent other SQL elements, such
  * as a 'local view' in a ''with'' clause, and, in some cases, be simply adapters of a mapping for a particular
  * SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]].
  * Different instances can represent different tables using the same mapping, but also different views on that table
  * (or an underlying relation in general). Any relation can be modified to include or exclude certain components of `M`
  * from database operations using methods [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.include include]],
  * [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.exclude]] and
  * [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.apply apply]]: the resulting relation will use the
  * same mapping `M` as its 'interface' mapping, but an [[net.noresttherein.oldsql.schema.Relation.export altered]]
  * instance as the ''effective'' mapping, which should be used when accessing the components of `M` for assembly
  * or in order to determine the column set which should be included in a database operation.
  * @see [[net.noresttherein.oldsql.schema.Relation.Table Table]]
  * @see [[net.noresttherein.oldsql.schema.Relation.BaseTable BaseTable]]
  * @author Marcin Mościcki
  */
trait Relation[+M[O] <: MappingAt[O]] extends AbstractRelation with RelationTemplate[M, Relation] {
	//we can't have OriginProjection here as M[O] is not a BaseMapping[S, O] and it can't be because it's used in Joins

	/** The mapping for this relation, with `Origin` type equal to `O`. This is the same as `apply[O]`, but can read
	  * better if the origin type parameter `O` is omitted and inferred. Additionally, calling `apply[O]`
	  * in the form shortened to `[O]` directly on the result of a no argument method can be reported as an error
	  * by IDE.
	  * @return `this[O]`.
	  */
	def row[O] :M[O] = apply[O]

	/** The mapping for this relation, with `Origin` type equal to `O`.
	  * All instances returned by this method, regardless of the origin type, must be interchangeable: each should
	  * recognise all components of the others as its own. All standard implementations reuse the same `Mapping`
	  * instance, casting it to the desired origin type.
	  */
	def apply[O] :M[O]

	/** The ''effective'' mapping of this relation, which is a view on mapping `M` with certain optional
	  * components/columns excluded (or included). The returned mapping will recognise the components of the mapping
	  * returned by [[net.noresttherein.oldsql.schema.Relation.apply apply]] as its (potentially non-export) components.
	  * Similarly to `this[O]`, returned mapping will always be the same instance, or one using the same, shared
	  * component set. For default views of a relation, this method returns simply `this[O]`.
	  */
	def export[O] :MappingAt[O]// = apply[O]//Adapted[M[O]]


	protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
			:Relation[M] =
		new AlteredRelation[M](this, includes, excludes)


	/** The default view of the underlying SQL relation, that is an instance with cleared 'include' and 'exclude'
	  * component lists.
	  */
	def default :Relation[M] = this

	/** Checks if the given relation and this relation represent the same SQL object, that is are (possibly different)
	  * views of the same relation.
	  * @return `default == that.default`
	  */
	def sameAs(that :Relation.*) :Boolean = default == that.default

	def canEqual(that :Any) :Boolean = that.isInstanceOf[Relation.* @unchecked]


	def sql :String

	override def toString :String = sql
}






object Relation {
	type * = Relation[M] forSome { type M[O] <: MappingAt[O] }


	def apply[M <: Mapping, S](name :String, template : => M)
	                          (implicit projection :OriginProjection[M, S]) :Relation[projection.WithOrigin] =
		new ProjectingRelation[projection.WithOrigin, S](projection[()](template), name)(projection.isomorphism)



	/** Extension methods for [[net.noresttherein.oldsql.schema.Relation Relation]], extracted due to its covariance
	  * in the mapping parameter.
	  */
	implicit class RelationAliasing[M[O] <: MappingAt[O]](private val self :Relation[M]) extends AnyVal {
		@inline def alias[A <: Label :ValueOf] :M Aliased A = new Aliased[M, A](self, valueOf[A])
		@inline def as[A <: Label](alias :A) :M Aliased A = new Aliased[M, A](self, alias)
	}

	implicit def identityCast[J[M[O] <: MappingAt[O]] <: _ Adjoin M, R[O] <: MappingAt[O], T[O] <: BaseMapping[_, O]]
	                         (source :Relation[R])
	                         (implicit cast :JoinedRelationSubject[J, R, T, BaseMapping.AnyAt]) :Relation[T] =
		cast(source)



	/** Factory methods of [[net.noresttherein.oldsql.schema.Relation Relation]] trait which return another
	  * instance of the same type, representing a different column set of the same relation.
	  */
	trait RelationTemplate[+M[O] <: MappingAt[O], +R[+T[O] <: MappingAt[O]] <: Relation[T]] {
		this :Relation[M] with RelationTemplate[M, R] =>

		def apply(components :M[this.type] => ComponentSelection[_, this.type]*) :R[M] = {
			val self = row[this.type]; val alt = export[this.type]
			val excludes = components.view.map { _(self) }.collect {
				case ExcludedComponent(c) => alt.export(c)
			}.to(Unique)
			val includes = components.view.map { _(self) }.collect {
				case IncludedComponent(c) => alt.export(c)
			}.filterNot(excludes.contains).to(Unique)
			(this :RelationTemplate[M, R]).alter(includes, excludes)
		}

		def include(components :M[this.type] => RefinedMapping[_, this.type]*) :R[M] = {
			val self = row[this.type]; val alt = export[this.type]
			val comps = components.view.map(_(self)).map(alt.export(_)).to(Unique)
			(this :RelationTemplate[M, R]).alter(comps, Unique.empty)
		}

		def exclude(components :M[this.type] => RefinedMapping[_, this.type]*) :R[M] = {
			val self = row[this.type]; val alt = export[this.type]
			val comps = components.view.map(_(self)).map(alt.export(_)).to(Unique)
			(this :RelationTemplate[M, R]).alter(Unique.empty, comps)
		}

		def +(component :M[this.type] => RefinedMapping[_, this.type]) :R[M] =
			(this :RelationTemplate[M, R]).alter(
				Unique.single(export[this.type].export(component(row[this.type]))), Unique.empty
			)

		def -(component :M[this.type] => RefinedMapping[_, this.type]) :R[M] =
			(this :RelationTemplate[M, R]).alter(
				Unique.empty, Unique.single(export[this.type].export(component(row[this.type])))
			)

		protected def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]]) :R[M]

	}



	/** Base class for most `Relation` implementations, which relies on a prototype
	  * [[net.noresttherein.oldsql.schema.Mapping mapping]] instance `M[_]` and
	  * an [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] in order to cast it for every
	  * [[net.noresttherein.oldsql.schema.Relation.apply apply]]/[[net.noresttherein.oldsql.schema.Relation.row row]]
	  * call. The prototype must be specified as a lazy expression value as there is a cycle of dependencies:
	  * this `Relation` requires a mapping, which might contain foreign key components referencing relation fields
	  * before they are initialized. It is simpler to break it here, and the `lazy val` overhead doesn't hurt,
	  * comparing to each `assemble` call. The downside is that one must still be careful to not refer to
	  * relation `val`s before they are initialized - either have everything declared in a singleton object, or declare
	  * relations as `lazy val`s (or `object`s themselves).
	  */
	private class ProjectingRelation[+M[O] <: BaseMapping[S, O], S]
	                                (prototype: => M[()], override val sql :String)
	                                (implicit projection :IsomorphicProjection[M, S, ()])
		extends Relation[M]
	{
		protected[this] lazy val template = prototype
		override def row[O] :M[O] = projection(template)
		override def apply[O] :M[O] = projection(template)
		override def export[O] :M[O] = projection(template)
	}



	/** A named relation, which is represented in generated SQL by its name.
	  * This is a generalization of the concept of a ''relation variable'' to any `Relation` implementation,
	  * not only static elements of a table schema. In particular, this includes local views of ''with'' clauses
	  * as well as query parameters.
	  */
	trait NamedRelation[+M[O] <: MappingAt[O]] extends Relation[M] {
		def name :String
		override def sql :String = name
	}

	/** A relation with a static name, typically (but not necessarily) representing a permament element
	  * of a table schema. It is a [[net.noresttherein.oldsql.schema.Relation.NamedRelation NamedRelation]]
	  * with its name displayed as `String` literal type parameter `N`, which allows it to be more easily identified
	  * or referred to. It is extended by implicit relation values in a schema, which can be then summoned
	  * by their names, in particular when resolving foreign key targets.
	  */
	trait StaticRelation[N <: String with Singleton, +M[O] <: MappingAt[O]] extends NamedRelation[M]

	/** A marker trait for `Relation` implementations which do not represent any SQL object or language element,
	  * but are synthetic instances used by the SQL DSL, such as query parameters or grouping expressions.
	  */
	trait PseudoRelation[+M[O] <: MappingAt[O]] extends Relation[M]






	/** A relation which can occur inside a ''from'' clause of an SQL ''select''. This includes not only 'true'
	  * database tables and views, but also ''derived tables'' of other ''selects''.
	  * @see [[net.noresttherein.oldsql.schema.Relation.BaseTable]]
	  * @see [[net.noresttherein.oldsql.schema.Relation.DerivedTable]]
	  */
	trait Table[+M[O] <: MappingAt[O]] extends Relation[M] with RelationTemplate[M, Table] { outer =>
		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:Table[M] =
			new AlteredRelation[M](this, includes, excludes) with Table[M] {
				override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]]) =
					outer.alter(
						(this.includes.view ++ includes).filterNot(excludes.contains(_)).to(Unique),
						(this.excludes.view.filterNot(includes.contains(_)) ++ excludes).to(Unique)
					)
			}
	}


	object Table {
		def apply[M <: Mapping, S](tableName :String, template : => M)
		                          (implicit project :OriginProjection[M, S]) :BaseTable[project.WithOrigin] =
			BaseTable[M, S](tableName, template)

		def apply[S] :TableFactory[S] = new TableFactory[S] {}

		trait StaticTable[N <: String with Singleton, M[O] <: MappingAt[O]] extends Table[M] with StaticRelation[N, M] {
			override def name :N
		}

		type * = Table[M] forSome { type M[O] <: MappingAt[O] }
	}




	trait RelVar[+M[O] <: MappingAt[O]] extends Table[M] with NamedRelation[M] with RelationTemplate[M, RelVar] {
		outer =>

		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:RelVar[M] =
			new AlteredRelation[M](this, includes, excludes) with RelVar[M] {
				override def name = outer.name

				override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]]) =
					outer.alter(
						(this.includes.view ++ includes).filterNot(excludes.contains(_)).to(Unique),
						(this.excludes.view.filterNot(includes.contains(_)) ++ excludes).to(Unique)
					)
			}
	}




	/** Supertype of all non-persistent (at least semantically) SQL relations.
	  * This includes [[net.noresttherein.oldsql.schema.Relation.View views]] and
	  * [[net.noresttherein.oldsql.sql.ast.QuerySQL.QueryRelation selects]] used in ''from'' and ''with'' clauses.
	  */
	trait DerivedTable[+M[O] <: MappingAt[O]] extends Table[M] with RelationTemplate[M, DerivedTable] { outer =>

		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:DerivedTable[M] =
			new AlteredRelation[M](this, includes, excludes) with DerivedTable[M] {
				override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]]) =
					outer.alter(
						(this.includes.view ++ includes).filterNot(excludes.contains(_)).to(Unique),
						(this.excludes.view.filterNot(includes.contains(_)) ++ excludes).to(Unique)
					)
			}
	}


	object DerivedTable {
		type * = DerivedTable[M] forSome { type M[O] <: MappingAt[O] }
	}




	/** An SQL view. Treated in the same way as [[net.noresttherein.oldsql.schema.Relation.BaseTable tables]]. */
	trait View[+M[O] <: MappingAt[O]]
		extends DerivedTable[M] with RelVar[M] with RelationTemplate[M, View]
	{ outer =>
		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:View[M] =
			new AlteredRelation[M](this, includes, excludes) with View[M] {
				override val name = outer.name

				override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]]) =
					outer.alter(
						(this.includes.view ++ includes).filterNot(excludes.contains(_)).to(Unique),
						(this.excludes.view.filterNot(includes.contains(_)) ++ excludes).to(Unique)
					)
			}
	}


	object View {

		def apply[M <: Mapping, S](tableName :String, template : => M)
		                          (implicit project :OriginProjection[M, S]) :View[project.WithOrigin] =
			new ProjectingRelation[project.WithOrigin, S](project[()](template), tableName)(project.isomorphism)
				with View[project.WithOrigin]
			{
				override val sql = tableName
				override def name = sql
			}

		def apply[S] :ViewFactory[S] = new ViewFactory[S] {}

		trait ViewFactory[S] extends Any { //fixme: lazy implicit
			def apply[M <: Mapping](tableName :String)(implicit mapping :M, project :OriginProjection[M, S])
					:View[project.WithOrigin] =
				View(tableName, mapping)
		}

		trait StaticView[N <: String with Singleton, M[O] <: MappingAt[O]] extends Table[M] with StaticRelation[N, M] {
			override def name :N
		}

		type * = View[M] forSome { type M[O] <: MappingAt[O] }

	}




	/** A 'true', persistent and updatable SQL table. */
	trait BaseTable[+M[O] <: MappingAt[O]]
		extends Table[M] with RelVar[M] with RelationTemplate[M, BaseTable]
	{ outer =>
		override protected def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:BaseTable[M] =
			new AlteredRelation[M](this, includes, excludes) with BaseTable[M] {
				override val name = outer.name

				protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
						:BaseTable[M] =
					outer.alter(
						(this.includes.view ++ includes).filterNot(excludes.contains(_)).to(Unique),
						(this.excludes.view.filterNot(includes.contains(_)) ++ excludes).to(Unique)
					)
			}
	}


	object BaseTable {

		def apply[M <: Mapping, S](tableName :String, template : => M)
		                          (implicit project :OriginProjection[M, S]) :BaseTable[project.WithOrigin] =
			new ProjectingRelation[project.WithOrigin, S](project[()](template), tableName)(project.isomorphism)
				with BaseTable[project.WithOrigin]
			{
				override val sql = tableName
				override def name = sql
			}

		def apply[S] :TableFactory[S] = new TableFactory[S] {}

		trait TableFactory[S] extends Any { //fixme: lazy implicit
			def apply[M <: Mapping](tableName :String)(implicit mapping :M, project :OriginProjection[M, S])
					:BaseTable[project.WithOrigin] =
				BaseTable(tableName, mapping)
		}


		trait StaticBaseTable[N <: String with Singleton, M[O] <: MappingAt[O]]
			extends BaseTable[M] with StaticRelation[N, M]

		type * = BaseTable[M] forSome { type M[O] <: MappingAt[O] }
	}






	/** A view on a `Relation[M]` which modifies the row mapping `M` by including and/or excluding
	  * certain optional components.
	  */
	class AlteredRelation[M[O] <: MappingAt[O]]
	                     (override val default :Relation[M],
	                      val includes :Unique[RefinedMapping[_, _]], val excludes :Unique[RefinedMapping[_, _]])
		extends Relation[M] with RelationTemplate[M, Relation]
	{
		private val mapping = {
			val mapping = default.export[this.type]
			AdjustedMapping[RefinedMapping[mapping.Subject, this.type], mapping.Subject, this.type](
				mapping.refine,
				includes.asInstanceOf[Unique[RefinedMapping[_, this.type]]],
				excludes.asInstanceOf[Unique[RefinedMapping[_, this.type]]]
			)
		}

		override def row[O] :M[O] = default[O]
		override def apply[O] :M[O] = default[O]
		override def export[O] :Adapted[M[O]] = mapping.asInstanceOf[Adapted[M[O]]]

		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:Relation[M] =
			new AlteredRelation[M](default, includes, excludes)


		override def sql :String = default.sql

		override lazy val toString :String =
			(includes.view.map("+" + _) ++ excludes.view.map("-" + _)).mkString(s"($sql){", ", ", "}")

	}



	/** A `Relation[M]` wrapper annotating it with an alias `L` (a string literal type).
	  * The mapping of the relation is adapted to a [[net.noresttherein.oldsql.schema.bits.LabeledMapping LabeledMapping]]
	  * `L @: M`. It exposes the labeled type as a single-argument type constructor `this.T[O]`
	  * accepting the `Origin` type for use in `RowProduct` subclasses and other types accepting such a type constructor:
	  * `Dual Join (Humans Aliased "humans")#T` (where `Humans[O] <: MappingAt[O]`).
	  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping.@:]]
	  */
	class Aliased[M[O] <: MappingAt[O], A <: Label](val relation :Relation[M], val alias :A)
		extends StaticRelation[A, ({ type T[O] = A @: M[O] })#T]
	{ //todo: the contract of name/sql must be specified here; is alias a part of sql?
		type T[O] = A @: M[O]

		override def name :A = alias
		override def sql :String = relation.sql

		override def apply[O] :A @: M[O] = labeled.asInstanceOf[A @: M[O]]
		override def export[O] = relation.export[O]

		private val labeled = alias @: relation[()].refine

	}



	private[oldsql] val Dummy = Table("Dummy", new ConstantMapping["Dummy", "Dummy"]("Dummy"))

}

