package net.noresttherein.oldsql.schema

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.{Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.IllegalComponentException
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.BaseTable.{StaticBaseTable, StaticTableFactory, TableFactoryForEntity}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.Relation.{AlteredRelation, AlteredRelationOverrides, NamedRelation, ProjectingRelation, RelationTemplate, StaticRelation}
import net.noresttherein.oldsql.schema.Table.{DerivedTable, StaticTable}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.ConstantMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.support.AlteredMapping
import net.noresttherein.oldsql.schema.support.AlteredMapping.annulled
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, DelegateAdapter}
import net.noresttherein.oldsql.slang.castTypeParam
import net.noresttherein.oldsql.sql.{AndFrom, From, RowProduct, WithClause}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.ast.{JoinedTable, QuerySQL, TableSQL}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.MappingSubject
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






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
  * same mapping `M` as its 'interface' mapping,
  * but an [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.export altered]]
  * instance as the ''effective'' mapping, which should be used when accessing the components of `M` for assembly
  * or in order to determine the column set which should be included in a database operation.
  * @see [[net.noresttherein.oldsql.schema.Table Table]]
  * @see [[net.noresttherein.oldsql.schema.BaseTable BaseTable]]
  * @author Marcin Mościcki
  */
//consider: adding an entity type, potentially different from the mapping's subject due to a conversion in a table expression
//Consider: making it invariant, because it makes no sense that it is variant while JoinedRelation is not.
// We can always declare type Subrelation[+M[O] <: MappingAt[O]] = Relation[T[O]] forSome { type T[O] > M[O] <: MappingAt[O] }
trait Relation[+M[O] <: MappingAt[O]] extends AbstractRelation with RelationTemplate[M, Relation[M]] {
//todo: migrate M to be a member type, as this will allow TableMapping extends Mapping with Table { type Row[O] = this.type { type Origin = O } }
//The relation being covariant is actually a bit of a problem, because they would not be homomorphic and
//  thus an attempt to use an SQLExpression created based on `Relation[S]` would throw an exception while anchoring
//  in Relation[T], T <: S (and vice versa). This will be mitigated in Scala 3 by demanding M[O] <: TypedMapping[_, O],
//  that is its `Subject` type is well defined, and thus relations for different classes in an inheritance hierarchy
//  would not be type compatible with one another (or their supertype).

//We could have val template :M[this.type] if we needed to refer to its Subject

	override def default :Relation[M] = this

	protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
			:Relation[M] =
		if (includes.isEmpty && excludes.isEmpty) this
		else new AlteredRelation[M, Relation[M]](this, includes, excludes)


	/** Number of JDBC parameters ('?' placeholders) in the definition of this relation. It defaults to zero
	  * and is overriden only by [[net.noresttherein.oldsql.schema.Table.TableExpression TableExpression]]
	  * and [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation GroupingRelation]] to
	  * the [[net.noresttherein.oldsql.sql.SQLExpression.sqlParamCount sqlParamCount]] property of their expressions.
	  */
	protected def sqlParamCount(implicit spelling :SQLSpelling) :Int = 0
	private[oldsql] final def `->sqlParamCount`(spelling :SQLSpelling) :Int = sqlParamCount(spelling)

	/** Checks if the given relation and this relation represent the same SQL object, that is are (possibly different)
	  * views of the same relation. By default this is defined as the equality of
	  * [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.alter unaltered]] relations, which is
	  * reference equality of the mappings of the [[net.noresttherein.oldsql.schema.Relation.row row]]
	  * property.
	  */ //todo: translate all row == other.row and default == other.default to sameAs(other)
	def sameAs(that :Relation[MappingAt]) :Boolean =
		canEqual(that) && that.canEqual(this) && default == that.default

	/** Two relations are ''isomorphic'' if their
	  *  [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.export export]] mappings
	  *  are [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] - have the same
	  * `export` component structure with the same buffs affecting their inclusion in database operations.
	  * Substituting one for another in an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]]
	  * will always result in a valid, albeit possibly different, SQL expression. This however applies only
	  * to the `SQLExpression` class, and not necessarily their final textual representation.
	  * It does not imply equality of names of tables and their columns, or the actual queries for table expressions.
	  * This is 'shape' equality which, for example, determines whether
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL component expressions]] for these relations can be aligned
	  * columnwise with each other, or whether two SQL ''selects'' can be combined
	  * in a [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL compound select]].
	  */
	def isomorphic(that :Relation[MappingAt]) :Boolean = export isomorphic that.export

	/** Two relations are ''equivalent'' if their
	  *  [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.export export]] mappings
	  *  are [[net.noresttherein.oldsql.schema.Mapping.equivalent equivalent]] - have the same
	  * `export` component structure with the same buffs affecting their inclusion in database operations,
	  * and their corresponding columns are equivalent.
	  * This implies that one can replace another within any SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]],
	  * always resulting in a valid expression, and substituting one for another in any SQL statement will have
	  * no effect on read results, although the actual tabular result sets may differ).
	  * It does not imply equality of names for tables, or the actual queries for table expressions.
	  */
	def equivalent(that :Relation[MappingAt]) :Boolean = export equivalent that.export

	/** Two relations are ''identical'', if their mappings, names,
	  * queries for [[net.noresttherein.oldsql.schema.Table.TableExpression table expressions]], etc., are identical.
	  * This is 'deep equality', where a deserialized instance should by identical to its original before serialization.
	  * Substituting one for another in any SQL statement does not change the generated SQL or the returned result.
	  */
	def identical(that :Relation[MappingAt]) :Boolean =
		canEqual(that) && that.canEqual(this) && (export identical that.export) && (row identical that.row)

//	override def equals(that :Any) :Boolean = super.equals(that)
	def canEqual(that :Any) :Boolean = that.isInstanceOf[Relation.__ @unchecked]


	/** The short name of this relation, to be used in `toString` implementations of SQL expressions,
	  * in the context of referring to the relation and its columns (rather than its declaration).
	  * Tables will normally return their name and the default implementation forwards to
	  * [[net.noresttherein.oldsql.schema.Mapping.mappingName mappingName]] of
	  * its [[net.noresttherein.oldsql.schema.Mapping Mapping]] as in `toString`, but relations with longer
	  * `toString` results - in particular ''select'' [[net.noresttherein.oldsql.schema.Table.TableExpression relations]]
	  * should return some abbreviation, which can be used as a practical prefix for column names.
	  * This name should not be used for any other purpose than logging and debugging and its format is unspecified
	  * and subject to change without notice.
	  */ //todo: ChunkedString
	def refString :String = export.mappingName

	/** A debug string used in `toString` implementations of [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]
	  * subtypes which list relations available to expressions within queries based on them.
	  */
	override def toString :String = export.mappingName
}






//consider: how me might preserve equality after serialization
object Relation {

	type __ = Relation[MappingAt]



	/** Factory methods of [[net.noresttherein.oldsql.schema.Relation Relation]] trait which return another
	  * instance of the same type, representing a different column set of the same relation.
	  */
	trait RelationTemplate[+M[O] <: MappingAt[O], +R <: Relation[M]] {
		protected lazy val self :M[this.type] = row //we'll need a stable value for PDTs after Scala 3 refactor

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
		  */ //todo: in Scala 3 make the return type have the same subject type by using member types
		def export[O] :MappingAt[O] //todo: rename to actual


		/** All temporary named derived tables used by this relation, which should be included in the ''with'' clause
		  * of any query or statement using this relation. It will return a non empty collection only
		  * for [[net.noresttherein.oldsql.sql.CommonTableExpression CommonTableExpression]] itself, and those of
		  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation GroupingRelation]] and
		  * [[net.noresttherein.oldsql.schema.Table.TableExpression TableExpression]] instances which contain subselects
		  * referencing them. Note that this refers only to `CommonTableExpression` instances and not all derived tables
		  * such as database ''views'' or an inlined ''select'' of `TableExpression` itself.
		  * Note that recursive common table expressions can reference each other, and their `withClause`
		  * always contains at least those expressions themselves, so the dependency graph can contain cycles.
		  */
		def withClause :WithClause = WithClause.empty


		/** The default view of the underlying SQL relation, that is an instance with cleared 'include' and 'exclude'
		  * component lists. The [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.export export]]
		  */
		def default :Relation[M]

		/** Checks if this relation is its own default version,
		  * that is `this == this.`[[net.noresttherein.oldsql.schema.Relation.default default]].
		  * A [[net.noresttherein.oldsql.schema.Table table]] is default
		  * if its [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.export export]] mapping
		  * is the same as [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.row row]] mapping, in particular
		  * that the relation is not altered by including or excluding some columns. Some synthetic relations may however
		  * always use an altered mapping, even in their default version.
		  */
		def isDefault :Boolean = default == this

		/** Alters this relation by including or excluding specified components of row mapping.
		  * Components wrapped in [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]]
		  * (which can be done by simply calling its [[net.noresttherein.oldsql.schema.Mapping.unary_+ +]] prefix method:
		  * `+component`) will be altered so that they are included by default in all database operations that permit it
		  * (components without a `NoXxx` [[net.noresttherein.oldsql.schema.Buff buff]] drop their `NoXxxByDefault` buff),
		  * while components wrapped in [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent ExcludedComponent]]
		  * (which can be done by simply calling its [[net.noresttherein.oldsql.schema.Mapping.- -]] method)
		  * will be altered so that they are excluded by default in all database operations that permit it
		  * (components with an `OptionalXxx` buff receive an additional `NoXxxByDefault` buff).
		  *
		  * Note that the export mapping of the returned relation will ''not'' recognize components of
		  * `this.`[[net.noresttherein.oldsql.schema.Relation.export export]] unless they are also components of
		  * `this.`[[net.noresttherein.oldsql.schema.Relation.row row]].
		  * @param components functions returning `ComponentSelection` instances for components
		  *                   of their argument mapping (which is always given as `this.row`).
		  */
		def apply(components :M[this.type] => ComponentSelection[_, this.type]*) :R = {
			val mapping = row[this.type]
			val excludes = components.view.map { _(mapping) }.collect {
				case ExcludedComponent(c) => mapping.export(c)
			}.to(Unique)
			val includes = components.view.map { _(mapping) }.collect {
				case IncludedComponent(c) => mapping.export(c)
			}.filter(!annulled(excludes)(_)).to(Unique)
			alter(includes, excludes)
		}

		/** Alters this relation so that the specified components are included in all permitted database operations.
		  * All components of the [[net.noresttherein.oldsql.schema.Relation.row row]] mapping of this relation
		  * returned by the argument functions which have a `NoXxxByDefault`
		  * [[net.noresttherein.oldsql.schema.Buff buff]] but not a `NoXxx` drop the former buff. This change
		  * is propagated to all columns and subcomponents of the component in that they no longer inherit that
		  * buff from the component; however, if any of them declares itself also a `NoXxxByDefault` buff, than
		  * it - and all its columns - will ''not'' be included, unless the subcomponent is also present in
		  * the argument list. All other components are ignored.
		  *
		  * Note that the export mapping of the returned relation will ''not'' recognize components of
		  * `this.`[[net.noresttherein.oldsql.schema.Relation.export export]] unless they are also components of
		  * `this.`[[net.noresttherein.oldsql.schema.Relation.row row]].
		  * @param components functions returning components of `this.row` given as their argument.
		  */
		def include(components :M[this.type] => TypedMapping[_, this.type]*) :R = {
			val mapping = row[this.type]
			val comps = components.view.map(c => mapping.export(c(mapping))).to(Unique)
			alter(comps, Unique.empty)
		}

		/** Alters this relation so that the specified components are excluded from all permitted database operations.
		  * All components of the [[net.noresttherein.oldsql.schema.Relation.row row]] mapping of this relation
		  * returned by the argument functions which have an `OptionalXxx`
		  * [[net.noresttherein.oldsql.schema.Buff buff]] but not a `NoXxx` receive an additional `NoXxxByDefault` buff.
		  * The new buff is inherited by all columns (and other subcomponents) of the component, which will exclude
		  * them regardless of their other buffs or presence as parts of other components.
		  * All other components are ignored.
		  *
		  * Note that the export mapping of the returned relation will ''not'' recognize components of
		  * `this.`[[net.noresttherein.oldsql.schema.Relation.export export]] unless they are also components of
		  * `this.`[[net.noresttherein.oldsql.schema.Relation.row row]].
		  * @param components functions returning components of `this.row` given as their argument.
		  */
		def exclude(components :M[this.type] => TypedMapping[_, this.type]*) :R = {
			val mapping = row[this.type]
			val comps = components.view.map(c => mapping.export(c(mapping))).to(Unique)
			alter(Unique.empty, comps)
		}

		/** Alters this relation so that the specified component is included in all permitted database operations.
		  * If it has a `NoXxxByDefault` [[net.noresttherein.oldsql.schema.Buff buff]], but not a `NoXxx`, the former
		  * is removed from its buff declaration. This change is propagated to all columns and subcomponents
		  * of the component in that they no longer inherit that buff from the component; however,
		  * if any of them declares itself also a `NoXxxByDefault` buff, than it - and all its columns -
		  * will ''not'' be included, unless the subcomponent is also present in the argument list.
		  * All other components are ignored.
		  *
		  * Note that the export mapping of the returned relation will ''not'' recognize components of
		  * `this.`[[net.noresttherein.oldsql.schema.Relation.export export]] unless they are also components of
		  * `this.`[[net.noresttherein.oldsql.schema.Relation.row row]].
		  * @param component a function returning a component of `this.row` given as its argument.
		  */ //todo: or should it be row[this.type].export(component(row[this.type])) ?
		def +(component :M[this.type] => TypedMapping[_, this.type]) :R =
			alter(Unique.single(row[this.type].export(component(row[this.type]))), Unique.empty)

		/** Alters this relation so that the specified component is excluded from all permitted database operations.
		  * If the returned component has an `OptionalXxx` [[net.noresttherein.oldsql.schema.Buff buff]],
		  * but not a `NoXxx`, it receives an additional `NoXxxByDefault` buff.
		  * The new buff is inherited by all columns (and other subcomponents) of the component, which will exclude
		  * them regardless of their other buffs or presence as parts of other components.
		  * All other components are ignored.
		  *
		  * Note that the export mapping of the returned relation will ''not'' recognize components of
		  * `this.`[[net.noresttherein.oldsql.schema.Relation.export export]] unless they are also components of
		  * `this.`[[net.noresttherein.oldsql.schema.Relation.row row]].
		  * @param component a function returning a component of `this.row` given as its argument.
		  */
		def -(component :M[this.type] => TypedMapping[_, this.type]) :R =
			alter(Unique.empty, Unique.single(row[this.type].export(component(row[this.type]))))

		/** Alters this relation by including/excluding specified components from all permitted database operations.
		  * The result is analogous to using an [[net.noresttherein.oldsql.schema.support.AlteredMapping AlteredMapping]]
		  * as the ([[net.noresttherein.oldsql.schema.Relation.export export]]) row mapping. If this relation is already
		  * altered (`this.export != this.`[[net.noresttherein.oldsql.schema.Relation.row row]]), then the alterations
		  * are stacked rather than replaced.
		  *
		  * Note that, unlike with `AlteredMapping`, the [[net.noresttherein.oldsql.schema.Relation.export export]]
		  * mapping of the created relation is not guaranteed to recognize the components of `this.export`
		  * (which are not also components of `this.row`).
		  * @param includes a set of components of `this.row` from which all `NoXxxByDefault` buffs
		  *                 will be removed, if possible.
		  * @param excludes a set of components of `this.row` which will receive `NoXxxByDefault` buffs,
		  *                 if possible.
		  */
		def alter[O](includes :Iterable[TypedMapping[_, O]], excludes :Iterable[TypedMapping[_, O]]) :R = {
			val self = row[O]
			val exportExcludes = excludes.view.map(self.export(_)).to(Unique)
			val exportIncludes = includes.view.map(self.export(_)).filter(!annulled(exportExcludes)(_)).to(Unique)
			alter(exportIncludes, exportExcludes)
		}

		/** Alters this relation by including/excluding specified components from all permitted database operations.
		  * The result is analogous to using an [[net.noresttherein.oldsql.schema.support.AlteredMapping AlteredMapping]]
		  * as the ([[net.noresttherein.oldsql.schema.Relation.export export]]) row mapping. If this relation is already
		  * altered (`this.export != this.`[[net.noresttherein.oldsql.schema.Relation.row row]]), then the alterations
		  * are stacked rather than replaced.
		  * @param includes a set of export components of `this.row` from which all `NoXxxByDefault` buffs
		  *                 will be removed, if possible. The collection is assumed to not contain any subcomponents
		  *                 of components in `excludes`.
		  * @param excludes a set of export components of `this.row` which will receive `NoXxxByDefault` buffs,
		  *                 if possible.
		  */
		//Must be components of this.row, not this.export, because `this.export` is not preserved by AlteredMapping.alter,
		//  and it is not preserved, because the only way it knows how to create copies of R is by delegating to default,
		//  and we don't want to implement it from scratch in every subclass. It is a solvable problem though and this may change.
		protected def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]]) :R

		private[oldsql] def `->alter`[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]]) :R =
			alter(includes, excludes)
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
	private[oldsql] abstract class ProjectingRelation[+M[O] <: BaseMapping[S, O], S]
	                                                 (prototype: => M[Unit])
	                                                 (implicit projection :IsomorphicProjection[M, S, Unit])
		extends Relation[M]
	{
		protected[this] lazy val template = prototype
		override def row[O] :M[O] = projection(template)
		override def apply[O] :M[O] = projection(template)
		override def export[O] :M[O] = projection(template)
		override def toString :String = template.mappingName
	}



	/** A named relation, which is represented in generated SQL by its name.
	  * This is a generalization of the concept of a ''relation variable'' to any `Relation` implementation,
	  * not only static elements of a table schema. In particular, this includes local views of ''with'' clauses
	  * as well as query parameters.
	  */
	trait NamedRelation[+M[O] <: MappingAt[O]] extends Relation[M] {
		/** The name of this relation, used within SQL referring to this relation and `toString` implementations
		  * of [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] subtypes.
		  */
		def name :String

		override def identical(that :Relation[MappingAt]) :Boolean = that match {
			case _ if this eq that => true
			case rel :RelVar[M @unchecked] if canEqual(rel) && (rel canEqual this) =>
				name == rel.name && (export identical that.export)
			case _ => false
		}
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[NamedRelation[MappingAt]]

		override def refString :String = name
		override def toString :String = name
	}

	/** A relation with a static name, typically (but not necessarily) representing a permanent element
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



	/** A view on a relation `R[M]` which modifies the row mapping `M` by including and/or excluding
	  * certain optional components.
	  *
	  * If this class is extended and some [[net.noresttherein.oldsql.schema.Relation.RelationTemplate RelationTemplate]]
	  * subtype is mixed in after it, then `alter` method must be overriden to delegate to the implementation
	  * in this class, as otherwise `default` would be incorrectly set to the altered `this` mapping.
	  * @param includes Export Components of mapping `default.row` which should be included in all operations
	  *                 on this relation, if possible. This list must not include altered components of `default.export`.
	  * @param excludes Export components of mapping `default.row` which should be excluded from all operations
	  *                 on this relation, if possible. This list must not include altered components of `default.export`.
	  */
	class AlteredRelation[M[O] <: MappingAt[O], R <: Relation[M] with RelationTemplate[M, R]]
	                     (override val default :R,
	                      val includes :Unique[TypedMapping[_, _]], val excludes :Unique[TypedMapping[_, _]])
		extends Relation[M]
	{
		private lazy val mapping = {
			val template = default.export[default.type].refine
			//this converts default.row.ExportComponent to default.export.ExportComponent
			new AlteredMapping[template.type, template.Subject, default.type](template,
					includes.asInstanceOf[Unique[TypedMapping[_, default.type]]],
					excludes.asInstanceOf[Unique[TypedMapping[_, default.type]]]
				) with DelegateAdapter[template.type, template.Subject, default.type]
		}

		override def row[O] :M[O] = default[O]
		override def apply[O] :M[O] = default[O]
		override def export[O] :Adapted[M[O]] = mapping.asInstanceOf[Adapted[M[O]]]
		private[Relation] def alteredMapping[O] :Adapted[M[O]] = mapping.asInstanceOf[Adapted[M[O]]]

		protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]]) :R =
			default.`->alter`(
				(this.includes.asInstanceOf[Unique[TypedMapping[_, O]]].view ++ includes).filter(!annulled(excludes)(_))
					.to(Unique),
				(this.excludes.asInstanceOf[Unique[TypedMapping[_, O]]].view.filterNot(includes.contains) ++ excludes)
					.to(Unique)
			)

		private def sameView(other :AlteredRelation[M, R]) :Boolean = {
			def indices(mapping :Mapping, components :Unique[TypedMapping[_, _]]) :Iterable[Int] =
				components.castParam[TypedMapping[_, Unit]].view.map(
					mapping.refine.withOrigin[Unit].subcomponents.sureIndexOf(_)
				)
			indices(row, includes) == indices(other.row, other.includes) &&
				indices(row, excludes) == indices(other.row, other.excludes)
		}
		private[Relation] def alteredIsomorphic(that :Relation[MappingAt]) :Boolean =
			super.isomorphic(that) || (that match {
				case _ if this eq that => true
				case other :AlteredRelation[M @unchecked, R @unchecked] =>
					(default isomorphic other.default) && sameView(other)
				case _ => false
			})
		private[Relation] def alteredEquivalent(that :Relation[MappingAt]) :Boolean =
			super.equivalent(that) || (that match {
				case _ if this eq that => true
				case other :AlteredRelation[M @unchecked, R @unchecked] =>
					(default equivalent other.default) && sameView(other)
				case _ => false
			})
		private[Relation] def alteredIdentical(that :Relation[MappingAt]) :Boolean =
			super.identical(that) || (that match {
				case _ if this eq that => true
				case other :AlteredRelation[M @unchecked, R @unchecked] if canEqual(other) && other.canEqual(this) =>
					(default identical other.default) && sameView(other)
				case _ => false
			})
		private[Relation] def alteredEquals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :AlteredRelation[M @unchecked, R @unchecked] if canEqual(other) && other.canEqual(this) =>
				default == other.default && includes == other.includes && excludes == other.excludes
			case _ => false
		}
		private[Relation] lazy val lazyHashCode = (default.hashCode * 31 + includes.hashCode) * 31 + excludes.hashCode
		override def isomorphic(that :Relation[MappingAt]) :Boolean = alteredIsomorphic(that)
		override def equivalent(that :Relation[MappingAt]) :Boolean = alteredEquivalent(that)
		override def identical(that :Relation[MappingAt]) :Boolean = alteredIdentical(that)
		override def equals(that :Any) :Boolean = alteredEquals(that)
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[AlteredRelation[M @unchecked, R @unchecked]]
		override def hashCode :Int = lazyHashCode

		private[Relation] lazy val alteredString :String =
			(includes.view.map("+" + _) ++ excludes.view.map("-" + _)).mkString("(" + default + "){", ", ", "}")
//		private lazy val alteredRefString :String =
//			(includes.view.map("+" + _) ++ excludes.view.map("-" + _)).mkString(default.refString + "{", ", ", "}")

		override def refString :String = default.refString
		override def toString :String = alteredString
	}


	/** A mixin trait for `AlteredRelation` subclasses overriding several standard methods to their implementations
	  * in `AlteredRelation`. Classes which extend both `AlteredRelation` and a `Relation` interface type
	  * (for example, [[net.noresttherein.oldsql.schema.Table Table]] will generally have the latter trait
	  * override the implementations inherited from the former, which is typically undesirable. Mixing this trait
	  * last addresses the issue.
	  */
	trait AlteredRelationOverrides[M[O] <: MappingAt[O], R <: Relation[M] with RelationTemplate[M, R]]
		extends AlteredRelation[M, R]
	{
		override def export[O] :Adapted[M[O]] = alteredMapping[O]

		protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
				:R =
//			super[AlteredRelation].alter(includes, excludes)
			default.`->alter`(
				(this.includes.asInstanceOf[Unique[TypedMapping[_, O]]].view ++ includes).filter(!annulled(excludes)(_))
					.to(Unique),
				(this.excludes.asInstanceOf[Unique[TypedMapping[_, O]]].view.filterNot(includes.contains) ++ excludes)
					.to(Unique)
			)

		//we are losing the implementations inherited from R, but it doesn't matter,
		// as we compare (default, other.default) and it will check if the names, queries and what not are the same.
		override def isomorphic(that :Relation[MappingAt]) :Boolean = alteredIsomorphic(that)
		override def equivalent(that :Relation[MappingAt]) :Boolean = alteredEquivalent(that)
		override def identical(that :Relation[MappingAt]) :Boolean = alteredIdentical(that)
		override def equals(that :Any) :Boolean = alteredEquals(that)
//		override def canEqual(that :Any) :Boolean = that.isInstanceOf[AlteredRelation[M @unchecked, R @unchecked]]
		override def hashCode :Int = lazyHashCode

		override def refString :String = default.refString
		override def toString  :String = alteredString
	}
}






/** A relation which can occur inside a ''from'' clause of an SQL ''select''. This includes not only 'true'
  * database tables and views, but also ''derived tables'' of other ''selects''.
  * @see [[net.noresttherein.oldsql.schema.BaseTable]]
  * @see [[net.noresttherein.oldsql.schema.RelVar]]
  * @see [[net.noresttherein.oldsql.schema.Table.DerivedTable]]
  */
//todo: base class for singleton objects being both a Table and a Mapping.
// Requires the Mapping type parameter to be a member type instead
trait Table[+M[O] <: MappingAt[O]] extends Relation[M] with RelationTemplate[M, Table[M]] {

	private[Table] val expr = TableSQL.LastTable[MappingOf[Any]#TypedProjection, Any](
		this.asInstanceOf[Table[MappingOf[Any]#TypedProjection]]
	)
	private[Table] val fromClause = From[MappingOf[Any]#TypedProjection, Any, Label](expr, None, True)

	/** Creates a ''from'' clause containing this table.
	  * This method is slightly preferable to `From(this)` as it caches the result.
	  * @return [[net.noresttherein.oldsql.sql.From From]]`(this)`.
	  */ //if we rename withOrigin in OriginProjection to from, then this will clash in TableMapping
	def from[T[O] >: M[O] <: MappingAt[O]] :From[T] = fromClause.asInstanceOf[From[T]]

	/** An SQL expression for the entity mapped by this table when it is the last table listed by a ''from'' clause.
	  * This method allows the creation of a `JoinedTable` even in contexts where `M` is not statically bound
	  * by [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] and is slightly preferable to
	  * `TableSQL.LastTable(this)` as it caches the result.
	  * @return [[net.noresttherein.oldsql.sql.ast.TableSQL.LastTable TableSQL.LastTable]]`(this)`.
	  */
	def joined[T[O] >: M[O] <: MappingAt[O]] :JoinedTable[RowProduct AndFrom T, T] =
		expr.asInstanceOf[JoinedTable[RowProduct AndFrom T, T]]

//	/** Creates an SQL ''select'' expression with this table in its ''from'' clause,
//	  * and the ''select'' clause consisting of all selectable columns the last relation in this clause.
//	  */
//	def select[T[O] >: M[O] <: MappingAt[O]]
//	          (implicit result :CanSelect[From[T], JoinedTable[RowProduct AndFrom T, T]]) :result.Select =
//		result(from, joined)

	protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
			:Table[M] =
		if (includes.isEmpty && excludes.isEmpty)
			this
		else
			new AlteredRelation[M, Table[M]](this, includes, excludes)
				with Table[M] with AlteredRelationOverrides[M, Table[M]]
			{
				override def defaultSpelling[P, F <: RowProduct]
				                            (from :F, context :SQLContext[P], params :Parameterization[P, F])
				                            (implicit spelling :SQLSpelling) =
					default.defaultSpelling(spelling)(from, context, params)
			}


	/** Spells a reference to the given column of this table. The argument column is a component of `table`,
	  * but not necessarily this instance, and might need substituting with a counterpart in this table,
	  * or a column of a mapping combining the changes in this table and the argument.
	  * @param table       An SQL expression for a table compatible with this one. It might be this instance,
	  *                    one sharing the same [[net.noresttherein.oldsql.schema.Relation.default default]] version
	  *                    of the relation (i.e, the same nominal mapping) with this table representing the same
	  *                    database table, or possibly an unrelated instance using a compatible row mapping type.
	  * @param column      A column of `table.`[[net.noresttherein.oldsql.sql.ast.JoinedTable.mapping mapping]].
	  * @param from        The ''from'' clause of the (most nested) SQL ''select'' which contains the expression
	  *                    for the spelled component. The table at `table.position` must be this instance.
	  *                    (possibly as a part of its [[net.noresttherein.oldsql.sql.RowProduct.outer outer]] clause).
	  * @param context     The list of tables in scope of the spelled component expression. In particular,
	  *                    it contains the final alias used in the ''from'' clause for `table` (a given occurrence
	  *                    o this table in the ''from'' clause).  It is purely an 'in' parameter, included unchanged
	  *                    in the returned `SpelledSQL`s, as only [[net.noresttherein.oldsql.sql.FromClause FromClause]]
	  *                    (including synthetic unbound parameters) can introduce new table aliases to scope.
	  * @param params      Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
	  *                    parameters of the formatted SQL. Irrelevant for tables,
	  *                    but may be used by SQL ''table expressions''.
	  */
	@throws[IllegalArgumentException]("If no counterpart for the argument column can be found in this table's mapping.")
	protected def spell[P, F <: O, O <: RowProduct, T[A] >: M[A] <: MappingAt[A], S]
	                   (table :JoinedTable[O, T], column :TypedColumn[S, O])
	                   (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                   (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		if (table.table == this)
			spelling.column(table, table.anchored.export(column))(from, context, params)
		else {
			val actualTable = table.anchor(this)
			val counterpart = actualTable.anchored.counterpart(table.mapping, column)
			spelling.column(actualTable, counterpart)(from, context, params)
		}

	private[oldsql] final def spell[P, F <: O, O <: RowProduct, T[A] >: M[A] <: MappingAt[A], S]
	                               (spelling :SQLSpelling)
	                               (table :JoinedTable[O, T], column :TypedColumn[S, O])
	                               (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
		spell(table, column)(from, context, params)(spelling)


	/** Spells all columns of `component`
	  * default to the spelling [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]].
	  * @param table       An SQL expression for a table compatible to this one. It might be this instance,
	  *                    one sharing the same [[net.noresttherein.oldsql.schema.Relation.default default]] version
	  *                    of the relation (i.e, the same nominal mapping) with this table representing the same
	  *                    database table, or possibly an unrelated instance using a compatible row mapping type.
	  * @param component   A component of `table.`[[net.noresttherein.oldsql.sql.ast.JoinedTable.mapping mapping]].
	  * @param independent If false, the column expressions will not be separated by any JDBC parameters
	  *                    or SQL fragments changing the `SQLContext`, which in case of some expressions can allow
	  *                    optimizations. This parameter is currently irrelevant for `Table`s.
	  * @param from        The ''from'' clause of the (most nested) SQL ''select'' which contains the expression
	  *                    for the spelled component. The table at `table.position` must be this instance.
	  *                    (possibly as a part of its [[net.noresttherein.oldsql.sql.RowProduct.outer outer]] clause).
	  * @param context     The list of tables in scope of the spelled component expression. In particular,
	  *                    it contains the final alias used in the ''from'' clause for `table` (a given occurrence
	  *                    o this table in the ''from'' clause).  It is purely an 'in' parameter, included unchanged
	  *                    in the returned `SpelledSQL`s, as only [[net.noresttherein.oldsql.sql.FromClause FromClause]]
	  *                    (including synthetic unbound parameters) can introduce new table aliases to scope.
	  * @param params      Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
	  *                    parameters of the formatted SQL. Irrelevant for tables,
	  *                    but may be used by SQL ''table expressions''.
	  */
	@throws[IllegalArgumentException]("If no counterpart for the argument component can be found in this table's mapping.")
	protected def spellExploded[P, F <: O, O <: RowProduct, T[A] >: M[A] <: MappingAt[A]]
	                           (table :JoinedTable[O, T], component :MappingAt[O], independent :Boolean)
	                           (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                           (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		component match { //consider: should we pass anchored.ExportComponent or mapping.ExportComponent to spelling?
			case column :TypedColumn[_, O] @unchecked =>
				PassedArray :+ spell(table, column)(from, context, params)
			case _ =>
				val altered = table.anchor(this)
				val counterpart = altered.anchored.counterpart(table.mapping, component)
				val columns = spelling.scope.defaultColumns(altered.anchored, counterpart)
				columns.view.scanLeft(SpelledSQL(context)) {
					(sql, col) => spell(altered, col)(from, sql.context, params)
				}.tail.to(PassedArray)
		}

	private[oldsql] final def spellExploded[P, F <: O, O <: RowProduct, T[A] >: M[A] <: MappingAt[A]]
	                                       (spelling :SQLSpelling)
	                                       (table :JoinedTable[O, T], component :MappingAt[O], independent :Boolean = true)
	                                       (from :F, context :SQLContext[P], params :Parameterization[P, F])
			:Seq[SpelledSQL[P]] =
		spellExploded(table, component, independent)(from, context, params)(spelling)


	/** Spells a column of this relation, as appearing inside an SQL ''select'' with this table in its ''from'' clause.
	  * The column expression should not have an ''as'' clause appended to it at this level.
	  * This is the callback method invoked from default implementations of `column` methods in `SQLSpelling`;
	  * it shouldn't be, as a rule, invoked directly. Use `spell` method with the same signature instead, which will
	  * eventually delegate to this method.
	  * @param origin an occurrence of this table in the from clause `from` (`origin.table` must be this instance).
	  * @param column a view of a column of `origin.mapping`: it might be an actual column
	  *               of `origin.mapping`/`origin.export`, or an instance not recognized by `origin` at all,
	  *               but derived from one of its columns by changing its buffs or other properties
	  *               (other than its name).
	  */
	protected def defaultSpellingOf[P, F <: O, O <: RowProduct, T[A] >: M[A] <: MappingAt[A], V]
	                               (origin :JoinedTable[O, T], column :TypedColumn[V, O])
	                               (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                               (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		if (spelling.scope.isProhibited(column)) //Extra/Custom buffs can't be prohibited in this case
			throw new IllegalComponentException(this, column)
		val tableAlias = context(origin)
		val fullAlias = if (tableAlias.length == 0) column.name else tableAlias + "." + column.name
		SpelledSQL(fullAlias, context)
	}

	protected[oldsql] final def defaultSpellingOf[P, F <: O, O <: RowProduct, T[A] >: M[A] <: MappingAt[A], V]
	                                             (spelling :SQLSpelling)
	                                             (origin :JoinedTable[O, T], column :TypedColumn[V, O])
	                                             (from :F, context :SQLContext[P], params :Parameterization[P, F])
			:SpelledSQL[P] =
		defaultSpellingOf(origin, column)(from, context, params)(spelling)



	/** Formats this table in the form it should appear inside a ''from'' clause.
	  * This will return either the [[net.noresttherein.oldsql.schema.RelVar.name name]] of
	  * for named tables - [[net.noresttherein.oldsql.schema.Table proper tables]],
	  * [[net.noresttherein.oldsql.schema.View views]] and
	  * ''[[net.noresttherein.oldsql.sql.CommonTableExpression common table expressions]]'', or a completely formatted
	  * SQL ''select'' for a [[net.noresttherein.oldsql.schema.Table.TableExpression TableExpression]].
	  *
	  * The spelling should ''not'' include any alias (i.e., an ''as'' clause following the table name).
	  *
	  * @param from     a ''from'' clause prefix of the formatted SQL ''select'' which ends with this table.
	  * @param context  additional spelling information required for formatting the whole SQL select, in particular
	  *                 the list of aliases used by all listed tables.
	  * @param params   a factory of `P => X` accessor functions, where `P` is the intended parameter(s) type
	  *                 of the query being built, and `X` is an unbound parameter introduced by
	  *                 a [[net.noresttherein.oldsql.sql.ParamClause ParamClause]].
	  * @param spelling a formatting strategy used for all fragments of the returned SQL, responsible in particular
	  *                 for implementing any deviations from the SQL standard demanded by the DBMS being used.
	  */
	protected def defaultSpelling[P, F <: RowProduct]
	                             (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                             (implicit spelling :SQLSpelling) :SpelledSQL[P]

	private[oldsql] def defaultSpelling[P, F <: RowProduct]
	                                   (spelling :SQLSpelling)
	                                   (from :F, context :SQLContext[P], params :Parameterization[P, F])
			:SpelledSQL[P] =
		defaultSpelling(from, context, params)(spelling)


/*
	/** Spells all columns of `component` expression default
	  * to the spelling [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]].
	  * @param component A component expression from a [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]] for
	  *                  a table with the same nominal row mapping. `component.origin.relation` might be this instance,
	  *                  one sharing the same [[net.noresttherein.oldsql.schema.Relation.default default]] version
	  *                  of the relation (i.e, the same nominal mapping) with this table representing the same
	  *                  database table, or possibly an unrelated instance using a compatible row mapping type.
	  */
	protected def explodedSpellingOf[P, F <: RowProduct, C[A] <: MappingAt[A], T[A] >: M[A] <: MappingAt[A]]
	                                (component :ComponentSQL[F, C] { type Entity[O] = T[O] }, independent :Boolean)
	                                (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
	component.origin match {
		case table :JoinedTable[component.Origin, component.Entity] =>
			component.export match {
				case column :TypedColumn[_, component.Origin @unchecked] =>
					val columnSpelling =
						if (table.table == this)
							spelling.column(table, column)(from, context, params)
						else if (table.index == 0 && component.isDefault) {
							val counterpart = export[RowProduct AndFrom M].counterpart(table.mapping, column)
							spelling.column(this.asLast, counterpart)(from, context, params)
						} else {
							val actualTable = table.anchor(this)
							val counterpart = table.anchored.counterpart(table.mapping, column)
							spelling.column(actualTable, counterpart)(from, context, params)
						}
					ReversedList :+ columnSpelling
				case mapping =>
					//fixme: one way or another, this must annul 'NonXxxByDefault' buffs on actual
					val altered = table.anchor(this)
					//fixme: this doesn't attempt finding a counterpart of the component if the relations are different
					val counterpart = altered.anchored.counterpart(table.mapping, mapping)
					val columns = spelling.scope.defaultColumns(altered.anchored, counterpart)
					columns.view.scanLeft(SpelledSQL(context)) {
						(sql, col) => spelling.column(altered, col)(from, sql.context, params)
					}.tail.to(ArraySeq)
			}
		case origin =>
			throw new IllegalArgumentException(
				"Cannot spell the expression for component " + component + " of " + origin.relation +
				" as its origin is not a JoinedTable: " + origin + " :" + origin.getClass.getName + "."
			)
	}

	protected[oldsql] final def explodedSpellingOf[P, F <: RowProduct, C[A] <: MappingAt[A], T[A] >: M[A] <: MappingAt[A]]
	                            (spelling :SQLSpelling)
	                            (component :ComponentSQL[F, C] { type Entity[O] = T[O] }, independent :Boolean = false)
	                            (from :F, context :SQLContext[P], params :Parameterization[P, F]) :Seq[SpelledSQL[P]] =
		explodedSpellingOf(component, independent)(from, context, params)(spelling)
*/


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Table.__ @unchecked]
}




//todo: TableMapping (Mapping with Table) - requires the mapping type in Relation to be a member type, not a type parameter
//todo: JoinedTables: a pseudo table based on a FromClause
object Table {
	def apply[M <: Mapping, S](tableName :String, template : => M)
	                          (implicit project :OriginProjection[M, S]) :BaseTable[project.WithOrigin] =
		BaseTable[M, S](tableName, template)

	def apply[M[O] <: MappingAt[O]]
	         (tableName :String)
	         (implicit template :M[Unit], //todo: lazy implicit
	                   project :OriginProjection[M[Unit], M[Unit]#Subject] { type WithOrigin[O] <: M[O] })
			:BaseTable[M] =
		BaseTable[M[Unit], M[Unit]#Subject](tableName, template)

	def apply[N <: Label, M[O] <: MappingAt[O]]
	         (implicit tableName :ValueOf[N], template :M[Unit], //todo: lazy implicit
	                   projection :OriginProjection[M[Unit], M[Unit]#Subject] { type WithOrigin[O] <: M[O] })
			:StaticBaseTable[N, M] =
		BaseTable[N, M]

	def apply[N <: Label :ValueOf] :StaticTableFactory[N] = BaseTable[N]

	def of[S] :TableFactoryForEntity[S] = BaseTable.of[S]

	def delay[M[O] <: MappingAt[O]](table : => Table[M]) :Table[M] = new LazyTable(table)



	/** A table with a static (literal constant) name, typically (but not necessarily) representing a permanent element
	  * of a table schema. It is a [[net.noresttherein.oldsql.schema.Relation.NamedRelation NamedRelation]]
	  * with its name displayed as `String` literal type parameter `N`, which allows it to be more easily identified
	  * or referred to. It is extended by implicit relation values in a schema, which can be then summoned
	  * by their names, in particular when resolving foreign key targets. Additionally, joining such a table
	  * automatically adds an [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause to the join,
	  * which allows retrieval of the table from it or any expanding ''from'' clause. Any `Table` can be converted
	  * to a `StaticTable` with extension methods [[net.noresttherein.oldsql.schema.Table.TableExtension.as as]]`(alias)`
	  * and [[net.noresttherein.oldsql.schema.Table.TableExtension.alias alias]]`[N]`. Instances of this type
	  * are typically also instances of [[net.noresttherein.oldsql.schema.RelVar RelVar]].
	  * @see [[net.noresttherein.oldsql.schema.Table.Aliased]]
	  */
	trait StaticTable[N <: String with Singleton, +M[O] <: MappingAt[O]] extends Table[M] with StaticRelation[N, M] {
		override def name :N
	}

	type __ = Table[M] forSome { type M[O] <: MappingAt[O] }




	/** Extension methods of [[net.noresttherein.oldsql.schema.Table Table]], extracted due to its covariance
	  * in the mapping parameter.
	  */ //todo: after the Relation factor, make these methods of Table
	implicit class TableExtension[M[O] <: MappingAt[O]](private val table :Table[M]) extends AnyVal {
		/** Assigns an alias to this table. If the returned table is used as an argument
		  * to [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] method (or equivalent for other
		  * join types), an [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause will be automatically added
		  * to the join. This allows type safe access to this table in the containing ''from'' clause
		  * as an SQL [[net.noresttherein.oldsql.sql.ast.JoinedTable expression]]
		  * for this table by the given alias. It also becomes the default alias used for the returned table when it is
		  * listed in a ''from'' clause; the name however is only a suggestion and can be overriden by SQL formatter.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations]]
		  */
		@inline def alias[A <: Label :ValueOf] :M Aliased A = Aliased[M, A](table, valueOf[A])

		/** Assigns an alias to this table. If the returned table is used as an argument
		  * to [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] method (or equivalent for other
		  * join types), an [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause will be automatically added
		  * to the join. This allows type safe access to this table in the containing ''from'' clause
		  * as an SQL [[net.noresttherein.oldsql.sql.ast.JoinedTable expression]]
		  * for this table by the given alias. It also becomes the default alias used for the returned table when it is
		  * listed in a ''from'' clause; the name however is only a suggestion and can be overriden by SQL formatter.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations]]
		  */
		@inline def as[A <: Label](alias :A) :M Aliased A = Aliased[M, A](table, alias)

		/** An SQL expression for the entity mapped by this table when it is the last table listed by a ''from'' clause.
		  * This method allows the creation of a `JoinedTable` even in contexts where `M` is not statically bound
		  * by [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] and is slightly preferable to
		  * `TableSQL.LastTable(this)` as it caches the result.
		  * @return [[net.noresttherein.oldsql.sql.ast.TableSQL.LastTable TableSQL.LastTable]]`(this)`.
		  */
		@inline def joined :JoinedTable[RowProduct AndFrom M, M] =
			table.expr.asInstanceOf[JoinedTable[RowProduct AndFrom M, M]]

		/** Creates a ''from'' clause containing this table.
		  * This method is slightly preferable to `From(this)` as it caches the result.
		  * @return [[net.noresttherein.oldsql.sql.From From]]`(this)`.
		  */ //if we rename withOrigin in OriginProjection to from, then this will clash in
		@inline def from :From[M] = table.fromClause.asInstanceOf[From[M]]
	}

	/** Extension methods of [[net.noresttherein.oldsql.schema.Table Table]]`[M]`
	  * available if `M[O] <: `[[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[_, O]`.
	  */
	class TableExtraExtension[S, M[O] <: BaseMapping[S, O]](private val table :Table[M]) extends AnyVal {
		/** A relation expression referring to this table, usable as the last expression in any
		  * [[net.noresttherein.oldsql.sql.FromClause FromClause]]. This method is slightly preferable
		  * to [[net.noresttherein.oldsql.sql.ast.TableSQL.LastTable TableSQL.LastTable]]`(this)`
		  * as it caches the result.
		  * @return [[net.noresttherein.oldsql.sql.ast.TableSQL.LastTable TableSQL.LastTable]]`(this)`.
		  */
		@inline def toSQL :TableSQL[RowProduct AndFrom M, M, S] =
			table.expr.toTableSQL.asInstanceOf[TableSQL[RowProduct AndFrom M, M, S]]
	}

	@inline implicit def TableExtraExtension[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                                        (table :Table[M])(implicit reveal :MappingSubject[M, T, S])
			:TableExtraExtension[S, T] =
		new TableExtraExtension[S, T](reveal(table))





	/** Supertype of all non-persistent (at least semantically) SQL relations.
	  * This includes [[net.noresttherein.oldsql.schema.View views]] and
	  * [[net.noresttherein.oldsql.schema.Table.TableExpression selects]] used in ''from'' and ''with'' clauses.
	  */
	trait DerivedTable[+M[O] <: MappingAt[O]] extends Table[M] with RelationTemplate[M, DerivedTable[M]] {

		protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
				:DerivedTable[M] =
			if (includes.isEmpty && excludes.isEmpty)
				this
			else
				new AlteredRelation[M, DerivedTable[M]](this, includes, excludes)
					with DerivedTable[M] with AlteredRelationOverrides[M, DerivedTable[M]]
				{
					override def defaultSpelling[P, F <: RowProduct]
					                            (from :F, context :SQLContext[P], params :Parameterization[P, F])
					                            (implicit spelling :SQLSpelling) =
						default.defaultSpelling(spelling)(from, context, params)
				}
	}


	object DerivedTable {
		type __ = DerivedTable[M] forSome { type M[O] <: MappingAt[O] }
	}



	/* Consider: how to allow parameterized queries Query[P, X] as tables?
	 * One idea is to add type ParamsClause to Relation itself, and not base TableExpression on a Query,
	 * but actually on a QuerySQL[_ <: ParamsClause, M]. BaseTable would naturally have an empty clause,
	 * while TableExpression and CommonTableExpression would inherit the type from from QuerySQL.
	 * This type would have to be concrete for a Table to be joined into a clause; when joining a table with params,
	 * all its parameters would be recursively joined before the table expression itself. Seems doable, and doesn't
	 * mess much with the current design, because it would be implemented largely on top of it.
	 * Alternatively (as in either a different solution, or an alternative option for client code),
	 * we would allow a join with F if table.ParamsClause PrefixOf F, so we can map those parameters to query parameters.
	 * This is more challenging; at the very list a TableExpression joined this way would have to be somehow
	 * marked/adapted, and we'd need to carry information about 'global clause parameters' in SQLContext.
	 *
	 * A very interesting side effect of the above design is that it could work automatically for ParamRelation
	 * without special handling of the join. We could also easily have a ParamsRelation with multiple parameters.
	 *
	 * Another idea would involve adding a Mapping#Params[P <: Chain] type, always abstract, appending required parameters
	 * to the param list P. Then:
	 *     type Paramless[M[O] <: MappingAt[O]] = M[O] { type Params[P <: Chain] = P }
	 *     type WithParam[M[O] <: MappingAt[O], X] = { type T[O] = M[O] { type Params[P <: Chain] = P ~ X } }
	 *     type UnboundParam[S, O] <: TypedMapping[S, O] { type Params[P <: Chain] = P ~ S }
	 *     JoinParam.Last[O] = JoinedParam[O, (M WithParam P)#T], or just use
	 *     _ JoinParam (M WithParam X)#T and _ JoinParam UnboundParam.Of[X]#M
	 *     Adjoin[L, R]#Params = R[_]#Params[L#Params].
	 * The problem here is that we would always have to use Paramless[M] instead of M, which is a high price to pay,
	 * or would need 4 extra join types - ouch.
	 * For this however we almost certainly would need Adjoin's to be covariant in the mapping type,
	 * which leads to all sort of problems with truncated expressions (and benefits).
	 */
	/** A (parameterless) SQL ''select'' adapted for use as a table in a ''from'' clause.
	  * It is not an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]]; for that, see
	  * [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]] wrapping this class.
	  */ //todo: move to package sql
	trait TableExpression[+M[O] <: MappingAt[O]]
		extends DerivedTable[M] with RelationTemplate[M, TableExpression[M]]
	{ outer =>
		type Row
		val query :QuerySQL[RowProduct, Row] { type RowMapping[O] <: M[O] }

		override def apply[O]  :M[O]         = query.mapping[O]
		override def export[O] :MappingAt[O] = query.export[O]

		override def withClause :WithClause = query.outerWithClause

		protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
				:TableExpression[M] =
			if (includes.isEmpty && excludes.isEmpty)
				this
			else
				new AlteredRelation[M, TableExpression[M]](this, includes, excludes)
					with TableExpression[M] with AlteredRelationOverrides[M, TableExpression[M]]
				{
					override type Row = default.Row
					override val query = default.query
				}

		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = spelling.sqlParamCount(expr)

		protected override def spell[P, F <: O, O <: RowProduct, T[A] >: M[A] <: MappingAt[A], S]
		                            (table :JoinedTable[O, T], column :TypedColumn[S, O])
		                            (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                            (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			if (table.table == this)
				spelling.column(table, table.anchored.export(column))(from, context, params)
			else {
				val actualTable = table.anchor(this)
				val counterpart = actualTable.anchored.counterpart(table.mapping, column)
				spelling.column(actualTable, counterpart)(from, context, params)
			}

		protected override def defaultSpelling[P, F <: RowProduct]
		                                      (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                      (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		{
			val select = spelling(query)(from, context, params)
			val sql =  "(" +: (select.sql + ")")
			SpelledSQL(sql, select.setter, select.context)
		}

		override def identical(that :Relation[MappingAt]) :Boolean = that match {
			case _ if this eq that => true
			case other :TableExpression[MappingAt] if canEqual(that) && that.canEqual(this) =>
				query identical other.query
			case _ => false
		}
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :TableExpression[MappingAt] if canEqual(that) && other.canEqual(this) =>
				query == other.query
			case _ => false
		}
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TableExpression[M @unchecked]]
		override def hashCode :Int = query.hashCode

		override def refString :String = "select[" + query.expr.typeString + "]"
		override def toString  :String = queryString
		private lazy val queryString = query.toString
	}


	object TableExpression {
		def apply[M[O] <: MappingAt[O], V](select :QuerySQL[RowProduct, V] { type RowMapping[O] = M[O] })
				:TableExpression[M] =
			new TableExpression[M] {
				override type Row = V
				override val query = select
			}

		type __ = TableExpression[M] forSome { type M[O] <: MappingAt[O] }
	}



	/** A `Relation[M]` wrapper annotating it with an alias `A` (a string literal type). The alias becomes
	  * the [[net.noresttherein.oldsql.schema.Relation.NamedRelation.name name]] of the relation and is also
	  * used as the 'name' type parameter to [[net.noresttherein.oldsql.schema.Relation.StaticRelation StaticRelation]]
	  * supertype; this means that when creating queries, joining such a table will automatically
	  * add an [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause to the returned join:
	  * {{{
	  *     val hamsters = Familiars as "hamsters"
	  *     val query = From(Rangers) as "rangers" join hamsters where {
	  *         row => row("rangers").familiar === row("hamsters").name
	  *     }
	  *     query :From[Rangers] As "rangers" Join Hamsters As "hamsters"
	  * }}}
	  * The added `As` clause identifies the mapping within the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]
	  * and can be used to retrieve the relation by the alias in a type safe manner.
	  */
	trait Aliased[+M[O] <: MappingAt[O], A <: Label] extends StaticTable[A, M] with RelationTemplate[M, M Aliased A] {
		val table :Table[M]
		val alias :A
		override def apply[O]  :M[O]         = table[O]
		override def export[O] :MappingAt[O] = table.export[O]
		override def default   :Relation[M]  = table.default
		override def name      :A            = alias

		protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
				:M Aliased A =
			if (includes.isEmpty && excludes.isEmpty) this
			else Aliased(table.`->alter`(includes, excludes), alias)

		protected override def defaultSpelling[P, F <: RowProduct]
		                                      (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                      (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			table.defaultSpelling(spelling)(from, context, params)

		protected override def defaultSpellingOf[P, F <: O, O <: RowProduct, T[B] >: M[B] <: MappingAt[B], V]
		                                        (origin :JoinedTable[O, T], column :TypedColumn[V, O])
		                                        (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                        (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			table.defaultSpellingOf(origin, column)(from, context, params)


		override def identical(that :Relation[MappingAt]) :Boolean = that match {
			case _ if this eq that => true
			case aliased :Aliased[M @unchecked, _] if canEqual(aliased) && (aliased canEqual this) =>
				alias == aliased.alias && (table identical aliased.table)
			case _ => false
		}
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case aliased :Aliased[M @unchecked, _] if canEqual(aliased) && (aliased canEqual this) =>
				aliased.alias == alias && aliased.table == table
			case _ => false
		}
		override def hashCode :Int = table.hashCode * 31 + alias.hashCode

		override def toString :String = table.toString + " as " + alias
	}



	object Aliased {
		def apply[M[O] <: MappingAt[O], A <: Label](table :Table[M], alias :A) :Aliased[M, A] =
			new AliasedTable(table, alias)

		def unapply[M[O] <: MappingAt[O]](relation :Relation[M]) :Opt[(Table[MappingAt], Label)] =
			relation match {
				case aliased :Aliased[M, _] => Got(aliased.table, aliased.alias)
				case _ => Lack
			}


		type __ = Aliased[M, _ <: Label] forSome { type M[O] <: MappingAt[O] }

		private class AliasedTable[M[O] <: MappingAt[O], A <: Label](val table :Table[M], val alias :A)
			extends Aliased[M, A]
	}



	class LazyTable[+M[O] <: MappingAt[O]](init : => Table[M]) extends Table[M] {
		lazy val underlying :Table[M] = init

		override def apply[O]  :M[O]         = underlying[O]
		override def export[O] :MappingAt[O] = underlying.export[O]
		override def default   :Relation[M]  = underlying.default

		protected override def defaultSpelling[P, F <: RowProduct]
		                                      (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                      (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			underlying.defaultSpelling(from, context, params)

		override def refString :String = underlying.refString
		override def toString  :String = underlying.toString
	}


	private[oldsql] lazy val Dummy = Table("Dummy", new ConstantMapping["Dummy", "Dummy"]("Dummy"))

}




/** A named, persistent relation in the schema. This includes [[net.noresttherein.oldsql.schema.View views]]
  * and [[net.noresttherein.oldsql.schema.BaseTable tables]].
  */
trait RelVar[+M[O] <: MappingAt[O]] extends Table[M] with NamedRelation[M] with RelationTemplate[M, RelVar[M]] {

	protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
			:RelVar[M] =
		if (includes.isEmpty && excludes.isEmpty)
			this
		else
			new AlteredRelation[M, RelVar[M]](this, includes, excludes)
				with RelVar[M] with AlteredRelationOverrides[M, RelVar[M]]
			{
				override val name = default.name
			}

	protected override def defaultSpelling[P, F <: RowProduct]
	                                      (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                      (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		SpelledSQL(context.nameOf(this) getOrElse name, context)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[RelVar[MappingAt]]

}


object RelVar {
	type __ = RelVar[M] forSome { type M[O] <: MappingAt[O] }
}




/** A 'true', persistent and updatable SQL table. */
trait BaseTable[+M[O] <: MappingAt[O]] extends RelVar[M] with RelationTemplate[M, BaseTable[M]] {
	protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
			:BaseTable[M] =
		if (includes.isEmpty && excludes.isEmpty)
			this
		else
			new AlteredRelation[M, BaseTable[M]](this, includes, excludes)
				with BaseTable[M] with AlteredRelationOverrides[M, BaseTable[M]]
			{
				override val name = default.name
			}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[BaseTable[M @unchecked]]
}


object BaseTable {

	def apply[M <: Mapping, S](tableName :String, template : => M)
	                          (implicit project :OriginProjection[M, S]) :BaseTable[project.WithOrigin] =
		new ProjectingRelation[project.WithOrigin, S](project[Unit](template))(project.isomorphism)
			with BaseTable[project.WithOrigin]
		{
			override val name = tableName
		}

	def apply[M[O] <: MappingAt[O]]
	         (tableName :String)
	         (implicit project :OriginProjection[M[Unit], M[Unit]#Subject] { type WithOrigin[O] <: M[O] },
	                   template : => M[Unit])
			:BaseTable[M] =
		BaseTable[M[Unit], M[Unit]#Subject](tableName, template)

	def apply[N <: Label, M[O] <: MappingAt[O]]
	         (implicit project :OriginProjection[M[Unit], M[Unit]#Subject] { type WithOrigin[O] <: M[O] },
	                   tableName :ValueOf[N], mapping :M[Unit]) //todo: lazy implicit
			:StaticBaseTable[N, M] =
		static[N, M[Unit], M[Unit]#Subject](tableName.value, mapping)(project)

	def static[N <: Label, M <: Mapping, S](tableName :N, mapping: => M)(implicit project :OriginProjection[M, S])
			:StaticBaseTable[N, project.WithOrigin] =
		new ProjectingRelation[project.WithOrigin, S](project[Unit](mapping))(project.isomorphism)
			with StaticBaseTable[N, project.WithOrigin]
		{
			override val name = tableName
		}

	def apply[N <: Label :ValueOf] :StaticTableFactory[N] = new StaticTableFactory(valueOf[N])

	class StaticTableFactory[N <: Label] private[BaseTable] (private val tableName :N) extends AnyVal {
		def apply[M <: Mapping, S](mapping: => M)(implicit project :OriginProjection[M, S])
				:StaticBaseTable[N, project.WithOrigin] = 
			static[N, M, S](tableName, mapping)
	}

	def of[S] :TableFactoryForEntity[S] = new TableFactoryForEntity[S] {}

	sealed trait TableFactoryForEntity[S] extends Any { //fixme: lazy implicit
		final def apply[M <: MappingOf[S]](tableName :String)(implicit mapping :M, project :OriginProjection[M, S])
				:BaseTable[project.WithOrigin] =
			BaseTable(tableName, mapping)
	}

	/** A database table marked on type level with a `String` literal type `N` with the name of the table.
	  * It behaves exactly as its supertype `BaseTable`, but when used as an argument to various join (extension)
	  * methods of [[net.noresttherein.oldsql.sql.RowProduct RowProduct]], it automatically adds
	  * an [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause propagating the identifier, allowing
	  * the retrieval of this table by its name from the whole ''from'' clause object.
	  */
	trait StaticBaseTable[N <: String with Singleton, +M[O] <: MappingAt[O]] extends BaseTable[M] with StaticTable[N, M]

	type __ = BaseTable[M] forSome { type M[O] <: MappingAt[O] }
}




/** An SQL view. Treated in the same way as [[net.noresttherein.oldsql.schema.BaseTable tables]]. */
trait View[+M[O] <: MappingAt[O]] extends DerivedTable[M] with RelVar[M] with RelationTemplate[M, View[M]] {
	protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
			:View[M] =
		new AlteredRelation[M, View[M]](this, includes, excludes)
			with View[M] with AlteredRelationOverrides[M, View[M]]
		{
			override val name = default.name
		}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[View[M @unchecked]]
}


object View {

	def apply[M <: Mapping, S](tableName :String, template : => M)
	                          (implicit project :OriginProjection[M, S]) :View[project.WithOrigin] =
		new ProjectingRelation[project.WithOrigin, S](project[Unit](template))(project.isomorphism)
			with View[project.WithOrigin]
		{
			override val name = tableName
		}

	def apply[M[O] <: MappingAt[O]]
	         (tableName :String)
	         (implicit project :OriginProjection[M[Unit], M[Unit]#Subject] { type WithOrigin[O] <: M[O] },
	                   template : => M[Unit]) :View[M] =
		View[M[Unit], M[Unit]#Subject](tableName, template)


	def apply[N <: Label, M[A] <: MappingAt[A]]
	         (implicit project :OriginProjection[M[Unit], M[Unit]#Subject] { type WithOrigin[O] <: M[O] },
	                   tableName :ValueOf[N], mapping :M[Unit]) //todo: lazy implicit
			:StaticView[N, M] =
		static[N, M[Unit], M[Unit]#Subject](tableName.value, mapping)(project)

	def static[N <: Label, M <: Mapping, S](tableName :N, mapping: => M)(implicit project :OriginProjection[M, S])
			:StaticView[N, project.WithOrigin] =
		new ProjectingRelation[project.WithOrigin, S](project[Unit](mapping))(project.isomorphism)
			with StaticView[N, project.WithOrigin]
		{
			override val name = tableName
		}

	def apply[N <: Label :ValueOf] :StaticViewFactory[N] = new StaticViewFactory(valueOf[N])

	class StaticViewFactory[N <: Label] private[View] (private val tableName :N) extends AnyVal {
		def apply[M <: Mapping, S](mapping: => M)(implicit project :OriginProjection[M, S])
				:StaticView[N, project.WithOrigin] = 
			static[N, M, S](tableName, mapping)
	}

	def of[S] :ViewFactoryForEntity[S] = new ViewFactoryForEntity[S] {}

	sealed trait ViewFactoryForEntity[S] extends Any { //fixme: lazy implicit
		final def apply[M <: MappingOf[S]](tableName :String)(implicit mapping :M, project :OriginProjection[M, S])
				:View[project.WithOrigin] =
			View(tableName, mapping)
	}


	trait StaticView[N <: String with Singleton, +M[O] <: MappingAt[O]] extends View[M] with StaticTable[N, M]

	type __ = View[M] forSome { type M[O] <: MappingAt[O] }
}





