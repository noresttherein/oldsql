package net.noresttherein.oldsql.schema

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.{Opt, Unique}
import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.Relation.{AlteredRelation, RelationTemplate}
import net.noresttherein.oldsql.schema.Relation.BaseTable.TableFactory
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.ConstantMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.@:.Labeled
import net.noresttherein.oldsql.schema.support.AdjustedMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter.Adapted
import net.noresttherein.oldsql.sql.{Adjoin, AndFrom, From, JoinedRelation, RowProduct}
import net.noresttherein.oldsql.sql.Adjoin.JoinedRelationSubject
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.ast.{JoinedTable, QuerySQL, TableSQL}
import net.noresttherein.oldsql.sql.ast.SQLLiteral.{False, True}
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.MappingSubject
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL
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
  * same mapping `M` as its 'interface' mapping, but an [[net.noresttherein.oldsql.schema.Relation.export altered]]
  * instance as the ''effective'' mapping, which should be used when accessing the components of `M` for assembly
  * or in order to determine the column set which should be included in a database operation.
  * @see [[net.noresttherein.oldsql.schema.Relation.Table Table]]
  * @see [[net.noresttherein.oldsql.schema.Relation.BaseTable BaseTable]]
  * @author Marcin Mościcki
  */
trait Relation[+M[O] <: MappingAt[O]] extends AbstractRelation with RelationTemplate[M, Relation] {
	//we can't have OriginProjection here as M[O] is not a BaseMapping[S, O] and it can't be because it's used in Joins
	/** The default view of the underlying SQL relation, that is an instance with cleared 'include' and 'exclude'
	  * component lists.
	  */
	def default :Relation[M] = this

	/** Checks if the given relation and this relation represent the same SQL object, that is are (possibly different)
	  * views of the same relation.
	  * @return `default == that.default`
	  */
	def sameAs(that :Relation.*) :Boolean = default == that.default


	protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
			:Relation[M] =
		new AlteredRelation[M, Relation](this, includes, excludes)


	/** Creates an SQL fragment referencing the given component of a relation expression for this relation.
	  *   1. For a table column (including ''derived'' tables, i.e. ''select'' expressions), this translates to
	  *      the form of `alias.columnName`, while other components print the above form for all their columns picked by
	  *      `spelling.scope.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.defaultColumns defaultColumns]];
	  *      whether these columns are surrounded by a parenthesis in order to form a tuple, or inlined for use
	  *      in a larger expression, is decided by `inline` flag.
	  *   1. [[net.noresttherein.oldsql.sql.UnboundParam.ParamRelation Unbound]] parameters insert the JDBC statement
	  *      parameter placeholder '?' (or multiple, as with regular components) and append a setter form for
	  *      the parameter to the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization Parameterization]]
	  *      returned with the SQL.
	  *   1. [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation Grouping]] relations always copy the whole
	  *      expression as appearing in the ''group by'' clause, together with repetitions of any bound or unbound
	  *      parameters included in the expression.
	  * Default implementation delegates to the overloaded variant for a single column and simply collects the results.
	  * Non standard implementations may resort to other schemes.
	  * @param origin a relation expression which `relation` property must equal this instance.
	  * @param component a component of the [[net.noresttherein.oldsql.schema.Relation.export export]] mapping
	  *                  of this relation.
	  */
	@throws[IllegalArgumentException]("if origin does not reference this relation.")
	def spell[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]]
	         (origin :JoinedRelation[O, T], component :MappingAt[O], inline :Boolean = false)
	         (context :SQLContext, params :Parameterization[P, F])
	         (implicit spelling :SQLSpelling) :SpelledSQL[P, F] =
	{
		val columns = this.inline(origin, component)(context, params)
		if (columns.isEmpty)
			SpelledSQL(context, params)
		else if (columns.sizeIs <= 1)
			columns.head
		else if (inline)
			("(" +: columns.reduce(_.sql +: ", " +: _)) + ")"
		else
			columns.reduce(_.sql +: ", " +: _)
	}

	/** Spells each column of `component` individually, returning them as a sequence.
	  * The [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext contexts]] and
	  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization parameterizations]]
	  * of all spelled columns form an additive sequence, with those from every item building upon the values
	  * from the preceding column. Thus, the final element in the sequence contains the values appropriate to
	  * the concatenation of all elements, with no combining necessary apart from the concatenation of the textual
	  * parts.
	  *
	  * This method is used as implementation target of method [[net.noresttherein.oldsql.schema.Relation.spell spell]]
	  * and in circumstances where a custom concatenation strategy is required (for example, as part of
	  * an SQL ''update's'' ''set'' clause). Note however than any SQL injected as a conjunction must be free of
	  * any bound or unbound parameters - it cannot modify `SQLContext` or `Parameterization` instances of its parts.
	  */
	@throws[IllegalArgumentException]("if origin does not reference this relation.")
	def inline[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]](origin :JoinedRelation[O, T], component :MappingAt[O])
	                                                            (context :SQLContext, params :Parameterization[P, F])
	                                                            (implicit spelling :SQLSpelling)
			:Seq[SpelledSQL[P, F]] =
	{
		if (origin.relation != this)
			throw new IllegalArgumentException(
				s"Cannot spell the expression for component $component of $origin as it is for a different relation than $this."
			)
		val columns = spelling.scope.defaultColumns(export[O])
		columns.view.scanLeft(SpelledSQL(context, params)) {
			(sql, col) => spell(origin, export[O].export(col))(sql.context, sql.params)
		}.tail.to(ArraySeq)
	}

	/** Creates an SQL fragment referencing the given column of a relation expression for this relation.
	  *   1. For a table column (including ''derived'' tables, i.e. ''select'' expressions), this translates to
	  *      the form of `alias.columnName`.
	  *   1. [[net.noresttherein.oldsql.sql.UnboundParam.ParamRelation Unbound]] parameters insert the JDBC statement
	  *      parameter placeholder '?' and append a setter form for the parameter
	  *      to the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization Parameterization]]
	  *      returned with the SQL.
	  *   1. [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation Grouping]] relations always copy the whole
	  *      expression as appearing in the ''group by'' clause, together with repetitions of any bound or unbound
	  *      parameters included in the expression.
	  * @param origin a relation expression which `relation` property must equal this instance.
	  * @param column a column of the [[net.noresttherein.oldsql.schema.Relation.export export]] mapping
	  *               of this relation.
	  */
	protected def spell[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A], V]
	                   (origin :JoinedRelation[O, T], column :ColumnMapping[V, O])
	                   (context :SQLContext, params :Parameterization[P, F])
	                   (implicit spelling :SQLSpelling) :SpelledSQL[P, F]


	def canEqual(that :Any) :Boolean = that.isInstanceOf[Relation.* @unchecked]

	//todo: chunked
	def refString :String = export.mappingName
	override def toString :String = export.mappingName
}






//consider: how me might preserve equality after serialization
object Relation {

	type * = Relation[MappingAt]


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
		this :RelationTemplate[M, R] =>

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

//		def where :M[RowProduct AndFrom M] = row[RowProduct AndFrom M]

		/** The ''effective'' mapping of this relation, which is a view on mapping `M` with certain optional
		  * components/columns excluded (or included). The returned mapping will recognise the components of the mapping
		  * returned by [[net.noresttherein.oldsql.schema.Relation.apply apply]] as its (potentially non-export) components.
		  * Similarly to `this[O]`, returned mapping will always be the same instance, or one using the same, shared
		  * component set. For default views of a relation, this method returns simply `this[O]`.
		  */ //todo: in Scala 3 make the return type have the same subject type by using member types
		def export[O] :MappingAt[O]// = apply[O]//Adapted[M[O]]


		def apply(components :M[this.type] => ComponentSelection[_, this.type]*) :R[M] = {
			val self = row[this.type]; val alt = export[this.type]
			val excludes = components.view.map { _(self) }.collect {
				case ExcludedComponent(c) => alt.export(c)
			}.to(Unique)
			val includes = components.view.map { _(self) }.collect {
				case IncludedComponent(c) => alt.export(c)
			}.filterNot(excludes.contains).to(Unique)
			alter(includes, excludes)
		}

		def include(components :M[this.type] => RefinedMapping[_, this.type]*) :R[M] = {
			val self = row[this.type]; val alt = export[this.type]
			val comps = components.view.map(_(self)).map(alt.export(_)).to(Unique)
			alter(comps, Unique.empty)
		}

		def exclude(components :M[this.type] => RefinedMapping[_, this.type]*) :R[M] = {
			val self = row[this.type]; val alt = export[this.type]
			val comps = components.view.map(_(self)).map(alt.export(_)).to(Unique)
			alter(Unique.empty, comps)
		}

		def +(component :M[this.type] => RefinedMapping[_, this.type]) :R[M] =
			alter(Unique.single(export[this.type].export(component(row[this.type]))), Unique.empty)

		def -(component :M[this.type] => RefinedMapping[_, this.type]) :R[M] =
			alter(Unique.empty, Unique.single(export[this.type].export(component(row[this.type]))))

		protected def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]]) :R[M]

		private[schema] def alterForwarder(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:R[M] =
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
	private abstract class ProjectingRelation[+M[O] <: BaseMapping[S, O], S]
	                                         (prototype: => M[Unit])(implicit projection :IsomorphicProjection[M, S, Unit])
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
		def name :String
		override def refString :String = name
		override def toString :String = name
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
	  * @see [[net.noresttherein.oldsql.schema.Relation.RelVar]]
	  * @see [[net.noresttherein.oldsql.schema.Relation.DerivedTable]]
	  */
	trait Table[+M[O] <: MappingAt[O]] extends Relation[M] with RelationTemplate[M, Table] {

		private[Table] val expr = TableSQL.LastTable[MappingOf[Any]#TypedProjection, Any](
			this.asInstanceOf[Table[MappingOf[Any]#TypedProjection]]
		)
		private[Table] val fromClause = From[MappingOf[Any]#TypedProjection, Any, Label](expr, None, True)

		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:Table[M] =
			new AlteredRelation[M, Table](this, includes, excludes) with Table[M] {
				override def defaultSpelling[P, F <: RowProduct]
				                            (context :SQLContext, params :Parameterization[P, F])
				                            (implicit spelling :SQLSpelling) =
					default.defaultSpelling(spelling)(context, params)
			}

		/** Formats the relation for use in a ''from'' clause. For example,
		  * a [[net.noresttherein.oldsql.schema.Relation.BaseTable table]] will simply return its name,
		  * while a [[net.noresttherein.oldsql.schema.Relation.SelectRelation select]] will format its whole SQL.
		  * This is the default spelling used by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
		  */
		protected def defaultSpelling[P, F <: RowProduct]
		                             (context :SQLContext, params :Parameterization[P, F])
		                             (implicit spelling :SQLSpelling) :SpelledSQL[P, F]

		private[oldsql] def defaultSpelling[P, F <: RowProduct]
		                                   (spelling :SQLSpelling)
		                                   (context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
			defaultSpelling(context, params)(spelling)

		protected override def spell[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A], V]
		                            (origin :JoinedRelation[O, T], column :ColumnMapping[V, O])
		                            (context :SQLContext, params :Parameterization[P, F])
		                            (implicit spelling :SQLSpelling) :SpelledSQL[P, F] =
			spelling.column(origin, export[O].export(column))(context, params)
	}


	object Table {
		def apply[M <: Mapping, S](tableName :String, template : => M)
		                          (implicit project :OriginProjection[M, S]) :BaseTable[project.WithOrigin] =
			BaseTable[M, S](tableName, template)

		def apply[S] :TableFactory[S] = BaseTable[S]

		trait StaticTable[N <: String with Singleton, M[O] <: MappingAt[O]] extends Table[M] with StaticRelation[N, M] {
			override def name :N
		}

		type * = Table[M] forSome { type M[O] <: MappingAt[O] }


		implicit class TableExtension[M[O] <: MappingAt[O]](private val table :Table[M]) extends AnyVal {
			@inline def asLast :JoinedTable[RowProduct AndFrom M, M] =
				table.expr.asInstanceOf[JoinedTable[RowProduct AndFrom M, M]]

			@inline def from :From[M] = table.expr.asInstanceOf[From[M]]
		}

		class TableExtraExtension[S, M[O] <: BaseMapping[S, O]](private val table :Table[M]) extends AnyVal {
			@inline def toSQL :TableSQL[RowProduct AndFrom M, M, S, RowProduct AndFrom M] =
				table.expr.toTableSQL.asInstanceOf[TableSQL[RowProduct AndFrom M, M, S, RowProduct AndFrom M]]
		}

		@inline implicit def TableExtraExtension[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                                        (table :Table[M])(implicit reveal :MappingSubject[M, T, S])
				:TableExtraExtension[S, T] =
			new TableExtraExtension[S, T](reveal(table))
	}




	/** A named, persistent relation in the schema. This includes [[net.noresttherein.oldsql.schema.Relation.View views]]
	  * and [[net.noresttherein.oldsql.schema.Relation.BaseTable tables]].
	  */
	trait RelVar[+M[O] <: MappingAt[O]] extends Table[M] with NamedRelation[M] with RelationTemplate[M, RelVar] {

		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:RelVar[M] =
			new AlteredRelation[M, RelVar](this, includes, excludes) with RelVar[M] {
				override val name = default.name
				override def toString = alteredString
			}

		protected override def defaultSpelling[P, F <: RowProduct]
		                                      (context :SQLContext, params :Parameterization[P, F])
		                                      (implicit spelling :SQLSpelling) :SpelledSQL[P, F] =
			SpelledSQL(name, context, params)
	}


	object RelVar {
		type * = RelVar[M] forSome { type M[O] <: MappingAt[O] }
	}




	/** A 'true', persistent and updatable SQL table. */
	trait BaseTable[+M[O] <: MappingAt[O]] extends Table[M] with RelVar[M] with RelationTemplate[M, BaseTable] {
		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:BaseTable[M] =
			new AlteredRelation[M, BaseTable](this, includes, excludes) with BaseTable[M] {
				override val name = default.name
				override def toString = alteredString
			}
	}


	object BaseTable {

		def apply[M <: Mapping, S](tableName :String, template : => M)
		                          (implicit project :OriginProjection[M, S]) :BaseTable[project.WithOrigin] =
			new ProjectingRelation[project.WithOrigin, S](project[Unit](template))(project.isomorphism)
				with BaseTable[project.WithOrigin]
			{
				override val name = tableName
			}

		def apply[S] :TableFactory[S] = new TableFactory[S] {}

		trait TableFactory[S] extends Any { //fixme: lazy implicit
			final def apply[M <: Mapping](tableName :String)(implicit mapping :M, project :OriginProjection[M, S])
					:BaseTable[project.WithOrigin] =
				BaseTable(tableName, mapping)
		}


		trait StaticBaseTable[N <: String with Singleton, M[O] <: MappingAt[O]]
			extends BaseTable[M] with StaticRelation[N, M]

		type * = BaseTable[M] forSome { type M[O] <: MappingAt[O] }
	}




	/** Supertype of all non-persistent (at least semantically) SQL relations.
	  * This includes [[net.noresttherein.oldsql.schema.Relation.View views]] and
	  * [[net.noresttherein.oldsql.schema.Relation.SelectRelation selects]] used in ''from'' and ''with'' clauses.
	  */
	trait DerivedTable[+M[O] <: MappingAt[O]] extends Table[M] with RelationTemplate[M, DerivedTable] {

		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:DerivedTable[M] =
			new AlteredRelation[M, DerivedTable](this, includes, excludes) with DerivedTable[M] {
				override def defaultSpelling[P, F <: RowProduct]
				                            (context :SQLContext, params :Parameterization[P, F])
				                            (implicit spelling :SQLSpelling) =
					default.defaultSpelling(spelling)(context, params)
			}
	}


	object DerivedTable {
		type * = DerivedTable[M] forSome { type M[O] <: MappingAt[O] }
	}




	/** An SQL view. Treated in the same way as [[net.noresttherein.oldsql.schema.Relation.BaseTable tables]]. */
	trait View[+M[O] <: MappingAt[O]] extends DerivedTable[M] with RelVar[M] with RelationTemplate[M, View] {
		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:View[M] =
			new AlteredRelation[M, View](this, includes, excludes) with View[M] {
				override val name = default.name
			}
	}


	object View {

		def apply[M <: Mapping, S](tableName :String, template : => M)
		                          (implicit project :OriginProjection[M, S]) :View[project.WithOrigin] =
			new ProjectingRelation[project.WithOrigin, S](project[Unit](template))(project.isomorphism)
				with View[project.WithOrigin]
			{
				override val name = tableName
			}

		def apply[S] :ViewFactory[S] = new ViewFactory[S] {}

		trait ViewFactory[S] extends Any { //fixme: lazy implicit
			final def apply[M <: Mapping](tableName :String)(implicit mapping :M, project :OriginProjection[M, S])
					:View[project.WithOrigin] =
				View(tableName, mapping)
		}

		trait StaticView[N <: String with Singleton, M[O] <: MappingAt[O]] extends Table[M] with StaticRelation[N, M] {
			override def name :N
		}

		type * = View[M] forSome { type M[O] <: MappingAt[O] }

	}




	trait SelectRelation[M[O] <: MappingAt[O]] extends DerivedTable[M] with RelationTemplate[M, SelectRelation] { outer =>
		type Row
		val query :QuerySQL[RowProduct, Row] { type ResultMapping[O] = M[O] }

		override def apply[O] :M[O] = query.mapping[O]
		override def export[O] :MappingAt[O] = query.export[O]

		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:SelectRelation[M] =
			new AlteredRelation[M, SelectRelation](this, includes, excludes) with SelectRelation[M] {
				override type Row = default.Row
//				override val default = outer //stabilizer for Row type
				override val query = default.query
				override def export[O] = super[AlteredRelation].export[O]
				override lazy val toString :String = alteredString
			}

		protected override def defaultSpelling[P, F <: RowProduct]
		                                      (context :SQLContext, params :Parameterization[P, F])
		                                      (implicit spelling :SQLSpelling) :SpelledSQL[P, F] =
		{
			val sql =  "(" +: (spelling.paramless(query)(context) + ")")
			val boundParams = sql.params.settersReversed.map(_.unmap { _ :P => @~ })
			val totalParams = params.reset(boundParams ::: params.settersReversed)
			SpelledSQL(sql.sql, context, totalParams)
		}

		override def refString :String = "select[" + query.mapping.mappingName + "]"
		override lazy val toString :String = query.toString
	}


	object SelectRelation {
		def apply[M[O] <: MappingAt[O], V](select :QuerySQL[RowProduct, V] { type ResultMapping[O] = M[O] })
				:SelectRelation[M] =
			new SelectRelation[M] {
				override type Row = V
				override val query = select
			}

		type * = SelectRelation[M] forSome { type M[O] <: MappingAt[O] }
	}




	/** A view on a relation `R[M]` which modifies the row mapping `M` by including and/or excluding
	  * certain optional components.
	  */
	class AlteredRelation[M[O] <: MappingAt[O], R[T[O] <: MappingAt[O]] <: Relation[T] with RelationTemplate[T, R]]
	                     (override val default :R[M],
	                      val includes :Unique[RefinedMapping[_, _]], val excludes :Unique[RefinedMapping[_, _]])
		extends Relation[M]
	{
		private val mapping = {
			val template = default.export[this.type]
			AdjustedMapping[RefinedMapping[template.Subject, this.type], template.Subject, this.type](
				template.refine,
				includes.asInstanceOf[Unique[RefinedMapping[_, this.type]]],
				excludes.asInstanceOf[Unique[RefinedMapping[_, this.type]]]
			)
		}

		override def row[O] :M[O] = default[O]
		override def apply[O] :M[O] = default[O]
		override def export[O] :Adapted[M[O]] = mapping.asInstanceOf[Adapted[M[O]]]

		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:R[M] =
			default.alterForwarder(
				(this.includes.view ++ includes).filterNot(excludes.contains).to(Unique),
				(this.excludes.view.filterNot(includes.contains) ++ excludes).to(Unique)
			)

		override def spell[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]]
		                  (origin :JoinedRelation[O, T], component :MappingAt[O], inline :Boolean)
		                  (context :SQLContext, params :Parameterization[P, F])
		                  (implicit spelling :SQLSpelling) :SpelledSQL[P, F] =
			default.spell(origin, mapping.withOrigin[O].unexport(component.refine))(context, params)

		//todo: verify this calls the column overload
		protected override def spell[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A], V]
		                            (origin :JoinedRelation[O, T], column :ColumnMapping[V, O])
		                            (context :SQLContext, params :Parameterization[P, F])
		                            (implicit spelling :SQLSpelling) :SpelledSQL[P, F] =
			default.spell(origin, mapping.withOrigin[O].unexport(column))(context, params)

		override def inline[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]]
		                   (origin :JoinedRelation[O, T], component :MappingAt[O])
		                   (context :SQLContext, params :Parameterization[P, F])
		                   (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, F]] =
			default.inline(origin, mapping.withOrigin[O].unexport(component.refine))(context, params)


		override def refString :String = default.refString

		protected lazy val alteredString :String =
			(includes.view.map("+" + _) ++ excludes.view.map("-" + _)).mkString(s"($default){", ", ", "}")

		override def toString :String = alteredString
	}



	/** A `Relation[M]` wrapper annotating it with an alias `L` (a string literal type).
	  * The mapping of the relation is adapted to a [[net.noresttherein.oldsql.schema.bits.LabeledMapping LabeledMapping]]
	  * `L @: M`. It exposes the labeled type as a single-argument type constructor `this.T[O]`
	  * accepting the `Origin` type for use in `RowProduct` subclasses and other types accepting such a type constructor:
	  * `Dual Join (Humans Aliased "humans")#T` (where `Humans[O] <: MappingAt[O]`).
	  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping.@:]]
	  */
	class Aliased[M[O] <: MappingAt[O], A <: Label](val relation :Relation[M], val alias :A)
		extends StaticRelation[A, Labeled[A, M]#Projection]
	{
		type T[O] = A @: M[O]

		override def apply[O] :A @: M[O] = labeled.asInstanceOf[A @: M[O]]
		override def export[O] = relation.export[O]

		override def name :A = alias

		override def spell[P, O <: RowProduct, F <: O, T[X] <: MappingAt[X]]
		                  (origin :JoinedRelation[O, T], component :MappingAt[O], inline :Boolean)
		                  (context :SQLContext, params :Parameterization[P, F])
		                  (implicit spelling :SQLSpelling) :SpelledSQL[P, F] =
		{
			val exported = if (component == labeled) relation.export[O] else component
			relation.spell(origin, exported)(context, params)
		}

		protected override def spell[P, O <: RowProduct, F <: O, T[X] <: MappingAt[X], V]
		                            (origin :JoinedRelation[O, T], column :ColumnMapping[V, O])
		                            (context :SQLContext, params :Parameterization[P, F])
		                            (implicit spelling :SQLSpelling) :SpelledSQL[P, F] =
		{
			val exported = if (column == labeled) relation.export[O].asInstanceOf[ColumnMapping[_, O]] else column
			relation.spell(origin, exported)(context, params)
		}

		override def inline[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]]
		                   (origin :JoinedRelation[O, T], component :MappingAt[O])
		                   (context :SQLContext, params :Parameterization[P, F])
		                   (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, F]] =
		{
			val exported = if (component == labeled) relation.export[O] else component
			relation.inline(origin, exported)(context, params)
		}


		private val labeled = alias @: relation[Unit].refine
	}


	object Aliased {
		def apply[M[O] <: MappingAt[O], A <: Label](relation :Relation[M], alias :A) :Aliased[M, A] =
			new Aliased(relation, alias)

		def apply[M[O] <: MappingAt[O], A <: Label](table :Table[M], alias :A) :AliasedTable[M, A] =
			new AliasedTable(table, alias)

		def unapply[M[O] <: MappingAt[O]](relation :Relation[M]) :Opt[(Relation[MappingAt], Label)] =
			relation match {
				case aliased :Aliased.* => Got(aliased.relation, aliased.alias)
				case _ => Lack
			}


		type * = Aliased[M, _ <: Label] forSome { type M[O] <: MappingAt[O] }

		class AliasedTable[M[O] <: MappingAt[O], A <: Label](override val relation :Table[M], override val alias :A)
			extends Aliased[M, A](relation, alias) with Table[Labeled[A, M]#Projection]
		{
			def table :Table[M] = relation

			protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
					:AliasedTable[M, A] =
				new AliasedTable[M, A](relation.alterForwarder(includes, excludes), alias)

			protected override def defaultSpelling[P, F <: RowProduct]
			                                      (context :SQLContext, params :Parameterization[P, F])
			                                      (implicit spelling :SQLSpelling) :SpelledSQL[P, F] =
				relation.defaultSpelling(spelling)(context, params)

			override lazy val toString :String = relation.toString + " as " + alias
		}
	}



	private[oldsql] val Dummy = Table("Dummy", new ConstantMapping["Dummy", "Dummy"]("Dummy"))

}

