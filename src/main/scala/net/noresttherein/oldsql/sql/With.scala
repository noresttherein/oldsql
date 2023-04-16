package net.noresttherein.oldsql.sql

import scala.collection.mutable.{ArrayBuffer, Builder, ReusableBuilder}
import scala.collection.{Factory, IterableOps, mutable}
import scala.util.hashing.MurmurHash3

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.{RelVar, Relation, Table}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.Relation.{AlteredRelation, AlteredRelationOverrides, NamedRelation, RelationTemplate}
import net.noresttherein.oldsql.schema.Table.{Aliased, StaticTable, TableExpression}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.slang.{classNameMethods, repeat, saferCasting}
import net.noresttherein.oldsql.sql.CommonTableExpression.{AnonymousName, CommonTableAlias, MappingOfQuery}
import net.noresttherein.oldsql.sql.WithClause.{CompleteWithClause, DerivedWithClauseBuilder, DerivedWithClauseClosureBuilder, LocalWithClauseClosureBuilder}
import net.noresttherein.oldsql.sql.ast.{MappingQuerySQL, QuerySQL}





//trait With[F <: RowProduct]
//trait AndWith[]

/** A factory of SQL queries and statements with a ''with'' clause.
  * The factory methods accept various queries which should be placed in the ''with'' clause,
  * followed by constructor functions accepting
  * [[net.noresttherein.oldsql.sql.CommonTableExpression CommonTableExpression]] tables wrapping those queries
  * and returning the final SQL statement. The tables passed as arguments to the function can be used in joins
  * like any static tables from the schema.
  * {{{
  *     //a rather redundant syntax example
  *     With(From(Hamsters).selectLast) (From(_).selectLast)
  * }}}
  * Several ''common table expressions'' can be introduced either by nesting calls to `With` factory methods,
  * or using Scala's ''for comprehensions'':
  * {{{
  *
  * }}}
  *
  */
object With {
	//todo: ParamTables
	def apply[A <: Label](alias :A) = new CTEName(alias)

	//todo: a varargs macro for apply(A, B, ...)(f :(A, B, ...) => X)
	def apply[Q <: QuerySQL[RowProduct, _]](query :Q)(implicit meta :MappingOfQuery[Q]) :WithQuerySQL[Q, meta.Row] =
		new WithQuerySQL(query)

	class WithQuerySQL[Q <: QuerySQL[RowProduct, _], M[O] <: MappingAt[O]] private[With] (private[With] val query :Q)
		extends AnyVal
	{
		def apply[R](f :Table[M] => R)(implicit meta :MappingOfQuery[Q] { type Row[O] = M[O] }) :R = //todo: remove the cast when Query will preserve the subject type
			f(meta.commonTableExpression(query))
//
//		def as[A <: Label](alias :A) :WithGenerator[StaticTable[A, M]] =
//			//because the extension method for query doesn't currently preserve the result type H
//			new WithGenerator(CommonTableAlias(alias, query).asInstanceOf[
//				StaticTable[A, MappingOf[H]#TypedProjection]]
//			)
	}
//
//	class WithGenerator[+T] private[With](private val table :T) extends AnyVal {
//		def apply[Q](f :T => Q)   :Q = f(table)
//		def flatMap[Q](f :T => Q) :Q = f(table)
//		def map[Q](f :T => Q)     :Q = f(table)
//	}



	//todo: a shared syntax import including these and everything else.
//	  * An implicit conversion is present in [[net.noresttherein.oldsql.sql.ast.QuerySQL$ QuerySQL]].
	/** Extension method `as` for `String` literals creating a named query
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression.CommonTableAlias CommonTableAlias]].
	  */
	implicit class CTEName[N <: Label](private val name :N) extends AnyVal {
		def as[Q <: QuerySQL[RowProduct, _]](query: => Q)(implicit meta :MappingOfQuery[Q])
				:CommonTableAlias[N, meta.Row] =
			CommonTableAlias(name :N, query)
	}


	implicit def cteOfName[N <: Label, M[O] <: MappingAt[O]](cteAlias :N)
	                                                        (implicit cte :CommonTableExpression[M]) :cte.type = cte
}





//todo: common table expressions for subselects (with clauses of subselects depending on outer selects)

/** A ''common table expression'' is a named, temporary view local to a query.
  * In SQL, they are placed within a ''with'' clause preceding a query or statement using syntax of
  * {{{
  *     WITH <name> as select ...[, <name> as select ...]* select ... from <name> ...
  * }}}
  * They are not unlike normal table expressions, that is queries located directly in a ''from'' clause,
  * but they are referenced by name just like persistent tables, and can also be reused multiple times within
  * the following query. Additionally, most databases support recursive and mutually recursive queries.
  *
  * In order to use a named table expression within your [[net.noresttherein.oldsql.sql.FromClause FromClause]],
  * you do not have to declare any ''with'' clause explicitly: simply create an instance in one of the following ways:
  * {{{
  *     val cte1 = "monsters" as (From(Dragons) selectLast (_.name)) //a StaticTable["monsters", MappingOf[String]#Projection]
  *     val cte2 = With(From(Companions) where (_.name ==? "Boo") select *) //an anonymous table
  * }}}
  * They can be later used as arguments to [[net.noresttherein.oldsql.sql.From From]]
  * and [[net.noresttherein.oldsql.sql.Join joins]] just as any other table, and a ''with'' clause containing
  * the definitions of all such table expressions used by the query, regardless if directly in a ''from'' clause,
  * or within a dependent select inside another clause, will be prepended to the final query automatically.
  */ //could extend Table.Aliased
trait CommonTableExpression[+M[O] <: MappingAt[O]]
	extends TableExpression[M] with NamedRelation[M] with RelationTemplate[M, CommonTableExpression[M]]
{
	override def default :CommonTableExpression[M] = this

	/** Aliases this expression, substituting its current [[net.noresttherein.oldsql.schema.RelVar.name name]]
	  * with the given label.
	  */
	def as[A <: Label](alias :A) :CommonTableAlias[A, M] =
		CommonTableAlias(alias :A, query)//(CommonTableExpression.mappingOfQuery)

	/** If `false`, the name/alias of this instance can be changed when incorporated into
	  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] which already contains another
	  * common table expression with the same name.
	  * @return `false`, unless constructed without specifying an explicit name.
	  */
	def isFinal :Boolean = !isAnonymous

	/** Anonymous common table expressions weren't provided a name at their construction and need assigning
	  * a unique one during spelling.
	  */
	def isAnonymous :Boolean = name == AnonymousName

	/** All `CommonTableExpression` instances directly or indirectly referenced by
	  * `this.`[[net.noresttherein.oldsql.sql.CommonTableExpression.query query]]
	  * together with the [[net.noresttherein.oldsql.sql.CommonTableExpression.default default]] (unaltered) version
	  * of this common table expression.
	  */
	override def withClause :WithClause = query.outerWithClause + default

	/** A table expression is recursive ''iff'' its [[net.noresttherein.oldsql.sql.CommonTableExpression.query query]]
	  * references, directly or indirectly, this table expression itself (or more specifically, its
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression.default unaltered]] version). It is in particular true
	  * for mutually recursive queries. This property deals only with this query itself,
	  * and referencing another, recursive, common table expression is not sufficient for it to be `true`.
	  * @return `this.query.`[[net.noresttherein.oldsql.sql.ast.QuerySQL.withClause withClause]]` contains this.default`.
	  * @see [[net.noresttherein.oldsql.sql.CommonTableExpression.isMutuallyRecursive]]
	  */
	def isRecursive :Boolean = query.outerWithClause contains default

	/** A table expression is mutually recursive ''iff'' it references a common table expression other than itself,
	  * which in turn references the former. In other words, two common table expressions `t1` and `t2`
	  * are mutually recursive, ''iff''
	  * `t1.`[[net.noresttherein.oldsql.sql.CommonTableExpression.query query]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.withClause withClause]]`.contains(t2)`
	  * and `t2.query.withClause.contains(t1)`. Note that [[net.noresttherein.oldsql.sql.WithClause]]`.contains`
	  * yields `true` if it contains the [[net.noresttherein.oldsql.sql.CommonTableExpression.default default]] version
	  * of the argument, meaning that [[net.noresttherein.oldsql.sql.CommonTableExpression.alter altered]] relations
	  * are considered same for the purpose of this definition.
	  * @see [[net.noresttherein.oldsql.sql.CommonTableExpression.isRecursive]]
	  */
	def isMutuallyRecursive :Boolean = query.outerWithClause.exists(_.query.outerWithClause.contains(default))

	/** A table expression is self recursive ''iff'' it references itself directly by its name.
	  * @return [[net.noresttherein.oldsql.sql.CommonTableExpression.isRecursive isRecursive]]` && `
	  *         `!`[[net.noresttherein.oldsql.sql.CommonTableExpression.isMutuallyRecursive isMutuallyRecursive]].
	  */
	def isSelfRecursive :Boolean = isRecursive && !isMutuallyRecursive

	protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
			:CommonTableExpression[M] =
		if (includes.isEmpty && excludes.isEmpty)
			this
		else
			new AlteredRelation[M, CommonTableExpression[M]](this, includes, excludes)
				with CommonTableExpression[M] with AlteredRelationOverrides[M, CommonTableExpression[M]]
			{
				override val default     = CommonTableExpression.this
				override val query       = default.query
				override def withClause  = default.withClause
				override def name        = default.name
				override def isFinal     = default.isFinal
				override def isAnonymous = default.isAnonymous //not final methods, so let's delegate
				override type Row        = default.Row

				override def as[A <: Label](alias :A) :CommonTableAlias[A, M] =
					default.as[A](alias).`->alter`(
						this.includes.asInstanceOf[Unique[TypedMapping[_, O]]],
						this.excludes.asInstanceOf[Unique[TypedMapping[_, O]]]
					)
			}

	override def identical(that :Relation[MappingAt]) :Boolean = that match {
		case _ if this eq that => true
		case other :CommonTableExpression[MappingAt] if canEqual(that) && that.canEqual(this) =>
			name == other.name && (query identical other.query)
		case _ => false
	}
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :CommonTableExpression[MappingAt] if canEqual(that) && other.canEqual(this) =>
			name == other.name && (query == other.query)
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[CommonTableExpression.__]
	override def hashCode :Int = name.hashCode * 31 + query.hashCode

	override def refString :String = name
	override def toString  :String = refString + " as " + super[TableExpression].toString
}




private[sql] sealed abstract class CommonTableExpressionFactory {
	//type Dummy makes sure that there is no subtyping with other implicit definitions
	implicit def mappingOfBasicQuery[V]
			:MappingOfQuery[QuerySQL[RowProduct, V]] { type Row[O] = TypedMapping[V, O]; type Dummy = 0 } =
		mappingOfBasicQueryPrototype.castFrom[
			MappingOfQuery[QuerySQL[RowProduct, Any]],
			MappingOfQuery[QuerySQL[RowProduct, V]] { type Row[O] = TypedMapping[V, O]; type Dummy = 0}
		]
	protected val mappingOfBasicQueryPrototype :MappingOfQuery[QuerySQL[RowProduct, Any]]
}


object CommonTableExpression extends CommonTableExpressionFactory {
	def apply[Q <: QuerySQL[RowProduct, _]]
	         (alias :String, query: => Q)(implicit meta :MappingOfQuery[Q]) :CommonTableExpression[meta.Row] =
		meta.commonTableExpression(alias, query)

	def apply[Q <: QuerySQL[RowProduct, _]]
	         (select: => Q)(implicit meta :MappingOfQuery[Q]) :CommonTableExpression[meta.Row] =
		meta.commonTableExpression(select)

	def apply[A <: Label](alias :A) :WithTableAsFactory[A] = new WithTableAsFactory(alias)

	class WithTableAsFactory[A <: Label] private[CommonTableExpression](private val alias :A) extends AnyVal {
		def as[Q <: QuerySQL[RowProduct, _]]
		      (query : => Q)(implicit meta :MappingOfQuery[Q]) :CommonTableAlias[A, meta.Row] =
			meta.commonTableAlias(alias, query)
	}


	trait MappingOfQuery[-Q] extends Serializable {
		type Row[O] <: MappingAt[O]

		def commonTableExpression(query: => Q) :CommonTableExpression[Row]
		def commonTableExpression(alias :String, query: => Q) :CommonTableExpression[Row]
		def commonTableAlias[A <: Label](alias :A, query: => Q) :CommonTableAlias[A, Row]
	}

	implicit def specificMappingOfQuery[M[O] <: MappingAt[O], V]
			:MappingOfQuery[QuerySQL[RowProduct, V] { type RowMapping[O] <: M[O] }] { type Row[O] = M[O]; type Dummy = 1 } =
		specificMappingOfQueryPrototype.castFrom[
			MappingOfQuery[QuerySQL[RowProduct, Any] { type RowMapping[O] <: TypedMapping[Any, O] }],
			MappingOfQuery[QuerySQL[RowProduct, V] { type RowMapping[O] <: M[O] }] { type Row[O] = M[O]; type Dummy = 1 }
		]

	private val specificMappingOfQueryPrototype
			:MappingOfQuery[QuerySQL[RowProduct, Any] { type RowMapping[O] <: TypedMapping[Any, O] }] =
	{
		type M[O] = TypedMapping[Any, O]
		type V = Any
		new MappingOfQuery[QuerySQL[RowProduct, V] { type RowMapping[O] <: M[O] }] {
			override type Row[O] = M[O]

			override def commonTableExpression(query: => QuerySQL[RowProduct, V] { type RowMapping[O] <: M[O] }) =
				meta[M, V](query)

			override def commonTableExpression(alias :String,
			                                   query: => QuerySQL[RowProduct, V] { type RowMapping[O] <: M[O] }) =
				new BaseCommonTableExpression[M, V](alias, query)

			override def commonTableAlias[A <: Label]
			                             (alias :A, query: => QuerySQL[RowProduct, V] { type RowMapping[O] <: M[O] }) =
				meta[A, Row, V](alias, query)
		}
	}
	protected override val mappingOfBasicQueryPrototype :MappingOfQuery[QuerySQL[RowProduct, Any]] = {
		type V = Any
		new MappingOfQuery[QuerySQL[RowProduct, V]] {
			override type Row[O] = TypedMapping[V, O]
			override def commonTableExpression(query: => QuerySQL[RowProduct, V]) =
				meta[Row, V](query.asInstanceOf[QuerySQL[RowProduct, V] { type RowMapping[O] = TypedMapping[V, O] }])

			override def commonTableExpression(alias :String, query: => QuerySQL[RowProduct, V]) =
				new BaseCommonTableExpression[MappingOf[V]#Projection, V](
					alias, query.asInstanceOf[QuerySQL[RowProduct, V] { type RowMapping[O] = TypedMapping[V, O] }]
				)

			override def commonTableAlias[A <: Label](alias :A, query: => QuerySQL[RowProduct, V]) =
				meta[A, Row, V](alias, query.asInstanceOf[QuerySQL[RowProduct, V] { type RowMapping[O] = TypedMapping[V, O] }])
		}
	}

	private def meta[M[O] <: MappingAt[O], V]
	                (query : => QuerySQL[RowProduct, V] { type RowMapping[O] <: M[O] }) :CommonTableExpression[M] =
		new BaseCommonTableExpression[M, V](AnonymousName, query) {
			override val name = AnonymousName + "@" + this.shortHashString
			override def isAnonymous = true
			override def isFinal = false
		}

	private def meta[A <: Label, M[O] <: MappingAt[O], V]
	                (alias :A, query: => QuerySQL[RowProduct, V] { type RowMapping[O] <: M[O] })
			:CommonTableAlias[A, M] =
		new BaseCommonTableExpression[M, V](alias, query) with CommonTableAlias[A, M] {
			override val name = alias
		}


	type __ = CommonTableExpression[MappingAt]

	trait CommonTableAlias[A <: Label, +M[O] <: MappingAt[O]]
		extends StaticTable[A, M] with CommonTableExpression[M] with RelationTemplate[M, CommonTableAlias[A, M]]
	{
//		/** Enables to create a 'with clause cache' tuple-like object containing aliased common table expressions
//		  * with syntax `With("alias1") as query1 andWith (alias2 as query2) ... */
//		def andWith[A2 <: Label, M2[O] <: MappingAt[O]](cte :CommonTableAlias[A2, M2]) = ???

		override def isAnonymous = false

		protected override def alter[O](includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
				:CommonTableAlias[A, M] =
			if (includes.isEmpty && excludes.isEmpty)
				this
			else
				new AlteredRelation[M, CommonTableAlias[A, M]](this, includes, excludes)
					with CommonTableAlias[A, M] with AlteredRelationOverrides[M, CommonTableAlias[A, M]]
				{
					override val default    = CommonTableAlias.this
					override val query      = default.query
					override def name       = default.name
					override def withClause = default.withClause
					override def isFinal    = default.isFinal
					override type Row       = default.Row

					override def as[N <: Label](alias :N) :CommonTableAlias[N, M] =
						default.as[N](alias).`->alter`(
							this.includes.asInstanceOf[Unique[TypedMapping[_, O]]],
							this.excludes.asInstanceOf[Unique[TypedMapping[_, O]]]
						)
				}

		override def identical(that :Relation[MappingAt]) :Boolean = super[CommonTableExpression].identical(that)
		override def equals(that :Any) :Boolean = super[CommonTableExpression].equals(that)
		override def canEqual(that :Any) :Boolean = super[CommonTableExpression].canEqual(that)
		override def hashCode :Int = super[CommonTableExpression].hashCode
	}


	object CommonTableAlias {
		def apply[A <: Label, Q <: QuerySQL[RowProduct, _]]
		         (alias :A, query: => Q)(implicit meta :MappingOfQuery[Q]) :CommonTableAlias[A, meta.Row] =
			meta.commonTableAlias(alias, query)

		type __ = CommonTableAlias[_ <: Label, MappingAt]
	}


	private class BaseCommonTableExpression[+M[O] <: MappingAt[O], V]
	              (override val name :String, select: => QuerySQL[RowProduct, V] { type RowMapping[O] <: M[O] })
		extends CommonTableExpression[M]
	{
		override type Row = V
		override lazy val query = select
		private  lazy val inner = query.outerWithClause
		override lazy val withClause :WithClause = this +: inner
		override def isRecursive = inner.contains(default)
		override def isMutuallyRecursive = inner.exists(_.query.outerWithClause.contains(default))
	}

	private final val AnonymousName = "cte"
}






/** A collection of [[net.noresttherein.oldsql.sql.CommonTableExpression common table expressions]],
  * that is named queries serving as temporary tables local to a query, and freely usable within it.
  * While it can be and is used to represent an actual SQL ''with'' clause of a query, it is more abstract than that,
  * and is commonly used to mark a dependency of any [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]],
  * or any other syntactical element such as a ''from'' clause, on such temporary tables. Any class which is used
  * directly or indirectly as a part of a [[net.noresttherein.oldsql.sql.Query Query]]
  * or [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]] defines a `withClause` property listing all
  * `CommonTableExpression` instances referenced within it. These are built recursively by performing unions
  * of such 'virtual with clauses' of their subexpressions.
  *
  * While a ''with'' clause for a query can be defined explicitly using the [[net.noresttherein.oldsql.sql.With With]]
  * factory object, this is not necessary in most cases. A `CommonTableExpression` can be created on the spot,
  * using its companion object, [[net.noresttherein.oldsql.sql.ast.QuerySQL.TopQuerySQLExtension.as as]] extension
  * method of a [[net.noresttherein.oldsql.sql.ast.QuerySQL.GroundQuery GroundQuery]], or
  * [[net.noresttherein.oldsql.sql.CommonTableExpression.as as]] extension method of `String` literals, accepting
  * a lazily evaluated query expression. These can be used in a [[net.noresttherein.oldsql.sql.FromClause FromClause]]
  * the same way as any persistent tables in the schema, and this use is tracked in the manner described above.
  * When formatting the final SQL statement from an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]],
  * an appropriate ''with'' clause is generated automatically. By default, all tables are included in a single
  * ''with'' clause prepended to the whole statement, but this is at the discretion
  * of the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] strategy used.
  *
  * This behaviour can be however influenced by the application by explicitly declaring certain table expressions
  * as ''local'' to a dependent select, or any `WithClause` associated with any `SQLExpression`, which is taken to mean
  * 'local to the directly encompassing query/statement'. This will result in prepending a ''with'' clause
  * to a dependent select, although it also can be influenced by the spelling strategy. This can happen either
  * by the use of the `With` factory or extension methods of various ''from'' clauses accepting individual tables,
  * or by manually building a `WithClause` instance for an expression.
  *
  * Any `WithClause` can thus contain such explicit, [[net.noresttherein.oldsql.sql.WithClause.local local]]
  * declarations, and implicit dependencies. The latter are referred to
  * as [[net.noresttherein.oldsql.sql.WithClause.outer outer]] ''with'' clauses, as they contain tables whose
  * definitions must happen in some outer expression for a particular subexpression to form valid SQL.
  * An outer clause of any `WithClause` omits the local declarations from its parent. However,
  * as the information about their presence should be available to `SQLSpelling`, they are instead moved
  * to the outer clauses [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] clause, which collects
  * all tables declared as local to a subexpression of the expression associated with the clause. These tables
  * are ''not'' elements of this collection, but a separate collection tagged along with this one.
  *
  * A union of two ''with'' clauses combines all these three collections pair separately: a local table in any
  * of the summands becomes a local table in the result, and so on. Local and outer subsets must be disjoint,
  * so if the same table was present as a local declaration in one collection and an outer one in another,
  * it always becomes a part of the local subset of the result. Similarly, a presence of a table in either local
  * or outer subsets will result in it being omitted from the suppressed clause, at least when using the provided
  * builders and class methods to construct new collections.
  *
  * To complicate matters further, when adding a table expression to a collection in the use case of recursive
  * building of an 'outer' clause, one typically wants to include also
  * its whole [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]] (which is assumed to
  * contain both the table expression in question, and any table expressions referenced in its definition.
  * In order to follow the principle of the least surprise, the default builder and `Iterable` add/concat methods
  * add only the specified tables, without their dependencies; this is also important when filtering a `WithClause`
  * and in other cases where the contract is defined, or at least assumed, by the standard collection interface.
  * However, in order to address the need of a more fine grained control and preserving the contract declared
  * by the [[net.noresttherein.oldsql.sql.SQLExpression.outerWithClause outerWithClause]] property of `SQLExpression`,
  * additional builders and factories are provided by the companion object, which force the assignment of every
  * table to either the local or outer part, or to follow all transitive dependencies and build
  * a [[net.noresttherein.oldsql.sql.WithClause.closure closure]] of the table set given. This is on top of several
  * non-standard collection methods defined by this interface itself,
  * such as [[net.noresttherein.oldsql.sql.WithClause.local_+ local_+]]
  * and [[net.noresttherein.oldsql.sql.WithClause.local_++ local_++]], which add all tables to the local subset
  * of the result, or [[net.noresttherein.oldsql.sql.WithClause.+* +*]]
  * and [[net.noresttherein.oldsql.sql.WithClause.++* ++*]], which include all tables listed by
  * `withClause` properties of the passed new elements.
  *
  * Equality of this type is thus defined as equality of the above pair of disjoint subsets, which together cover
  * the whole collection. While the order of added table definitions is preserved, it is not taken into account
  * when testing for equality. A presence and contents of a suppressed clause is completely ignored in that context.
  */
sealed trait WithClause
	extends Iterable[CommonTableExpression.__] with IterableOps[CommonTableExpression.__, Iterable, WithClause]
{
	/** Members of this ''with'' clause which were explicitly declared as local to this clause and its associated query,
	  * rather than potentially being shared with any expression containing the latter as a subexpression.
	  * The user expressed an intent for these temporary tables to be defined in the SQL within a ''with'' clause
	  * directly preceding the ''select'', rather than being extracted to a common top-level clause.
	  * Were any of these tables used outside of this scope, they will be rendered as separate definitions.
	  * The returned clause will, most likely, be not closed with regard
	  * to the [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]] property
	  * of contained [[net.noresttherein.oldsql.sql.CommonTableExpression common table expressions]].
	  * Its [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]]
	  * related collection will be the same as in this instance.
	  * @return a ''with'' clause identical to this one, but with an empty
	  *         [[net.noresttherein.oldsql.sql.WithClause.outer outer]] property.
	  * @see [[net.noresttherein.oldsql.sql.WithClause.outer]]
	  * @see [[net.noresttherein.oldsql.sql.WithClause.local_+]]
	  * @see [[net.noresttherein.oldsql.sql.WithClause.local_++]]
	  */ //consider: maybe don't require the presence of suppressed?
	def local :WithClause = WithClause.empty

	/** Members of this collection which do not belong to the ''with'' clause local to the directly associated (sub)query.
	  * This excludes all tables present in the [[net.noresttherein.oldsql.sql.WithClause.local local]] subset
	  * of this instance. The remaining tables are undeclared and their definitions need to be placed
	  * in a ''with'' clause of some enclosing query, typically preceding the whole formatted statement.
	  * The returned instance will have an empty `local` subset, but all tables in `this.local` will be moved
	  * to its [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] collection instead, together
	  * with `this.suppressed`.
	  *
	  * No table in the `outer` clause should depend on any table in the `local` clause (that is, include it in its
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]]).
	  *
	  * All individually added tables become a part of this subset, unless explicitly specified otherwise
	  * through use of a dedicated method.
	  * @return a clause equivalent to `this -- local`.
	  */
	def outer :WithClause = this -- local

	/** Computes the closure of this ''with'' clause, that is a `WithClause` containing all tables from this instance,
	  * as well as all tables reachable through their
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]] properties (that is all
	  * common table expressions among the latter as well as the closures of their ''with'' clauses).
	  *
	  * The [[net.noresttherein.oldsql.sql.WithClause.local local]] subset of the returned instance will be empty,
	  * regardless of whether any of the instance contributing tables contains 'local' elements or not.
	  * Instead, all tables become part of the [[net.noresttherein.oldsql.sql.WithClause.outer outer]] part
	  * of the result. The [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] collections
	  * are combined however as normally.
	  */
	def closure :WithClause = (closureBuilder /: this)(_ += _).result()

	/** A ''with'' clause containing all common table expressions which are used in subexpressions
	  * of the [[net.noresttherein.oldsql.sql.SQLExpression expression]]
	  * [[net.noresttherein.oldsql.sql.SQLExpression.outerWithClause associated]] with this clause (or another class
	  * representing some SQL syntax element), but which were declared
	  * as [[net.noresttherein.oldsql.sql.WithClause.local local]] by a ''with'' clause of an enclosed subexpression.
	  * Said `WithClause` instance contributed to creating this object by being combined with clauses for other
	  * subexpressions of the associated expression. These tables are ''not'' elements of this collection,
	  * as is the case with `local` and [[net.noresttherein.oldsql.sql.WithClause.outer outer]] ''with'' clause parts,
	  * and is typically disjoint with it, although this is not guaranteed.
	  */ //consider: renaming to declared/nested/inner/excluded
	def suppressed :WithClause

	/** All common table expressions used within the associated expression, including those nominally declared
	  * in nested ''with'' clauses. It is a superset of this collection containing all elements
	  * of `this ++ `[[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]], but,
	  * unlike the standard union, the [[net.noresttherein.oldsql.sql.WithClause.local local]] property of the result
	  * is always empty, with all elements of the union belonging
	  * to its [[net.noresttherein.oldsql.sql.WithClause.outer outer]] part, as is its `suppressed` property.
	  *  [[net.noresttherein.oldsql.sql.WithClause.outer outer]]`.`[[net.noresttherein.oldsql.sql.WithClause.all all]]` == this`.
	  */
	def all :WithClause =
		if (suppressed.isEmpty && local.isEmpty) this
		else new CompleteWithClause(local.toUnique, outer.toUnique, suppressed)

	/** True for clauses with all their tables belonging to their [[net.noresttherein.oldsql.sql.WithClause.local local]]
	  * part. Implies
	  * `(!`[[net.noresttherein.oldsql.sql.WithClause.isOuter isOuter]]` || isEmpty) && local == this && outer.isEmpty`.
	  */
	def isLocal :Boolean = outer.isEmpty

	/** True for clauses with all their tables belonging to their [[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * part. Implies
	  * `(!`[[net.noresttherein.oldsql.sql.WithClause.isLocal isLocal]]` || isEmpty) && outer == this && local.isEmpty`.
	  */
	def isOuter :Boolean = local.isEmpty

	/** A `WithClause` is ''complete'' 'iff' it contains all direct and transitive dependencies from its elements
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]] property.
	  * This test is equivalent to `this == `[[net.noresttherein.oldsql.sql.WithClause.closure closure]].
	  */
	def isComplete :Boolean = forall(_.withClause.forall(contains))

	/** True if any of the [[net.noresttherein.oldsql.sql.CommonTableExpression common table expressions]]
	  * in this clause (including those included by transitivity)
	  * is [[net.noresttherein.oldsql.sql.CommonTableExpression.isRecursive recursive]].
	  */
	def isRecursive :Boolean = exists(_.isRecursive)

	/** True if any of the [[net.noresttherein.oldsql.sql.CommonTableExpression common table expressions]]
	  * in this clause (including those included by transitivity)
	  * is [[net.noresttherein.oldsql.sql.CommonTableExpression.isMutuallyRecursive mutually recursive]].
	  */
	def isMutuallyRecursive :Boolean = exists(_.isMutuallyRecursive)

	/** An instance with the same elements, but sorted such that queries do not reference queries following them.
	  * If this clause contains both [[net.noresttherein.oldsql.sql.WithClause.local local]]
	  * and [[net.noresttherein.oldsql.sql.WithClause.outer outer]] table definitions, then they are sorted separately
	  * within each group, so that the result is equal to this instance.
	  * The tables in the clause [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] by this clause
	  * are not sorted; you may consider using `outer.sortedTopologically` if such a need arises.
	  * If this clause is [[net.noresttherein.oldsql.sql.WithClause.isMutuallyRecursive mutually recursive]]
	  * the order of elements in the cycle(s) is undefined.
	  */
	def sortedTopologically :WithClause = {
		val visited = mutable.HashSet.empty[CommonTableExpression.__]
		def dfs(tail :List[CommonTableExpression.__], cte :CommonTableExpression.__) :List[CommonTableExpression.__] =
			if (visited(cte))
				tail
			else {
				visited += cte
				cte::(tail /: cte.withClause)(dfs)
			}
		fromSpecific((List.empty[CommonTableExpression.__] /: this)(dfs).reverse)
	}

	/** Adds the given table, without dependencies, to the [[net.noresttherein.oldsql.sql.WithClause.local local]]
	  * declarations of this clause, returning a new object. The table will occur in the iteration order after all
	  * current `local` tables, unless it is already present, in which case the method may return the same instance.
	  * If `table` is already present in [[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * or [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] declarations, it is moved
	  * to the `local` subset. This method is equivalent to a union with a singleton local `WithClause`:
	  * {{{
	  *     this ++ WithClause.local(table)
	  * }}}
	  * The instance added is actually `table.`[[net.noresttherein.oldsql.sql.CommonTableExpression.default default]],
	  * so any alterations made to the default column selection are abstracted over by this class.
	  */
	def local_+(table :CommonTableExpression.__) :WithClause

	/** Adds the given table, without dependencies, to the [[net.noresttherein.oldsql.sql.WithClause.local local]]
	  * declarations of this clause, returning a new object. The table will occur in the iteration order before all
	  * current `local` tables, unless it is already present, in which case the method may return the same instance.
	  * Note however that the iteration order doesn't feature in equality of `WithClause`, which has the semantics
	  * of a pair of [[scala.collection.mutable.LinkedHashSet LinkedHashSet]] sets.
	  * If `table` is already present in [[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * or [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] declarations, it is moved
	  * to the `local` subset. This method is equivalent to a union with a singleton local `WithClause`:
	  * {{{
	  *     WithClause.local(table) ++ this
	  * }}}
	  * The instance added is actually `table.`[[net.noresttherein.oldsql.sql.CommonTableExpression.default default]],
	  * so any alterations made to the default column selection are abstracted over by this class.
	  */
	def local_+:(table :CommonTableExpression.__) :WithClause

	/** Adds all common table definitions from the argument clause to
	  * the [[net.noresttherein.oldsql.sql.WithClause.local local]] definitions in this clause, without modifying it,
	  * and returns the result. Additionally,
	  * the result's [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] clause will contain the tables
	  * of the `suppressed` clauses of both operands, with the exception of tables occurring in the
	  * [[net.noresttherein.oldsql.sql.WithClause.local local]] or [[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * subsets (that is, in the collections proper) of the other instance. Transitive dependencies of neither
	  * of the clauses will be included in the result. All tables are always added in their canonical,
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression.default default]] versions.
	  */
	def local_++(tables :WithClause) :WithClause = (WithClause.local.newBuilder(this) ++= tables).result()

	/** Adds the given tables, without dependencies, to the [[net.noresttherein.oldsql.sql.WithClause.local local]]
	  * declarations of this clause, returning a new object. The tables will occur in the iteration order after all
	  * current `local` tables, unless any of them is already present, in which case they may occur at the same
	  * position as in this instance. If a table is already present
	  * in [[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * or [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] declarations, it is moved
	  * to the `local` subset. This method is equivalent to a union with a local `WithClause`:
	  * {{{
	  *     this ++ WithClause.local.fromSpecific(tables)
	  * }}}
	  * All tables are always added in their canonical,
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression.default default]] versions.
	  */
	def local_++(tables :IterableOnce[CommonTableExpression.__]) :WithClause = tables match {
		case clause :WithClause => this local_++ clause
		case empty :Iterable[_] if empty.isEmpty => this
		case empty :Iterator[_] if !empty.hasNext => this
		case it :Iterable[CommonTableExpression.__] if it.sizeIs == 1 => this local_+ it.head
		case _ =>
			(WithClause.local.newBuilder(this) ++= tables).result()
	}

	/** Adds the given table to the [[net.noresttherein.oldsql.sql.WithClause.local local]] declarations of this clause,
	  * returning a new object. All transitive dependencies of the argument, that is tables present in
	  * `table.`[[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]], tables in the
	  * `withClause` properties of those tables, and so on, will be added to the
	  * [[net.noresttherein.oldsql.sql.WithClause.outer outer]] collections in the result. This makes this method
	  * not equivalent to [[net.noresttherein.oldsql.sql.WithClause.local_++* local_++*]]`(table.withClause)`,
	  * as the latter will add all the dependencies to the `local` declarations. In both cases however,
	  * the [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] clauses of every processed `WithClause`
	  * are combined into a single set, excluding any tables which are present as (proper) elements of the result.
	  *
	  * Added tables will occur in the iteration order after all current `local` tables, unless any is already present,
	  * in which case they may occur at the same position as in this instance. The order in which the dependencies
	  * are added is undefined. If a table is already present
	  * in [[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * or [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] declarations, it is moved
	  * to the `local` subset. All tables are always added in their canonical,
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression.default default]] versions.
	  */
	def local_+*(table :CommonTableExpression.__) :WithClause =
		if (table.withClause.sizeIs == 1) local_+(table)
		else if (table.withClause.isComplete) local_++(table.withClause)
//		else (new WithClauseClosureBuilder(this).addLocal(table) ++= table.withClause).result()
		else (WithClause.closure.newBuilder(this) ++= WithClause.local.fromSpecific(Unique.single(table))).result()

	/** Adds all tables present in the argument, with their dependencies,
	  * to the [[net.noresttherein.oldsql.sql.WithClause.local local]] declarations of this clause,
	  * returning a new object. The tables included
	  * in their [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]] properties are also added,
	  * and so are all tables recursively reachable through this dependency chain, regardless of their presence
	  * in original `withClause` collections. If a table is already present
	  * in the [[net.noresttherein.oldsql.sql.WithClause.outer outer]] clause of this instance, it is moved
	  * to the `local` clause of the result. Similarly,
	  * all [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] declarations of processed ''with'' clauses
	  * are combined together with `this.suppressed` to form the corresponding property of the result, after removing
	  * any present in the collection proper. Note that this method is ''not'' in general equivalent to
	  * `this `[[net.noresttherein.oldsql.sql.WithClause.++* ++*]]` `[[net.noresttherein.oldsql.sql.WithClause$ WithClause]]`.`[[net.noresttherein.oldsql.sql.WithClause$.local local]]`.`[[net.noresttherein.oldsql.sql.WithClause$.fromSpecific fromSpecific]]`(tables)`,
	  * as the latter would add any dependencies not present directly in the argument
	  * to the [[net.noresttherein.oldsql.sql.WithClause.outer outer]] subclause of the result,
	  * while this method will add everything to the `local` subclause. They will however produce equal instances
	  * (although possibly with a different iteration order) if the argument
	  * is [[net.noresttherein.oldsql.sql.WithClause.isComplete closed]] with respect to the dependency relation.
	  * All tables are always added in their canonical,
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression.default default]] versions.
	  */
	def local_++*(tables :WithClause) :WithClause =
		if (tables.isComplete) this local_++ tables
		else (WithClause.local.closure.newBuilder(this) ++= tables).result()

	/** Adds all tables present in the argument, with their dependencies,
	  * to the [[net.noresttherein.oldsql.sql.WithClause.local local]] declarations of this clause,
	  * returning a new object. The tables included
	  * in their [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]] properties are also added,
	  * and so are all tables recursively reachable through this dependency chain, regardless of their presence
	  * in original `withClause` collections. If a table is already present
	  * in the [[net.noresttherein.oldsql.sql.WithClause.outer outer]] clause of this instance, it is moved
	  * to the `local` clause of the result. Similarly,
	  * all [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] declarations of processed ''with'' clauses
	  * are combined together with `this.suppressed` to form the corresponding property of the result, after removing
	  * any present in the collection proper. Note that this method is ''not'' in general equivalent to
	  * `this `[[net.noresttherein.oldsql.sql.WithClause.++* ++*]]` `[[net.noresttherein.oldsql.sql.WithClause$ WithClause]]`.`[[net.noresttherein.oldsql.sql.WithClause$.local local]]`.`[[net.noresttherein.oldsql.sql.WithClause$.fromSpecific fromSpecific]]`(tables)`,
	  * as the latter would add any dependencies not present directly in the argument
	  * to the [[net.noresttherein.oldsql.sql.WithClause.outer outer]] subclause of the result,
	  * while this method will add everything to the `local` subclause. They will however produce equal instances
	  * (although possibly with a different iteration order) if the argument
	  * is [[net.noresttherein.oldsql.sql.WithClause.isComplete closed]] with respect to the dependency relation.
	  * All tables are always added in their canonical,
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression.default default]] versions.
	  */
	def local_++*(tables :IterableOnce[CommonTableExpression.__]) :WithClause = tables match {
		case clause :WithClause => this local_++* clause
		case empty :Iterable[_] if empty.isEmpty => this
		case empty :Iterator[_] if empty.isEmpty => this
		case it :Iterable[CommonTableExpression.__] if it.sizeIs == 1 => this local_+* it.head
		case it :Iterable[CommonTableExpression.__] if it forall (_.withClause.isComplete) => local_++(tables)
		case _ => //fixme: adding a table already present in this will not try to add its dependencies
			(new LocalWithClauseClosureBuilder(this) ++= tables).result()
	}

	/** Adds the given common table expression to this clause's [[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * definitions. Unless already present, it will appear in the iteration order after all `outer` tables
	  * from this instance. If the argument is a member of the [[net.noresttherein.oldsql.sql.WithClause.local local]]
	  * table subset, it is left where it is. If it is present among
	  * the [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] definitions, it is removed
	  * from that set. No tables from its [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]] -
	  * other than itself - are added. The instance added is actually
	  * `table.`[[net.noresttherein.oldsql.sql.CommonTableExpression.default default]], as any alterations made
	  * to the default column selection are abstracted over by this class.
	  */
	def +(table :CommonTableExpression.__) :WithClause

	/** Adds the given common table expression to this clause's [[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * definitions. Unless already present, it will appear in the iteration order before all `outer` tables
	  * from this instance. If the argument is a member of the [[net.noresttherein.oldsql.sql.WithClause.local local]]
	  * table subset, it is left where it is. If it is present among
	  * the [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] definitions, it is removed
	  * from that set. No tables from its [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]] -
	  * other than itself - are added. The instance added is actually
	  * `table.`[[net.noresttherein.oldsql.sql.CommonTableExpression.default default]], as any alterations made
	  * to the default column selection are abstracted over by this class.
	  */
	def +:(table :CommonTableExpression.__) :WithClause

	/** Performs a union o this ''with'' clause and another instance.
	  * The [[net.noresttherein.oldsql.sql.WithClause.local local]],
	  * [[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * and [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] collections of the returned `WithClause`
	  * will be set unions of the corresponding collections of the operands, with the exception of tables
	  * which were present in different subcollections in both operands, in which case the instance
	  * in the 'more local' (earlier in the above list) set is retained. If the clauses are disjoint,
	  * then the definitions from this clause will precede in the iteration order the definitions in the argument;
	  * tables occurring in both clauses can occur at the position corresponding to either of the clauses.
	  *
	  * This method will not add transitive dependencies of either the argument or this clause to the result.
	  * All tables are always added in their canonical,
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression.default default]] versions.
	  */
	def ++(tables :WithClause) :WithClause =
		if (tables.isEmpty)
			this
		else {
			val b = newSpecificBuilder
			(b ++= this ++= tables).result()
		}

	/** Adds all elements of the argument to this collection. If `suffix` is a `WithClause`,
	  * then the overloaded [[net.noresttherein.oldsql.sql.WithClause.++ ++]] variant for this type is called,
	  * which will preserve the assignment of the elements between the
	  * [[net.noresttherein.oldsql.sql.WithClause.local local]] and [[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * subsets in the argument. If all elements of `suffix` are instances of
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression CommonTableExpression]], then the result will also
	  * be an instance of `WithClause` and this call will be equivalent to repeated calls
	  * to [[net.noresttherein.oldsql.sql.WithClause.+ +]] for all elements. Otherwise the union has default
	  * [[scala.collection.Iterable Iterable]] semantics. All tables are always added in their canonical,
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression.default default]] versions.
	  */
	override def concat[B >: CommonTableExpression.__](suffix :IterableOnce[B]) :Iterable[B] =
		suffix match {
			case clause :WithClause => this ++ clause
			case it :Iterable[_] if it.isEmpty => this
			case it :Iterable[B] =>
				if (it.forall(_.isInstanceOf[CommonTableExpression.__])) {
					val b = WithClause.newBuilder; b.sizeHint(size + it.size)
					(b ++= this ++= it.asInstanceOf[Iterable[CommonTableExpression.__]]).result()
				} else
					super.concat(suffix)
			case it :Iterator[_] if it.isEmpty => this
			case _ => //suffix may have an element which is not a CTE, but, if not, we want to still build a WithClause
				val b = WithClause.newBuilder; b.sizeHint(suffix, size)
				b ++= this
				val it = suffix.iterator
				val res = repeat {
					it.next() match {
						case table :CommonTableExpression.__ => b += table
						case other =>
							val b2 = iterableFactory.newBuilder[B]
							(b2 ++= b.result() += other ++= it)
					}
				} until (!it.hasNext)
				res.result()
		}

	/** Adds the given common table expression, together with its dependencies, to the definitions
	  * in this ''with'' clause. The table is added as
	  * `table.`[[net.noresttherein.oldsql.sql.CommonTableExpression.default default]], which might not be the exact
	  * instance given as the argument. This method is equivalent to
	  * `this `[[net.noresttherein.oldsql.sql.WithClause.++* ++*]]` table.`[[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]].
	  */
	def +*(table :CommonTableExpression.__) :WithClause = this ++* table.withClause

	/** Adds the given tables, together with all their transitive dependencies, to this ''with'' clause.
	  * If the argument is another `WithClause`, then definitions present in it directly will be assigned to the same
	  * [[net.noresttherein.oldsql.sql.WithClause.local local]]/[[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * subset in the result. Otherwise the collection will become a part of the `outer` clause of the result,
	  * as will their indirect dependencies in both cases. The order of distinct elements in `this` and `tables`
	  * will be preserved. However, tables which are a part of both collections may occur at positions corresponding
	  * to either of them, and the order of indirectly added tables from
	  * the [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]] properties of the arguments
	  * is undefined. The [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] property of the result
	  * will be a transitive union of `this.suppressed`, as if any processed table and `WithClause` was replaced
	  * with an empty `WithClause` containing all the elements of the former in its `suppressed` clause.
	  * The exception to the above are tables present as proper elements of the result (that is, which were directly
	  * or indirectly added themselves). All tables are always added in their canonical,
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression.default default]] versions.
	  */
	def ++*(tables :IterableOnce[CommonTableExpression.__]) :WithClause = tables match {
		case other :WithClause if other.isComplete => this ++ other
		case other :WithClause if isEmpty && suppressed.isEmpty => other.closure
		case _ => (WithClause.closure.newBuilder(this) ++= tables).result()
//		case _ => (WithClause.outer.closure.newBuilder(this) ++= tables).result()
	}

	/** Removes the given table expression from this ''with'' clause.
	  * The [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] is left unchanged.
	  */
	def -(table :CommonTableExpression.__) :WithClause =
		if (isEmpty || !contains(table)) this
		else filterNot(_ == table.default)

	/** Removes all the given table expressions from this ''with'' clause.
	  * The [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] is left unchanged.
	  * Note that if the argument is another `WithClause`, the assignment of common elements between
	  * [[net.noresttherein.oldsql.sql.WithClause.local local]] and
	  * [[net.noresttherein.oldsql.sql.WithClause.outer outer]] subsets is ignored, and both clauses are treated
	  * as sets being the unions of their `local` and `outer` parts.
	  */
	def --(tables :IterableOnce[CommonTableExpression.__]) :WithClause = tables match {
		case _ if isEmpty => this
		case it :Iterable[CommonTableExpression.__] if it.isEmpty => this
		case ctes :WithClause =>
			if (WithClause.disjoint(this, ctes)) this
			else filterNot(ctes.contains)
		case it :Iterable[CommonTableExpression.__] =>
			filterNot(it.toSet)
		case _ =>
			filterNot(tables.iterator.toSet)
	}


	/** Checks if this ''with'' clause contains the given table expression. Membership is always defined
	  * in terms of the equality between [[net.noresttherein.oldsql.sql.CommonTableExpression.default default]]
	  * versions of the tables, meaning that all instances altered by changing the default column selection
	  * will be considered the same as their unaltered versions.
	  * [[net.noresttherein.oldsql.sql.CommonTableExpression CommonTableExpression]] defines equality as
	  * reference equality rather than structural equality, so comparing two identical, but distinct instances
	  * will yield a negative.
	  */
	def contains(table :CommonTableExpression.__) :Boolean

	/** Finds a table of the given name in this ''with'' clause, if any.
	  * First, the [[net.noresttherein.oldsql.sql.WithClause.local local]] declarations are searched.
	  * If no such table is found, the search proceeds to the [[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * declarations.
	  * @throws net.noresttherein.oldsql.exceptions.AmbiguousAliasException if any of the above searches returns
	  *        more than one table.
	  * @throws NoSuchElementException if no table of the given name is present in this instance.
	  */
	def apply(name :String) :CommonTableExpression.__ = get(name) getOrElse {
		throw new NoSuchElementException("No table expression with name " + name + " in " + this + ".")
	}

	/** Finds a table of the given name in this ''with'' clause, if any.
	  * First, the [[net.noresttherein.oldsql.sql.WithClause.local local]] declarations are searched.
	  * If no such table is found, the search proceeds to the [[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * declarations.
	  * @throws net.noresttherein.oldsql.exceptions.AmbiguousAliasException if any of the above searches returns
	  *        more than one table.
	  */
	def get(name :String) :Option[CommonTableExpression.__] =
		if (isLocal || isOuter) filter(_.name == name) match {
			case unambiguous if unambiguous.sizeIs <= 1 => unambiguous.headOption
			case ambiguous =>
				throw new IllegalArgumentException(
					ambiguous.mkString("Multiple common table expressions named " + name + " found: ", ", ", ".")
				)
		} else
			local.get(name) orElse outer.get(name)

	override def isEmpty :Boolean = sizeIs == 0

	override def empty :WithClause = WithClause.empty

	/** A builder of ''with'' clauses associated in some way from this instance (in particular, its subsets and supersets).
	  * The difference from the default
	  * [[net.noresttherein.oldsql.sql.WithClause$ WithClause]]`.`[[net.noresttherein.oldsql.sql.WithClause.newBuilder newBuilder]]
	  * is that any table added, which is also present in `this` clause, is added to the same
	  * [[net.noresttherein.oldsql.sql.WithClause.local local]]/[[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * part of the result as in this instance. Additionally,
	  * the [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] collection of this instance is added
	  * in full to the result (as are any `suppressed` dependencies of added tables).
	  * The result will contain only the explicitly added arguments (either individually, or elements
	  * of added collections) and not their unlisted transitive dependencies.
	  * @see [[net.noresttherein.oldsql.sql.WithClause.closureBuilder closureBuilder]]
	  */
	override def newSpecificBuilder :Builder[CommonTableExpression.__, WithClause] =
		new DerivedWithClauseBuilder(this)

	/** A builder of ''with'' clauses, derived in some way from this instance, which automatically
	  * add dependencies of added tables
	  * (their [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]] properties) to the result.
	  * The difference from the standard
	  * [[net.noresttherein.oldsql.sql.WithClause$ WithClause]]`.`[[net.noresttherein.oldsql.sql.WithClause$.closure closure]]`.`[[net.noresttherein.oldsql.sql.WithClause.WithClauseFactory.newBuilder newBuilder]]
	  * is that any table added, which is present in `this` clause, is added to the same
	  * [[net.noresttherein.oldsql.sql.WithClause.local local]]/[[net.noresttherein.oldsql.sql.WithClause.outer outer]]
	  * part of the result as in this instance. Additionally,
	  * the [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] collection of this instance is added
	  * in full to the result (aside from any `suppressed` dependencies of added tables).
	  */
	def closureBuilder :Builder[CommonTableExpression.__, WithClause] =
		new DerivedWithClauseClosureBuilder(this)

	/** The factory method must:
	  *   1. only be called for elements coming from this clause,
	  *   1. assign every table to either `local` or `outer` part of the result, based on where it belonged in `this`,
	  *   1. reattach the `suppressed` collection from `this` instance.
	  */
	protected override def fromSpecific(coll :IterableOnce[CommonTableExpression.__]) :WithClause =
		(newSpecificBuilder ++= coll).result()

	def toSeq    :Seq[CommonTableExpression.__]
	def toUnique :Unique[CommonTableExpression.__] = Unique.from(iterator)
	override def toSet[U >: CommonTableExpression.__] :Set[U] = toUnique.toSet


	def canEqual(that :Any) :Boolean = that.isInstanceOf[WithClause]

	/** A ''with'' clause can equal only another `WithClause`. It is defined as set equality
	  * of the [[net.noresttherein.oldsql.sql.WithClause.local local]]
	  * and [[net.noresttherein.oldsql.sql.WithClause.outer outer]] properties of the two instances,
	  * with checks for infinite recursion. The [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]]
	  * collections as well as element order within the sets is ignored.
	  */
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :WithClause if other canEqual this =>
			isEmpty && other.isEmpty || size == other.size && (
				if (isOuter) forall(other.outer.contains)
				else if (isLocal) forall(other.local.contains)
				else local == other.local && outer == other.outer
			)
		case _ => false
	}
	override def hashCode :Int =
		if (isOuter) MurmurHash3.unorderedHash(this, WithClause.outerHashSeed)
		else if (isLocal) MurmurHash3.unorderedHash(this, WithClause.localHashSeed)
		else local.hashCode * 31 + outer.hashCode

	protected[this] override def className = "With"

	override def toString :String =
		if (local.isEmpty) super.toString //consider: swapping the order of outer/local here and in iteration
		else if (outer.isEmpty) local.mkString("With(local: ", ", ", ")")
		else mkString(local.mkString("With(local: ", ", ", "; "), ", ", ")")
}




/** A factory of ''with'' clauses containing [[net.noresttherein.oldsql.sql.CommonTableExpression CommonTableExpression]]
  * instances, which implements the standard Scala collection factory interface.
  * As a [[net.noresttherein.oldsql.sql.WithClause WithClause]] is more than a simple, flat collection,
  * additional factories are provided as properties of this instance. The differ in whether the declarations
  * should be treated as local to an associated SQL expression (or a whole query) and if any dependencies of
  * added tables should be included automatically with them:
  *   1. [[net.noresttherein.oldsql.sql.WithClause$.local WithClause.local]] builds instances by adding every table
  *      to the [[net.noresttherein.oldsql.sql.WithClause.local local]] declaration subset of the result;
  *   1. [[net.noresttherein.oldsql.sql.WithClause.local.closure WithClause.local.closure]] do the same as above,
  *      but also add recursively all tables present
  *      in the [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]] properties of the listed
  *      tables;
  *   1. [[net.noresttherein.oldsql.sql.WithClause$.outer WithClause.outer]] builds instances by adding every table
  *      to the [[net.noresttherein.oldsql.sql.WithClause.outer outer]] declaration subset of the result;
  *   1. [[net.noresttherein.oldsql.sql.WithClause.outer.closure WithClause.outer.closure]] do the same as above,
  *      but also add recursively all tables present
  *      in the [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]] properties of the listed
  *      tables.
  *   1. [[net.noresttherein.oldsql.sql.WithClause$.closure WithClause.closure]] adds tables to the `outer` subset
  *      by default, but when adding a whole `WithClause`, the tables are assigned to the same subset as in the argument.
  *      All dependencies are added however to the `outer` subset, regardless of the assignment of
  *      the common table expression referencing them.
  * This object itself creates collections consisting only of the elements listed explicitly (without dependencies),
  * and add every table either to the same subclause as in the argument, for those included in passed ''with'' clauses,
  * or in the outer subclause for tables added individually or within other collection types.
  */
object WithClause extends Factory[CommonTableExpression.__, WithClause] {
	def apply(tables :CommonTableExpression.__ *) :WithClause =
		if (tables.isEmpty) EmptyWithClause
		else if (tables.sizeIs == 1) tables.head.withClause
		else (newBuilder ++= tables).result()

	override def fromSpecific(tables :IterableOnce[CommonTableExpression.__]) :WithClause = tables match {
		case tables :WithClause => tables
		case iter :Iterable[CommonTableExpression.__] if iter.isEmpty => EmptyWithClause
		case iter :Iterable[CommonTableExpression.__] if iter.sizeIs == 1 => iter.head.withClause
		case iter :Iterator[_] if !iter.hasNext => EmptyWithClause
		case _ => (newBuilder ++= tables).result()
	}

	def empty :WithClause = EmptyWithClause

	def unapplySeq(ctes :WithClause) :Seq[CommonTableExpression.__] = ctes.toSeq


	override def newBuilder :Builder[CommonTableExpression.__, WithClause] = new WithClauseBuilder
	def outerBuilder        :Builder[CommonTableExpression.__, WithClause] = new OuterWithClauseBuilder
	def localBuilder        :Builder[CommonTableExpression.__, WithClause] = new LocalWithClauseBuilder
	def closureBuilder      :Builder[CommonTableExpression.__, WithClause] = new WithClauseClosureBuilder
	def localClosureBuilder :Builder[CommonTableExpression.__, WithClause] = new LocalWithClauseClosureBuilder
	def outerClosureBuilder :Builder[CommonTableExpression.__, WithClause] = new OuterWithClauseClosureBuilder

	/** A factory building [[net.noresttherein.oldsql.sql.WithClause.local local]] `WithClause` instances
	  * consisting exactly of the given elements, preserving their order.
	  */
	val local :WithClauseFactory = new WithClauseFactory("WithClause.local") {
		override def fromSpecific(it :IterableOnce[CommonTableExpression.__]) :WithClause = it match {
			case tables :WithClause if tables.isLocal => tables
			case empty :Iterable[_] if empty.isEmpty => EmptyWithClause
			case empty :Iterator[_] if empty.isEmpty => EmptyWithClause
			case unique :Unique[CommonTableExpression.__] if unique.forall(_.isDefault) =>
				new SplitWithClause(unique, Unique.empty, WithClause.empty)
			case _ =>
				new SplitWithClause(it.iterator.map(_.default) to Unique, Unique.empty, WithClause.empty)
		}
		override def newBuilder(init :WithClause) = new LocalWithClauseBuilder(init)

		override val closure :WithClauseFactory = new WithClauseFactory("WithClause.local.closure") {
			override def newBuilder(init :WithClause) = new LocalWithClauseClosureBuilder(init)
		}
	}
	/** A factory building [[net.noresttherein.oldsql.sql.WithClause.outer outer]] `WithClause` instances
	  * consisting exactly of the given elements, preserving their order.
	  */
	val outer :WithClauseFactory = new WithClauseFactory("WithClause.outer") {
		override def fromSpecific(it :IterableOnce[CommonTableExpression.__]) :WithClause = it match {
			case tables :WithClause if tables.isOuter => tables
			case empty :Iterable[_] if empty.isEmpty => EmptyWithClause
			case empty :Iterator[_] if empty.isEmpty => EmptyWithClause
			case unique :Unique[CommonTableExpression.__] if unique.forall(_.isDefault) =>
				new SplitWithClause(Unique.empty, unique, WithClause.empty)
			case _ =>
				new SplitWithClause(Unique.empty, it.iterator.map(_.default) to Unique, WithClause.empty)
		}
		override def newBuilder(init :WithClause) = new OuterWithClauseBuilder(init)

		override val closure :WithClauseFactory = new WithClauseFactory("WithClause.outer.closure") {
			override def newBuilder(init :WithClause) = new OuterWithClauseClosureBuilder(init)
		}
	}
	/** A factory building `WithClause` instances consisting of all tables passed as arguments, as well as any tables
	  * reachable recursively through their [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]]
	  * properties. The tables are assigned to [[net.noresttherein.oldsql.sql.WithClause.local local]]
	  * and [[net.noresttherein.oldsql.sql.WithClause.outer outer]] parts of the returned clause based on
	  * the part to which they belonged to in their encompassing clause; individual table arguments, as well as
	  * tables within collections other than a `WithClause`, are always added as their dependency `withClause`,
	  * which, bar any special cases, means they become a part of the `outer` subclause of the result.
	  * Dependencies not added explicitly will likewise be placed in the `outer` subset.
	  * If a table is added several times, then any occurrences as a `local` declaration take precedence.
	  * All [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] clauses are combined into a single
	  * clause under the same property of the result.
	  */
	val closure :WithClauseFactory = new WithClauseFactory("WithClause.closure") {
		override def fromSpecific(it :IterableOnce[CommonTableExpression.__]) :WithClause = it match {
			case tables :WithClause if tables.isComplete => tables
			case tables :WithClause => tables.closure
			case _ => super.fromSpecific(it)
		}
		override def newBuilder(init :WithClause) = new WithClauseClosureBuilder(init)
	}
	//where to place these docs on local and outer?
//	/** A factory building `WithClause` instances consisting of all tables passed as arguments, as well as any tables
//	  * reachable recursively through their [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]]
//	  * properties. The tables are always assigned to [[net.noresttherein.oldsql.sql.WithClause.local local]]
//	  * part of the returned clause, regardless of the part to which they belonged to in their encompassing clause
//	  * and whether they were added as explicit arguments, or automatically as dependencies.
//	  * All [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] clauses are combined into a single
//	  * clause under the same property of the result.
//	  */
//	val localClosure :WithClauseFactory = new WithClauseFactory("WithClause.localClosure") {
//		override def newBuilder(init :WithClause) = new LocalWithClauseClosureBuilder(init)
//	}
//	/** A factory building `WithClause` instances consisting of all tables passed as arguments, as well as any tables
//	  * reachable recursively through their [[net.noresttherein.oldsql.sql.CommonTableExpression.withClause withClause]]
//	  * properties. The tables are always assigned to [[net.noresttherein.oldsql.sql.WithClause.outer outer]]
//	  * part of the returned clause, regardless of the part to which they belonged to in their encompassing clause
//	  * and whether they were added as explicit arguments, or automatically as dependencies.
//	  * All [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] clauses are combined into a single
//	  * clause under the same property of the result.
//	  */
//	val outerClosure :WithClauseFactory = new WithClauseFactory("WithClause.outerClosure") {
//		override def newBuilder(init :WithClause) = new OuterWithClauseClosureBuilder(init)
//	}
	/** A special `WithClause` factory which produces only empty instances, with all elements added becoming part
	  * of the result's [[net.noresttherein.oldsql.sql.WithClause.suppressed suppressed]] property instead.
	  */
	val suppressed :WithClauseFactory = new WithClauseFactory("WithClause.suppressed") {
		override def fromSpecific(it :IterableOnce[CommonTableExpression.__]) = fromParts(Unique.empty, Unique.empty, it)
		override def newBuilder(init :WithClause) :Builder[CommonTableExpression.__, WithClause] =
			new SuppressedWithClauseBuilder(init)

		override val closure = new WithClauseFactory("WithClause.suppressed.closure") {
			override def newBuilder(init :WithClause) :Builder[CommonTableExpression.__, WithClause] =
				new SuppressedWithClauseClosureBuilder(init)
		}
	}


	/** A factory of `WithClause` instances conforming to the standard Scala collection framework.
	  * Several implementations exist, which vary in whether they automatically add transitive dependencies
	  * of explicitly added elements and to which subset of the result, between `local` `outer`, is any given item added.
	  * @see [[net.noresttherein.oldsql.sql.WithClause$.local]]
	  * @see [[net.noresttherein.oldsql.sql.WithClause$.outer]]
	  * @see [[net.noresttherein.oldsql.sql.WithClause$.closure]]
	  */
	sealed class WithClauseFactory private[WithClause] (override val toString :String)
		extends Factory[CommonTableExpression.__, WithClause] with Serializable
	{
		/** A shortcut for the standard
		  * [[net.noresttherein.oldsql.sql.WithClause.WithClauseFactory.fromSpecific fromSpecific]] method.
		  */
		def apply(tables :CommonTableExpression.__ *) :WithClause = fromSpecific(tables)

		override def fromSpecific(it :IterableOnce[CommonTableExpression.__]) :WithClause = it match {
			case _ :WithClause => (newBuilder ++= it).result()
			case empty :Iterable[_] if empty.isEmpty => EmptyWithClause
			case empty :Iterator[_] if empty.isEmpty => EmptyWithClause
			case _ => (newBuilder ++= it).result()
		}
		override def newBuilder :Builder[CommonTableExpression.__, WithClause] = newBuilder(empty)
		protected[WithClause] def newBuilder(init :WithClause) :Builder[CommonTableExpression.__, WithClause] =
			new WithClauseBuilder(init)

		/** A factory which assigns all elements to the same subclause as this one, but also adding all their
		  * transitive dependencies.
		  */
		val closure :WithClauseFactory = this //warning: this is correct only for closure factories, everyone else must override
	}


	private class WithClauseBuilder(init :WithClause) extends ReusableBuilder[CommonTableExpression.__, WithClause] {
		def this() = this(empty)

		private[this] var local :ArrayBuffer[CommonTableExpression.__] = _
		private[this] var outer :ArrayBuffer[CommonTableExpression.__] = _
		private[this] var suppressed :ArrayBuffer[CommonTableExpression.__] = _
		if (init.sizeIs > 0) { //same as add(init), but that one is overriden by subclasses
			local = add(local, init.local)
			outer = add(outer, init.outer)
		}
		suppressed = add(suppressed, init.suppressed)

		protected def localTables = {
			if (local == null) {
				local = ArrayBuffer.empty
			}
			local
		}
		protected def outerTables = {
			if (outer == null)
				outer = ArrayBuffer.empty
			outer
		}
		protected def suppressedTables = {
			if (suppressed == null)
				suppressed = ArrayBuffer.empty
			suppressed
		}

		@inline private final def add(buffer :ArrayBuffer[CommonTableExpression.__], elems :WithClause) =
			if (elems.isEmpty)
				buffer
			else {
				val res = if (buffer == null) new ArrayBuffer[CommonTableExpression.__] else buffer
				res ++= elems
				res
			}
		protected def addLocal(elems :WithClause) :this.type = {
			local = add(local, elems)
			suppressed = add(suppressed, elems.suppressed)
			this
		}
		protected def addOuter(elems :WithClause) :this.type = {
			outer = add(outer, elems)
			suppressed = add(suppressed, elems.suppressed)
			this
		}
		protected def addSuppressed(elems :WithClause) :this.type = {
			suppressed = add(suppressed, elems)
			this
		}
		protected def addLocal(elem :CommonTableExpression.__) :this.type = {
			if (local == null)
				local = new ArrayBuffer
			local += elem
			this
		}
		protected def addOuter(elem :CommonTableExpression.__) :this.type = {
			if (outer == null)
				outer = new ArrayBuffer
			outer += elem
			this
		}
		protected def addSuppressed(elem :CommonTableExpression.__) :this.type = {
			if (suppressed == null)
				suppressed = new ArrayBuffer
			suppressed += elem
			this
		}


		def add(elems :WithClause) :this.type = {
			if (elems.sizeIs > 0) {
				local = add(local, elems.local)
				outer = add(outer, elems.outer)
			}
			suppressed = add(suppressed, elems.suppressed)
			this
		}
		override def addOne(elem :CommonTableExpression.__) = addOuter(elem)

		override def addAll(xs :IterableOnce[CommonTableExpression.__]) :this.type = xs match {
			case tables :WithClause => add(tables)
			case _ => super.addAll(xs)
		}

		override def result() = {
			val localCTEs =
				if (local == null || local.isEmpty) Unique.empty
				else local to Unique
			val outerCTEs =
				if (outer == null || outer.isEmpty) Unique.empty
				else if (localCTEs.isEmpty) outer to Unique
				else outer.iterator.filterNot(localCTEs.contains) to Unique
			val suppressedCTEs =
				if (suppressed == null || suppressed.isEmpty) Unique.empty
				else if (localCTEs.isEmpty && outerCTEs.isEmpty) outer to Unique
				else outer.iterator.filterNot(t => localCTEs.contains(t) || outerCTEs.contains(t)) to Unique
			val res = fromParts(localCTEs, outerCTEs, suppressedCTEs)
			clear()
			res
		}

		override def clear() :Unit = {
			local = null
			outer = null
			suppressed = null
		}
		override def sizeHint(size :Int) :Unit = {
			if (local == null)
				local = new ArrayBuffer
			local sizeHint size
			if (outer == null)
				outer = new ArrayBuffer
			outer sizeHint size
		}
	}


	private class LocalWithClauseBuilder(init :WithClause) extends WithClauseBuilder(init) {
		def this() = this(empty)
		override def add(elems :WithClause) :this.type = addLocal(elems)
		override def addOne(elem :CommonTableExpression.__) :this.type = addLocal(elem)
		override def sizeHint(size :Int) :Unit = localTables sizeHint size
	}
	private class OuterWithClauseBuilder(init :WithClause) extends WithClauseBuilder(init) {
		def this() = this(empty)
		override def add(elems :WithClause) :this.type = addOuter(elems)
		override def addOne(elem :CommonTableExpression.__) :this.type = addOuter(elem)
		override def sizeHint(size :Int) :Unit = outerTables sizeHint size
	}
	private class SuppressedWithClauseBuilder(init :WithClause) extends WithClauseBuilder(init) {
		def this() = this(empty)
		override def add(elems :WithClause) :this.type = { addSuppressed(elems); addSuppressed(elems.suppressed) }
		override def addOne(elem :CommonTableExpression.__) :this.type = addSuppressed(elem)
		override def sizeHint(size :Int) :Unit = suppressedTables sizeHint size
	}
	private class DerivedWithClauseBuilder(original :WithClause)
		extends WithClauseBuilder with Builder[CommonTableExpression.__, WithClause]
	{
		private[this] val local = original.local

		override def add(elems :WithClause) :this.type = super[Builder].addAll(elems)

		override def addOne(elem :CommonTableExpression.__) :this.type =
			if (local.contains(elem)) addLocal(elem)
			else addOuter(elem)

		override def result() :WithClause =
			if (suppressedTables.nonEmpty) {
				addSuppressed(original.suppressed)
				super.result()
			} else {
				val res = fromParts(localTables, outerTables, original.suppressed)
				clear()
				res
			}
	}



	private class WithClauseClosureBuilder(init :WithClause) extends ReusableBuilder[CommonTableExpression.__, WithClause] {
		def this() = this(empty)

		private[this] var local :mutable.LinkedHashSet[CommonTableExpression.__] = _
		private[this] var outer :mutable.LinkedHashSet[CommonTableExpression.__] = _
		private[this] var suppressed :mutable.LinkedHashSet[CommonTableExpression.__] = _

		{   //An ugly 'feature' is that if we add later an already present element we'll not recurse down
			val initLocal = init.local
			if (!initLocal.isEmpty) {
				local = mutable.LinkedHashSet.empty
				local ++= initLocal
			}
			val initOuter = init.outer
			if (!initOuter.isEmpty) {
				outer = mutable.LinkedHashSet.empty
				outer ++= initOuter
			}
			val initSuppressed = init.suppressed
			if (!initSuppressed.isEmpty) {
				suppressed = mutable.LinkedHashSet.empty
				suppressed ++= initSuppressed
			}
		}

		protected def localBuilder = {
			if (local == null) {
				local = mutable.LinkedHashSet.empty
			}
			local
		}
		protected def outerBuilder = {
			if (outer == null)
				outer = mutable.LinkedHashSet.empty
			outer
		}
		protected def suppressedBuilder = {
			if (suppressed == null)
				suppressed = mutable.LinkedHashSet.empty
			suppressed
		}
		def addLocal(elem :CommonTableExpression.__) :this.type = {
			val default = elem.default
			if (local == null)
				local = mutable.LinkedHashSet.empty
			if (local.add(default) && (outer == null || !outer.remove(default)) && default.withClause.sizeIs > 1)
				addOuter(default.withClause)
			if (suppressed != null)
				suppressed -= default
			this
		}
		def addOuter(elem :CommonTableExpression.__) :this.type = {
			val default = elem.default
			if (local != null && !local.contains(default)) {
				if (outer == null)
					outer = mutable.LinkedHashSet.empty
				if (outer.add(default) && default.withClause.sizeIs > 1)
					addOuter(default.withClause)
				if (suppressed != null)
					suppressed -= default
			}
			this
		}

		def addLocal(elems :WithClause) :this.type = {
			if (!elems.isEmpty) {
				if (local == null)
					local = mutable.LinkedHashSet.empty
				elems foreach { t =>
					if (local.add(t) && (outer == null || !outer.remove(t)) && t.withClause.sizeIs > 1)
						addLocal(t.withClause)
					if (suppressed != null)
						suppressed -= t
				}
			}
			addSuppressed(elems.suppressed)
		}
		def addOuter(elems :WithClause) :this.type = {
			if (!elems.isEmpty) {
				if (outer == null)
					outer = mutable.LinkedHashSet.empty
				elems foreach { t =>
					if ((local == null || !local.contains(t)) && outer.add(t) && t.withClause.sizeIs > 1)
						addOuter(t.withClause)
					if (suppressed != null)
						suppressed -= t
				}
			}
			addSuppressed(elems.suppressed)
		}
		def addSuppressed(elems :WithClause) :this.type = {
			if (!elems.isEmpty) {
				if (suppressed == null)
					suppressed = mutable.LinkedHashSet.empty
				elems.foreach { table =>
					if ((local == null || !local.contains(table)) && (outer == null || !outer.contains(table)))
						suppressed += table
				}
			}
			this
		}

		def add(elems :WithClause) :this.type = {
			addSuppressed(elems.suppressed) //first in an attempt to preserve the order, if possible, rather than adding local.suppressed and outer.suppressed first
//			addLocal(elems.local)           //addLocal calls recursively addLocal, not addOuter
			val elemsLocal = elems.local
			if (!elemsLocal.isEmpty) {
				elemsLocal foreach addLocal
			}
			 addOuter(elems.outer)
		}

		/** Called as the direct result o `+=` and `++=`, but never called recursively - only for explicitly added tables. */
		override def addOne(elem :CommonTableExpression.__) =
			if ((local == null || !local.contains(elem.default)) && (outer == null || !outer.contains(elem.default)))
				add(elem.withClause)
			else this

		override def addAll(xs :IterableOnce[CommonTableExpression.__]) :this.type = xs match {
			case clause :WithClause => add(clause)
			case _ => super.addAll(xs)
		}

		override def clear() :Unit = {
			local = null
			outer = null
			suppressed = null
		}

		override def result() = {
			def unique(set :mutable.Set[CommonTableExpression.__]) =
				if (set == null) Unique.empty else set to Unique
			val res = fromParts(unique(local), unique(outer), unique(suppressed))
			clear()
			res
		}
	}


	private class LocalWithClauseClosureBuilder(init :WithClause) extends WithClauseClosureBuilder(init) {
		def this() = this(empty)
		override def add(elems :WithClause) :this.type = addLocal(elems)
		override def addOne(elem :CommonTableExpression.__) :this.type = {
			if (this.localBuilder.add(elem)) {
				val clause = elem.withClause
				if (clause.sizeIs > 1)
					addLocal(clause)
				addSuppressed(clause.suppressed)
			}
			this
		}
	}
	private class OuterWithClauseClosureBuilder(init :WithClause) extends WithClauseClosureBuilder(init) {
		def this() = this(empty)
		override def add(elems :WithClause) :this.type = addOuter(elems)
		override def addOne(elem :CommonTableExpression.__) :this.type = {
			if (this.outerBuilder.add(elem)) {
				val clause = elem.withClause
				if (clause.sizeIs > 1)
					addOuter(clause)
				addSuppressed(clause.suppressed)
			}
			this
		}
	}
	private class SuppressedWithClauseClosureBuilder(init :WithClause) extends WithClauseClosureBuilder(init) {
		def this() = this(empty)

		override def add(elems :WithClause) :this.type = {
			if (elems.nonEmpty)
				elems foreach addOne
			if (elems.suppressed.nonEmpty)
				elems.suppressed foreach addOne
			this
		}
		override def addOne(elem :CommonTableExpression.__) :this.type = {
			if (suppressedBuilder.add(elem)) {
				val clause = elem.withClause
				if (clause.sizeIs > 1)
					add(clause)
				else
					add(clause.suppressed)
			}
			this
		}
	}

	private class DerivedWithClauseClosureBuilder(original :WithClause)
		extends WithClauseClosureBuilder with Builder[CommonTableExpression.__, WithClause]
	{
		private[this] val local = original

		override def add(elems :WithClause) = {
			super[Builder].addAll(elems)
			addSuppressed(elems.suppressed)
		}
		override def addOne(elem :CommonTableExpression.__) :this.type =
			if (local.contains(elem)) {
				if (this.localBuilder.add(elem.default)) {
					addOuter(elem.withClause)
					suppressedBuilder -= elem.default
				}
				this
			} else
				addOuter(elem)

		override def result() :WithClause =
			if (suppressedBuilder.isEmpty)
				if (original.suppressed.isEmpty)
					super.result()
				else {
					val res = fromParts(this.localBuilder, this.outerBuilder, original.suppressed)
					clear()
					res
				}
			else {
				suppressedBuilder ++= original.suppressed
				super.result()
			}
	}




	private abstract class EmptyWithClause extends WithClause { //extracted for clauses with non-empty suppressed
		override def size = 0
		override def knownSize = 0
		override def iterator = Iterator.empty
		override def foreach[U](f :CommonTableExpression.__ => U) :Unit = ()
		override def contains(table :CommonTableExpression.__) = false

		override def local :WithClause = this
		override def outer :WithClause = this
		override def closure :WithClause = this
		override def isLocal = true
		override def isOuter = true
		override def isComplete = true
		override def isEmpty = true

		override def local_+:(table :CommonTableExpression.__) :WithClause = this local_+(table)
		override def local_+(table :CommonTableExpression.__) =
			new SplitWithClause(Unique.single(table), Unique.empty, this)

		override def -(cte :CommonTableExpression.__) :WithClause = this
		override def --(ctes :IterableOnce[CommonTableExpression.__]) :WithClause = this

		override def get(name :String) = None

		override def toSeq = Nil
		override def toUnique = Unique.empty
		override def toSet[U >: CommonTableExpression.__] = Set.empty
		override def toString :String = "With()"
	}

	private object EmptyWithClause extends EmptyWithClause {
		override def suppressed = this
		override def all = this

		override def local_++(tables :WithClause) :WithClause =
			if (tables.isLocal) tables
			else WithClause.local.fromSpecific(tables)

		override def local_+*(table :CommonTableExpression.__) = {
			val clause = table.withClause
			if (clause.isComplete)
				if (clause.isLocal) clause
				else { //is closed
					val outer =
						if (table.withClause.sizeIs == 1) Unique.empty
						else table.withClause.view.filterNot(_ == table.default) to Unique
					new SplitWithClause(Unique.single(table), outer, clause.suppressed)
				}
			else
				(localClosureBuilder += table).result()
		}
		override def local_++*(tables :WithClause) =
			if (tables.isLocal && tables.isComplete)
				tables
			else (localClosureBuilder ++= tables).result()

		override def +(table :CommonTableExpression.__)   = table.withClause
		override def +:(table :CommonTableExpression.__)  = table.withClause
		override def ++(tables :WithClause)  = tables
//		override def ++:(tables :WithClause) = tables
	}


	private trait OuterWithClause extends WithClause {
		override def local      :WithClause = this.empty
		override def outer      :WithClause = this
		override def suppressed :WithClause = this.empty
		override def all        :WithClause = this

		override def local_+(table :CommonTableExpression.__) :WithClause = {
			val default = table.default
			val clause = default.withClause
			val outer =
				if (contains(table)) iterator.filterNot(_ == default) to Unique
				else if (clause.sizeIs == 1) this.toUnique
				else iterator ++ clause.iterator.filterNot(_ == default) to Unique
			val suppressed =
				if (clause.suppressed.exists(contains))
					clause.suppressed.filterNot(contains)
				else
					clause.suppressed
			new SplitWithClause(Unique.single(default), outer, suppressed)
		}

		override def local_+:(table :CommonTableExpression.__) :WithClause = {
			val default = table.default
			val clause = default.withClause
			val outer =
				if (contains(table)) iterator.filterNot(_ == default) to Unique
				else if (clause.sizeIs == 1) this.toUnique
				else clause.iterator.filterNot(_ == default) ++ iterator to Unique
			val suppressed =
				if (clause.suppressed.exists(contains))
					clause.suppressed.filterNot(contains)
				else
					clause.suppressed
			new SplitWithClause(Unique.single(default), outer, suppressed)
		}
	}


	/** A `WithClause` combining three disjoint `Unique` collections from the `WithClause` API into a single collection.
	  * The parameter names reflect the source of the tables, not the contents of the standard subclauses:
	  * the `local` subset of this instance is empty, and all tables are instead included in `this.outer`
	  * (which is equal to `this`). The `suppressedCTEs` collection is included in the `WithClause` proper,
	  * and the `suppressed` part is empty.
	  * This implementation is used for the `WithClause.all` property of other clauses.
	  */
	private class CompleteWithClause(localCTEs :Unique[CommonTableExpression.__],
	                                 outerCTEs :Unique[CommonTableExpression.__],
	                                 suppressedCTEs :WithClause)
		extends OuterWithClause
	{
		override def size = knownSize
		override def knownSize = localCTEs.size + outerCTEs.size + suppressedCTEs.size
//		override def closure = this
		override def foreach[U](f :CommonTableExpression.__ => U) :Unit = {
			localCTEs foreach f
			outerCTEs foreach f
			suppressedCTEs foreach f
		}
		override def iterator = concat(concat(localCTEs.iterator, outerCTEs.iterator), suppressedCTEs.iterator)

		private def concat(first :Iterator[CommonTableExpression.__],
		                   second :Iterator[CommonTableExpression.__]) :Iterator[CommonTableExpression.__] =
			if (first.isEmpty) second
			else if (second.isEmpty) first
			else first ++ second

		override def contains(table :CommonTableExpression.__) = {
			val default = table.default
			localCTEs.contains(default) || outerCTEs.contains(default) || suppressedCTEs.contains(default)
		}

		override def +(table :CommonTableExpression.__) :WithClause =
			if (contains(table)) this
			else new CompleteWithClause(localCTEs, outerCTEs + table.default, suppressedCTEs - table.default)

		override def +:(table :CommonTableExpression.__) =
			if (contains(table)) this
			else new CompleteWithClause(localCTEs, table.default +: outerCTEs, suppressedCTEs - table.default)
	}



	/** A `WithClause` implementation which keeps all contained tables in three disjoint collections. */
	private class SplitWithClause(protected val localCTEs :Unique[CommonTableExpression.__],
	                              protected val outerCTEs :Unique[CommonTableExpression.__],
	                              override val suppressed :WithClause)
		extends WithClause
	{ //consider: we could have a more efficient implementation if it is complete and checks arguments for completeness
		override def knownSize = localCTEs.size + outerCTEs.size
		override def size = localCTEs.size + outerCTEs.size

		override def foreach[U](f :CommonTableExpression.__ => U) :Unit = {
			localCTEs foreach f
			outerCTEs foreach f
		}

		override def iterator :Iterator[CommonTableExpression.__] =
			if (localCTEs.isEmpty) outerCTEs.iterator
			else if (outerCTEs.isEmpty) localCTEs.iterator
			else localCTEs.iterator ++ outerCTEs.iterator

		override def toSeq :Seq[CommonTableExpression.__] =
			if (localCTEs.isEmpty) outerCTEs.toSeq
			else if (outerCTEs.isEmpty) localCTEs.toSeq
			else (outerCTEs.toList /: localCTEs.reverseIterator) { (res, cte) => cte::res }

		override def toUnique :Unique[CommonTableExpression.__] = localCTEs ++ outerCTEs

		override def contains(table :CommonTableExpression.__) :Boolean =
			outerCTEs.contains(table.default) || localCTEs.contains(table.default)

		override lazy val local :WithClause =
			if (outerCTEs.isEmpty) this
			else if (localCTEs.isEmpty && suppressed.isEmpty) this.empty
			else new SplitWithClause(localCTEs, Unique.empty, suppressed)

		override lazy val outer :WithClause =
			if (localCTEs.isEmpty) this
//			else if (outerCTEs.isEmpty) new SuppressedEmptyWithClause(localCTEs ++ suppressed)
			else if (suppressed.isEmpty) new SplitWithClause(Unique.empty, outerCTEs, suppressed)
			else new SplitWithClause(Unique.empty, outerCTEs, (outerBuilder ++= localCTEs ++= suppressed).result())

		override lazy val all = new CompleteWithClause(localCTEs, outerCTEs, suppressed)

//		override def closure = this

		override def local_+(table :CommonTableExpression.__) :WithClause =
			if (localCTEs.contains(table.default))
				this
			else if (outerCTEs.contains(table.default))
				new SplitWithClause(localCTEs + table.default, outerCTEs - table.default, suppressed)
			else
				new SplitWithClause(localCTEs + table.default, outerCTEs, suppressed - table.default)

		override def local_+:(table :CommonTableExpression.__) :WithClause =
			if (localCTEs.contains(table.default))
				this
			else if (outerCTEs.contains(table.default))
				new SplitWithClause(table.default +: localCTEs, outerCTEs - table.default, suppressed)
			else
				new SplitWithClause(table.default +: localCTEs, outerCTEs, suppressed - table.default)

		override def local_++(tables :WithClause) :WithClause =
			if (tables.isEmpty && tables.suppressed.isEmpty)
				this
			else {
				val local = localCTEs ++ tables
				val outer = addAbsent(outerCTEs, tables, this.empty, this.empty)
				val suppressed = addAbsent(this.suppressed, tables, tables.suppressed, this)
				new SplitWithClause(local, outer to Unique, suppressed to WithClause)
			}

		override def local_++(tables :IterableOnce[CommonTableExpression.__]) :WithClause = tables match {
			case clause :WithClause => local_++(clause) //first because of suppressed
			case it :Iterable[_] if it.isEmpty => this
			case it :Iterator[_] if it.isEmpty => this
			case it :Iterable[CommonTableExpression.__] if it.sizeIs == 1 => this local_+ it.head
			case _ =>
				val other = tables.iterator.map(_.default) to Unique
				val local = localCTEs ++ other
				val outer = outerCTEs -- other
				val suppressed =
					if (this.suppressed.isEmpty) this.suppressed
					else this.suppressed.filterNot(t => other.contains(t) || outer.contains(t))
				new SplitWithClause(local, outer, suppressed)
		}

		override def +(table :CommonTableExpression.__) :WithClause =
			if (contains(table)) this
			else new SplitWithClause(localCTEs, outerCTEs + table.default, suppressed - table.default)

		override def +:(table :CommonTableExpression.__) :WithClause =
			if (contains(table)) this
			else new SplitWithClause(localCTEs, table.default +: outerCTEs, suppressed - table.default)

		override def ++(tables :WithClause) :WithClause =
			if (tables.isEmpty && tables.suppressed.isEmpty)
				this
			else if (this.isEmpty && suppressed.isEmpty)
				tables
			else {
				val otherLocal = tables.local
				val local = localCTEs ++ otherLocal
				val outer = addAbsent(outerCTEs, otherLocal, tables.outer, this.local)
				val suppressed = addAbsent(this.suppressed, tables, tables.suppressed, this)
				new SplitWithClause(local, outer to Unique, suppressed to WithClause.outer)
			}
	}





	@inline private def isEmpty(tables :IterableOnce[CommonTableExpression.__]) :Boolean = tables match {
		case it :Iterable[_] => it.isEmpty
		case _ => !tables.iterator.hasNext
	}
	@inline private def toUnique(tables :IterableOnce[CommonTableExpression.__]) = tables match {
		case it :Iterable[CommonTableExpression.__] => it to Unique
		case _ => tables.iterator to Unique
	}

	private def fromParts(local :IterableOnce[CommonTableExpression.__], outer :IterableOnce[CommonTableExpression.__],
	                      suppressed :IterableOnce[CommonTableExpression.__]) :WithClause =
	{
		val newSuppressed = suppressed match {
			case tables :WithClause if tables.isOuter && tables.suppressed.isEmpty => tables
			case empty :Iterable[_] if empty.isEmpty => WithClause.empty
			case unique :Unique[CommonTableExpression.__] => new SplitWithClause(Unique.empty, unique, empty)
			case tables :Iterable[CommonTableExpression.__] if tables.sizeIs == 1 =>
				val clause = tables.head.withClause
				if (clause.isOuter && clause.suppressed.isEmpty) clause
				else new SplitWithClause(Unique.empty, Unique.single(tables.head), empty)
			case empty if !empty.iterator.hasNext => WithClause.empty
			case _ => new SplitWithClause(Unique.empty, toUnique(suppressed), empty)
		}
		if (isEmpty(local))
			if (isEmpty(outer))
				if (newSuppressed.isEmpty) empty
				else new SplitWithClause(Unique.empty, Unique.empty, newSuppressed)
			else
				if (newSuppressed.isEmpty) outer match {
					case tables :WithClause if tables.isOuter && tables.suppressed.isEmpty => tables
					case _ => new SplitWithClause(Unique.empty, toUnique(outer), newSuppressed)
				} else
					new SplitWithClause(Unique.empty, toUnique(outer), newSuppressed)
		else local match {
				case tables :WithClause
						if newSuppressed.isEmpty && isEmpty(outer) && tables.isLocal && tables.suppressed.isEmpty =>
					tables
				case _ =>
					new SplitWithClause(toUnique(local), toUnique(outer), newSuppressed)
			}
	}


	private def disjoint(first :WithClause, second :WithClause) :Boolean =
		(first.size, second.knownSize) match {
			case (0, _) => true
			case (_, 0) => true
			case (1, _) => !second.contains(first.head)
			case (_, 1) => !first.contains(second.head)
//			case (_, -1) => !second.exists(first.contains)
			case (a, b) if a >= b => !first.exists(second.contains)
			case _ => !second.exists(first.contains)
		}
	private def disjoint(first :Unique[CommonTableExpression.__], second :WithClause) :Boolean =
		(first.size, second.knownSize) match {
			case (0, _) => true
			case (_, 0) => true
			case (1, _) => !second.contains(first.head)
			case (_, 1) => !first.contains(second.head)
			case (_, -1) => !second.exists(first.contains)
			case (a, b) if a >= b => !first.exists(second.contains)
			case _ => !second.exists(first.contains)
		}


	private def addAbsent(left :Unique[CommonTableExpression.__], excludedLeft :WithClause,
	                      right :WithClause, excludedRight :WithClause)
			:Iterable[CommonTableExpression.__] =
		if (right.sizeIs == 0)
			if (disjoint(left, excludedLeft)) left
			else left -- excludedLeft
		else if (left.size == 0)
			right -- excludedRight
		else {
			val leftOk = disjoint(left, excludedLeft)
			val rightOk = disjoint(excludedRight, right)
			val leftLeft = if (leftOk) left else left.view.filterNot(excludedLeft.contains)
			val rightLeft = if (rightOk) right else right.iterator.filterNot(excludedRight.contains)
			leftLeft ++ rightLeft
		}
	private def addAbsent(left :WithClause, excludedLeft :WithClause,
	                      right :WithClause, excludedRight :WithClause)
			:Iterable[CommonTableExpression.__] =
		if (right.sizeIs == 0)
			if (disjoint(left, excludedLeft)) left
			else left -- excludedLeft
		else if (left.size == 0)
			right -- excludedRight
		else {
			val leftOk = disjoint(left, excludedLeft)
			val rightOk = disjoint(right, excludedRight)
			val leftLeft = if (leftOk) left else left.view.filterNot(excludedLeft.contains)
			val rightLeft = if (rightOk) right else right.iterator.filterNot(excludedRight.contains)
			leftLeft ++ rightLeft
		}


	private final val localHashSeed = "FlatLocalWithClause".hashCode
	private final val outerHashSeed = "OuterWithClause".hashCode
}



