package net.noresttherein.oldsql.sql

import scala.collection.{EvidenceIterableFactory, IterableFactory}

import net.noresttherein.oldsql.collection.{Chain, Listing, Opt}
import net.noresttherein.oldsql.collection.Chain.ChainApplication
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{IllegalSQLException, InseparableExpressionException}
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.schema.{ColumnMapping, Relation, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult
import net.noresttherein.oldsql.sql.RowProduct.{GroundFrom, TopFrom}
import net.noresttherein.oldsql.sql.Query.{CompoundSelect, CompoundSelectMapping, MappingQuery, QueryTemplate}
import net.noresttherein.oldsql.sql.Select.{ArbitrarySelect, Minus, SelectTemplate, SetOperator}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.LocalScope
import net.noresttherein.oldsql.sql.ast.{ConversionSQL, QuerySQL, SelectSQL, TupleSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{ComponentSQL, RelationSQL}
import net.noresttherein.oldsql.sql.ast.QuerySQL.{ColumnMappingQuery, ColumnQuery, CompoundSelectColumn, CompoundSelectColumnMapping, CompoundSelectMappingSQL, CompoundSelectSQL, MappingQuerySQL}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ListingSQL
import net.noresttherein.oldsql.sql.ast.TupleSQL.ListingSQL.{ListingColumn, ListingValueSQL}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{TopSelectAs, TopSelectMapping, TopSelectSQL}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** An SQL statement returning a row cursor. It is the common base type for
  * the [[net.noresttherein.oldsql.sql.Select Select]] type hierarchy and
  * set operations on them (such as `UNION`): [[net.noresttherein.oldsql.sql.Query.CompoundSelect CompoundSelect]].
  */ //todo: batching; common supertype with DMLStatement
trait Query[P, V] extends QueryTemplate[V, ({ type Q[X] = Query[P, X] })#Q] with Serializable {
	def rowForm :SQLReadForm[V]
	//overrides to grant access to classes located in the companion object
//	protected override def component[O] :ResultMapping[O]
//	protected override def export[O] :RefinedMapping[ResultMapping[O]#Subject, O] //= component[O]

	//semantics of unions and rest:
	// - if the column set is the same, normal union
	// - if not, but the mapping is the same, than missing columns are added with null values to each operand
	// - if both are mapping-based, with mappings from the same hierarchy, use union column set with a discriminator column
	// - otherwise the column set becomes two separate sets with a discriminator
	def union(other :Query[P, V]) :Query[P, V] = Select.Union(this, other)
	def unionAll(other :Query[P, V]) :Query[P, V] = Select.UnionAll(this, other)
	def minus(other :Query[P, V]) :Query[P, V] = Select.Minus(this, other)
	def intersect(other :Query[P, V]) :Query[P, V] = Select.Intersect(this, other)

	/** Replaces all [[net.noresttherein.oldsql.sql.UnboundParam unbound]] parameters in this instance with
	  * [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter bound]] parameters of values taken from the argument,
	  * turning this parameterized query statement
	  * into a ground [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]].
	  */
	def bind(params :P) :QuerySQL[RowProduct, V]

	/** Converts this query into its textual representation for the DBMS specified implicitly by the `spelling` argument,
	  * with additional meta data about query parameters. This is a simple caching forwarder to
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.spell[P,V](query:Query[P,V])* spell]] method
	  * of [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] and should not be generally overriden
	  * by subclasses unless to enrich the result with additional information without changing the complete SQL.
	  * Implement [[net.noresttherein.oldsql.sql.Query.defaultSpelling(context:SQLContext)(implicit spelling:SQLSpelling)]]
	  * in order to define how this AST node should be formatted as SQL instead.
	  * It is a lower level method and, in most cases, the top-level method
	  * [[net.noresttherein.oldsql.sql.Query.chant chant]] should be used should be used instead, unless specifically
	  * in order to create an [[net.noresttherein.oldsql.sql.Incantation Incantation]].
	  */
	@throws[InseparableExpressionException]("if a subexpression cannot be separated into individual column strings, " +
	                                        "for example a multi-column SQL select.")
	@throws[IllegalSQLException]("if the expression is dedicated to a particular DMBS and requires a more specific " +
		                         "SQLSpelling implementation than the provided implicit spelling argument.")
	def spell(implicit spelling :SQLSpelling = StandardSQL.spelling) :SpelledSQL[P, RowProduct] =
		cachedSpelling match {
			case null =>
				val sql = spelling.spell(this)
				cachedSQL = sql
				cachedSpelling = spelling
				sql
			case s if s == spelling => cachedSQL
			case _ => spelling.spell(this)
		}

	@volatile private var cachedSQL :SpelledSQL[P, RowProduct] = _
	@volatile private var cachedSpelling :SQLSpelling = _


	/** Generates the SQL `String` for this query as a parameterized expression. This is a fallback method
	  * used by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] if no non-standard representation
	  * is required for the used DBMS. Subclasses should return standard SQL unless the class itself is dedicated
	  * to a particular DBMS.
	  */
	@throws[InseparableExpressionException]("if a subexpression cannot be separated into individual column strings, " +
	                                        "for example a multi-column SQL select.")
	@throws[IllegalSQLException]("if the expression is dedicated to a particular DBMS and requires a more specific " +
	                             "SQLSpelling implementation than the provided implicit spelling argument.")
	protected def defaultSpelling(context :SQLContext)(implicit spelling :SQLSpelling) :SpelledSQL[P, RowProduct]

	private[oldsql] final def defaultSpelling(spelling :SQLSpelling, context :SQLContext)
			:SpelledSQL[P, RowProduct] =
		defaultSpelling(context)(spelling)


	/** Converts this query SQL AST into an executable [[net.noresttherein.oldsql.sql.Incantation Incantation]]
	  * proper for the DBMS using the implicit [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]].
	  * The implementation is delegated to the dialect object.
	  */
	@throws[IllegalSQLException]("if the expression cannot be rendered as SQL for the DBMS particular to the given dialect.")
	def chant[R](implicit composition :StatementResult[V, R], dialect :SQLDialect = StandardSQL) :Incantation[P, R] =
		dialect(this)

	/** Converts this query SQL AST into an executable
	  * [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[P, C[V]]` proper to the implicit
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]. The results of the query will be returned as
	  * a collection `C[V]` with the given factory.
	  */
	def returnAs[C[_]](collection :IterableFactory[C])
	                  (implicit dialect :Maybe[SQLDialect]) :Incantation[P, C[V]] =
		chant(StatementResult(collection)(rowForm), dialect getOrElse StandardSQL)

	/** Converts this query SQL AST into an executable
	  * [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[P, C[V]]` proper to the implicit
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]. The results of the query will be returned as
	  * a collection `C[V]` with the given factory.
	  */
	def returnAs[C[_], E[_]](collection :EvidenceIterableFactory[C, E])
	                  (implicit ev :E[V], dialect :Maybe[SQLDialect]) :Incantation[P, C[V]] =
		chant(StatementResult(collection)(ev, rowForm), dialect getOrElse StandardSQL)

}






object Query {

	/** A template for classes representing generalized ''selects'' - both standard ''select'' queries and
	  * compound ''selects'' combining several individual ''selects'' with set operators.
	  * It is inherited by both [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]] and
	  * [[net.noresttherein.oldsql.sql.Query Query]].
	  * @tparam V value type representing the whole ''select'' clause, used as its return type and
	  *           [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] value type.
	  * @tparam S the self type of this interface, that is the whole public type of the ''select'' parameterized
	  *           with its value type `V`.
	  */
	trait QueryTemplate[V, +S[_]] {
		type ResultMapping[O] <: MappingAt[O]
		def mapping[O] :ResultMapping[O]
		def export[O] :RefinedMapping[ResultMapping[O]#Subject, O]

		def map[X](f :V => X) :S[X]

		def map[Fun, C <: Chain, X](f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C) :S[X] =
			map(applyFun(f))

		protected def applyFun[Fun, C <: Chain, X]
		                      (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C) :V => X =
			{ v => application(f, isChain(v)) }

		def canEqual(that :Any) :Boolean = that.getClass == getClass
	}



	type * = Query[_, _]




	/** An SQL query, that is an SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL.SelectAs select]] or
	  * a [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectMappingSQL set operation]] on them,
	  * which provides a [[net.noresttherein.oldsql.schema.Mapping Mapping]] for the returned rows.
	  */
	trait MappingQuery[P, M[O] <: MappingAt[O]] extends Query[P, M[Unit]#Subject] {
		override type ResultMapping[O] = M[O]

		def union(other :MappingQuery[P, M]) :MappingQuery[P, M] = Select.Union(this, other)
		def unionAll(other :MappingQuery[P, M]) :MappingQuery[P, M] = Select.UnionAll(this, other)
		def minus(other :MappingQuery[P, M]) :MappingQuery[P, M] = Select.Minus(this, other)
		def intersect(other :MappingQuery[P, M]) :MappingQuery[P, M] = Select.Intersect(this, other)

		override def bind(params :P) :MappingQuerySQL[RowProduct, M]
	}




	/** Implements a set operation combining the result sets of two
	  * [[net.noresttherein.oldsql.sql.Select parameterized selects]]
	  * (or other [[net.noresttherein.oldsql.sql.Query queries]]). The kind of operation is defined by
	  * the [[net.noresttherein.oldsql.sql.Select.SetOperator SetOperator]]
	  * of the [[net.noresttherein.oldsql.sql.Query.CompoundSelect.operator operator]] member property.
	  * The row schemas of both arguments must match or an exception will be thrown when this expression
	  * is converted into an executable SQL statement. If the schema of any of the member ''selects'' is flexible
	  * (it is defined by a mapping with [[net.noresttherein.oldsql.schema.Buff.OptionalSelect Optional]] columns),
	  * the schema of the first member is used for both of the arguments.
	  */
	trait CompoundSelect[P, V] extends Query[P, V] {
		validateCompatibility()

		val left :Query[P, V]
		val right :Query[P, V]
		val operator :SetOperator

		override def rowForm :SQLReadForm[V] = left.rowForm

		override def map[X](f :V => X) :Query[P, X] = CompoundSelect(left.map(f), operator, right.map(f))

		override def bind(params :P) :QuerySQL[RowProduct, V] = operator(left.bind(params), right.bind(params))

		protected override def defaultSpelling(context :SQLContext)(implicit spelling :SQLSpelling)
				:SpelledSQL[P, RowProduct] =
		{
			val l = left match {
				case CompoundSelect(_, op, _) if op != operator =>
					"(" +: (spelling(left)(context) + ")")
				case _ =>
					spelling(left)(context)
			}
			val r = right match {
				case CompoundSelect(_, op, _) if op != operator || operator == Minus =>
					"(" +: (spelling(right)(context) + ")")
				case _ =>
					spelling(right)(context)
			}
			SpelledSQL(l.sql + (" " + spelling(operator) +" ") + r.sql, context, l.params :++ r.params)
		}


		/** Validates the compatibility of `left` and `right` queries, throwing an `IllegalArgumentException`
		  * if a compound select combining the pair would be illegal. This method is called as part of this trait's
		  * initialization, but depends on `left` and `right` fields, meaning they must be declared by subclasses
		  * as constructor fields. Default implementation verifies only if column counts of the forms are equal
		  * and not zero, but it can be overriden by subclasses.
		  */
		protected def validateCompatibility() :Unit = { //todo: allow different column sets for the same table
		}


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :CompoundSelect[_, _] if canEqual(other) && other.canEqual(this) =>
				operator == other.operator && left == other.left && right == other.right
			case _ => false
		}

		override def hashCode :Int = (operator.hashCode * 31 + left.hashCode) * 31 + right.hashCode

		override def toString :String = s"($left) $operator ($right)"
	}


	object CompoundSelect {
		def apply[P, V](left :Query[P, V], operator :SetOperator, right :Query[P, V])
				:CompoundSelect[P, V] =
			new BaseCompoundSelect(left, operator, right)

		def unapply[P, V](query :Query[P, V]) :Opt[(Query[P, V], SetOperator, Query[P, V])] =
			query match {
				case op :CompoundSelect[P @unchecked, V @unchecked] => Got((op.left, op.operator, op.right))
				case _ => Lack
			}

		private[Query] class BaseCompoundSelect[P, V]
		                     (override val left :Query[P, V], override val operator :SetOperator,
		                      override val right :Query[P, V])
			extends CompoundSelect[P, V]
		{
			override type ResultMapping[O] = left.ResultMapping[O]
			override def mapping[O] = left.mapping[O]
			override def export[O] = left.export[O] //todo: this should involve some reconciliation
		}
	}




	/** Implements a set operation combining the result sets of two parameterized
	  * [[net.noresttherein.oldsql.sql.Select.SelectMapping selects]]
	  * (or other [[net.noresttherein.oldsql.sql.Query.MappingQuery queries]]), sharing the same row schema,
	  * as defined by the shared [[net.noresttherein.oldsql.schema.Mapping mapping]] type `M`. The kind of operation
	  * is defined by the [[net.noresttherein.oldsql.sql.Select.SetOperator SetOperator]]
	  * of the [[net.noresttherein.oldsql.sql.Query.CompoundSelect.operator operator]] member property.
	  */
	trait CompoundSelectMapping[P, M[O] <: MappingAt[O]]
		extends CompoundSelect[P, M[Unit]#Subject] with MappingQuery[P, M]
	{
		override val left :MappingQuery[P, M]
		override val right :MappingQuery[P, M]

		override def mapping[O] = left.mapping
		override def export[O] = left.export //todo: this should involve some reconciliation

		override def bind(params :P) :MappingQuerySQL[RowProduct, M] = operator(left.bind(params), right.bind(params))
	}


	object CompoundSelectMapping {
		def apply[P, M[O] <: MappingAt[O]]
		         (left :MappingQuery[P, M], operator :SetOperator, right :MappingQuery[P, M])
				:CompoundSelectMapping[P, M] =
			new BaseCompoundSelectMapping(left, operator, right)

		def unapply[P, V](query :Query[P, V])
				:Opt[(MappingQuery[P, M], SetOperator, MappingQuery[P, M]) forSome { type M[O] <: MappingAt[O] }] =
			query match {
				case op :CompoundSelectMapping[P @unchecked, MappingAt @unchecked] =>
					Got((op.left, op.operator, op.right))
				case _ => Lack
			}

		private[Query] class BaseCompoundSelectMapping[P, M[O] <: MappingAt[O]]
		                     (override val left :MappingQuery[P, M], override val operator :SetOperator,
		                      override val right :MappingQuery[P, M])
			extends CompoundSelectMapping[P, M]

	}

}






/**
  * @author Marcin Mościcki
  */
trait Select[P, V] extends Query[P, V] with SelectTemplate[V, ({ type S[X] = Select[P, X] })#S] {

	/** An empty parameterization (without any setter forms) for the ''from'' clause of this ''select''.
	  * Normally initialized with [[net.noresttherein.oldsql.sql.RowProduct.parameterization RowProduct.parameterization]]
	  */
	def parameterization :Parameterization[P, From]

	/** The from clause of this select. */
	override type From <: TopFrom { type Params = P }

	override def rowForm :SQLReadForm[V] = selectClause.readForm

	override def map[X](f :V => X) :Select[P, X] =
		new ArbitrarySelect[P, From, X](from, selectClause.map(f), parameterization, isDistinct)

	protected def reverseCollect[X](fun: PartialFunction[SQLExpression.*, X], acc: List[X]): List[X] = {
		//we ignore filters in the implicit portion as, if this is a subselect, they would be collected by the enclosing expression.
		val headerItems = selectClause.reverseCollectForwarder(fun, acc)
		from.filter.reverseCollectForwarder(fun, from match { //todo: make this a method in RowProduct
			case GroupByClause(ungrouped) => ungrouped.filter.reverseCollectForwarder(fun, headerItems)
			case _ => headerItems
		})
	}


	def bind(params :P) :TopSelectSQL[V]


	protected override def defaultSpelling(context :SQLContext)(implicit spelling :SQLSpelling)
			:SpelledSQL[P, RowProduct] =
	{
		val fromSQL = spelling(from)(context)
		val selectSQL = spelling.inSelect(selectClause)(fromSQL.context, parameterization)
		val allParams = fromSQL.params :++ selectSQL.params
		val select = SpelledSQL(spelling.SELECT + " " + selectSQL.sql, context, allParams)
		if (fromSQL.sql.isEmpty)
			select
		else
			select + " " + fromSQL.sql
	}


//	override def canEqual(that :Any) :Boolean = that.getClass == getClass
}







object Select {
	//todo: mapping indexed headers


	@inline def apply(from :TopFrom) :ParamSelectFactory[from.type] = new ParamSelectFactory[from.type](from)

	class ParamSelectFactory[F <: TopFrom](val from :F) extends AnyVal {
		def apply[M[O] <: BaseMapping[V, O], V](header :ComponentSQL[from.Generalized, M]) :SelectMapping[from.Params, M] =
			new SelectComponent[from.Params, from.Self, M, V](from.self, header, from.parameterization)

		def apply[V <: Chain](header :SQLExpression[from.Generalized, LocalScope, V]) :Select[from.Params, V] =
			new ArbitrarySelect[from.Params, from.Self, V](
				from.self, header.anchor(from.self), from.parameterization
			)

		def apply[V](header :TupleSQL[from.Generalized, LocalScope, V]) :Select[from.Params, V] =
			new ArbitrarySelect[from.Params, from.Self, V](
				from.self, header.anchor(from.self), from.parameterization
			)

		def apply[V <: Listing](header :ListingSQL[from.Generalized, LocalScope, V])
				:SelectMapping[from.Params, IndexedMapping.Of[V]#Projection] =
			new IndexedSelect(from.self, header, from.parameterization)

		//an unused type parameter due to an overloading resolution bug in scala 2
		def apply[X, Y, _](header :ConversionSQL[from.Generalized, LocalScope, X, Y]) :Select[from.Params, Y] =
			new ArbitrarySelect[from.Params, from.Self, Y](
				from.self, header.anchor(from.self), from.parameterization
			)

		def apply[A <: Label, V](header :ListingColumn[from.Generalized, LocalScope, A, V])
				:SelectMapping[from.Params, IndexedMapping.Of[V]#Column] =
			new IndexedColumnSelect[from.Params, from.Self, A, V](
				from.self, ListingColumnSQLMapping[from.Self, LocalScope, A, V, Unit](header.anchor(from.self)),
				from.parameterization
			)

		def apply[V](header :ColumnSQL[from.Generalized, LocalScope, V]) :Select[from.Params, V] =
			new ArbitrarySelect[from.Params, from.Self, V](
				from.self, header.anchor(from.self), from.parameterization
			)
	}




	/** An SQL binary operator which can be used to create a query out of two SQL ''selects'', by combining their
	  * result sets. The most notable instance is [[net.noresttherein.oldsql.sql.Select.Union UNION]], but other
	  * predefined operators provided by some database engines have also their definition in the enclosing
	  * [[net.noresttherein.oldsql.sql.Select Select]] object.
	  */
	case class SetOperator(name :String) {
		def NAME :String = name.toUpperCase

		def apply[P, V](left :Query[P, V], right :Query[P, V]) :Query[P, V] =
			CompoundSelect(left, this, right)

		def apply[P, M[O] <: MappingAt[O]]
		         (left :MappingQuery[P, M], right :MappingQuery[P, M]) :MappingQuery[P, M] =
			CompoundSelectMapping(left, this, right)


		def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V]) :QuerySQL[F, V] =
			CompoundSelectSQL(left, this, right)

		def apply[F <: RowProduct, V](left :ColumnQuery[F, V], right :ColumnQuery[F, V]) :ColumnQuery[F, V] =
			CompoundSelectColumn(left, this, right)

		def apply[F <: RowProduct, M[O] <: MappingAt[O]]
		         (left :MappingQuerySQL[F, M], right :MappingQuerySQL[F, M]) :MappingQuerySQL[F, M] =
			CompoundSelectMappingSQL(left, this, right)

		def apply[F <: RowProduct, M[O] <: ColumnMapping[V, O], V]
		         (left :ColumnMappingQuery[F, M, V], right :ColumnMappingQuery[F, M, V]) :ColumnMappingQuery[F, M, V] =
			CompoundSelectColumnMapping(left, this, right)


		override def toString :String = name
	}


	/** A union operator combining two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). It follows the semantics of set union,
	  * with no duplicate rows in the returned result set.
	  */
	final val Union = new SetOperator("union")

	/** A union operator combining two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). This is a multiset variant, with every row
	  * from either of the ''selects'' mapping to a single row in the returned result set; if a row is present
	  * in both ''selects'' (or it has duplicates within either of them), each occurrence will be represented by
	  * a separate row in the result.
	  */
	final val UnionAll = new SetOperator("union all")

	/** An operator implementing set difference between two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). The query created will return every row
	  * from the first (left) argument which is not present in the second (right) argument.
	  */
	final val Minus = new SetOperator("minus")

	/** An operator implementing set intersection of two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). The query created will return all rows
	  * which are present in both of its arguments, with every row occurring exactly once, regardless of the number
	  * of duplicates in the input ''selects''.
	  */
	final val Intersect = new SetOperator("intersect")




	/** A template for classes representing SQL ''selects'' - both standard SQL
	  * [[net.noresttherein.oldsql.sql.ast.SelectSQL expressions]] and
	  * [[net.noresttherein.oldsql.sql.Select parameterized]] ''selects'', including their compound forms.
	  * @tparam V value type representing the whole ''select'' clause, used as its return type and
	  *           [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] value type.
	  * @tparam S the self type of this interface, that is the whole public type of the ''select'' parameterized
	  *           with its value type `V`.
	  */
	trait SelectTemplate[V, +S[_]] extends QueryTemplate[V, S] {
		/** The from clause of this select. */
		type From <: RowProduct

		trait SelectedColumn[X] extends Serializable {
			def name :String
			def expr :ColumnSQL[From, LocalScope, X]
			override def toString :String = expr.toString + " as " + name
		}

		val from :From
		val selectClause :SQLExpression[From, LocalScope, V]
		def columns :Seq[SelectedColumn[_]]

		//caution: in group by queries this returns the elements of the group by clause, not the actual from clause
		def relations :Seq[RelationSQL.AnyIn[from.Generalized]] = from.tableStack.reverse
		def tables :Seq[Relation.*] = from.fromClause.tableStack.reverse.map(_.relation :Relation[MappingAt]).toList

		def isDistinct :Boolean
		def distinct :S[V]


		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case s :AnyRef if s eq this => true
			case s :Select[_, _] if s canEqual this=>
				isDistinct == s.isDistinct && s.selectClause == selectClause && s.from == from
			case _ => false
		}

		override def hashCode :Int = (selectClause.hashCode * 31 + from.hashCode) * 31 + isDistinct.hashCode

		override def toString :String =
			if (isDistinct) s"SELECT DISTINCT $selectClause FROM $from"
			else  s"SELECT $selectClause FROM $from"
	}



	type * = Select[_, _]


	/** A parameterized ''select'' interface exposing the mapping type `H` used for the ''select'' clause. */
	trait SelectMapping[P, H[A] <: MappingAt[A]] extends Select[P, H[Unit]#Subject] with MappingQuery[P, H] {
		override type ResultMapping[O] = H[O]

		override def distinct :SelectMapping[P, H]

		override def bind(params :P) :TopSelectAs[H]
	}




	private class SelectComponent[P, F <: TopFrom { type Params = P }, H[A] <: BaseMapping[V, A], V]
	                             (override val from :F, override val selectClause :ComponentSQL[F, H],
	                              override val parameterization :Parameterization[P, F],
	                              override val isDistinct :Boolean = false)
		extends SelectMapping[P, H]
	{
		override type From = F

		override def mapping[O] :ResultMapping[O] = selectClause.mapping.withOrigin[O]
		override def export[O] :RefinedMapping[V, O] = selectClause.export.withOrigin[O]

		override val columns: Seq[SelectedColumn[_]] = //todo: is this the place where we finally decide on the column set?
			selectClause.export.selectedByDefault.toSeq.map(include(_))

		private def include[X](column :ColumnMapping[X, selectClause.Origin]) :SelectedColumn[X] =
			new SelectedColumn[X] {
				override val name :String = column.name
				override val expr  = selectClause \ column
			}

		override def distinct :SelectMapping[P, H] =
			if (isDistinct) this
			else new SelectComponent[P, F, H, V](from, selectClause, parameterization, true)

		override def bind(params :P) :TopSelectAs[H] = {
			val paramless = from.bind(params)
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
			val select = SelectSQL(
				paramless.asInstanceOf[GroundFrom], header.asInstanceOf[ComponentSQL[GroundFrom, H]]
			) :TopSelectMapping[GroundFrom, H, V]
			if (isDistinct) select.distinct else select
		}
	}




	/** A select expression based on the given row source and selecting an arbitrary expression `header` in its ''select''
	  * clause. This header will be translated by recursively flat mapping the header expression to obtain a flat sequence
	  * of columns.
	  */
	private[sql] abstract class BaseArbitrarySelect[P, F <: TopFrom { type Params = P }, V] protected
	                            (override val from :F, protected val result :SQLMapping[F, LocalScope, V, Unit])
		extends Select[P, V]
	{
		def this(from :F, header :SQLExpression[F, LocalScope, V]) =
			this(from, SQLMapping[F, LocalScope, V, Unit](header))

		override type From = F

		override val selectClause = result.expr

		/** A column in the header of owning select.
		  * @param column the `ColumnMapping` implementation based on a `ColumnSQL` expression
		  *               providing the value for the column.
		  */
		protected class HeaderColumn[T](column :ColumnSQLMapping[F, LocalScope, T, _])
			extends SelectedColumn[T]
		{
			override def expr :ColumnSQL[F, LocalScope, T] = column.expr
			override def name :String = column.name
		}

		override val columns :Seq[SelectedColumn[_]] = result.columns.map { col => new HeaderColumn(col) }

		override def bind(params :P) :TopSelectSQL[V] = {
			val paramless = from.bind(params).asInstanceOf[GroundFrom]
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
			val select = header.topSelectFrom(paramless)
			if (isDistinct) select.distinct else select
		}
	}


	private[sql] trait ArbitrarySelectTemplate[P, F <: TopFrom { type Params = P }, M[O] <: BaseMapping[V, O], V]
		extends Select[P, V]
	{ this :BaseArbitrarySelect[P, F, V] =>
		override type ResultMapping[O] = M[O]

		protected val result :M[Unit]
		override def mapping[O] :M[O] = (this :ArbitrarySelectTemplate[P, F, M, V]).result.withOrigin[O]
		override def export[O] :M[O] = mapping[O]
	}




	private[sql] class ArbitrarySelect[P, F <: TopFrom { type Params = P }, V]
	                   (override val from :F, protected override val result :SQLMapping[F, LocalScope, V, Unit],
	                    override val parameterization :Parameterization[P, F],
	                    override val isDistinct :Boolean = false)
		extends BaseArbitrarySelect[P, F, V](from, result)
			with ArbitrarySelectTemplate[P, F, SQLMapping.Project[F, LocalScope, V]#Expression, V]
	{
		def this(from :F, expression :SQLExpression[F, LocalScope, V],
		         params :Parameterization[P, F], isDistinct :Boolean) =
			this(from, SQLMapping[F, LocalScope, V, Unit](expression), params, isDistinct)

		def this(from :F, expression :SQLExpression[F, LocalScope, V], params :Parameterization[P, F]) =
			this(from, SQLMapping[F, LocalScope, V, Unit](expression), params)

		override def distinct :Select[P, V] =
			if (isDistinct) this else new ArbitrarySelect(from, result, parameterization, true)
	}




	private[sql] class IndexedSelect[P, F <: TopFrom { type Params = P }, V <: Listing]
	                  (override val from :F, override val result :ListingSQLMapping[F, LocalScope, V, Unit],
	                   override val parameterization :Parameterization[P, F],
	                   override val isDistinct :Boolean = false)
		extends BaseArbitrarySelect[P, F, V](from, result)
			with ArbitrarySelectTemplate[P, F, IndexedMapping.Of[V]#Projection, V]
			with SelectMapping[P, IndexedMapping.Of[V]#Projection]
	{
		def this(from :F, expression :ListingValueSQL[F, LocalScope, V], params :Parameterization[P, F]) =
			this(from, expression.mapping[Unit], params)

		override val selectClause = result.expr

		override def distinct :SelectMapping[P, IndexedMapping.Of[V]#Projection] =
			if (isDistinct) this else new IndexedSelect[P, F, V](from, result, parameterization, true)

		override def bind(params :P) :TopSelectAs[IndexedMapping.Of[V]#Projection] = {
			val paramless = from.bind(params).asInstanceOf[GroundFrom]
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
				.asInstanceOf[ListingSQL[GroundFrom, LocalScope, V]]
			val select = header.topSelectFrom(paramless)
			if (isDistinct) select.distinct else select
		}
	}


	private[sql] class IndexedColumnSelect[P, F <: TopFrom { type Params = P }, A <: Label, V]
	                   (override val from :F, override val result :ListingColumnSQLMapping[F, LocalScope, A, V, Unit],
	                    override val parameterization :Parameterization[P, F],
	                    override val isDistinct :Boolean = false)
		extends BaseArbitrarySelect[P, F, V](from, result)
		   with ArbitrarySelectTemplate[P, F, IndexedMapping.Of[V]#Column, V]
		   with SelectMapping[P, IndexedMapping.Of[V]#Column]
	{
//		def this(from :F, selectClause :ListingColumn[F, LocalScope, A, V]) =
//			this(from, ListingColumnSQLMapping[F, LocalScope, A, V, ()](selectClause))

		override val selectClause = result.expr

		override def distinct :SelectMapping[P, IndexedMapping.Of[V]#Column] =
			if (isDistinct) this
			else new IndexedColumnSelect[P, F, A, V](from, result, parameterization, true)

		override def bind(params :P) :TopSelectAs[IndexedMapping.Of[V]#Column] = {
			val paramless = from.bind(params).asInstanceOf[GroundFrom]
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
				.asInstanceOf[ListingColumn[GroundFrom, LocalScope, A, V]]
			val select = header.topSelectFrom(paramless)
			if (isDistinct) select.distinct else select
		}
	}

}

