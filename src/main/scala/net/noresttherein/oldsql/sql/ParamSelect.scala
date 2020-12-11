package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.{Chain, IndexedChain}
import net.noresttherein.oldsql.collection.Chain.ChainApplication
import net.noresttherein.oldsql.schema.{ColumnMapping, Relation}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.ParamQuery.{ParamCompoundSelect, ParamCompoundSelectMapping, ParamMappingQuery}
import net.noresttherein.oldsql.sql.ParamSelect.ArbitraryParamSelect
import net.noresttherein.oldsql.sql.RowProduct.{GroundFrom, TopFrom}
import net.noresttherein.oldsql.sql.SelectAPI.{QueryTemplate, SelectTemplate, SetOperator}
import net.noresttherein.oldsql.sql.SQLExpression.LocalScope
import net.noresttherein.oldsql.sql.ast.{ConversionSQL, QuerySQL, SelectSQL, TupleSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{ComponentSQL, RelationSQL}
import net.noresttherein.oldsql.sql.ast.QuerySQL.{ColumnMappingQuery, ColumnQuery, CompoundSelectColumn, CompoundSelectColumnMapping, CompoundSelectMapping, CompoundSelectSQL, MappingQuery}
import net.noresttherein.oldsql.sql.ast.TupleSQL.IndexedChainTuple
import net.noresttherein.oldsql.sql.ast.TupleSQL.IndexedChainTuple.IndexedSQLExpression
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SelectMapping, TopSelectAs, TopSelectSQL}
import net.noresttherein.oldsql.sql.mechanics.SQLScribe






trait SelectAPI[-F <: RowProduct, V] extends SelectTemplate[V, ({ type S[X] = SelectAPI[F, X] })#S]



object SelectAPI {

	/** An SQL binary operator which can be used to create a query out of two SQL ''selects'', by combining their
	  * result sets. The most notable instance is [[net.noresttherein.oldsql.sql.ast.QuerySQL.Union UNION]], but other
	  * predefined operators provided by some database engines have also their definition in the enclosing
	  * [[net.noresttherein.oldsql.sql.ParamQuery$ ParamQuery]] object.
	  */
	class SetOperator(val name :String) extends AnyVal {
		def NAME :String = name.toUpperCase

		def apply[P, V](left :ParamQuery[P, V], right :ParamQuery[P, V]) :ParamQuery[P, V] =
			ParamCompoundSelect(left, this, right)

		def apply[P, M[O] <: MappingAt[O]]
		         (left :ParamMappingQuery[P, M], right :ParamMappingQuery[P, M]) :ParamMappingQuery[P, M] =
			ParamCompoundSelectMapping(left, this, right)


		def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V]) :QuerySQL[F, V] =
			CompoundSelectSQL(left, this, right)

		def apply[F <: RowProduct, V](left :ColumnQuery[F, V], right :ColumnQuery[F, V]) :ColumnQuery[F, V] =
			CompoundSelectColumn(left, this, right)

		def apply[F <: RowProduct, M[O] <: MappingAt[O]]
		         (left :MappingQuery[F, M], right :MappingQuery[F, M]) :MappingQuery[F, M] =
			CompoundSelectMapping(left, this, right)

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



	trait QueryTemplate[V, +S[_]] {
		type ResultMapping[O] <: MappingAt[O]

		protected def component[O] :ResultMapping[O]
		protected def export[O] :RefinedMapping[ResultMapping[O]#Subject, O] //= component[O]

		def map[X](f :V => X) :S[X]

		def map[Fun, C <: Chain, X](f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C)
				:S[X] =
			map(applyFun(f))

		protected def applyFun[Fun, C <: Chain, X]
		                      (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :V <:< C) :V => X =
			{ v => application(f, isChain(v)) }
	}



	trait SelectTemplate[V, +S[_]] extends QueryTemplate[V, S] {

		/** The from clause of this select. */
		type From <: RowProduct

		trait SelectedColumn[X] {
			def name :String
			def expression :ColumnSQL[From, LocalScope, X]
			override def toString :String = expression.toString + " as " + name
		}

		val from :From
		val selectClause :SQLExpression[From, LocalScope, V]
		def columns :Seq[SelectedColumn[_]]

		//caution: in group by queries this returns the elements of the group by clause, not the actual from clause
		def relations :Seq[RelationSQL.AnyIn[from.Generalized]] = from.tableStack.reverse

		def tables :Seq[Relation.*] = from.fromClause.tableStack.reverse.map(_.relation :Relation[MappingAt]).toList


		def isDistinct :Boolean
		def distinct :S[V]


		def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case s :AnyRef if s eq this => true
			case s :ParamSelect[_, _] if s canEqual this=>
				isDistinct == s.isDistinct && s.selectClause == selectClause && s.from == from
			case _ => false
		}

		override def hashCode :Int = (selectClause.hashCode * 31 + from.hashCode) * 31 + isDistinct.hashCode

		override def toString :String =
			if (isDistinct) s"SELECT DISTINCT $selectClause FROM $from"
			else  s"SELECT $selectClause FROM $from"
	}

}












/** An SQL expression returning a row cursor. It is the common base type for
  * the [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] type hierarchy and
  * set operations on them (such as `UNION`): [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectSQL ParamCompoundSelect]].
  * It also forms its own hierarchy parallel to that of `SelectSQL` and `ParamCompoundSelect`, with subtypes
  * for queries returning a single [[net.noresttherein.oldsql.sql.ast.QuerySQL.ColumnQuery column]]
  * and a [[net.noresttherein.oldsql.sql.ast.QuerySQL.MappingQuery mapping]].
  */
trait ParamQuery[P, V] extends QueryTemplate[V, ({ type Q[X] = ParamQuery[P, X] })#Q] {
	//overrides to grant access to classes located in the companion object
	protected override def component[O] :ResultMapping[O]
	protected override def export[O] :RefinedMapping[ResultMapping[O]#Subject, O] //= component[O]

	//semantics of unions and rest:
	// - if the column set is the same, normal union
	// - if not, but the mapping is the same, than missing columns are added with null values to each operand
	// - if both are mapping-based, with mappings from the same hierarchy, use union column set with a discriminator column
	// - otherwise the column set becomes two separate sets with a discriminator
	def union(other :ParamQuery[P, V]) :ParamQuery[P, V] = SelectAPI.Union(this, other)
	def unionAll(other :ParamQuery[P, V]) :ParamQuery[P, V] = SelectAPI.UnionAll(this, other)
	def minus(other :ParamQuery[P, V]) :ParamQuery[P, V] = SelectAPI.Minus(this, other)
	def intersect(other :ParamQuery[P, V]) :ParamQuery[P, V] = SelectAPI.Intersect(this, other)

	def bind(params :P) :QuerySQL[RowProduct, V]

	def canEqual(that :Any) :Boolean = that.getClass == getClass
}






object ParamQuery {

	/** An SQL query, that is an SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL.SelectAs select]] or
	  * a [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectMapping set operation]] on them,
	  * which provides a [[net.noresttherein.oldsql.schema.Mapping Mapping]] for the returned rows.
	  */
	trait ParamMappingQuery[P, M[O] <: MappingAt[O]] extends ParamQuery[P, M[()]#Subject] {
		override type ResultMapping[O] = M[O]

		def union(other :ParamMappingQuery[P, M]) :ParamMappingQuery[P, M] = SelectAPI.Union(this, other)
		def unionAll(other :ParamMappingQuery[P, M]) :ParamMappingQuery[P, M] = SelectAPI.UnionAll(this, other)
		def minus(other :ParamMappingQuery[P, M]) :ParamMappingQuery[P, M] = SelectAPI.Minus(this, other)
		def intersect(other :ParamMappingQuery[P, M]) :ParamMappingQuery[P, M] = SelectAPI.Intersect(this, other)

		override def bind(params :P) :MappingQuery[RowProduct, M]
	}






	/** Implements a set operation combining the result sets of two
	  * [[net.noresttherein.oldsql.sql.ParamSelect parameterized selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ParamQuery queries]]). The kind of operation is defined by
	  * the [[net.noresttherein.oldsql.sql.SelectAPI.SetOperator SetOperator]]
	  * of the [[net.noresttherein.oldsql.sql.ParamQuery.ParamCompoundSelect.operator operator]] member property.
	  * The row schemas of both arguments must match or an exception will be thrown when this expression
	  * is converted into an executable SQL statement. If the schema of any of the member ''selects'' is flexible
	  * (it is defined by a mapping with [[net.noresttherein.oldsql.schema.Buff.OptionalSelect optional]] columns),
	  * the schema of the first member is used for both of the arguments.
	  */
	trait ParamCompoundSelect[P, V] extends ParamQuery[P, V] {
		val left :ParamQuery[P, V]
		val right :ParamQuery[P, V]
		val operator :SetOperator

		override def map[X](f :V => X) :ParamQuery[P, X] = ParamCompoundSelect(left.map(f), operator, right.map(f))

		override def bind(params :P) :QuerySQL[RowProduct, V] = operator(left.bind(params), right.bind(params))


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :ParamCompoundSelect[_, _] if canEqual(other) && other.canEqual(this) =>
				operator == other.operator && left == other.left && right == other.right
			case _ => false
		}

		override def hashCode :Int = (operator.hashCode * 31 + left.hashCode) * 31 + right.hashCode

		override def toString :String = s"($left) $operator ($right)"
	}



	object ParamCompoundSelect {
		def apply[P, V](left :ParamQuery[P, V], operator :SetOperator, right :ParamQuery[P, V])
				:ParamCompoundSelect[P, V] =
			new BaseParamCompoundSelect(left, operator, right)

		def unapply[P, V](query :ParamQuery[P, V]) :Option[(ParamQuery[P, V], SetOperator, ParamQuery[P, V])] =
			query match {
				case op :ParamCompoundSelect[P @unchecked, V @unchecked] => Some((op.left, op.operator, op.right))
				case _ => None
			}

		private[ParamQuery] class BaseParamCompoundSelect[P, V]
		                          (override val left :ParamQuery[P, V], override val operator :SetOperator,
		                           override val right :ParamQuery[P, V])
			extends ParamCompoundSelect[P, V]
		{
			override type ResultMapping[O] = left.ResultMapping[O]
			protected override def component[O] = left.component[O]
			protected override def export[O] = left.export[O] //todo: this should involve some reconciliation
		}
	}






	/** Implements a set operation combining the result sets of two parameterized
	  * [[net.noresttherein.oldsql.sql.ParamSelect.ParamSelectAs selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ParamQuery.ParamMappingQuery queries]]), sharing the same row schema,
	  * as defined by the shared [[net.noresttherein.oldsql.schema.Mapping mapping]] type `M`. The kind of operation
	  * is defined by the [[net.noresttherein.oldsql.sql.SelectAPI.SetOperator SetOperator]]
	  * of the [[net.noresttherein.oldsql.sql.ParamQuery.ParamCompoundSelect.operator operator]] member property.
	  */
	trait ParamCompoundSelectMapping[P, M[O] <: MappingAt[O]]
		extends ParamCompoundSelect[P, M[()]#Subject] with ParamMappingQuery[P, M]
	{
		override val left :ParamMappingQuery[P, M]
		override val right :ParamMappingQuery[P, M]

		protected override def component[O] = left.component
		protected override def export[O] = left.export //todo: this should involve some reconciliation

		override def bind(params :P) :MappingQuery[RowProduct, M] = operator(left.bind(params), right.bind(params))
	}



	object ParamCompoundSelectMapping {
		def apply[P, M[O] <: MappingAt[O]]
		         (left :ParamMappingQuery[P, M], operator :SetOperator, right :ParamMappingQuery[P, M])
				:ParamCompoundSelectMapping[P, M] =
			new BaseParamCompoundSelectMapping(left, operator, right)

		def unapply[P, V](query :ParamQuery[P, V])
				:Option[(ParamMappingQuery[P, M], SetOperator, ParamMappingQuery[P, M]) forSome { type M[O] <: MappingAt[O] }] =
			query match {
				case op :ParamCompoundSelectMapping[P @unchecked, MappingAt @unchecked] =>
					Some((op.left, op.operator, op.right))
				case _ => None
			}

		private[ParamQuery] class BaseParamCompoundSelectMapping[P, M[O] <: MappingAt[O]]
		                          (override val left :ParamMappingQuery[P, M], override val operator :SetOperator,
		                           override val right :ParamMappingQuery[P, M])
			extends ParamCompoundSelectMapping[P, M]

	}

}






/**
  * @author Marcin Mościcki
  */
trait ParamSelect[P, V] extends SelectAPI[RowProduct, V] with SelectTemplate[V, ({ type S[X] = ParamSelect[P, X] })#S] {

//	def readForm :SQLReadForm[Rows[V]] = selectClause.readForm.nullMap(Rows(_))

	/** The from clause of this select. */
	override type From <: TopFrom { type Params = P }

	override def map[X](f :V => X) :ParamSelect[P, X] =
		new ArbitraryParamSelect[P, From, X](from, selectClause.map(f), isDistinct)

	protected def reverseCollect[X](fun: PartialFunction[SQLExpression.*, X], acc: List[X]): List[X] = {
		//we ignore filters in the implicit portion as, if this is a subselect, they would be collected by the enclosing expression.
		val headerItems = selectClause.bridgeReverseCollect(fun, acc)
		from.filter.bridgeReverseCollect(fun, from match { //todo: make this a method in RowProduct
			case GroupByClause(ungrouped) => ungrouped.filter.bridgeReverseCollect(fun, headerItems)
			case _ => headerItems
		})
	}



	def bind(params :P) :TopSelectSQL[V]

}







object ParamSelect {
	//todo: mapping indexed headers

	def apply[P <: Chain, F <: TopFrom { type Params = P }, M[A] <: BaseMapping[V, A], V]
	         (from :F, header :ComponentSQL[F, M]) :ParamSelectMapping[P, F, M, V] =
		new ParamSelectComponent[P, F, M, V](from, header)

	def apply[P <: Chain, F <: TopFrom { type Params = P }, V]
	         (from :F, header :TupleSQL[F, LocalScope, V]) :ParamSelect[P, V] =
		new ArbitraryParamSelect[P, F, V](from, header.anchor(from))

	def apply[P <: Chain, F <: TopFrom { type Params = P }, V <: IndexedChain]
	         (from :F, header :IndexedChainTuple[F, LocalScope, V])
			:ParamSelectAs[P, IndexedMapping.Of[V]#Projection] =
		new ParamIndexedSelect(from, header)

	def apply[P <: Chain, F <: TopFrom { type Params = P }, X, Y]
	         (from :F, header :ConversionSQL[F, LocalScope, X, Y]) :ParamSelect[P, Y] =
		new ArbitraryParamSelect[P, F, Y](from, header.anchor(from))

	def apply[P <: Chain, F <: TopFrom { type Params = P }, V]
	         (from :F, header :ColumnSQL[F, LocalScope, V]) :ParamSelect[P, V] =
		new ArbitraryParamSelect[P, F, V](from, header.anchor(from))



	type * = ParamSelect[_, _]



	/** A `SelectSQL` interface exposing the mapping type `H` used for the ''select'' clause. */
	trait ParamSelectAs[P, H[A] <: MappingAt[A]]
		extends ParamSelect[P, H[()]#Subject]
	{
		override type ResultMapping[O] = H[O]

		override def distinct :ParamSelectAs[P, H]

		override def bind(params :P) :TopSelectAs[H]
	}



	trait ParamSelectMapping[P, F <: TopFrom { type Params = P }, H[A] <: BaseMapping[V, A], V]
		extends ParamSelectAs[P, H]
	{
		override type From = F

		override def distinct :ParamSelectMapping[P, F, H, V]
	}






	private class ParamSelectComponent[P, F <: TopFrom { type Params = P }, H[A] <: BaseMapping[V, A], V]
	                                  (override val from :F, override val selectClause :ComponentSQL[F, H],
	                                   override val isDistinct :Boolean = false)
		extends ParamSelectMapping[P, F, H, V]
	{
		override type From = F

		protected override def component[O] :ResultMapping[O] = selectClause.mapping.withOrigin[O]
		protected override def export[O] :RefinedMapping[V, O] = selectClause.export.withOrigin[O]

		override val columns: Seq[SelectedColumn[_]] = //todo: is this the place where we finally decide on the column set?
			selectClause.export.selectedByDefault.toSeq.map(include(_))

		private def include[X](column :ColumnMapping[X, selectClause.Origin]) :SelectedColumn[X] =
			new SelectedColumn[X] {
				override val name :String = column.name
				override val expression  = selectClause \ column
			}

		override def distinct :ParamSelectMapping[P, F, H, V] =
			if (isDistinct) this else new ParamSelectComponent(from, selectClause, true)

		override def bind(params :P) :TopSelectAs[H] = {
			val paramless = from.bind(params)
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
			val select = SelectSQL(
				paramless.asInstanceOf[GroundFrom], header.asInstanceOf[ComponentSQL[GroundFrom, H]]
			) :SelectMapping[GroundFrom, H, V]
			if (isDistinct) select.distinct else select
		}
	}






	/** A select expression based on the given row source and selecting an arbitrary expression `header` in its ''select''
	  * clause. This header will be translated by recursively flat mapping the header expression to obtain a flat sequence
	  * of columns.
	  */
	private[sql] abstract class BaseArbitraryParamSelect[P, F <: TopFrom { type Params = P }, V] protected
	                            (override val from :F, protected val mapping :SQLMapping[F, LocalScope, V, ()])
		extends ParamSelect[P, V]
	{
		def this(from :F, header :SQLExpression[F, LocalScope, V]) =
			this(from, SQLMapping[F, LocalScope, V, ()](header))

		override type From = F

		override val selectClause = mapping.expr

		/** A column in the header of owning select.
		  * @param column the `ColumnMapping` implementation based on a `ColumnSQL` expression
		  *               providing the value for the column.
		  */
		protected class HeaderColumn[T](column :ColumnSQLMapping[F, LocalScope, T, _])
			extends SelectedColumn[T]
		{
			override def expression :ColumnSQL[F, LocalScope, T] = column.expr
			override def name :String = column.name
		}

		override val columns :Seq[SelectedColumn[_]] = mapping.columns.map { col => new HeaderColumn(col) }

		override def bind(params :P) :TopSelectSQL[V] = {
			val paramless = from.bind(params).asInstanceOf[GroundFrom]
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
			val select = header.topSelectFrom(paramless)
			if (isDistinct) select.distinct else select
		}
	}


	private[sql] trait ArbitraryParamSelectTemplate[P, F <: TopFrom { type Params = P }, M[O] <: BaseMapping[V, O], V]
		extends ParamSelect[P, V]
	{ this :BaseArbitraryParamSelect[P, F, V] =>
		override type ResultMapping[O] = M[O]

		protected val mapping :M[()]

		protected override def component[O] :M[O] =
			(this :ArbitraryParamSelectTemplate[P, F, M, V]).mapping.withOrigin[O]

		protected override def export[O] :M[O] = component[O]
	}




	private[sql] class ArbitraryParamSelect[P, F <: TopFrom { type Params = P }, V]
	                   (override val from :F, protected override val mapping :SQLMapping[F, LocalScope, V, ()],
	                    override val isDistinct :Boolean = false)
		extends BaseArbitraryParamSelect[P, F, V](from, mapping)
			with ArbitraryParamSelectTemplate[P, F, SQLMapping.Project[F, LocalScope, V]#Expression, V]
	{
		def this(from :F, expression :SQLExpression[F, LocalScope, V], isDistinct :Boolean) =
			this(from, SQLMapping[F, LocalScope, V, ()](expression), isDistinct)

		def this(from :F, expression :SQLExpression[F, LocalScope, V]) =
			this(from, SQLMapping[F, LocalScope, V, ()](expression))

		override def distinct :ParamSelect[P, V] =
			if (isDistinct) this else new ArbitraryParamSelect(from, mapping, true)
	}



	private[sql] class ParamIndexedSelect[P, F <: TopFrom { type Params = P }, V <: IndexedChain]
	                  (override val from :F, override val mapping :IndexedSQLMapping[F, LocalScope, V, ()],
	                   override val isDistinct :Boolean = false)
		extends BaseArbitraryParamSelect[P, F, V](from, mapping)
			with ArbitraryParamSelectTemplate[P, F, IndexedMapping.Of[V]#Projection, V]
			with ParamSelectAs[P, IndexedMapping.Of[V]#Projection]
	{
		def this(from :F, expression :IndexedSQLExpression[F, LocalScope, V]) =
			this(from, expression.mapping[()])

		override val selectClause = mapping.expr

		override def distinct :ParamSelectAs[P, IndexedMapping.Of[V]#Projection] =
			if (isDistinct) this else new ParamIndexedSelect[P, F, V](from, mapping, true)

		override def bind(params :P) :TopSelectAs[IndexedMapping.Of[V]#Projection] = {
			val paramless = from.bind(params).asInstanceOf[GroundFrom]
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
				.asInstanceOf[IndexedChainTuple[GroundFrom, LocalScope, V]]
			val select = header.topSelectFrom(paramless)
			if (isDistinct) select.distinct else select
		}
	}


}

