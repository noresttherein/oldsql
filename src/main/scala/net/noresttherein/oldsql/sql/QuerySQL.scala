package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.{ColumnMapping, ColumnReadForm, Relation, SQLReadForm}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.QuerySQL.ColumnMappingSetOperation.ColumnMappingSetOperationMatcher
import net.noresttherein.oldsql.sql.QuerySQL.MappingSetOperation.{CaseMappingSetOperation, MappingSetOperationMatcher}
import net.noresttherein.oldsql.sql.QuerySQL.{ColumnQuery, QueryRelation}
import net.noresttherein.oldsql.sql.QuerySQL.SetColumnOperation.{CaseSetColumnOperation, SetColumnOperationMatcher}
import net.noresttherein.oldsql.sql.QuerySQL.SetOperationSQL.CaseSetOperation
import net.noresttherein.oldsql.sql.SelectSQL.{CaseSelect, CaseSelectColumn, CaseSelectMapping, SelectAs, SelectColumn, SelectColumnMappingMatcher, SelectColumnMatcher, SelectMappingMatcher, SelectMatcher}
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher, GlobalScope, LocalScope}






/** An SQL expression returning a row cursor. It is the common base type for
  * the [[net.noresttherein.oldsql.sql.SelectSQL SelectSQL]] type hierarchy and
  * set operations on them (such as `UNION`): [[net.noresttherein.oldsql.sql.QuerySQL.SetOperationSQL SetOperationSQL]].
  * It also forms its own hierarchy parallel to that of `SelectSQL` and `SetOperationSQL`, with subtypes
  * for queries returning a single [[net.noresttherein.oldsql.sql.QuerySQL.ColumnQuery column]]
  * and a [[net.noresttherein.oldsql.sql.QuerySQL.MappingQuery mapping]].
  */
trait QuerySQL[-F <: FromClause, V] extends SQLExpression[F, GlobalScope, Rows[V]] {
	type HeaderMapping[O] <: MappingAt[O]

	protected def component[O] :HeaderMapping[O]

	override def isAnchored = true
	override def anchor(from :F) :QuerySQL[F, V] = this

	def union[E <: F](other :QuerySQL[E, V]) :QuerySQL[E, V] = QuerySQL.Union(this, other)
	def unionAll[E <: F](other :QuerySQL[E, V]) :QuerySQL[E, V] = QuerySQL.UnionAll(this, other)
	def minus[E <: F](other :QuerySQL[E, V]) :QuerySQL[E, V] = QuerySQL.Minus(this, other)
	def intersect[E <: F](other :QuerySQL[E, V]) :QuerySQL[E, V] = QuerySQL.Intersect(this, other)

//	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//	                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
//		matcher.query(this)
}






sealed abstract class ImplicitQueryRelations {
	implicit def arbitraryQueryRelation[V](query :QuerySQL[FromClause, V]) :Relation[MappingOf[V]#Projection] =
		new QueryRelation[query.HeaderMapping](query).asInstanceOf[Relation[MappingOf[V]#Projection]]

	implicit def singleColumnRelation[V](query :ColumnQuery[FromClause, V]) :Relation[MappingOf[V]#ColumnProjection] =
		new QueryRelation[query.HeaderMapping](query)
}



object QuerySQL extends ImplicitQueryRelations {


	/** An SQL query, that is an SQL [[net.noresttherein.oldsql.sql.SelectSQL.SelectColumn select]] or
	  * a [[net.noresttherein.oldsql.sql.QuerySQL.SetColumnOperation set operation]] on them, which returns
	  * a single column.
	  */
	trait ColumnQuery[-F <: FromClause, V] extends QuerySQL[F, V] with ColumnSQL[F, GlobalScope, Rows[V]] {
		override type HeaderMapping[O] <: ColumnMapping[V, O]
	
		override def anchor(from :F) :ColumnQuery[F, V] = this

		def union[E <: F](other :ColumnQuery[E, V]) :ColumnQuery[E, V] = QuerySQL.Union(this, other)
		def unionAll[E <: F](other :ColumnQuery[E, V]) :ColumnQuery[E, V] = QuerySQL.UnionAll(this, other)
		def minus[E <: F](other :ColumnQuery[E, V]) :ColumnQuery[E, V] = QuerySQL.Minus(this, other)
		def intersect[E <: F](other :ColumnQuery[E, V]) :ColumnQuery[E, V] = QuerySQL.Intersect(this, other)

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.query(this)
	}


	/** An SQL query, that is an SQL [[net.noresttherein.oldsql.sql.SelectSQL.SelectAs select]] or
	  * a [[net.noresttherein.oldsql.sql.QuerySQL.MappingSetOperation set operation]] on them,
	  * which provides a [[net.noresttherein.oldsql.schema.Mapping Mapping]] for the returned rows.
	  */
	trait MappingQuery[-F <: FromClause, M[O] <: MappingAt[O]] extends QuerySQL[F, M[Any]#Subject] {
		override type HeaderMapping[O] = M[O]

		def union[E <: F](other :MappingQuery[E, M]) :MappingQuery[E, M] = QuerySQL.Union(this, other)
		def unionAll[E <: F](other :MappingQuery[E, M]) :MappingQuery[E, M] = QuerySQL.UnionAll(this, other)
		def minus[E <: F](other :MappingQuery[E, M]) :MappingQuery[E, M] = QuerySQL.Minus(this, other)
		def intersect[E <: F](other :MappingQuery[E, M]) :MappingQuery[E, M] = QuerySQL.Intersect(this, other)

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[M[Any]#Subject]] =
//			matcher.mappingQuery(this)
	}


	/** An SQL query returning a single column of some relation.
	  * @see [[net.noresttherein.oldsql.sql.SelectSQL.SelectColumnAs]]
	  * @see [[net.noresttherein.oldsql.sql.QuerySQL.ColumnMappingSetOperation]]
	  */
	trait ColumnMappingQuery[-F <: FromClause, M[O] <: ColumnMapping[V, O], V]
		extends ColumnQuery[F, V] with MappingQuery[F, M]
	{
		def union[E <: F](other :ColumnMappingQuery[E, M, V]) :ColumnMappingQuery[E, M, V] =
			QuerySQL.Union(this, other)

		def unionAll[E <: F](other :ColumnMappingQuery[E, M, V]) :ColumnMappingQuery[E, M, V] =
			QuerySQL.UnionAll(this, other)

		def minus[E <: F](other :ColumnMappingQuery[E, M, V]) :ColumnMappingQuery[E, M, V] =
			QuerySQL.Minus(this, other)

		def intersect[E <: F](other :ColumnMappingQuery[E, M, V]) :ColumnMappingQuery[E, M, V] =
			QuerySQL.Intersect(this, other)

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.mappingQuery(this)
	}






	/** An SQL binary operator which can be used to create a query out of two SQL ''selects'', by combining their
	  * result sets. The most notable instance is [[net.noresttherein.oldsql.sql.QuerySQL.Union UNION]], but other
	  * predefined operators provided by some database engines have also their definition in the enclosing
	  * [[net.noresttherein.oldsql.sql.QuerySQL$ QuerySQL]] object.
	  */
	class SetOperator(val name :String) extends AnyVal {
		def NAME :String = name.toUpperCase

		def apply[F <: FromClause, V](left :QuerySQL[F, V], right :QuerySQL[F, V]) :QuerySQL[F, V] =
			SetOperationSQL(left, this, right)

		def apply[F <: FromClause, V](left :ColumnQuery[F, V], right :ColumnQuery[F, V]) :ColumnQuery[F, V] =
			SetColumnOperation(left, this, right)

		def apply[F <: FromClause, M[O] <: MappingAt[O]]
		         (left :MappingQuery[F, M], right :MappingQuery[F, M]) :MappingQuery[F, M] =
			MappingSetOperation(left, this, right)

		def apply[F <: FromClause, M[O] <: ColumnMapping[V, O], V]
		         (left :ColumnMappingQuery[F, M, V], right :ColumnMappingQuery[F, M, V]) :ColumnMappingQuery[F, M, V] =
			ColumnMappingSetOperation(left, this, right)

		override def toString :String = name
	}

	/** A union operator combining two SQL [[net.noresttherein.oldsql.sql.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.QuerySQL queries]]). It follows the semantics of set union,
	  * with no duplicate rows in the returned result set.
	  */
	final val Union = new SetOperator("union")

	/** A union operator combining two SQL [[net.noresttherein.oldsql.sql.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.QuerySQL queries]]). This is a multiset variant, with every row
	  * from either of the ''selects'' mapping to a single row in the returned result set; if a row is present
	  * in both ''selects'' (or it has duplicates within either of them), each occurrence will be represented by
	  * a separate row in the result.
	  */
	final val UnionAll = new SetOperator("union all")

	/** An operator implementing set difference between two SQL [[net.noresttherein.oldsql.sql.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.QuerySQL queries]]). The query created will return every row
	  * from the first (left) argument which is not present in the second (right) argument.
	  */
	final val Minus = new SetOperator("minus")

	/** An operator implementing set intersection of two SQL [[net.noresttherein.oldsql.sql.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.QuerySQL queries]]). The query created will return all rows
	  * which are present in both of its arguments, with every row occuring exactly once, regardless of the number
	  * of duplicates in the input ''selects''.
	  */
	final val Intersect = new SetOperator("intersect")



	/** Implements a set operation combining the result sets of two SQL [[net.noresttherein.oldsql.sql.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.QuerySQL queries]]). The kind of operation is defined by
	  * the [[net.noresttherein.oldsql.sql.QuerySQL.SetOperator SetOperator]]
	  * of the [[net.noresttherein.oldsql.sql.QuerySQL.SetOperationSQL.operator operator]] member property.
	  * The row schemas of both arguments must match or an exception will be thrown when this expression
	  * is converted into an executable SQL statement. If the schema of any of the member ''selects'' is flexible
	  * (it is defined by a mapping with [[net.noresttherein.oldsql.schema.Buff.OptionalSelect optional]] columns),
	  * the schema of the first member is used for both of the arguments.
	  */
	trait SetOperationSQL[-F <: FromClause, V] extends CompositeSQL[F, GlobalScope, Rows[V]] with QuerySQL[F, V] {
		val left :QuerySQL[F, V]
		val right :QuerySQL[F, V]
		val operator :SetOperator

		override def parts :Seq[QuerySQL[F, V]] = left::right::Nil

		override def readForm :SQLReadForm[Rows[V]] = left.readForm


		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :SQLExpression[E, GlobalScope, Rows[V]] =
			(mapper(left), mapper(right)) match {
				case (l :QuerySQL[E @unchecked, V @unchecked], r :QuerySQL[E @unchecked, V @unchecked]) =>
					SetOperationSQL(l, operator, r)
				case (l, r) =>
					throw new IllegalArgumentException(
						s"Failed $mapper transformation of $this: ($l, $r) is not a valid QuerySQL pair."
					)
			}

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.setOperation(this)


		override def sameAs(that :CompositeSQL.*) :Boolean = that match {
			case op :SetOperationSQL[_, _] => op.operator == operator
			case _ => false
		}

		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :SetOperationSQL[_, _] if canEqual(other) && other.canEqual(this) =>
				operator == other.operator && left == other.left && right == other.right
			case _ => false
		}

		override def hashCode :Int = (operator.hashCode * 31 + left.hashCode) * 31 + right.hashCode

		override def toString :String = s"($left) $operator ($right)"
	}



	object SetOperationSQL {
		def apply[F <: FromClause, V](left :QuerySQL[F, V], operator :SetOperator, right :QuerySQL[F, V])
				:SetOperationSQL[F, V] =
			new BaseSetOperationSQL(left, operator, right)

		def unapply[F <: FromClause, V](e :SQLExpression[F, LocalScope, Rows[V]])
				:Option[(QuerySQL[F, V], SetOperator, QuerySQL[F, V])] =
			e match {
				case op :SetOperationSQL[F @unchecked, V @unchecked] => Some((op.left, op.operator, op.right))
				case _ => None
			}

		private[QuerySQL] class BaseSetOperationSQL[-F <: FromClause, V]
		                                           (override val left :QuerySQL[F, V], override val operator :SetOperator,
		                                            override val right :QuerySQL[F, V])
			extends SetOperationSQL[F, V]
		{
			override type HeaderMapping[O] = left.HeaderMapping[O]
			protected override def component[O] = left.component[O]
		}



		trait SetOperationMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends MappingSetOperationMatcher[F, Y] with SetColumnOperationMatcher[F, Y]
		{
			def setOperation[V](e :SetOperationSQL[F, V]) :Y[GlobalScope, Rows[V]]
		}

		trait MatchSetOperation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends SetOperationMatcher[F, Y] with CaseMappingSetOperation[F, Y]
		{
			override def setOperation[V](e :SetColumnOperation[F, V]) :Y[GlobalScope, Rows[V]] =
				setOperation(e :SetOperationSQL[F, V])
		}

		trait CaseSetOperation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends MatchSetOperation[F, Y]
		{
			override def mappingSetOperation[M[O] <: MappingAt[O]]
			                                (e :MappingSetOperation[F, M]) :Y[GlobalScope, Rows[M[Any]#Subject]] =
				setOperation(e)
		}

	}






	/** Implements a set operation combining the result sets of two single-column
	  * SQL [[net.noresttherein.oldsql.sql.SelectSQL.SelectColumn selects]]
	  * (or other [[net.noresttherein.oldsql.sql.QuerySQL.ColumnQuery queries]]). The kind of operation is defined by
	  * the [[net.noresttherein.oldsql.sql.QuerySQL.SetOperator SetOperator]]
	  * of the [[net.noresttherein.oldsql.sql.QuerySQL.SetOperationSQL.operator operator]] member property.
	  */
	trait SetColumnOperation[-F <: FromClause, V]
		extends CompositeColumnSQL[F, GlobalScope, Rows[V]] with ColumnQuery[F, V] with SetOperationSQL[F, V]
	{
		override val left :ColumnQuery[F, V]
		override val right :ColumnQuery[F, V]

		override def readForm :ColumnReadForm[Rows[V]] = left.readForm

		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, GlobalScope, Rows[V]] =
			(mapper(left), mapper(right)) match {
				case (l :ColumnQuery[E @unchecked, V @unchecked], r :ColumnQuery[E @unchecked, V @unchecked]) =>
					SetColumnOperation(l, operator, r)
				case (l, r) =>
					throw new IllegalArgumentException(
						s"Failed $mapper transformation of $this: ($l, $r) is not a valid ColumnQuery pair."
					)
			}

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.setOperation(this)
	}



	object SetColumnOperation {
		def apply[F <: FromClause, V](left :ColumnQuery[F, V], operator :SetOperator, right :ColumnQuery[F, V])
				:SetColumnOperation[F, V] =
			new BaseSetColumnOperationSQL(left, operator, right)

		def unapply[F <: FromClause, V](e :SQLExpression[F, LocalScope, Rows[V]])
				:Option[(ColumnQuery[F, V], SetOperator, ColumnQuery[F, V])] =
			e match {
				case op :SetColumnOperation[F @unchecked, V @unchecked] => Some((op.left, op.operator, op.right))
				case _ => None
			}

		private[QuerySQL] class BaseSetColumnOperationSQL[-F <: FromClause, V]
		                                                 (override val left :ColumnQuery[F, V],
		                                                  override val operator :SetOperator,
		                                                  override val right :ColumnQuery[F, V])
			extends SetColumnOperation[F, V]
		{
			override type HeaderMapping[O] = ColumnMapping[V, O]
			protected override def component[O] = left.component
		}


		trait SetColumnOperationMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends ColumnMappingSetOperationMatcher[F, Y]
		{
			def setOperation[V](e :SetColumnOperation[F, V]) :Y[GlobalScope, Rows[V]]
		}

		type MatchSetColumnOperation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			SetColumnOperationMatcher[F, Y]

		trait CaseSetColumnOperation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends MatchSetColumnOperation[F, Y]
		{
			override def mappingSetOperation[M[O] <: ColumnMapping[V, O], V]
		                                    (e :ColumnMappingSetOperation[F, M, V]) :Y[GlobalScope, Rows[V]] =
				setOperation(e)
		}
	}






	/** Implements a set operation combining the result sets of two
	  * SQL [[net.noresttherein.oldsql.sql.SelectSQL.SelectAs selects]]
	  * (or other [[net.noresttherein.oldsql.sql.QuerySQL.MappingQuery queries]]), sharing the same row schema,
	  * as defined by the mapping `M`. The kind of operation is defined by
	  * the [[net.noresttherein.oldsql.sql.QuerySQL.SetOperator SetOperator]]
	  * of the [[net.noresttherein.oldsql.sql.QuerySQL.SetOperationSQL.operator operator]] member property.
	  */
	trait MappingSetOperation[-F <: FromClause, M[O] <: MappingAt[O]]
		extends SetOperationSQL[F, M[Any]#Subject] with MappingQuery[F, M]
	{
		override val left :MappingQuery[F, M]
		override val right :MappingQuery[F, M]

		protected override def component[O] = left.component

		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :SQLExpression[E, GlobalScope, Rows[M[Any]#Subject]] =
			(mapper(left), mapper(right)) match {
				case (l :MappingQuery[E @unchecked, M @unchecked], r :MappingQuery[E @unchecked, M @unchecked]) =>
					MappingSetOperation(l, operator, r)
				case (l, r) =>
					throw new IllegalArgumentException(
						s"Failed $mapper transformation of $this: ($l, $r) is not a valid ColumnQuery pair."
					)
			}

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[M[Any]#Subject]] =
			matcher.mappingSetOperation(this)
	}



	object MappingSetOperation {
		def apply[F <: FromClause, M[O] <: MappingAt[O]]
		         (left :MappingQuery[F, M], operator :SetOperator, right :MappingQuery[F, M])
				:MappingSetOperation[F, M] =
			new BaseMappingSetOperation(left, operator, right)

		def unapply[F <: FromClause, V](e :SQLExpression[F, LocalScope, Rows[V]])
				:Option[(MappingQuery[F, M], SetOperator, MappingQuery[F, M]) forSome { type M[O] <: MappingAt[O] }] =
			e match {
				case op :MappingSetOperation[F @unchecked, MappingAt @unchecked] => Some((op.left, op.operator, op.right))
				case _ => None
			}

		private[QuerySQL] class BaseMappingSetOperation[-F <: FromClause, M[O] <: MappingAt[O]]
		                        (override val left :MappingQuery[F, M], override val operator :SetOperator,
		                         override val right :MappingQuery[F, M])
			extends MappingSetOperation[F, M]



		trait MappingSetOperationMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends ColumnMappingSetOperationMatcher[F, Y]
		{
			def mappingSetOperation[M[O] <: MappingAt[O]]
			                       (e :MappingSetOperation[F, M]) :Y[GlobalScope, Rows[M[Any]#Subject]]
		}

		trait MatchMappingSetOperation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends MappingSetOperationMatcher[F, Y]
		{
			override def mappingSetOperation[M[O] <: ColumnMapping[V, O], V]
			                                (e :ColumnMappingSetOperation[F, M, V]) :Y[GlobalScope, Rows[V]] =
				{ val res = mappingSetOperation(e :MappingSetOperation[F, M]); res  }
		}

		type CaseMappingSetOperation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			MatchMappingSetOperation[F, Y]

	}






	trait ColumnMappingSetOperation[-F <: FromClause, M[O] <: ColumnMapping[V, O], V]
		extends SetColumnOperation[F, V] with ColumnMappingQuery[F, M, V] with MappingSetOperation[F, M]
	{
		override val left :ColumnMappingQuery[F, M, V]
		override val right :ColumnMappingQuery[F, M, V]


		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ColumnSQL[E, GlobalScope, Rows[V]] =
			(mapper(left), mapper(right)) match {
				case (l :ColumnMappingQuery[E @unchecked, M @unchecked, V @unchecked],
				      r :ColumnMappingQuery[E @unchecked, M @unchecked, V @unchecked]) =>
					ColumnMappingSetOperation(l, operator, r)
				case (l, r) =>
					throw new IllegalArgumentException(
						s"Failed $mapper transformation of $this: ($l, $r) is not a valid ColumnQuery pair."
					)
			}

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[M[Any]#Subject]] =
			matcher.mappingSetOperation(this)

	}



	object ColumnMappingSetOperation {
		def apply[F <: FromClause, M[O] <: ColumnMapping[V, O], V]
		         (left :ColumnMappingQuery[F, M, V], operator :SetOperator, right :ColumnMappingQuery[F, M, V])
				:ColumnMappingSetOperation[F, M, V] =
			new BaseColumnMappingSetOperation(left, operator, right)

		def unapply[F <: FromClause, V](e :SQLExpression[F, LocalScope, Rows[V]])
				:Option[(ColumnMappingQuery[F, M, V], SetOperator, ColumnMappingQuery[F, M, V]) forSome { type M[O] <: ColumnMapping[V, O] }] =
			e match {
				case op :ColumnMappingSetOperation[F @unchecked, MappingOf[V]#ColumnProjection @unchecked, V @unchecked] =>
					Some((op.left, op.operator, op.right))
				case _ => None
			}

		private[QuerySQL] class BaseColumnMappingSetOperation[-F <: FromClause, M[O] <: ColumnMapping[V, O], V]
		                        (override val left :ColumnMappingQuery[F, M, V], override val operator :SetOperator,
		                         override val right :ColumnMappingQuery[F, M, V])
			extends ColumnMappingSetOperation[F, M, V]



		trait ColumnMappingSetOperationMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def mappingSetOperation[M[O] <: ColumnMapping[V, O], V]
			                       (e :ColumnMappingSetOperation[F, M, V]) :Y[GlobalScope, Rows[V]]
		}

		type MatchColumnMappingSetOperation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			ColumnMappingSetOperationMatcher[F, Y]

		type CaseColumnMappingSetOperation[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			ColumnMappingSetOperationMatcher[F, Y]
	}






	implicit def derivedTable[M[O] <: MappingAt[O]](query :MappingQuery[FromClause, M]) :Relation[M] =
		new QueryRelation[M](query)

	class QueryRelation[M[O] <: MappingAt[O]](val query :QuerySQL[FromClause, _] { type HeaderMapping[O] = M[O] })
		extends Relation[M]
	{
		override def apply[O] :M[O] = query.component[O]

		override def sql :String = ??? //todo: default dialect SQL
	}






	trait ColumnMappingQueryMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnMappingSetOperationMatcher[F, Y] with SelectColumnMappingMatcher[F, Y]
	{
		def mappingQuery[M[O] <: ColumnMapping[V, O], V](e :ColumnMappingQuery[F, M, V]) :Y[GlobalScope, Rows[V]]
	}

	type MatchColumnMappingQuery[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] =
		ColumnMappingQueryMatcher[F, Y]

	trait CaseColumnMappingQuery[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MatchColumnMappingQuery[F, Y]
	{
		override def selectMapping[H[O] <: ColumnMapping[V, O], V]
		                          (e :SelectSQL.SelectColumnAs[F, H, V]) :Y[GlobalScope, Rows[V]] =
			mappingQuery(e)

		override def mappingSetOperation[M[O] <: ColumnMapping[V, O], V]
		                                (e :ColumnMappingSetOperation[F, M, V]) :Y[GlobalScope, Rows[V]] =
			mappingQuery(e)
	}



	trait MappingQueryMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnMappingQueryMatcher[F, Y] with MappingSetOperationMatcher[F, Y] with SelectMappingMatcher[F, Y]
	{
		def mappingQuery[M[O] <: MappingAt[O]](e :MappingQuery[F, M]) :Y[GlobalScope, Rows[M[Any]#Subject]]
	}

	trait MatchMappingQuery[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MappingQueryMatcher[F, Y] with CaseSelectMapping[F, Y] with CaseMappingSetOperation[F, Y]
	{
		override def mappingQuery[M[O] <: ColumnMapping[V, O], V](e :ColumnMappingQuery[F, M, V]) :Y[GlobalScope, Rows[V]] =
			{ val res = mappingQuery(e :MappingQuery[F, M]); res }
	}

	trait CaseMappingQuery[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MatchMappingQuery[F, Y]
	{
		override def selectMapping[H[O] <: MappingAt[O]]
		                          (e :SelectSQL.SelectAs[F, H]) :Y[GlobalScope, Rows[H[Any]#Subject]] =
			mappingQuery(e)

		override def mappingSetOperation[M[O] <: MappingAt[O]]
		                                (operation :MappingSetOperation[F, M]) :Y[GlobalScope, Rows[M[Any]#Subject]] =
			mappingQuery(operation)
	}



	trait ColumnQueryMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnMappingQueryMatcher[F, Y] with SelectColumnMatcher[F, Y] with SetColumnOperationMatcher[F, Y]
	{
		def query[V](e :ColumnQuery[F, V]) :Y[GlobalScope, Rows[V]]
	}

	trait MatchColumnQuery[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnQueryMatcher[F, Y] with CaseColumnMappingQuery[F, Y]
		   with CaseSelectColumn[F, Y] with CaseSetColumnOperation[F, Y]
	{
		override def mappingQuery[M[O] <: ColumnMapping[V, O], V]
		                         (e :ColumnMappingQuery[F, M, V]) :Y[GlobalScope, Rows[V]] =
			query(e)
	}

	trait CaseColumnQuery[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MatchColumnQuery[F, Y]
	{
		override def select[V](e :SelectColumn[F, V]) :Y[GlobalScope, Rows[V]] = query(e)

		override def setOperation[V](e :SetColumnOperation[F, V]) :Y[GlobalScope, Rows[V]] = query(e)
	}



	trait QueryMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnQueryMatcher[F, Y] with MappingQueryMatcher[F, Y] with SelectMatcher[F, Y]
	{
		def query[V](e :QuerySQL[F, V]) :Y[GlobalScope, Rows[V]]
	}

	trait MatchQuery[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends QueryMatcher[F, Y] with MatchMappingQuery[F, Y] with CaseSelect[F, Y] with CaseSetOperation[F, Y]
	{
		override def query[V](e :ColumnQuery[F, V]) :Y[GlobalScope, Rows[V]] = query(e :QuerySQL[F, V])

		override def mappingQuery[M[O] <: MappingAt[O]](e :MappingQuery[F, M]) :Y[GlobalScope, Rows[M[Any]#Subject]] =
			query(e)
	}

	trait CaseQuery[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchQuery[F, Y] {

		override def select[V](e :SelectSQL[F, V]) :Y[GlobalScope, Rows[V]] = query(e)

		override def setOperation[V](e :SetColumnOperation[F, V]) :Y[GlobalScope, Rows[V]] = query(e)
	}

}

