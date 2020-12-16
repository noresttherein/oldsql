package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.{ColumnMapping, ColumnReadForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.Relation.{AlteredRelation, DerivedTable, Table}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.SelectAPI.{Intersect, Minus, QueryTemplate, SetOperator, Union, UnionAll}
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.ConditionSQL.ExistsSQL
import net.noresttherein.oldsql.sql.ast.QuerySQL.{ColumnQuery, QueryRelation, Rows}
import net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectColumnMapping.CompoundSelectColumnMappingMatcher
import net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectMapping.{CaseCompoundSelectMapping, CompoundSelectMappingMatcher}
import net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectColumn.{CaseCompoundSelectColumn, CompoundSelectColumnMatcher}
import net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectSQL.CaseCompoundSelect
import net.noresttherein.oldsql.sql.ast.SelectSQL.{CaseSelect, CaseSelectColumn, CaseSelectMapping, SelectColumn, SelectColumnMappingMatcher, SelectColumnMatcher, SelectMappingMatcher, SelectMatcher}
import net.noresttherein.oldsql.sql.mechanics.SQLScribe
import net.noresttherein.oldsql.sql.RowProduct.{ExtendedBy, PartOf}






/** An SQL expression returning a row cursor. It is the common base type for
  * the [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] type hierarchy and
  * compound selects on them (such as `UNION`): [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectSQL CompoundSelectSQL]].
  * It also forms its own hierarchy parallel to that of `SelectSQL` and `CompoundSelectSQL`, with subtypes
  * for queries returning a single [[net.noresttherein.oldsql.sql.ast.QuerySQL.ColumnQuery column]]
  * and a [[net.noresttherein.oldsql.sql.ast.QuerySQL.MappingQuery mapping]].
  */
trait QuerySQL[-F <: RowProduct, V]
	extends SQLExpression[F, GlobalScope, Rows[V]] with QueryTemplate[V, ({ type Q[X] = QuerySQL[F, X] })#Q]
{
	//overriden to grant access to subclasses in the companion object
	protected override def component[O] :ResultMapping[O]
	protected override def export[O] :RefinedMapping[ResultMapping[O]#Subject, O] //= component[O]

	override def isAnchored = true
	override def anchor(from :F) :QuerySQL[F, V] = this

	def exists :ColumnSQL[F, GlobalScope, Boolean] = ExistsSQL(this)

	def notExists :ColumnSQL[F, GlobalScope, Boolean] = !ExistsSQL(this)

	def single :SQLExpression[F, GlobalScope, V] = to[V]

	def rows :SQLExpression[F, GlobalScope, Seq[V]] = to[Seq[V]]

	//semantics of unions and rest:
	// - if the column set is the same, normal union
	// - if not, but the mapping is the same, than missing columns are added with null values to each operand
	// - if both are mapping-based, with mappings from the same hierarchy, use union column set with a discriminator column
	// - otherwise the column set becomes two separate sets with a discriminator
	def union[E <: F](other :QuerySQL[E, V]) :QuerySQL[E, V] = Union(this, other)
	def unionAll[E <: F](other :QuerySQL[E, V]) :QuerySQL[E, V] = UnionAll(this, other)
	def minus[E <: F](other :QuerySQL[E, V]) :QuerySQL[E, V] = Minus(this, other)
	def intersect[E <: F](other :QuerySQL[E, V]) :QuerySQL[E, V] = Intersect(this, other)


	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :QuerySQL[E, V]

	override def extend[U <: F, E <: RowProduct]
	                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :QuerySQL[E, V]

//	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//	                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
//		matcher.query(this)
}






sealed abstract class ImplicitQueryRelations {
	//we can't have a single implicit into Table[query.ResultMapping] and we need these casts as, above all,
	//  we need to preserve the value type of the query as the subject type, and bound MappingAt does not specify it.
	//todo: replace typedprojection with projection once we get rid of referernces to BaseMapping in API
	implicit def arbitraryQueryRelation[V](query :QuerySQL[RowProduct, V]) :Table[MappingOf[V]#TypedProjection] =
		QueryRelation[query.ResultMapping, V](query).asInstanceOf[Table[MappingOf[V]#TypedProjection]]

	implicit def singleColumnRelation[V](query :ColumnQuery[RowProduct, V]) :Table[MappingOf[V]#ColumnProjection] =
		QueryRelation[query.ResultMapping, V](query)
}



object QuerySQL extends ImplicitQueryRelations {

	/** The value type of `SelectSQL` instances with header (select clause) type `V`.
	  * This indirection allows the use of a SQL select expression both as a sequence (for example, inside `exists`)
	  * and as a single value (or rather, single row). Implicit conversions exist from `SQLExpression[F, S, Rows[V]]` to
	  * both `SQLExpression[F, S, V]` and `SQLExpression[F, S, Seq[V]]`.
	  */
	trait Rows[+V] {
		def size :Int = seq.size
		def isEmpty :Boolean = seq.isEmpty
		def nonEmpty :Boolean = seq.nonEmpty

		def seq :Seq[V]
		def single :V
		def head :V
		def headOption :Option[V]
	}


	object Rows {
		def apply[E](items :E*) :Rows[E] =
			if (items.isEmpty || items.sizeIs > 1) MultipleRows(items)
			else new SingleRow(items.head)

		def single[E](item :E) :Rows[E] = new SingleRow(item)


		implicit def readForm[T :SQLReadForm] :SQLReadForm[Rows[T]] =
			SQLReadForm.map("Rows[_]>")((t :T) => Rows(t))

		implicit def writeForm[T :SQLWriteForm] :SQLWriteForm[Rows[T]] =
			SQLWriteForm.unsupported("SQLWriteForm[Rows[_]]")


		private case class MultipleRows[+E](seq :Seq[E]) extends Rows[E] {
			def single :E = seq match {
				case Seq(res) => res
				case _ => throw new IllegalStateException("Expected a single result from a Rows instance, got " + seq.size)
			}
			def head :E = seq.head
			def headOption :Option[E] = seq.headOption
		}

		private class SingleRow[E](override val single :E) extends Rows[E] {
			override def head = single
			override def headOption = Some(single)
			override def seq :Seq[E] = single::Nil
		}
	}




	implicit class relationConversionMethod[M[O] <: MappingAt[O], V]
	               (private val self :QuerySQL[RowProduct, V] { type ResultMapping[O] = M[O] })
		extends AnyVal
	{
		def toRelation :Table[M] = QueryRelation[M, V](self)
		def toTable :Table[M] = QueryRelation[M, V](self)
//		def as[A <: Label](alias :A) :With[M] As A = With(alias, this) //todo: With for MappingQuery
	}


	implicit def derivedTable[M[O] <: MappingAt[O]](query :MappingQuery[RowProduct, M]) :Table[M] =
		QueryRelation[M, M[()]#Subject](query)



	def QueryRelation[M[O] <: MappingAt[O], V](query :QuerySQL[RowProduct, V] { type ResultMapping[O] = M[O] })
			:QueryRelation[M] =
		{ val q = query; new QueryRelation[M] { val query = q } }

	trait QueryRelation[M[O] <: MappingAt[O]] extends DerivedTable[M] { outer =>
		val query :QuerySQL[RowProduct, _] { type ResultMapping[O] = M[O] }

		override def apply[O] :M[O] = query.component[O]
//		override def altered[O] :RefinedMapping[M[O]#Subject, O] = query.export[O]
		override def export[O] :MappingAt[O] = query.export[O]

		override def sql :String = ??? //todo: default dialect SQL

		protected override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]])
				:QueryRelation[M] =
			new AlteredRelation[M](this, includes, excludes) with QueryRelation[M] {
				override val query = outer.query

				override def export[O] = super[AlteredRelation].export[O]

				override def alter(includes :Unique[RefinedMapping[_, _]], excludes :Unique[RefinedMapping[_, _]]) =
					outer.alter(
						(this.includes.view ++ includes).filterNot(excludes.contains(_)).to(Unique),
						(this.excludes.view.filterNot(includes.contains(_)) ++ excludes).to(Unique)
					)
			}
	}






	type * = QuerySQL[_ <: RowProduct, _]

	type GroundQuery[V] = QuerySQL[RowProduct, V]
	type GroundColumnQuery[V] = ColumnQuery[RowProduct, V]
	type GroundMappingQuery[H[A] <: MappingAt[A]] = MappingQuery[RowProduct, H]
	type GroundColumnMappingQuery[H[A] <: ColumnMapping[V, A], V] = ColumnMappingQuery[RowProduct, H, V]



	/** An SQL query, that is an SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL.SelectColumn select]] or
	  * a [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectColumn compound select]] on them, which returns
	  * a single column.
	  */
	trait ColumnQuery[-F <: RowProduct, V]
		extends QuerySQL[F, V] with ColumnSQL[F, GlobalScope, Rows[V]]
		   with QueryTemplate[V, ({ type Q[X] = ColumnQuery[F, X ]})#Q]
	{
		override type ResultMapping[O] <: ColumnMapping[V, O]

		override def anchor(from :F) :ColumnQuery[F, V] = this

		def union[E <: F](other :ColumnQuery[E, V]) :ColumnQuery[E, V] = Union(this, other)
		def unionAll[E <: F](other :ColumnQuery[E, V]) :ColumnQuery[E, V] = UnionAll(this, other)
		def minus[E <: F](other :ColumnQuery[E, V]) :ColumnQuery[E, V] = Minus(this, other)
		def intersect[E <: F](other :ColumnQuery[E, V]) :ColumnQuery[E, V] = Intersect(this, other)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnQuery[E, V]

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :ColumnQuery[E, V]

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.query(this)
	}


	/** An SQL query, that is an SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL.SelectAs select]] or
	  * a [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectMapping compound select]] on them,
	  * which provides [[net.noresttherein.oldsql.schema.Mapping mapping]] `M` for the returned rows.
	  */
	trait MappingQuery[-F <: RowProduct, M[O] <: MappingAt[O]] extends QuerySQL[F, M[()]#Subject] {
		override type ResultMapping[O] = M[O]

		def union[E <: F](other :MappingQuery[E, M]) :MappingQuery[E, M] = Union(this, other)
		def unionAll[E <: F](other :MappingQuery[E, M]) :MappingQuery[E, M] = UnionAll(this, other)
		def minus[E <: F](other :MappingQuery[E, M]) :MappingQuery[E, M] = Minus(this, other)
		def intersect[E <: F](other :MappingQuery[E, M]) :MappingQuery[E, M] = Intersect(this, other)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :MappingQuery[E, M]

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :MappingQuery[E, M]

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[M[()]#Subject]] =
//			matcher.mappingQuery(this)
	}


	/** An SQL query returning a single column of some relation.
	  * @see [[net.noresttherein.oldsql.sql.ast.SelectSQL.SelectColumnAs]]
	  * @see [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectColumnMapping]]
	  */
	trait ColumnMappingQuery[-F <: RowProduct, M[O] <: ColumnMapping[V, O], V]
		extends ColumnQuery[F, V] with MappingQuery[F, M]
	{
		def union[E <: F](other :ColumnMappingQuery[E, M, V]) :ColumnMappingQuery[E, M, V] = Union(this, other)
		def unionAll[E <: F](other :ColumnMappingQuery[E, M, V]) :ColumnMappingQuery[E, M, V] = UnionAll(this, other)
		def minus[E <: F](other :ColumnMappingQuery[E, M, V]) :ColumnMappingQuery[E, M, V] = Minus(this, other)
		def intersect[E <: F](other :ColumnMappingQuery[E, M, V]) :ColumnMappingQuery[E, M, V] = Intersect(this, other)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnMappingQuery[E, M, V]

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope)
				:ColumnMappingQuery[E, M, V]

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.mappingQuery(this)
	}



	/** Implements a compound select combining the result sets of two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). The kind of operation is defined by
	  * the [[net.noresttherein.oldsql.sql.SelectAPI.SetOperator SetOperator]]
	  * of the [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectSQL.operator operator]] member property.
	  * The row schemas of both arguments must match or an exception will be thrown when this expression
	  * is converted into an executable SQL statement. If the schema of any of the member ''selects'' is flexible
	  * (it is defined by a mapping with [[net.noresttherein.oldsql.schema.Buff.OptionalSelect optional]] columns),
	  * the schema of the first member is used for both of the arguments.
	  */
	trait CompoundSelectSQL[-F <: RowProduct, V] extends CompositeSQL[F, GlobalScope, Rows[V]] with QuerySQL[F, V] {
		val left :QuerySQL[F, V]
		val right :QuerySQL[F, V]
		val operator :SetOperator

		override def parts :Seq[QuerySQL[F, V]] = left::right::Nil
		override def readForm :SQLReadForm[Rows[V]] = left.readForm

		override def map[X](f :V => X) :QuerySQL[F, X] = CompoundSelectSQL(left.map(f), operator, right.map(f))

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :CompoundSelectSQL[E, V] =
			(mapper(left), mapper(right)) match {
				case (l :QuerySQL[E @unchecked, V @unchecked], r :QuerySQL[E @unchecked, V @unchecked]) =>
					CompoundSelectSQL(l, operator, r)
				case (l, r) =>
					throw new IllegalArgumentException(
						s"Failed $mapper transformation of $this: ($l, $r) is not a valid QuerySQL pair."
					)
			}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :QuerySQL[E, V] =
			(left.basedOn(base), right.basedOn(base)) match {
				case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[QuerySQL[E, V]]
				case (l, r) => operator(l, r)
			}

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :QuerySQL[E, V] =
			(left.extend(base), right.extend(base)) match {
				case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[QuerySQL[E, V]]
				case (l, r) => operator(l, r)
			}

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.compoundSelect(this)


		override def sameAs(that :CompositeSQL.*) :Boolean = that match {
			case op :CompoundSelectSQL[_, _] => op.operator == operator
			case _ => false
		}

		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :CompoundSelectSQL[_, _] if canEqual(other) && other.canEqual(this) =>
				operator == other.operator && left == other.left && right == other.right
			case _ => false
		}

		override def hashCode :Int = (operator.hashCode * 31 + left.hashCode) * 31 + right.hashCode

		override def toString :String = s"($left) $operator ($right)"
	}



	object CompoundSelectSQL {
		def apply[F <: RowProduct, V](left :QuerySQL[F, V], operator :SetOperator, right :QuerySQL[F, V])
				:CompoundSelectSQL[F, V] =
			new BaseCompoundSelectSQL(left, operator, right)

		def unapply[F <: RowProduct, V](e :SQLExpression[F, LocalScope, Rows[V]])
				:Option[(QuerySQL[F, V], SetOperator, QuerySQL[F, V])] =
			e match {
				case op :CompoundSelectSQL[F @unchecked, V @unchecked] => Some((op.left, op.operator, op.right))
				case _ => None
			}

		private[QuerySQL] class BaseCompoundSelectSQL[-F <: RowProduct, V]
		                        (override val left :QuerySQL[F, V], override val operator :SetOperator,
		                         override val right :QuerySQL[F, V])
			extends CompoundSelectSQL[F, V]
		{
			override type ResultMapping[O] = left.ResultMapping[O]
			protected override def component[O] = left.component[O]
			protected override def export[O] = left.export[O] //todo: this should involve some reconciliation
		}



		trait CompoundSelectMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompoundSelectMappingMatcher[F, Y] with CompoundSelectColumnMatcher[F, Y]
		{
			def compoundSelect[V](e :CompoundSelectSQL[F, V]) :Y[GlobalScope, Rows[V]]
		}

		trait MatchCompoundSelect[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompoundSelectMatcher[F, Y] with CaseCompoundSelectMapping[F, Y]
		{
			override def compoundSelect[V](e :CompoundSelectColumn[F, V]) :Y[GlobalScope, Rows[V]] =
				compoundSelect(e :CompoundSelectSQL[F, V])
		}

		trait CaseCompoundSelect[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends MatchCompoundSelect[F, Y]
		{
			override def compoundSelectMapping[M[O] <: MappingAt[O]]
			                                (e :CompoundSelectMapping[F, M]) :Y[GlobalScope, Rows[M[()]#Subject]] =
				compoundSelect(e)
		}

	}






	/** Implements a compound select combining the result sets of two single-column
	  * SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL.SelectColumn selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL.ColumnQuery queries]]). The kind of operation is defined by
	  * the [[net.noresttherein.oldsql.sql.SelectAPI.SetOperator SetOperator]]
	  * of the [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectSQL.operator operator]] member property.
	  */
	trait CompoundSelectColumn[-F <: RowProduct, V]
		extends CompositeColumnSQL[F, GlobalScope, Rows[V]] with ColumnQuery[F, V] with CompoundSelectSQL[F, V]
	{
		override val left :ColumnQuery[F, V]
		override val right :ColumnQuery[F, V]

		override def readForm :ColumnReadForm[Rows[V]] = left.readForm

		override def map[X](f :V => X) :ColumnQuery[F, X] = CompoundSelectColumn(left.map(f), operator, right.map(f))

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :CompoundSelectColumn[E, V] =
			(mapper(left), mapper(right)) match {
				case (l :ColumnQuery[E @unchecked, V @unchecked], r :ColumnQuery[E @unchecked, V @unchecked]) =>
					CompoundSelectColumn(l, operator, r)
				case (l, r) =>
					throw new IllegalArgumentException(
						s"Failed $mapper transformation of $this: ($l, $r) is not a valid ColumnQuery pair."
					)
			}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnQuery[E, V] =
			(left.basedOn(base), right.basedOn(base)) match {
				case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[ColumnQuery[E, V]]
				case (l, r) => operator(l, r)
			}

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :ColumnQuery[E, V] =
			(left.extend(base), right.extend(base)) match {
				case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[ColumnQuery[E, V]]
				case (l, r) => operator(l, r)
			}


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[V]] =
			matcher.compoundSelect(this)
	}



	object CompoundSelectColumn {
		def apply[F <: RowProduct, V](left :ColumnQuery[F, V], operator :SetOperator, right :ColumnQuery[F, V])
				:CompoundSelectColumn[F, V] =
			new BaseCompoundSelectColumn(left, operator, right)

		def unapply[F <: RowProduct, V](e :SQLExpression[F, LocalScope, Rows[V]])
				:Option[(ColumnQuery[F, V], SetOperator, ColumnQuery[F, V])] =
			e match {
				case op :CompoundSelectColumn[F @unchecked, V @unchecked] => Some((op.left, op.operator, op.right))
				case _ => None
			}

		private[QuerySQL] class BaseCompoundSelectColumn[-F <: RowProduct, V]
		                                                 (override val left :ColumnQuery[F, V],
		                                                  override val operator :SetOperator,
		                                                  override val right :ColumnQuery[F, V])
			extends CompoundSelectColumn[F, V]
		{
			override type ResultMapping[O] = ColumnMapping[V, O]
			protected override def component[O] = left.component
			protected override def export[O] = left.export //todo: this should involve some reconciliation
		}


		trait CompoundSelectColumnMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompoundSelectColumnMappingMatcher[F, Y]
		{
			def compoundSelect[V](e :CompoundSelectColumn[F, V]) :Y[GlobalScope, Rows[V]]
		}

		type MatchCompoundSelectColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			CompoundSelectColumnMatcher[F, Y]

		trait CaseCompoundSelectColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends MatchCompoundSelectColumn[F, Y]
		{
			override def compoundSelectMapping[M[O] <: ColumnMapping[V, O], V]
		                                      (e :CompoundSelectColumnMapping[F, M, V]) :Y[GlobalScope, Rows[V]] =
				compoundSelect(e)
		}
	}






	/** Implements a compound select combining the result sets of two
	  * SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL.SelectAs selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL.MappingQuery queries]]), sharing the same row schema,
	  * as defined by the mapping `M`. The kind of operation is defined by
	  * the [[net.noresttherein.oldsql.sql.SelectAPI.SetOperator SetOperator]]
	  * of the [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectSQL.operator operator]] member property.
	  */
	trait CompoundSelectMapping[-F <: RowProduct, M[O] <: MappingAt[O]]
		extends CompoundSelectSQL[F, M[()]#Subject] with MappingQuery[F, M]
	{
		override val left :MappingQuery[F, M]
		override val right :MappingQuery[F, M]

		protected override def component[O] = left.component
		protected override def export[O] = left.export //todo: this should involve some reconciliation

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :CompoundSelectMapping[E, M] =
			(mapper(left), mapper(right)) match {
				case (l :MappingQuery[E @unchecked, M @unchecked], r :MappingQuery[E @unchecked, M @unchecked]) =>
					CompoundSelectMapping(l, operator, r)
				case (l, r) =>
					throw new IllegalArgumentException(
						s"Failed $mapper transformation of $this: ($l, $r) is not a valid ColumnQuery pair."
					)
			}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :MappingQuery[E, M] =
			(left.basedOn(base), right.basedOn(base)) match {
				case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[MappingQuery[E, M]]
				case (l, r) => operator(l, r)
			}

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :MappingQuery[E, M] =
			(left.extend(base), right.extend(base)) match {
				case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[MappingQuery[E, M]]
				case (l, r) => operator(l, r)
			}

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ExpressionMatcher[F, Y]) :Y[GlobalScope, Rows[M[()]#Subject]] =
			matcher.compoundSelectMapping(this)
	}



	object CompoundSelectMapping {
		def apply[F <: RowProduct, M[O] <: MappingAt[O]]
		         (left :MappingQuery[F, M], operator :SetOperator, right :MappingQuery[F, M])
				:CompoundSelectMapping[F, M] =
			new BaseCompoundSelectMapping(left, operator, right)

		def unapply[F <: RowProduct, V](e :SQLExpression[F, LocalScope, Rows[V]])
				:Option[(MappingQuery[F, M], SetOperator, MappingQuery[F, M]) forSome { type M[O] <: MappingAt[O] }] =
			e match {
				case op :CompoundSelectMapping[F @unchecked, MappingAt @unchecked] =>
					Some((op.left, op.operator, op.right))
				case _ => None
			}

		private[QuerySQL] class BaseCompoundSelectMapping[-F <: RowProduct, M[O] <: MappingAt[O]]
		                        (override val left :MappingQuery[F, M], override val operator :SetOperator,
		                         override val right :MappingQuery[F, M])
			extends CompoundSelectMapping[F, M]



		trait CompoundSelectMappingMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompoundSelectColumnMappingMatcher[F, Y]
		{
			def compoundSelectMapping[M[O] <: MappingAt[O]]
			                         (e :CompoundSelectMapping[F, M]) :Y[GlobalScope, Rows[M[()]#Subject]]
		}

		trait MatchCompoundSelectMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompoundSelectMappingMatcher[F, Y]
		{
			override def compoundSelectMapping[M[O] <: ColumnMapping[V, O], V]
			                                  (e :CompoundSelectColumnMapping[F, M, V]) :Y[GlobalScope, Rows[V]] =
				{ val res = compoundSelectMapping(e :CompoundSelectMapping[F, M]); res  }
		}

		type CaseCompoundSelectMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			MatchCompoundSelectMapping[F, Y]

	}






	trait CompoundSelectColumnMapping[-F <: RowProduct, M[O] <: ColumnMapping[V, O], V]
		extends CompoundSelectColumn[F, V] with ColumnMappingQuery[F, M, V] with CompoundSelectMapping[F, M]
	{
		override val left :ColumnMappingQuery[F, M, V]
		override val right :ColumnMappingQuery[F, M, V]


		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnMappingQuery[E, M, V] =
			(left.basedOn(base), right.basedOn(base)) match {
				case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[ColumnMappingQuery[E, M, V]]
				case (l, r) => operator(l, r)
			}

		override def extend[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :ColumnMappingQuery[E, M, V] =
			(left.extend(base), right.extend(base)) match {
				case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[ColumnMappingQuery[E, M, V]]
				case (l, r) => operator(l, r)
			}

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :CompoundSelectColumnMapping[E, M, V] =
			(mapper(left), mapper(right)) match {
				case (l :ColumnMappingQuery[E @unchecked, M @unchecked, V @unchecked],
				      r :ColumnMappingQuery[E @unchecked, M @unchecked, V @unchecked]) =>
					CompoundSelectColumnMapping(l, operator, r)
				case (l, r) =>
					throw new IllegalArgumentException(
						s"Failed $mapper transformation of $this: ($l, $r) is not a valid ColumnQuery pair."
					)
			}

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                    (matcher :ColumnMatcher[F, Y]) :Y[GlobalScope, Rows[M[()]#Subject]] =
			matcher.compoundSelectMapping(this)
	}



	object CompoundSelectColumnMapping {
		def apply[F <: RowProduct, M[O] <: ColumnMapping[V, O], V]
		         (left :ColumnMappingQuery[F, M, V], operator :SetOperator, right :ColumnMappingQuery[F, M, V])
				:CompoundSelectColumnMapping[F, M, V] =
			new BaseCompoundSelectColumnMapping(left, operator, right)

		def unapply[F <: RowProduct, V](e :SQLExpression[F, LocalScope, Rows[V]])
				:Option[(ColumnMappingQuery[F, M, V], SetOperator, ColumnMappingQuery[F, M, V]) forSome { type M[O] <: ColumnMapping[V, O] }] =
			e match {
				case op :CompoundSelectColumnMapping[F @unchecked, MappingOf[V]#ColumnProjection @unchecked, V @unchecked] =>
					Some((op.left, op.operator, op.right))
				case _ => None
			}

		private[QuerySQL] class BaseCompoundSelectColumnMapping[-F <: RowProduct, M[O] <: ColumnMapping[V, O], V]
		                        (override val left :ColumnMappingQuery[F, M, V], override val operator :SetOperator,
		                         override val right :ColumnMappingQuery[F, M, V])
			extends CompoundSelectColumnMapping[F, M, V]



		trait CompoundSelectColumnMappingMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def compoundSelectMapping[M[O] <: ColumnMapping[V, O], V]
			                       (e :CompoundSelectColumnMapping[F, M, V]) :Y[GlobalScope, Rows[V]]
		}

		type MatchCompoundSelectColumnMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			CompoundSelectColumnMappingMatcher[F, Y]

		type CaseCompoundSelectColumnMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			CompoundSelectColumnMappingMatcher[F, Y]
	}






	trait ColumnMappingQueryMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends CompoundSelectColumnMappingMatcher[F, Y] with SelectColumnMappingMatcher[F, Y]
	{
		def mappingQuery[M[O] <: ColumnMapping[V, O], V](e :ColumnMappingQuery[F, M, V]) :Y[GlobalScope, Rows[V]]
	}

	type MatchColumnMappingQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
		ColumnMappingQueryMatcher[F, Y]

	trait CaseColumnMappingQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MatchColumnMappingQuery[F, Y]
	{
		override def selectMapping[H[O] <: ColumnMapping[V, O], V]
		                          (e :SelectSQL.SelectColumnAs[F, H, V]) :Y[GlobalScope, Rows[V]] =
			mappingQuery(e)

		override def compoundSelectMapping[M[O] <: ColumnMapping[V, O], V]
		                                  (e :CompoundSelectColumnMapping[F, M, V]) :Y[GlobalScope, Rows[V]] =
			mappingQuery(e)
	}



	trait MappingQueryMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnMappingQueryMatcher[F, Y] with CompoundSelectMappingMatcher[F, Y] with SelectMappingMatcher[F, Y]
	{
		def mappingQuery[M[O] <: MappingAt[O]](e :MappingQuery[F, M]) :Y[GlobalScope, Rows[M[()]#Subject]]
	}

	trait MatchMappingQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MappingQueryMatcher[F, Y] with CaseSelectMapping[F, Y] with CaseCompoundSelectMapping[F, Y]
	{
		override def mappingQuery[M[O] <: ColumnMapping[V, O], V](e :ColumnMappingQuery[F, M, V]) :Y[GlobalScope, Rows[V]] =
			{ val res = mappingQuery(e :MappingQuery[F, M]); res }
	}

	trait CaseMappingQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MatchMappingQuery[F, Y]
	{
		override def selectMapping[H[O] <: MappingAt[O]]
		                          (e :SelectSQL.SelectAs[F, H]) :Y[GlobalScope, Rows[H[()]#Subject]] =
			mappingQuery(e)

		override def compoundSelectMapping[M[O] <: MappingAt[O]]
		                                  (operation :CompoundSelectMapping[F, M]) :Y[GlobalScope, Rows[M[()]#Subject]] =
			mappingQuery(operation)
	}



	trait ColumnQueryMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnMappingQueryMatcher[F, Y] with SelectColumnMatcher[F, Y] with CompoundSelectColumnMatcher[F, Y]
	{
		def query[V](e :ColumnQuery[F, V]) :Y[GlobalScope, Rows[V]]
	}

	trait MatchColumnQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnQueryMatcher[F, Y] with CaseColumnMappingQuery[F, Y]
		   with CaseSelectColumn[F, Y] with CaseCompoundSelectColumn[F, Y]
	{
		override def mappingQuery[M[O] <: ColumnMapping[V, O], V]
		                         (e :ColumnMappingQuery[F, M, V]) :Y[GlobalScope, Rows[V]] =
			query(e)
	}

	trait CaseColumnQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MatchColumnQuery[F, Y]
	{
		override def select[V](e :SelectColumn[F, V]) :Y[GlobalScope, Rows[V]] = query(e)

		override def compoundSelect[V](e :CompoundSelectColumn[F, V]) :Y[GlobalScope, Rows[V]] = query(e)
	}



	trait QueryMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnQueryMatcher[F, Y] with MappingQueryMatcher[F, Y] with SelectMatcher[F, Y]
	{
		def query[V](e :QuerySQL[F, V]) :Y[GlobalScope, Rows[V]]
	}

	trait MatchQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends QueryMatcher[F, Y] with MatchMappingQuery[F, Y] with CaseSelect[F, Y] with CaseCompoundSelect[F, Y]
	{
		override def query[V](e :ColumnQuery[F, V]) :Y[GlobalScope, Rows[V]] = query(e :QuerySQL[F, V])

		override def mappingQuery[M[O] <: MappingAt[O]](e :MappingQuery[F, M]) :Y[GlobalScope, Rows[M[()]#Subject]] =
			query(e)
	}

	trait CaseQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchQuery[F, Y] {

		override def select[V](e :SelectSQL[F, V]) :Y[GlobalScope, Rows[V]] = query(e)

		override def compoundSelect[V](e :CompoundSelectColumn[F, V]) :Y[GlobalScope, Rows[V]] = query(e)
	}

}

