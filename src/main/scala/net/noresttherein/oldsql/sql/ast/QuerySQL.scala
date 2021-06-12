package net.noresttherein.oldsql.sql.ast

import scala.collection.{EvidenceIterableFactory, IterableFactory}

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{IllegalSQLException, InseparableExpressionException}
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.schema.{ColumnMapping, ColumnReadForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.Relation.{SelectRelation, Table}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLDialect, SQLExpression, StandardSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult
import net.noresttherein.oldsql.sql.Incantation.Cantrip
import net.noresttherein.oldsql.sql.Query.QueryTemplate
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, ParamlessFrom, PartOf}
import net.noresttherein.oldsql.sql.Select.{Intersect, Minus, SetOperator, Union, UnionAll}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionVisitor, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.CompositeColumnSQL
import net.noresttherein.oldsql.sql.ast.ConditionSQL.ExistsSQL
import net.noresttherein.oldsql.sql.ast.QuerySQL.{ColumnQuery, Rows}
import net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectColumnMapping.CompoundSelectColumnMappingVisitor
import net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectMappingSQL.{CaseCompoundSelectMapping, CompoundSelectMappingVisitor}
import net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectColumn.{CaseCompoundSelectColumn, CompoundSelectColumnVisitor}
import net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectSQL.CaseCompoundSelect
import net.noresttherein.oldsql.sql.ast.SelectSQL.{CaseSelect, CaseSelectColumn, CaseSelectMapping, SelectColumn, SelectColumnMappingVisitor, SelectColumnVisitor, SelectMappingVisitor, SelectVisitor}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** An SQL expression returning a row cursor. It is the common base type for
  * the [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] type hierarchy and
  * compound selects on them (such as `UNION`): [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectSQL CompoundSelectSQL]].
  * It also forms its own hierarchy parallel to that of `SelectSQL` and `CompoundSelectSQL`, with subtypes
  * for queries returning a single [[net.noresttherein.oldsql.sql.ast.QuerySQL.ColumnQuery column]]
  * and a [[net.noresttherein.oldsql.sql.ast.QuerySQL.MappingQuerySQL mapping]].
  */
trait QuerySQL[-F <: RowProduct, V]
	extends SQLExpression[F, GlobalScope, Rows[V]] with QueryTemplate[V, ({ type Q[X] = QuerySQL[F, X] })#Q]
{
	def rowForm :SQLReadForm[V]

	override def isGlobal = true
	override def asGlobal :Option[QuerySQL[F, V]] = Some(this)
	override def groundValue :Opt[Rows[V]] = Lack

	override def isAnchored = true
	override def anchor(from :F) :QuerySQL[F, V] = this

	/** Wraps this ''select'' in an SQL `EXISTS(...)` function. */
	def exists :ColumnSQL[F, GlobalScope, Boolean] = ExistsSQL(this)

	/** Wraps this ''select'' in SQL `NOT EXISTS(...)`. */
	def notExists :ColumnSQL[F, GlobalScope, Boolean] = !ExistsSQL(this)

	/** Converts this ''select'' to an expression for its returned value, representing its usage in SQL assuming
	  * that exactly one row is returned.
	  */
	def one :SQLExpression[F, GlobalScope, V] = to[V]

	/** Converts this ''select'' to an expression for a result set which can be used in conjunction with
	  * [[net.noresttherein.oldsql.sql.ast.ConditionSQL.ExistsSQL ExistsSQL]] or
	  * [[net.noresttherein.oldsql.sql.ast.ConditionSQL.InSQL InSQL]] and the like.
	  */
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

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :QuerySQL[E, V]

//	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//	                    (matcher :ExpressionVisitor[F, Y]) :Y[GlobalScope, Rows[V]] =
//		matcher.query(this)

	@throws[InseparableExpressionException]("If this query is a not a ColumnQuerySQL.")
	override def split(implicit scope :OperationType) :Seq[ColumnSQL[F, GlobalScope, _]] =
		throw new InseparableExpressionException("Cannot split a non-column query " + this + " into columns.")

	/** Target of [[net.noresttherein.oldsql.sql.ast.QuerySQL.TopQuerySQLExtension.spell spell]] extension method.
	  * Forwards to
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.spell[V](query:QuerySQL[RowProduct,V])* SQLSpelling.spell]]
	  * and caches the result. All future calls using the same `spelling` argument will return the cached SQL.
	  * This method should not be generally overriden, unless to additionally enrich the result without changing
	  * the actual SQL.
	  */
	protected def spellParamless(implicit spelling :SQLSpelling, top :QuerySQL[F, V] <:< QuerySQL[RowProduct, V])
			:SpelledSQL[@~, RowProduct] =
		cachedSpelling match {
			case null =>
				val sql = spelling.spell(top(this))
				cachedSQL = sql
				cachedSpelling = spelling
				sql
			case s if s == spelling => cachedSQL
			case _ => spelling.spell(top(this))
		}
	//		if (spelling == StandardSQL.spelling) standardSQL else spelling.spell(this)

	@volatile private var cachedSQL :SpelledSQL[@~, RowProduct] = _
	@volatile private var cachedSpelling :SQLSpelling = _
	//	private lazy val standardSQL = StandardSQL.spelling.spell(this)


	/** Formats this query as an SQL `String` expression for use inside a tuple with other expressions,
	  * or where tuples cannot be used. As ''selects'' cannot be inlined, this method throws an `IllegalStateException`
	  * if its [[net.noresttherein.oldsql.sql.SQLExpression.readForm read form]]'s column count is greater than `1`.
	  * Single column instances, even when not conforming to [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]],
	  * delegate to [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]].
	  */
	protected override def inlineSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, E]] =
		readForm.readColumns match {
			case 1 => defaultSpelling(context, params)::Nil
			case n =>
				throw new InseparableExpressionException(
					s"Cannot inline a compound select of multiple ($n :$readForm) columns: $this."
				)
		}

	/** Generates the SQL `String` for this query as a parameterless expression. The rationale for not combining this
	  * together with `defaultSpelling` is that it once was impossible to create an instance of `Parameterization`,
	  * even without any unbound parameters, without providing a `RowProduct` - and it might be so again.
	  * Currently though this method does forward to
	  * [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]].
	  */
	protected def paramlessSpelling(context :SQLContext)
	                               (implicit spelling :SQLSpelling, top :QuerySQL[F, V] <:< QuerySQL[RowProduct, V])
			:SpelledSQL[@~, RowProduct] =
		top(this).defaultSpelling[@~, RowProduct](spelling)(context, Parameterization.paramless[ParamlessFrom])

	private[oldsql] final def paramlessSpelling(spelling :SQLSpelling, context :SQLContext)
	                                           (implicit top :QuerySQL[F, V] <:< QuerySQL[RowProduct, V])
			:SpelledSQL[@~, RowProduct] =
		paramlessSpelling(context)(spelling, top)


	/** Target of [[net.noresttherein.oldsql.sql.ast.QuerySQL.TopQuerySQLExtension.spell spell]] extension method.
	  * Simply forwards to [[net.noresttherein.oldsql.sql.SQLDialect.apply[R,Y](query:QuerySQL[RowProduct,V])* apply]]
	  * method of the implicit [[net.noresttherein.oldsql.sql.SQLDialect dialect]].
	  */
	protected def chantParamless[R](implicit composition :StatementResult[V, R], dialect :SQLDialect,
	                                         top :QuerySQL[F, V] <:< QuerySQL[RowProduct, V]) :Cantrip[R] =
		dialect(top(this))


	override def canEqual(that :Any) :Boolean = that.getClass == getClass
}






sealed abstract class ImplicitDerivedTables {
	//we can't have a single implicit into Table[query.ResultMapping] and we need these casts as, above all,
	//  we need to preserve the value type of the query as the subject type, and bound MappingAt does not specify it.
	//todo: replace typed projection with projection once we get rid of references to BaseMapping in API
	implicit def arbitraryQueryRelation[V](query :QuerySQL[RowProduct, V]) :Table[MappingOf[V]#TypedProjection] =
		SelectRelation[query.ResultMapping, V](query).asInstanceOf[Table[MappingOf[V]#TypedProjection]]

	implicit def singleColumnRelation[V](query :ColumnQuery[RowProduct, V]) :Table[MappingOf[V]#ColumnProjection] =
		SelectRelation[query.ResultMapping, V](query)
}



object QuerySQL extends ImplicitDerivedTables {

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
		def one :V
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
			override def one :E = seq match {
				case Seq(res) => res
				case _ => throw new IllegalStateException("Expected a single result from a Rows instance, got " + seq.size)
			}
			override def head :E = seq.head
			override def headOption :Option[E] = seq.headOption
		}

		private class SingleRow[E](override val one :E) extends Rows[E] {
			override def head = one
			override def headOption = Some(one)
			override def seq :Seq[E] = one::Nil
		}
	}




	implicit class TopQuerySQLExtension[M[O] <: MappingAt[O], V]
	               (private val self :QuerySQL[RowProduct, V] { type ResultMapping[O] = M[O] })
		extends AnyVal
	{
		def toRelation :Table[M] = SelectRelation[M, V](self)
		def toTable :Table[M] = SelectRelation[M, V](self)

//		def as[A <: Label](alias :A) :With[M] As A = With(alias, this) //todo: With for MappingQuerySQL
		//extracted because of conflicts in TopSelect with Query
		/** Translates the abstract syntax tree of this SQL expression into textual SQL.
		  * The result is adapted to the DBMS at hand using implicitly available
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] strategy. Aside from the `String`
		  * representation, returned [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]] contains
		  * write form setting all bound parameters of the query.
		  */ //an extension method as TopSelect would inherit conflicting signatures from Select and SelectSQL
		@throws[IllegalSQLException]("if the expression cannot be rendered as SQL for the DMBS particular to the given dialect.")
		def spell(implicit spelling :SQLSpelling = StandardSQL.spelling) :SpelledSQL[@~, RowProduct] =
			self.spellParamless


		/** Converts this query SQL expression into an executable [[net.noresttherein.oldsql.sql.Incantation Incantation]]
		  * proper to the DBMS using the implicit [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]].
		  * The implementation is delegated to the dialect object.
		  */
		@throws[IllegalSQLException]("if the expression cannot be rendered as SQL for the DMBS particular to the given dialect.")
		def chant[R](implicit composition :StatementResult[V, R], dialect :SQLDialect = StandardSQL) :Cantrip[R] =
			self.chantParamless

		/** Converts this query SQL expression into an executable
		  * [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[(), C[V]]` proper to the implicit
		  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]. The results of the query will be returned as
		  * a collection `C[V]` with the given factory.
		  */
		def returnAs[C[_]](collection :IterableFactory[C])(implicit dialect :Maybe[SQLDialect]) :Cantrip[C[V]] =
			chant[C[V]](StatementResult(collection)(self.rowForm), dialect getOrElse StandardSQL)

		/** Converts this query SQL expression into an executable
		  * [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[(), C[V]]` proper to the implicit
		  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]. The results of the query will be returned as
		  * a collection `C[V]` with the given factory.
		  */
		def returnAs[C[_], E[_]](collection :EvidenceIterableFactory[C, E])
		                        (implicit ev :E[V], dialect :Maybe[SQLDialect]) :Cantrip[C[V]] =
			chant[C[V]](StatementResult(collection)(ev, self.rowForm), dialect getOrElse StandardSQL)
	}


	implicit def derivedTable[M[O] <: MappingAt[O]](query :MappingQuerySQL[RowProduct, M]) :Table[M] =
		SelectRelation[M, M[Unit]#Subject](query)




	type * = QuerySQL[_ <: RowProduct, _]

	type GroundQuery[V] = QuerySQL[RowProduct, V]
	type GroundColumnQuery[V] = ColumnQuery[RowProduct, V]
	type GroundMappingQuery[H[A] <: MappingAt[A]] = MappingQuerySQL[RowProduct, H]
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

		override def asGlobal :Option[ColumnQuery[F, V]] = Some(this)
		override def anchor(from :F) :ColumnQuery[F, V] = this

		def union[E <: F](other :ColumnQuery[E, V]) :ColumnQuery[E, V] = Union(this, other)
		def unionAll[E <: F](other :ColumnQuery[E, V]) :ColumnQuery[E, V] = UnionAll(this, other)
		def minus[E <: F](other :ColumnQuery[E, V]) :ColumnQuery[E, V] = Minus(this, other)
		def intersect[E <: F](other :ColumnQuery[E, V]) :ColumnQuery[E, V] = Intersect(this, other)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnQuery[E, V]

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :ColumnQuery[E, V]

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ColumnVisitor[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.query(this)
	}


	/** An SQL query, that is an SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL.SelectAs select]] or
	  * a [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectMappingSQL compound select]] on them,
	  * which provides [[net.noresttherein.oldsql.schema.Mapping mapping]] `M` for the returned rows.
	  */
	trait MappingQuerySQL[-F <: RowProduct, M[O] <: MappingAt[O]] extends QuerySQL[F, M[Unit]#Subject] {
		override type ResultMapping[O] = M[O]

		def union[E <: F](other :MappingQuerySQL[E, M]) :MappingQuerySQL[E, M] = Union(this, other)
		def unionAll[E <: F](other :MappingQuerySQL[E, M]) :MappingQuerySQL[E, M] = UnionAll(this, other)
		def minus[E <: F](other :MappingQuerySQL[E, M]) :MappingQuerySQL[E, M] = Minus(this, other)
		def intersect[E <: F](other :MappingQuerySQL[E, M]) :MappingQuerySQL[E, M] = Intersect(this, other)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :MappingQuerySQL[E, M]

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :MappingQuerySQL[E, M]

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ExpressionVisitor[F, Y]) :Y[GlobalScope, Rows[M[()]#Subject]] =
//			matcher.mappingQuery(this)
	}


	/** An SQL query returning a single column of some relation.
	  * @see [[net.noresttherein.oldsql.sql.ast.SelectSQL.SelectColumnAs]]
	  * @see [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectColumnMapping]]
	  */
	trait ColumnMappingQuery[-F <: RowProduct, M[O] <: ColumnMapping[V, O], V]
		extends ColumnQuery[F, V] with MappingQuerySQL[F, M]
	{
		def union[E <: F](other :ColumnMappingQuery[E, M, V]) :ColumnMappingQuery[E, M, V] = Union(this, other)
		def unionAll[E <: F](other :ColumnMappingQuery[E, M, V]) :ColumnMappingQuery[E, M, V] = UnionAll(this, other)
		def minus[E <: F](other :ColumnMappingQuery[E, M, V]) :ColumnMappingQuery[E, M, V] = Minus(this, other)
		def intersect[E <: F](other :ColumnMappingQuery[E, M, V]) :ColumnMappingQuery[E, M, V] = Intersect(this, other)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnMappingQuery[E, M, V]

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:ColumnMappingQuery[E, M, V]

//		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
//		                    (matcher :ColumnVisitor[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.mappingQuery(this)
	}



	/** Implements a compound select combining the result sets of two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). The kind of operation is defined by
	  * the [[net.noresttherein.oldsql.sql.Select.SetOperator SetOperator]]
	  * of the [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectSQL.operator operator]] member property.
	  * The row schemas of both arguments must match or an exception will be thrown when this expression
	  * is converted into an executable SQL statement. If the schema of any of the member ''selects'' is flexible
	  * (it is defined by a mapping with [[net.noresttherein.oldsql.schema.Buff.OptionalSelect Optional]] columns),
	  * the schema of the first member is used for both of the arguments.
	  */
	trait CompoundSelectSQL[-F <: RowProduct, V] extends CompositeSQL[F, GlobalScope, Rows[V]] with QuerySQL[F, V] {
		validateCompatibility()

		val left :QuerySQL[F, V]
		val right :QuerySQL[F, V]
		val operator :SetOperator

		override def parts :Seq[QuerySQL[F, V]] = left::right::Nil
		override def readForm :SQLReadForm[Rows[V]] = left.readForm
		override def rowForm :SQLReadForm[V] = left.rowForm

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

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :QuerySQL[E, V] =
			(left.expand(base), right.expand(base)) match {
				case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[QuerySQL[E, V]]
				case (l, r) => operator(l, r)
			}

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                              (visitor :ExpressionVisitor[F, Y]) :Y[GlobalScope, Rows[V]] =
			visitor.compoundSelect(this)


		override def columnCount(implicit spelling :SQLSpelling) :Int = {
			val l = left.columnCount
			if (l != right.columnCount)
				throw new IllegalStateException(
					s"Differing column counts of member selects of $this: $l vs ${right.columnCount}."
				)
			l
		}

		protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		{
			val l = left match {
				case CompoundSelectSQL(_, op, _) if op != operator =>
					"(" +: (spelling(left :QuerySQL[E, V])(context, params) + ")")
				case _ =>
					spelling(left :QuerySQL[E, V])(context, params)
			}
			val r = right match {
				case CompoundSelectSQL(_, op, _) if op != operator || operator == Minus =>
					"(" +: (spelling(right :QuerySQL[E, V])(context, params.reset()) + ")")
				case _ =>
					spelling(right :QuerySQL[E, V])(context, l.params)
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
			if (left.readForm.readColumns != right.readForm.readColumns)
				throw new IllegalArgumentException(
					s"Cannot combine two selects of varying column counts ${left.readForm.readColumns}(${left.readForm})" +
						s" and ${right.readForm.readColumns}(${right.readForm}) into a combined select $this."
				)
			if (left.readForm.readColumns == 0)
				throw new IllegalArgumentException(
					s"Cannot create a compound select with a zero column count (${left.readForm}): $this."
				)
		}


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
				:Opt[(QuerySQL[F, V], SetOperator, QuerySQL[F, V])] =
			e match {
				case op :CompoundSelectSQL[F @unchecked, V @unchecked] => Got((op.left, op.operator, op.right))
				case _ => Lack
			}

		private[QuerySQL] class BaseCompoundSelectSQL[-F <: RowProduct, V]
		                        (override val left :QuerySQL[F, V], override val operator :SetOperator,
		                         override val right :QuerySQL[F, V])
			extends CompoundSelectSQL[F, V]
		{
			override type ResultMapping[O] = left.ResultMapping[O]
			override def mapping[O] = left.mapping[O]
			override def export[O] = left.export[O] //todo: this should involve some reconciliation
		}



		trait CompoundSelectVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompoundSelectMappingVisitor[F, Y] with CompoundSelectColumnVisitor[F, Y]
		{
			def compoundSelect[V](e :CompoundSelectSQL[F, V]) :Y[GlobalScope, Rows[V]]
		}

		trait MatchCompoundSelect[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompoundSelectVisitor[F, Y] with CaseCompoundSelectMapping[F, Y]
		{
			override def compoundSelect[V](e :CompoundSelectColumn[F, V]) :Y[GlobalScope, Rows[V]] =
				compoundSelect(e :CompoundSelectSQL[F, V])
		}

		trait CaseCompoundSelect[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends MatchCompoundSelect[F, Y]
		{
			override def compoundSelectMapping[M[O] <: MappingAt[O]]
			                                (e :CompoundSelectMappingSQL[F, M]) :Y[GlobalScope, Rows[M[Unit]#Subject]] =
				compoundSelect(e)
		}

	}






	/** Implements a compound select combining the result sets of two single-column
	  * SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL.SelectColumn selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL.ColumnQuery queries]]). The kind of operation is defined by
	  * the [[net.noresttherein.oldsql.sql.Select.SetOperator SetOperator]]
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

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :ColumnQuery[E, V] =
			(left.expand(base), right.expand(base)) match {
				case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[ColumnQuery[E, V]]
				case (l, r) => operator(l, r)
			}


		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                              (visitor :ColumnVisitor[F, Y]) :Y[GlobalScope, Rows[V]] =
			visitor.compoundSelect(this)
	}



	object CompoundSelectColumn {
		def apply[F <: RowProduct, V](left :ColumnQuery[F, V], operator :SetOperator, right :ColumnQuery[F, V])
				:CompoundSelectColumn[F, V] =
			new BaseCompoundSelectColumn(left, operator, right)

		def unapply[F <: RowProduct, V](e :SQLExpression[F, LocalScope, Rows[V]])
				:Opt[(ColumnQuery[F, V], SetOperator, ColumnQuery[F, V])] =
			e match {
				case op :CompoundSelectColumn[F @unchecked, V @unchecked] => Got((op.left, op.operator, op.right))
				case _ => Lack
			}

		private[QuerySQL] class BaseCompoundSelectColumn[-F <: RowProduct, V]
		                                                (override val left :ColumnQuery[F, V],
		                                                 override val operator :SetOperator,
		                                                 override val right :ColumnQuery[F, V])
			extends CompoundSelectColumn[F, V]
		{
			override type ResultMapping[O] = ColumnMapping[V, O]
			override def mapping[O] = left.mapping
			override def export[O] = left.export //todo: this should involve some reconciliation
		}


		trait CompoundSelectColumnVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompoundSelectColumnMappingVisitor[F, Y]
		{
			def compoundSelect[V](e :CompoundSelectColumn[F, V]) :Y[GlobalScope, Rows[V]]
		}

		type MatchCompoundSelectColumn[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			CompoundSelectColumnVisitor[F, Y]

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
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL.MappingQuerySQL queries]]), sharing the same row schema,
	  * as defined by the mapping `M`. The kind of operation is defined by
	  * the [[net.noresttherein.oldsql.sql.Select.SetOperator SetOperator]]
	  * of the [[net.noresttherein.oldsql.sql.ast.QuerySQL.CompoundSelectSQL.operator operator]] member property.
	  */
	trait CompoundSelectMappingSQL[-F <: RowProduct, M[O] <: MappingAt[O]]
		extends CompoundSelectSQL[F, M[Unit]#Subject] with MappingQuerySQL[F, M]
	{
		override val left :MappingQuerySQL[F, M]
		override val right :MappingQuerySQL[F, M]

		override def mapping[O] = left.mapping
		override def export[O] = left.export //todo: this should involve some reconciliation

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :CompoundSelectMappingSQL[E, M] =
			(mapper(left), mapper(right)) match {
				case (l :MappingQuerySQL[E @unchecked, M @unchecked], r :MappingQuerySQL[E @unchecked, M @unchecked]) =>
					CompoundSelectMappingSQL(l, operator, r)
				case (l, r) =>
					throw new IllegalArgumentException(
						s"Failed $mapper transformation of $this: ($l, $r) is not a valid ColumnQuery pair."
					)
			}

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :MappingQuerySQL[E, M] =
			(left.basedOn(base), right.basedOn(base)) match {
				case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[MappingQuerySQL[E, M]]
				case (l, r) => operator(l, r)
			}

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :MappingQuerySQL[E, M] =
			(left.expand(base), right.expand(base)) match {
				case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[MappingQuerySQL[E, M]]
				case (l, r) => operator(l, r)
			}

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                              (visitor :ExpressionVisitor[F, Y]) :Y[GlobalScope, Rows[M[Unit]#Subject]] =
			visitor.compoundSelectMapping(this)
	}



	object CompoundSelectMappingSQL {
		def apply[F <: RowProduct, M[O] <: MappingAt[O]]
		         (left :MappingQuerySQL[F, M], operator :SetOperator, right :MappingQuerySQL[F, M])
				:CompoundSelectMappingSQL[F, M] =
			new BaseCompoundSelectMappingSQL(left, operator, right)

		def unapply[F <: RowProduct, V](e :SQLExpression[F, LocalScope, Rows[V]])
				:Opt[(MappingQuerySQL[F, M], SetOperator, MappingQuerySQL[F, M]) forSome { type M[O] <: MappingAt[O] }] =
			e match {
				case op :CompoundSelectMappingSQL[F @unchecked, MappingAt @unchecked] =>
					Got((op.left, op.operator, op.right))
				case _ => Lack
			}

		private[QuerySQL] class BaseCompoundSelectMappingSQL[-F <: RowProduct, M[O] <: MappingAt[O]]
		                        (override val left :MappingQuerySQL[F, M], override val operator :SetOperator,
		                         override val right :MappingQuerySQL[F, M])
			extends CompoundSelectMappingSQL[F, M]



		trait CompoundSelectMappingVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompoundSelectColumnMappingVisitor[F, Y]
		{
			def compoundSelectMapping[M[O] <: MappingAt[O]]
			                         (e :CompoundSelectMappingSQL[F, M]) :Y[GlobalScope, Rows[M[Unit]#Subject]]
		}

		trait MatchCompoundSelectMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
			extends CompoundSelectMappingVisitor[F, Y]
		{
			override def compoundSelectMapping[M[O] <: ColumnMapping[V, O], V]
			                                  (e :CompoundSelectColumnMapping[F, M, V]) :Y[GlobalScope, Rows[V]] =
				{ val res = compoundSelectMapping(e :CompoundSelectMappingSQL[F, M]); res  }
		}

		type CaseCompoundSelectMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			MatchCompoundSelectMapping[F, Y]

	}






	trait CompoundSelectColumnMapping[-F <: RowProduct, M[O] <: ColumnMapping[V, O], V]
		extends CompoundSelectColumn[F, V] with ColumnMappingQuery[F, M, V] with CompoundSelectMappingSQL[F, M]
	{
		override val left :ColumnMappingQuery[F, M, V]
		override val right :ColumnMappingQuery[F, M, V]


		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnMappingQuery[E, M, V] =
			(left.basedOn(base), right.basedOn(base)) match {
				case (l, r) if (l eq left) && (r eq right) => this.asInstanceOf[ColumnMappingQuery[E, M, V]]
				case (l, r) => operator(l, r)
			}

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :ColumnMappingQuery[E, M, V] =
			(left.expand(base), right.expand(base)) match {
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

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
		                              (visitor :ColumnVisitor[F, Y]) :Y[GlobalScope, Rows[M[Unit]#Subject]] =
			visitor.compoundSelectMapping(this)
	}



	object CompoundSelectColumnMapping {
		def apply[F <: RowProduct, M[O] <: ColumnMapping[V, O], V]
		         (left :ColumnMappingQuery[F, M, V], operator :SetOperator, right :ColumnMappingQuery[F, M, V])
				:CompoundSelectColumnMapping[F, M, V] =
			new BaseCompoundSelectColumnMapping(left, operator, right)

		def unapply[F <: RowProduct, V](e :SQLExpression[F, LocalScope, Rows[V]])
				:Opt[(ColumnMappingQuery[F, M, V], SetOperator, ColumnMappingQuery[F, M, V]) forSome { type M[O] <: ColumnMapping[V, O] }] =
			e match {
				case op :CompoundSelectColumnMapping[F @unchecked, MappingOf[V]#ColumnProjection @unchecked, V @unchecked] =>
					Got((op.left, op.operator, op.right))
				case _ => Lack
			}

		private[QuerySQL] class BaseCompoundSelectColumnMapping[-F <: RowProduct, M[O] <: ColumnMapping[V, O], V]
		                        (override val left :ColumnMappingQuery[F, M, V], override val operator :SetOperator,
		                         override val right :ColumnMappingQuery[F, M, V])
			extends CompoundSelectColumnMapping[F, M, V]



		trait CompoundSelectColumnMappingVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def compoundSelectMapping[M[O] <: ColumnMapping[V, O], V]
			                       (e :CompoundSelectColumnMapping[F, M, V]) :Y[GlobalScope, Rows[V]]
		}

		type MatchCompoundSelectColumnMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			CompoundSelectColumnMappingVisitor[F, Y]

		type CaseCompoundSelectColumnMapping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
			CompoundSelectColumnMappingVisitor[F, Y]
	}






	trait ColumnMappingQueryVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends CompoundSelectColumnMappingVisitor[F, Y] with SelectColumnMappingVisitor[F, Y]
	{
		def mappingQuery[M[O] <: ColumnMapping[V, O], V](e :ColumnMappingQuery[F, M, V]) :Y[GlobalScope, Rows[V]]
	}

	type MatchColumnMappingQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] =
		ColumnMappingQueryVisitor[F, Y]

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



	trait MappingQueryVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnMappingQueryVisitor[F, Y] with CompoundSelectMappingVisitor[F, Y] with SelectMappingVisitor[F, Y]
	{
		def mappingQuery[M[O] <: MappingAt[O]](e :MappingQuerySQL[F, M]) :Y[GlobalScope, Rows[M[Unit]#Subject]]
	}

	trait MatchMappingQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MappingQueryVisitor[F, Y] with CaseSelectMapping[F, Y] with CaseCompoundSelectMapping[F, Y]
	{
		override def mappingQuery[M[O] <: ColumnMapping[V, O], V](e :ColumnMappingQuery[F, M, V]) :Y[GlobalScope, Rows[V]] =
			{ val res = mappingQuery(e :MappingQuerySQL[F, M]); res }
	}

	trait CaseMappingQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends MatchMappingQuery[F, Y]
	{
		override def selectMapping[H[O] <: MappingAt[O]]
		                          (e :SelectSQL.SelectAs[F, H]) :Y[GlobalScope, Rows[H[Unit]#Subject]] =
			mappingQuery(e)

		override def compoundSelectMapping[M[O] <: MappingAt[O]]
		                                  (operation :CompoundSelectMappingSQL[F, M]) :Y[GlobalScope, Rows[M[Unit]#Subject]] =
			mappingQuery(operation)
	}



	trait ColumnQueryVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnMappingQueryVisitor[F, Y] with SelectColumnVisitor[F, Y] with CompoundSelectColumnVisitor[F, Y]
	{
		def query[V](e :ColumnQuery[F, V]) :Y[GlobalScope, Rows[V]]
	}

	trait MatchColumnQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnQueryVisitor[F, Y] with CaseColumnMappingQuery[F, Y]
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



	trait QueryVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ColumnQueryVisitor[F, Y] with MappingQueryVisitor[F, Y] with SelectVisitor[F, Y]
	{
		def query[V](e :QuerySQL[F, V]) :Y[GlobalScope, Rows[V]]
	}

	trait MatchQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends QueryVisitor[F, Y] with MatchMappingQuery[F, Y] with CaseSelect[F, Y] with CaseCompoundSelect[F, Y]
	{
		override def query[V](e :ColumnQuery[F, V]) :Y[GlobalScope, Rows[V]] = query(e :QuerySQL[F, V])

		override def mappingQuery[M[O] <: MappingAt[O]](e :MappingQuerySQL[F, M]) :Y[GlobalScope, Rows[M[Unit]#Subject]] =
			query(e)
	}

	trait CaseQuery[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends MatchQuery[F, Y] {

		override def select[V](e :SelectSQL[F, V]) :Y[GlobalScope, Rows[V]] = query(e)

		override def compoundSelect[V](e :CompoundSelectColumn[F, V]) :Y[GlobalScope, Rows[V]] = query(e)
	}

}

