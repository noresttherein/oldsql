package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.OperationType.{FILTER, SELECT}
import net.noresttherein.oldsql.collection.{Chain, Unique}
import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.morsels.TextCase
import net.noresttherein.oldsql.morsels.TextCase.LowerCase
import net.noresttherein.oldsql.schema.ColumnMapping
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.Relation.{Aliased, RelVar, SelectRelation, Table}
import net.noresttherein.oldsql.slang.OptionGuardExtension
import net.noresttherein.oldsql.sql.RowProduct.ParamlessFrom
import net.noresttherein.oldsql.sql.SelectAPI.{Intersect, Minus, SetOperator, Union, UnionAll}
import net.noresttherein.oldsql.sql.SQLDialect.{DefaultSpelling, SQLSpelling}
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.{FromScope, GroupByScope, HavingScope, SelectScope, WhereScope}
import net.noresttherein.oldsql.sql.SQLExpression.{CaseExpression, ExpressionMapper, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.SQLStatement.StatementResult
import net.noresttherein.oldsql.sql.ast.QuerySQL
import net.noresttherein.oldsql.sql.ast.SQLTerm.True
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/**
  * @author Marcin Mościcki
  */
trait SQLDialect {
	def apply[V, R](query :QuerySQL[RowProduct, V])(implicit result :StatementResult[V, R]) :SQLCommand[R]
	//todo: don't expose SpelledSQL here, go immediately for an SQLStatement
	def spell[V](query :QuerySQL[RowProduct, V]) :SpelledSQL[@~, RowProduct]
}




object SQLDialect {

	trait SpellingScope {
		/** All columns, direct or indirect, of the given mapping which are applicable to this operation type.
		  * These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list ''without''
		  * the [[net.noresttherein.oldsql.OperationType.Prohibited Prohibited]] buff.
		  * @see [[net.noresttherein.oldsql.OperationType.defaultColumns]]
		  */
		def columns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]]

		/** Columns, both direct and indirect, of the given mapping which are used by default in this operation type.
		  * These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list ''without''
		  * the [[net.noresttherein.oldsql.OperationType.NonDefault NonDefault]] buff.
		  * @see [[net.noresttherein.oldsql.OperationType.columns]]
		  */
		def defaultColumns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]]

	}

	object SpellingScope {

		private class OperationTypeScope(op :OperationType, override val toString :String) extends SpellingScope {
			override def columns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] = op.columns(mapping)

			override def defaultColumns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] =
				op.defaultColumns(mapping)
		}

		val SelectScope :SpellingScope = new OperationTypeScope(SELECT, "SELECT")
		val FromScope :SpellingScope = new OperationTypeScope(SELECT, "FROM")
		val WhereScope :SpellingScope = new OperationTypeScope(FILTER, "WHERE")
		val GroupByScope :SpellingScope = new OperationTypeScope(SELECT, "GROUP BY")
		val HavingScope :SpellingScope = new OperationTypeScope(FILTER, "HAVING")
	}




	trait SQLSpelling {
		def spell[P, V](query :ParamQuery[P, V]) :SpelledSQL[P, RowProduct] =
			query.defaultSpelling(this, newContext)

		def spell[V](query :QuerySQL[RowProduct, V]) :SpelledSQL[@~, RowProduct] =
			query.paramlessSpelling(this, newContext)

		def paramless[V](query :QuerySQL[RowProduct, V])(context :SQLContext) :SpelledSQL[@~, RowProduct] =
			query.paramlessSpelling(this, context)


		def apply[P, V](query :ParamQuery[P, V])(context :SQLContext) :SpelledSQL[P, RowProduct] =
			query.defaultSpelling(this, context)

		def apply[P, F <: RowProduct, V]
                 (e :SQLExpression[F, LocalScope, V])
                 (context :SQLContext, params: Parameterization[P, F]) :SpelledSQL[P, F]

		def apply(from :RowProduct)(context :SQLContext) :SpelledSQL[from.Params, from.Generalized]

		def apply[P, O <: RowProduct, F <: O, M[A] <: MappingAt[A]]
		         (origin :JoinedRelation[O, M], component :MappingAt[O])
		         (context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F]

		def apply[P, F <: RowProduct](function :AggregateFunction, distinct :Boolean)
		                             (arg :ColumnSQL[F, LocalScope, _])
		                             (context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
			function.spell(this)(arg, distinct)(context, params)

		def apply[P, F <: RowProduct, X <: Chain, Y](function :SQLFunction[X, Y])(args :ChainTuple[F, LocalScope, X])
		                                            (context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
			function.spell(this)(args)(context, params)

		def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		         (table :Table[M])(context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F]

		def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		         (table :Table[M], alias :String)(context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F]

		def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		         (table :Table[M], alias :Option[String])(context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
			alias.mapOrElse(this.table(table, _)(context, params), this.table(table)(context, params))

		//context must correspond to the RowProduct table is based on (same number of tables)
		def column[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]]
		          (table :JoinedRelation[O, T], column :ColumnMapping[_, O])
		          (implicit context :SQLContext, params: Parameterization[P, F]) :SpelledSQL[P, F] =
			SpelledSQL(context(table) + "." + column.name, context, params)


		def fromWhere(from :FromClause)(context :SQLContext) :SpelledSQL[from.Params, from.Generalized] = {
			val fromSQL = inFrom(from)(context)
			val resultContext = fromSQL.context.copy(whereReversed = Nil)
			if (fromSQL.context.whereReversed.isEmpty)
				SpelledSQL(fromSQL.sql, resultContext, fromSQL.params)
			else {
				val whereSQL = fromSQL.context.whereReversed.reduceLeft((_1, _2) => _2 + ", " + _1)
				fromSQL + (" " + WHERE + " ") + whereSQL
			}
		}

		def emptyFrom(context :SQLContext) :SpelledSQL[@~, Dual]

		def join[L <: FromSome, R[O] <: MappingAt[O]]
		        (join :L Join R, clause :String)(context :SQLContext) :SpelledSQL[join.Params, join.Generalized]

		def groupByHaving(from :GroupByClause)(context :SQLContext) :SpelledSQL[from.Params, from.Generalized] = {
			val groupBySQL = inGroupBy(from)(context)
			if (from.filter == True) groupBySQL
			else groupBySQL + (" " + HAVING + " ") + (inHaving(from.filter)(_, _))
		}

		def apply(operator :SetOperator) :String = operator match {
			case Union => UNION
			case UnionAll => UNION_ALL
			case Intersect => INTERSECT
			case Minus => MINUS
			case _ => this.operator(operator.name)
		}

		def literal(sql :String) :String
		def operator(sql :String) :String
		def function(sql :String) :String
		def keyword(sql :String) :String

		def scope :SpellingScope

		def inline :SQLSpelling
		def inSelect :SQLSpelling
		def inFrom :SQLSpelling
		def inWhere :SQLSpelling
		def inGroupBy :SQLSpelling
		def inHaving :SQLSpelling

		def in(scope :SpellingScope) :SQLSpelling = scope match {
			case SelectScope => inSelect
			case FromScope => inFrom
			case WhereScope => inWhere
			case GroupByScope => inGroupBy
			case HavingScope => inHaving
			case _ =>
				throw new IllegalArgumentException(
					"Unknown SQL expression scope: " + scope + ": " + scope.getClass.getName
				)
		}

		def NULL       :String = "null"
		def TRUE       :String = "true"
		def FALSE      :String = "false"
		def CONCAT     :String = "+"
		def LIKE       :String = "like"
		def BETWEEN    :String = "between"
		def NOT        :String = "not"
		def AND        :String = "and"
		def OR         :String = "or"
		def UNION      :String = "union"
		def UNION_ALL  :String = "union all"
		def INTERSECT  :String = "intersect"
		def MINUS      :String = "minus"
		def SELECT     :String = "select"
		def FROM       :String = "from"
		def WHERE      :String = "where"
		def GROUP_BY   :String = "group by"
		def HAVING     :String = "having"
		def AS         :String = "as"
		def INNER_JOIN :String = "join"
		def OUTER_JOIN :String = "outer join"
		def LEFT_JOIN  :String = "left join"
		def RIGHT_JOIN :String = "right join"
		def ON         :String = "on"
		def INSERT     :String = "insert"
		def INTO       :String = "into"
		def VALUES     :String = "values"
		def UPDATE     :String = "update"
		def MERGE      :String = "merge"
		def DELETE     :String = "delete"


		protected def newContext :SQLContext = SQLContext()


		protected def defaultSpelling[P](e :ParamQuery[P, _])(implicit context :SQLContext) :SpelledSQL[P, RowProduct] =
			e.defaultSpelling(this, context)

		protected def defaultSpelling[P, F <: RowProduct, X <: Chain, Y]
		                             (f :SQLFunction[X, Y])(args :ChainTuple[F, LocalScope, X])
		                             (implicit context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
			f.spell(this)(args)

		protected def defaultSpelling[P, F <: RowProduct]
		                             (f :AggregateFunction)(arg :ColumnSQL[F, LocalScope, _], distinct :Boolean = false)
		                             (implicit context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
			f.spell(this)(arg, distinct)

		protected def defaultSpelling[P, F <: RowProduct](e :SQLExpression[F, LocalScope, _])
		                                                 (implicit context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
			e.defaultSpelling(this)

		protected def inlineSpelling[P, F <: RowProduct](e :SQLExpression[F, LocalScope, _])
		                                                (implicit context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
			e.inlineSpelling(this)

		protected def defaultSpelling(from :RowProduct)(context :SQLContext) :SpelledSQL[from.Params, from.Generalized] =
			from.defaultSpelling(this)(context)
	}



	object SQLSpelling {
		private val default = new DefaultSpelling(SelectScope)

		def apply() :SQLSpelling = default

		def apply(scope :SpellingScope) :SQLSpelling = new DefaultSpelling(scope)

//		def apply[P](exprSpelling :SQLContext[P] => ExpressionSpelling,
//		             fromSpelling :SQLContext[P] => RowProductMatcher[SpelledSQL[P]]) :SQLSpelling =
//			new TweakedDefaultSpelling(SelectScope, false)(exprSpelling, fromSpelling)


		type ExpressionSpelling[P, F <: RowProduct] =
			ExpressionMapper[F, ({type T[-S >: LocalScope <: GlobalScope, V] = SpelledSQL[P, F] })#T]

		class ExpressionSpellingBase[P, F <: RowProduct](spelling :SQLSpelling, inline :Boolean = false)
		                                                (context :SQLContext)
		                                                (implicit params :Parameterization[P, F])
			extends CaseExpression[F, ({type T[-S >: LocalScope <: GlobalScope, V] = SpelledSQL[P, F] })#T]
		{
			override def expression[S >: LocalScope <: GlobalScope, X](e :SQLExpression[F, S, X]) :SpelledSQL[P, F] =
				if (inline) inlineSpelling(e)(context) else defaultSpelling(e)(context)

			protected def defaultSpelling(e :SQLExpression[F, LocalScope, _])(context :SQLContext) :SpelledSQL[P, F] =
				e.defaultSpelling(spelling)(context, params)

			protected def inlineSpelling(e :SQLExpression[F, LocalScope, _])(context :SQLContext) :SpelledSQL[P, F] =
				e.inlineSpelling(spelling)(context, params)
		}


//		class RowProductSpellingBase[P, F](spelling :SQLSpelling)(context :SQLContext[P])
//		                                  (implicit params :Parameterization[P, F])
//			extends RowProductMatcher[SpelledSQL[P]]
//		{
//			override def rowProduct(from :RowProduct) :SpelledSQL[P] = defaultSpelling(from, context)
//
//			protected def defaultSpelling(from :RowProduct, context :SQLContext[P]) :SpelledSQL[P] =
//				from.spellDefault(spelling, context)
//		}


/*
		private class TweakedDefaultSpelling(override val scope :SpellingScope, override val isInline :Boolean = false)
		                                    (spellExpression :SQLContext => ExpressionSpelling,
		                                     spellRowProduct :SQLContext => RowProductMatcher[SpelledSQL])
			extends DefaultSpelling(scope, isInline)
		{
			protected override def copy(scope :SpellingScope, isInline :Boolean) :SQLSpelling =
				new TweakedDefaultSpelling(scope, isInline)(spellExpression, spellRowProduct)

			override def apply(e :RowProduct)(context :SQLContext) :SpelledSQL =
				spellRowProduct(context)(e)

			override def apply(e :SQLExpression.*)(context :SQLContext) :SpelledSQL =
				spellExpression(context)(e)

		}
*/

	}



	class DefaultSpelling(literals :TextCase = LowerCase, operators :TextCase = LowerCase,
	                      functions :TextCase = LowerCase, keywords :TextCase = LowerCase, aliases :TextCase = LowerCase)
	                     (override val scope :SpellingScope, protected val isInline :Boolean = false)
		extends SQLSpelling
	{
		def this(scope :SpellingScope, textCase :TextCase, isInline :Boolean) =
			this(textCase, textCase, textCase, textCase, textCase)(scope, isInline)

		def this(scope :SpellingScope, isInline :Boolean) = this(scope, LowerCase, isInline)
		def this(scope :SpellingScope) = this(scope, LowerCase, false)

		protected def copy(scope :SpellingScope = this.scope, isInline :Boolean = this.isInline) :SQLSpelling =
			new DefaultSpelling(literals, operators, functions, keywords, aliases)(scope, isInline)

		override val inline :SQLSpelling = if (isInline) this else copy(scope, true)
		override def inSelect :SQLSpelling = copy(SelectScope, false)
		override def inFrom :SQLSpelling = copy(FromScope, false)
		override def inWhere :SQLSpelling = copy(WhereScope, false)
		override def inGroupBy :SQLSpelling = copy(GroupByScope, false)
		override def inHaving :SQLSpelling = copy(HavingScope, false)

		override def apply[P, F <: RowProduct, V]
                          (e :SQLExpression[F, LocalScope, V])
                          (context :SQLContext, params: Parameterization[P, F]) :SpelledSQL[P, F] =
			if (isInline) inlineSpelling(e)(context, params) else defaultSpelling(e)(context, params)

		override def apply(from :RowProduct)(context :SQLContext) :SpelledSQL[from.Params, from.Generalized] =
			from.defaultSpelling(this)(context)

		override def apply[P, O <: RowProduct, F <: O, M[A] <: MappingAt[A]]
		                  (origin :JoinedRelation[O, M], component :MappingAt[O])
		                  (context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
			origin.relation.spell(origin, component, isInline)(context, params)(this)


		protected val tableAliasRoot :String = aliases("table")
		protected val selectAliasRoot :String = aliases("select")

		protected def alias(root :String)(context :SQLContext) :String = {
			var i = 1; var alias = root + i
			while (context.tablesReversed.contains(alias)) {
				i += 1; alias = root + i
			}
			alias
		}

		//consider: making these a virtual method in Table
		override def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                  (table :Table[M])(context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
			table match {
				case Aliased(t :Table.*, alias) => this.table(t, alias)(context, params)
				case t :RelVar[M] => this.table(t, t.name)(context, params)
				case _ =>
					val default = tableAliasRoot + context.tablesReversed.length
					if (!context.contains(default)) this.table(table, default)(context, params)
					else this.table(table, selectAliasRoot)(context, params)
			}

		override def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                  (table :Table[M], alias :String)(context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
		{
			val sql = table.defaultSpelling(this)(context, params)
			val unique =
				if (!context.tablesReversed.contains(alias)) alias
				else this.alias(alias)(context)
			SpelledSQL(sql.sql + (" " + AS + " " + unique), context.join(unique), sql.params)
		}

		override def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                  (table :Table[M], alias :Option[String])
		                  (context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
			alias.mapOrElse(this.table(table, _)(context, params), this.table(table)(context, params))


		override def emptyFrom(context :SQLContext) :SpelledSQL[@~, Dual] =
			SpelledSQL(context)

		override def join[L <: FromSome, R[O] <: MappingAt[O]]
		                 (join :L Join R, clause :String)(context :SQLContext) :SpelledSQL[join.Params, join.Generalized] =
		{
			val left = apply(join.left :join.left.type)(context)
			val right = table(join.right, join.aliasOpt)(left.context, left.params)
			val sql = left.sql + (" " + clause + " ") + right.sql
			val joined = SpelledSQL(sql, right.context, right.params.join[join.Generalized, join.left.Generalized, R])
			if (join.condition == True)
				joined
			else if (useJoinOnClause)
				joined + (" " + ON + " ") + (inWhere(join.condition)(_, _))
			else
				joined && (inWhere(join.condition)(_, _))
		}

		protected def useJoinOnClause :Boolean = true


		override def literal(sql :String) :String = literals(sql)
		override def operator(sql :String) :String = operators(sql)
		override def function(sql :String) :String = functions(sql)
		override def keyword(sql :String) :String = keyword(sql)

		override val NULL       :String = literals("null")
		override val TRUE       :String = literals("true")
		override val FALSE      :String = literals("false")
		override val CONCAT     :String = operators("+")
		override val LIKE       :String = operators("like")
		override val BETWEEN    :String = operators("between")
		override val NOT        :String = operators("not")
		override val AND        :String = operators("and")
		override val OR         :String = operators("or")
		override val UNION      :String = operators("union")
		override val UNION_ALL  :String = operators("union all")
		override val INTERSECT  :String = operators("intersect")
		override val MINUS      :String = operators("minus")
		override val SELECT     :String = keywords("select")
		override val FROM       :String = keywords("from")
		override val WHERE      :String = keywords("where")
		override val GROUP_BY   :String = keywords("group by")
		override val HAVING     :String = keywords("having")
		override val AS         :String = keywords("as")
		override val INNER_JOIN :String = keywords("join")
		override val OUTER_JOIN :String = keywords("outer join")
		override val LEFT_JOIN  :String = keywords("left join")
		override val RIGHT_JOIN :String = keywords("right join")
		override val ON         :String = keywords("on")
		override val INSERT     :String = keywords("insert")
		override val INTO       :String = keywords("into")
		override val VALUES     :String = keywords("values")
		override val UPDATE     :String = keywords("update")
		override val MERGE      :String = keywords("merge")
		override val DELETE     :String = keywords("delete")
	}


}






sealed class StandardSQL extends SQLDialect {
	def spelling :SQLSpelling = new DefaultSpelling(SelectScope)

	override def spell[V](query :QuerySQL[RowProduct, V]) :SpelledSQL[@~, RowProduct] = spelling.spell(query)

	override def apply[V, R](query :QuerySQL[RowProduct, V])(implicit result :StatementResult[V, R]) :SQLCommand[R] = ???
}


object StandardSQL extends StandardSQL

