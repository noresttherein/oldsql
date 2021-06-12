package net.noresttherein.oldsql

import net.noresttherein.oldsql.schema.SQLForm
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.GroupBy.AndBy
import net.noresttherein.oldsql.sql.GroupByClause.{Group, GroupingRelation}
import net.noresttherein.oldsql.sql.RowProduct.GroupingOfGeneralized
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.UnboundParam.{FromParam, NamedParamRelation, ParamRelation}
import net.noresttherein.oldsql.sql.ast.SQLLiteral



package object sql {
	//todo: case/decode
	/** Represents the multipurpose 'everything' wildcard `*` in SQL. Can be used as the argument to
	  * [[net.noresttherein.oldsql.sql.AggregateFunction.Count Count]] and
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]].
	  */
	final class * private[sql] ()

	/** Represents the multipurpose 'everything' wildcard `*` in SQL. Can be used as an argument to
	  * [[net.noresttherein.oldsql.sql.AggregateFunction.Count Count]] and
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]].
	  */
	final val * = new *


	/** The 'NVL' SQL function. Accepts 2 arguments of the same type, returning the first ''iff'' it is not null,
	  * otherwise the second.
	  */
	final val NVL = GenericFunction.of2("NVL")

	/** The 'NVL' SQL function. Accepts 3 arguments of the same type, returning the first ''iff'' it is not null,
	  * otherwise the second if it is not null, defaulting to the third if the first two are null.
	  */
	final val NVL2 = GenericFunction.of3("NVL2")


//	type SQL[-F <: RowProduct, V] = SQLExpression[F, Aggregate] { type Value = V }
//	type DiscreteSQL[-F <: RowProduct, V] = SQLExpression[F, Discrete] { type Value = V }
	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of Boolean value and
	  * with [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope scope]] `S`.
	  */
	type SQLBoolean[-F <: RowProduct, -S >: LocalScope <: GlobalScope] = ColumnSQL[F, S, Boolean]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of Boolean value which can be used solely
	  * within ''select'' and ''having'' clauses of the most deeply nested select represented by the ''from'' clause `F`.
	  * It is typically a derivative expression, containing an [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]]
	  * expression as its subexpression.
	  * Note that [[net.noresttherein.oldsql.sql.GlobalBoolean GlobalBoolean]]`[F] <: LocalBoolean[F]`.
	  */
	type LocalBoolean[-F <: RowProduct] = ColumnSQL[F, LocalScope, Boolean]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of Boolean value which can occur in any place
	  * of the SQL select represented by the ''from'' clause `F` or its dependent selects (for subselect clauses,
	  * after expanding with [[net.noresttherein.oldsql.sql.SQLExpression.expand expand]] method).
	  * Such expressions cannot use [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] subexpressions
	  * from an enclosing ''select''. This is in particular the expression type used in all ''where'' clauses.
	  * Note that `GlobalBoolean[F] <: `[[net.noresttherein.oldsql.sql.LocalBoolean LocalBoolean]]`[F]`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  */
	type GlobalBoolean[-F <: RowProduct] = ColumnSQL[F, GlobalScope, Boolean]

	object SQLBoolean {
		val True = SQLLiteral.True
		val False = SQLLiteral.False
	}

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of `String` type and
	  * with [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope scope]] `S`.
	  */
	type SQLString[-F <: RowProduct, -S >: LocalScope <: GlobalScope] = ColumnSQL[F, S, String]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of `String` type which can be used solely
	  * within ''select'' and ''having'' clauses of the most deeply nested select represented by the ''from'' clause `F`.
	  * It is typically a derivative expression, containing an [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]]
	  * expression as its subexpression.
	  * Note that [[net.noresttherein.oldsql.sql.GlobalString GlobalString]]`[F] <: LocalString[F]`.
	  */
	type LocalString[-F <: RowProduct] = ColumnSQL[F, LocalScope, String]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of `String` type which can occur in any place
	  * of the SQL select represented by the ''from'' clause `F` or its enclosing selects (for subselect clauses).
	  * Such expressions can also be expanded to subselects of the clause `F` using the
	  * [[net.noresttherein.oldsql.sql.SQLExpression.basedOn basedOn]] method. This is in particular the expression type
	  * used in all ''where'' clauses.
	  * Note that `GlobalString[F] <: `[[net.noresttherein.oldsql.sql.LocalString LocalString]]`[F]`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  */
	type GlobalString[-F <: RowProduct] = ColumnSQL[F, GlobalScope, String]



	/** An occurrence of an SQL [[net.noresttherein.oldsql.schema.Relation relation]] in a ''from'' clause
	  * of an SQL ''select''. It declares the [[net.noresttherein.oldsql.schema.Mapping mapping]] for that relation
	  * and is a valid SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] based on the ''from'' clause
	  * of its second type parameter. The relation object (both this value and wrapped `Relation`) may
	  * be a relation only in the abstract sense, and not a valid ''derived table''; chief example here
	  * are grouping relations which represent a subset of attributes from a ''group by'' clause.
	  * @see [[net.noresttherein.oldsql.sql.JoinedTable]]
	  */
	type JoinedRelation[F <: RowProduct, M[O] <: MappingAt[O]] = ast.JoinedRelation[F, M]

	val JoinedRelation = ast.JoinedRelation

	/** An occurrence of an SQL [[net.noresttherein.oldsql.schema.Relation.Table Table]] in a ''from'' clause
	  * of an SQL ''select''. It declares the [[net.noresttherein.oldsql.schema.Mapping mapping]] for that table
	  * and is a valid SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] based on the ''from'' clause
	  * of its second type parameter.
	  * @see [[net.noresttherein.oldsql.sql.JoinedRelation]]
	  */
	type JoinedTable[F <: RowProduct, M[O] <: MappingAt[O]] = ast.JoinedTable[F, M]

	val JoinedTable = ast.JoinedTable

	type JoinedParam[F <: RowProduct, X] = ast.JoinedRelation[F, FromParam.Of[X]#P]

	object JoinedParam {
		type Last[X] = JoinedParam[RowProduct AndFrom ParamRelation[X]#Param, X]
	}

	type JoinedGroup[F <: RowProduct, X] = ast.JoinedRelation[F, Group[X]#T]

	object JoinedGroup {
		type First[X] = JoinedGroup[FromSome GroupBy Group[X]#T, X]
		type Last[X] = JoinedGroup[GroupByClause AndBy Group[X]#T, X]
	}



	/** A type wrapper for the type constructor of parameter mappings with string literals as names, present in their
	  * type signature. Importing it imports also a companion method `?:[X :SQLForm]` returning a new relation
	  * for parameter of type `X` as well as an implicit conversion adding a `?:[X]` factory method to string literals.
	  */ //todo: get rid of the whole LabeledFromParam and use the label for an alias
	type ?:[N <: Label, X] = NamedParamRelation[N, X]

	/** Creates a dedicated [[net.noresttherein.oldsql.schema.Relation Relation]] for the synthetic parameter mapping
	  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]]. It represents a query/statement parameter
	  * of type `X`, the value of which is unspecified at this point, to be provided when executing the statement.
	  * The relation can only be used in [[net.noresttherein.oldsql.sql.UnboundParam UnboundParam]] join types.
	  * Importing this symbol imports also an overloaded factory method accepting a
	  * [[net.noresttherein.oldsql.schema.bits.LabeledMapping.Label Label]] type parameter for the parameter name,
	  * as well as an implicit conversion adding a [[net.noresttherein.oldsql.sql.UnboundParam.method_?:.?: ?:]]
	  * extension method to `String` literals, creating parameter relations for labeled mappings.
	  * @see [[net.noresttherein.oldsql.sql.UnboundParam.?:[N<:Label:ValueOf,X:SQLForm] ?:]]
	  */
	def ?:[X :SQLForm] :ParamRelation[X] = ParamRelation()

	/** Creates a dedicated [[net.noresttherein.oldsql.schema.Relation Relation]] for the synthetic parameter mapping
	  * [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]]. It represents a query/statement
	  * parameter of type `X`, the value of which is unspecified at this point, to be provided when executing
	  * the statement. When used to create an `UnboundParam`, it will automatically add
	  * an [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause with the parameter name (as a string literal
	  * type `N`) to the result. Importing this symbol imports also an overloaded factory method without the label,
	  * as well as an implicit conversion adding a [[net.noresttherein.oldsql.sql.method_?:.?: ?:]]
	  * extension method to `String` literals, creating parameter relations for labeled mappings.
	  * @see [[net.noresttherein.oldsql.sql.?:[X:SQLForm] ?:]]
	  */
	def ?:[N <: Label :ValueOf, X :SQLForm] :NamedParamRelation[N, X] = NamedParamRelation()

	/** Adds a [[net.noresttherein.oldsql.sql.method_?:.?: ?:]] method to `String` literals for creating
	  * labeled synthetic parameter relations.
	  */
	implicit def ?:[N <: Label](name :N) = new method_?:[N](name)

	/** Extension method [[net.noresttherein.oldsql.sql.method_?:.?: ?:]] for `String` literals,
	  * creating a dedicated [[net.noresttherein.oldsql.schema.Relation Relation]] for a parameter mapping
	  * labeled with this literal type.
	  */
	class method_?:[N <: Label](private val name :N) extends AnyVal {
		/** Creates a synthetic [[net.noresttherein.oldsql.schema.Relation.StaticRelation StaticRelation]]
		  * using this `String` literal as relation the name, SQL statement parameter name and mapping label
		  * for access from [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]].
		  */
		def ?:[X :SQLForm] :NamedParamRelation[N, X] = NamedParamRelation(name)
	}



	/** A type alias for `JoinParam` accepting parameter type `X`. As a `RowProduct` containing a `JoinParam` join
	  * in its type is a preliminary from clause which will be translated to a parameterized statement, it uses
	  * an a 'inverse function symbol' as a mnemonic: `From[Users] <=? String`. This is equivalent to `WithParam[F, X]`.
	  */ //todo: some less confusing shorthand
//	type <=?[+F <: FromSome, X] = WithParam[F, X]

	/** An alias for `JoinParam` accepting the parameter type as the second (right) argument, hiding the
	  * `FromParam[X, _]` from the type signature.
	  */ //not F <: TopFromSome so it can be used in Generalized types
	type WithParam[+F <: FromSome, X] = JoinParam[F, ParamRelation[X]#Param] //todo: rename, conflicts with param for WithClause

	object WithParam {
		//todo: make this FromSome WithParam X
		type Last[X] = FromSome WithParam X
		type FromLast[X] = RowProduct AndFrom ParamRelation[X]#Param
	}

	/** An alias for `GroupParam` accepting the parameter type as the second (right) argument, hiding the
	  * `FromParam[X, _]` from the type signature.
	  */
	type ByParam[+F <: GroupByClause, X] = GroupParam[F, ParamRelation[X]#Param]

	object ByParam {
		type FromLast[X] = GroupByClause AndBy ParamRelation[X]#Param
	}



	/** The first grouping expression of a [[net.noresttherein.oldsql.sql.GroupBy group by]] clause,
	  * represented by a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[S, _]`.
	  */ //consider: renaming to GroupBy. Question only if this or GroupBy
	type GroupByVal[+F <: FromSome, S] = F GroupBy Group[S]#T

	object GroupByVal {

		def apply[U <: RowProduct, F <: FromSome { type Generalized <: U }, S] //fixme: add filter once factory methods for all joins are refactored
		         (from :F, group :SQLExpression[U, GlobalScope, S])//, filter :LocalBoolean[F#Generalized GroupByVal S] = True)
				:F GroupByVal S =
			GroupBy[F, Group[S]#T, Group[S]#T, S](from, GroupingRelation(group), group)//, filter)

		type * = GroupBy.*

		type WithLeft[L <: FromSome] = { type F[X] = L GroupBy MappingOf[X]#TypedProjection }

		type WithRight[X] = { type F[L <: FromSome] = L GroupBy MappingOf[X]#TypedProjection }
	}



	/** A [[net.noresttherein.oldsql.sql.By following]] grouping expression of a ''group by'' clause,
	  * represented by a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[S, _]`.
	  */
	type ByVal[+F <: GroupByClause, S] = F By Group[S]#T

	object ByVal {

		def apply[F <: RowProduct, G <: GroupingOfGeneralized[F], S] //fixme: add filter once joins are refactored
		         (from :G, group :SQLExpression[F, GlobalScope, S])//, filter :LocalBoolean[G#Generalized ByVal S] = True)
				:G ByVal S =
			By[G, Group[S]#T, Group[S]#T, S](from, GroupingRelation(group), group)//, filter)

		type * = By.*

		type WithLeft[L <: GroupByClause] = { type F[X] = L By MappingOf[X]#TypedProjection }

		type WithRight[X] = { type F[L <: GroupByClause] = L By MappingOf[X]#TypedProjection }
	}



	/** The first grouping expression of a [[net.noresttherein.oldsql.sql.GroupBy group by]] clause,
	  * represented by a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[S, _]`.
	  */
	type GroupByOne[+F <: FromSome, S] = F GroupBy Group[S]#C

	object GroupByOne {

		def apply[U <: RowProduct, F <: FromSome { type Generalized <: U }, S]//fixme: add filter once join factory methods are refactored
		         (from :F, group :ColumnSQL[U, GlobalScope, S])//, filter :LocalBoolean[F#Generalized GroupByOne S] = True)
				:F GroupByOne S =
			GroupBy[F, Group[S]#C, Group[S]#C, S](from, GroupingRelation(group), group)//, filter)

		type * = GroupBy.*

		type WithLeft[L <: FromSome] = { type F[X] = L GroupBy MappingOf[X]#ColumnProjection }

		type WithRight[X] = { type F[L <: FromSome] = L GroupBy MappingOf[X]#ColumnProjection }
	}



	/** A [[net.noresttherein.oldsql.sql.By following]] grouping expression of a ''group by'' clause,
	  * represented by a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[S, _]`.
	  */
	type ByOne[+F <: GroupByClause, S] = F By Group[S]#C

	object ByOne {

		def apply[F <: RowProduct, G <: GroupingOfGeneralized[F], S]//fixme: add filter once joins are refactored
		         (from :G, group :ColumnSQL[F, GlobalScope, S])//, filter :LocalBoolean[G#Generalized ByOne S] = True)
				:G ByOne S =
			By[G, Group[S]#C, Group[S]#C, S](from, GroupingRelation(group), group)//, filter)

		type * = By.*

		type WithLeft[L <: GroupByClause] = { type F[X] = L By MappingOf[X]#ColumnProjection }

		type WithRight[X] = { type F[L <: GroupByClause] = L By MappingOf[X]#ColumnProjection }
	}



	implicit class nulleq[F <: RowProduct, S >: LocalScope <: GlobalScope, L](private val left :SQLExpression[F, S, L])
		extends AnyVal
	{
		def nulleq[R, U](right :SQLExpression[F, S, R])(implicit sameish :SQLTypeUnification[L, R, U]) :SQLBoolean[F, S] =
			??? //todo: requires DECODE/CASE
	}



	/** Escapes some problematic characters such as `'` and `\`. This is intended as a convenience for fixed strings
	  * created by the application and is ''not'' a sufficient defence against SQL injection. It should not be relied
	  * upon for user-submitted values.
	  */
	def sanitize(string :String) :String =
		if (string == null)
			null
		else {
			val s = new java.lang.StringBuilder(string.length * 11 / 10)
			var i = 0; val len = string.length
			while (i < len) {
				string.charAt(i) match {
					case 0 => s append '\\' append '0'
					case '\n' => s append '\\' append 'n'
					case '\r' => s append '\\' append 'r'
					case '\\' => s append '\\' append '\\'
					case '\'' => s append '\\' append '\''
					case '\u001a' => s append '\\' append 'Z' //ctrl-Z / Windows EoF
					case '\u00a5' | '\u20a9' => // escape characters interpreted as backslash by mysql
					case c => s append c
				}
				i += 1
			}
			s.toString
		}

}

