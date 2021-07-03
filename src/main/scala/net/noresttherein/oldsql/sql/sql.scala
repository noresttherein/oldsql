package net.noresttherein.oldsql

import net.noresttherein.oldsql.schema.SQLForm
import net.noresttherein.oldsql.schema.Mapping.MappingOf
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.GroupByClause.{Group, GroupingRelation}
import net.noresttherein.oldsql.sql.ParamClause.{NamedParamRelation, ParamRelation}
import net.noresttherein.oldsql.sql.RowProduct.GroupingOfGeneralized
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.ast.{ColumnLiteral, SQLLiteral}



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
		object True extends ColumnLiteral[Boolean](true) {
			/** This literal upcast to `GlobalBoolean[F]`, useful for better type inference in fold-like scenarios. */
			def apply[F <: RowProduct] :GlobalBoolean[F] = this

			def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
				case SQLLiteral(v :Boolean) => v
				case _ => false
			}

			override def &&[E <: RowProduct, O >: LocalScope <: GlobalScope]
			               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = other

			override def ||[E <: RowProduct, O >: LocalScope <: GlobalScope]
			               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = this

			override def toString = "True"
		}

		object False extends ColumnLiteral[Boolean](false) {
			/** This literal upcast to `GlobalBoolean[F]`, useful for better type inference in fold-like scenarios. */
			def apply[F <: RowProduct] :GlobalBoolean[F] = this

			def unapply(expression :SQLExpression[Nothing, LocalScope, _]) :Boolean = expression match {
				case SQLLiteral(v :Boolean) => !v
				case _ => false
			}

			override def &&[E <: RowProduct, O >: LocalScope <: GlobalScope]
			               (other :SQLBoolean[E, O])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = this

			override def ||[E <: RowProduct, O >: LocalScope <: GlobalScope]
			               (other :SQLBoolean[E, O])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = other

			override def toString = "False"
		}
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


	/** A type wrapper for the type constructor of parameter mappings with string literals as names, present in their
	  * type signature. Importing it imports also a companion method `?:[X :SQLForm]` returning a new relation
	  * for parameter of type `X` as well as an implicit conversion adding a `?:[X]` factory method to string literals.
	  */ //todo: get rid of the whole LabeledUnboundParam and use the label for an alias
	type ?:[N <: Label, X] = NamedParamRelation[N, X]

	/** Creates a dedicated [[net.noresttherein.oldsql.schema.Relation Relation]] for the synthetic parameter mapping
	  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]. It represents a query/statement parameter
	  * of type `X`, the value of which is unspecified at this point, to be provided when executing the statement.
	  * The relation can only be used in [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] join types.
	  * Importing this symbol imports also an overloaded factory method accepting a
	  * [[net.noresttherein.oldsql.schema.bits.LabeledMapping.Label Label]] type parameter for the parameter name,
	  * as well as an implicit conversion adding a [[net.noresttherein.oldsql.sql.method_?:.?: ?:]]
	  * extension method to `String` literals, creating parameter relations for labeled mappings.
	  * @see [[net.noresttherein.oldsql.sql.?:[N<:Label:ValueOf,X:SQLForm] ?:]]
	  */
	def ?:[X :SQLForm] :ParamRelation[X] = ParamRelation()

	/** Creates a dedicated [[net.noresttherein.oldsql.schema.Relation Relation]] for the synthetic parameter mapping
	  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]. It represents a query/statement
	  * parameter of type `X`, the value of which is unspecified at this point, to be provided when executing
	  * the statement. When used to create an `ParamClause`, it will automatically add
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
	  * `UnboundParam[X, _]` from the type signature.
	  */ //not F <: TopFromSome so it can be used in Generalized types
	type WithParam[+F <: FromSome, X] = JoinParam[F, ParamRelation[X]#Param] //todo: rename, conflicts with param for WithClause

	object WithParam {
		type Last[X] = FromSome WithParam X
		type FromLast[X] = RowProduct AndFrom ParamRelation[X]#Param
	}

	/** An alias for `GroupParam` accepting the parameter type as the second (right) argument, hiding the
	  * `UnboundParam[X, _]` from the type signature.
	  */
	type ByParam[+F <: GroupByClause, X] = GroupParam[F, ParamRelation[X]#Param]

	object ByParam {
		type FromLast[X] = GroupByClause AndBy ParamRelation[X]#Param
	}



	/** The first grouping expression of a [[net.noresttherein.oldsql.sql.GroupBy group by]] clause,
	  * represented by a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[S, _]`.
	  */
	type GroupByVal[+F <: FromSome, S] = F GroupBy Group[S]#E

	object GroupByVal {

		def apply[U <: RowProduct, F <: FromSome { type Generalized <: U }, S] //fixme: add filter once factory methods for all joins are refactored
		         (from :F, group :SQLExpression[U, GlobalScope, S])//, filter :LocalBoolean[F#Generalized GroupByVal S] = True)
				:F GroupByVal S =
			GroupBy[F, Group[S]#E, Group[S]#E, S](from, GroupingRelation(group), group)//, filter)

		type * = GroupBy.*

		type WithLeft[L <: FromSome] = { type F[X] = L GroupBy MappingOf[X]#TypedProjection }

		type WithRight[X] = { type F[L <: FromSome] = L GroupBy MappingOf[X]#TypedProjection }
	}



	/** A [[net.noresttherein.oldsql.sql.By following]] grouping expression of a ''group by'' clause,
	  * represented by a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[S, _]`.
	  */
	type ByVal[+F <: GroupByClause, S] = F By Group[S]#E

	type AndByVal[+F <: GroupByClause, S] = F AndBy Group[S]#E

	object ByVal {
		def apply[F <: RowProduct,
		          G <: GroupByClause { type Generalized <: U; type GeneralizedDiscrete = F; type Discrete <: F },
		          U <: RowProduct, S]
		         (from :G, group :SQLExpression[F, GlobalScope, S], filter :LocalBoolean[U ByOne S] = True) :G ByVal S =
			By[G, Group[S]#E, Group[S]#E, S](from)(GroupingRelation(group), True)//, filter)

		type * = By.*

		type WithLeft[L <: GroupByClause] = { type F[X] = L By MappingOf[X]#TypedProjection }

		type WithRight[X] = { type F[L <: GroupByClause] = L By MappingOf[X]#TypedProjection }
	}



	/** The first grouping expression of a [[net.noresttherein.oldsql.sql.GroupBy group by]] clause,
	  * represented by a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[S, _]`.
	  */
	type GroupByOne[+F <: FromSome, S] = F GroupBy Group[S]#C

	object GroupByOne {

		def apply[U <: RowProduct, F <: FromSome { type Generalized <: U }, S]
		         (from :F, group :ColumnSQL[U, GlobalScope, S], filter :LocalBoolean[U GroupByOne S] = True)
				:F GroupByOne S =
			GroupBy[F, Group[S]#C, Group[S]#C, S](from)(GroupingRelation[F, Group[S]#C, S](group), filter)

		type * = GroupBy.*

		type WithLeft[L <: FromSome] = { type F[X] = L GroupBy MappingOf[X]#ColumnProjection }

		type WithRight[X] = { type F[L <: FromSome] = L GroupBy MappingOf[X]#ColumnProjection }
	}



	/** A [[net.noresttherein.oldsql.sql.By following]] grouping expression of a ''group by'' clause,
	  * represented by a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[S, _]`.
	  */
	type ByOne[+F <: GroupByClause, S] = F By Group[S]#C

	type AndByOne[+F <: GroupByClause, S] = F AndBy Group[S]#C

	object ByOne {

		def apply[F <: RowProduct,
		          G <: GroupByClause { type Generalized <: U; type GeneralizedDiscrete = F; type Discrete <: F },
		          U <: RowProduct, S]
		         (from :G, group :ColumnSQL[F, GlobalScope, S], filter :LocalBoolean[U ByOne S] = True) :G ByOne S =
			By[G, Group[S]#C, Group[S]#C, S](from)(GroupingRelation[F, Group[S]#C, S](group), filter)

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

	def enquote(string :String) :String = //todo: find a proper implementation on the net/in the drivers
		if (string == null)
			null
		else {
			val s = new java.lang.StringBuilder(string.length * 11 / 10)
			var i = 0; val len = string.length
			s append '\''
			while (i < len) {
				string.charAt(i) match {
					case '\'' => s append '\'' append '\''
					case 0 => s append '\\' append '0'
					case '\n' => s append '\\' append 'n'
					case '\r' => s append '\\' append 'r'
					case '\\' => s append '\\' append '\\'
					case '\u001a' => s append '\\' append 'Z' //ctrl-Z / Windows EoF
					case '\u00a5' | '\u20a9' => // escape characters interpreted as backslash by mysql
					case c => s append c
				}
				i += 1
			}
			s append '\''
			s.toString
		}

}

