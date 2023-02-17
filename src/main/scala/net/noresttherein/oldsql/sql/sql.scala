package net.noresttherein.oldsql

import scala.annotation.showAsInfix

import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.schema.{ColumnForm, SQLForm}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bases.BaseColumn
import net.noresttherein.oldsql.sql.GroupByClause.{Group, GroupingRelation}
import net.noresttherein.oldsql.sql.ParamClause.{NamedParamRelation, ParamAt, ParamRelation, UnboundParam}
import net.noresttherein.oldsql.sql.RowProduct.{As, GroupingOfGeneralized, NoParams, ParamsRow}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, Single}
import net.noresttherein.oldsql.sql.ast.{ColumnLiteral, ColumnLValueSQL, GroupingSQL, LValueSQL, SQLLiteral}
import net.noresttherein.oldsql.sql.ast.ParamSQL.LastParam
import net.noresttherein.oldsql.sql.ast.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.mechanics.=~=




//todo: rename to org.oldsql.sql.syntax
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

	/** An upper bound of all [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] subtypes
	  * with the value type `V` and based on the [[net.noresttherein.oldsql.sql.RowProduct ''from'' clause]] `F`
	  * which can be used freely in the context of any SQL ''select'' based on `F` as well as any of its subselects.
	  * Most expression types derive from this type rather than
	  * its supertype [[net.noresttherein.oldsql.sql.GroupedSQL LocalSQL]], with the sole exception being
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.Single]]
	  * @see [[net.noresttherein.oldsql.sql.GroupedSQL]]
	  */
	type SingleSQL[-F <: RowProduct, V] = SQLExpression[F, Single, V]

	/** An upper bound of all [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] subtypes
	  * with the value type `V` and based on the [[net.noresttherein.oldsql.sql.RowProduct ''from'' clause]] `F`
	  * which can be used freely in the context of any SQL ''select'' based on `F` as well as any of its subselects.
	  * This is a subtype of the [[net.noresttherein.oldsql.sql.SingleSQL GlobalSQL]]`[F, V]`.
	  * Most expression types derive from this type rather than
	  * its supertype [[net.noresttherein.oldsql.sql.GroupedSQL LocalSQL]], with the sole exception being
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.Single]]
	  * @see [[net.noresttherein.oldsql.sql.GroupedColumn]]
	  */
	type SingleColumn[-F <: RowProduct, V] = ColumnSQL[F, Single, V]

	/** An upper bound of all [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] subtypes
	  * with the value type `V` and based on the [[net.noresttherein.oldsql.sql.RowProduct ''from'' clause]] `F`.
	  * It restricts the application scope of the expression to the ''select'' and ''having'' clauses
	  * of an SQL ''select'' based on the clause `F`. It is not allowed in the ''where'' clause
	  * or within subselects of the select it is based on. This allows it to include aggregate expressions
	  * such as `count(*)` as its subtypes.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.Grouped]]
	  * @see [[net.noresttherein.oldsql.sql.SingleSQL]]
	  * @see [[net.noresttherein.oldsql.sql.ast.AggregateSQL]]
	  */
	type GroupedSQL[-F <: RowProduct, V] = SQLExpression[F, Grouped, V]

	/** An upper bound of all [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] subtypes
	  * with the value type `V` and based on the [[net.noresttherein.oldsql.sql.RowProduct ''from'' clause]] `F`.
	  * It restricts the application scope of the expression to the ''select'' and ''having'' clauses
	  * of an SQL ''select'' based on the clause `F`. It is not allowed in the ''where'' clause
	  * or within subselects of the select it is based on. This allows it to include aggregate expressions
	  * such as `count(*)` as its subtypes.
	  * This is a subtype of the [[net.noresttherein.oldsql.sql.GroupedSQL LocalSQL]]`[F, V]`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.Grouped]]
	  * @see [[net.noresttherein.oldsql.sql.SingleColumn]]
	  * @see [[net.noresttherein.oldsql.sql.ast.AggregateSQL]]
	  */
	type GroupedColumn[-F <: RowProduct, V] = ColumnSQL[F, Grouped, V]

//	type GlobalColumn[-]
	/** A type alias for [[net.noresttherein.oldsql.sql.SQLExpression SQL expressions]] depending on no relations
	  * in the ''from'' clause (including parameters), that is applicable
	  * to any [[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.Single]]
	  */
	type GroundSQL[T] = SQLExpression[RowProduct, Single, T]

	/** A type alias for SQL column [[net.noresttherein.oldsql.sql.ColumnSQL expressions]] independent of any relations
	  * in the FROM clause, that is applicable to any [[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
	  * It is a subtype of [[net.noresttherein.oldsql.sql.GroundSQL GroundSQL]]`[V]`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.Single]]
	  */
	type GroundColumn[V] = ColumnSQL[RowProduct, Single, V]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of Boolean value and
	  * with [[net.noresttherein.oldsql.sql.SQLExpression.Single scope]] `S`.
	  */
	type SQLBoolean[-F <: RowProduct, -S >: Grouped <: Single] = ColumnSQL[F, S, Boolean]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of Boolean value which can occur in any place
	  * of the SQL select represented by the ''from'' clause `F` or its dependent selects (for subselect clauses,
	  * after expanding with [[net.noresttherein.oldsql.sql.SQLExpression.expand expand]] method).
	  * Such expressions cannot use [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] subexpressions
	  * from an enclosing ''select''. This is in particular the expression type used in all ''where'' clauses.
	  * Note that `SingleBoolean[F] <: `[[net.noresttherein.oldsql.sql.GroupedBoolean GroupedBoolean]]`[F]`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.Single]]
	  */
	type SingleBoolean[-F <: RowProduct] = ColumnSQL[F, Single, Boolean]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of Boolean value which can be used solely
	  * within ''select'' and ''having'' clauses of the most deeply nested select represented by the ''from'' clause `F`.
	  * It is typically a derivative expression, containing an [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]]
	  * expression as its subexpression.
	  * Note that [[net.noresttherein.oldsql.sql.SingleBoolean GlobalBoolean]]`[F] <: LocalBoolean[F]`.
	  */
	type GroupedBoolean[-F <: RowProduct] = ColumnSQL[F, Grouped, Boolean]

	object SQLBoolean {
		object True extends ColumnLiteral[Boolean] {
			override val value = true
			override val form :ColumnForm[Boolean] = ColumnForm[Boolean]

			/** This literal upcast to `GlobalBoolean[F]`, useful for better type inference in fold-like scenarios. */
			def apply[F <: RowProduct] :SingleBoolean[F] = this

			def unapply(expression :SQLExpression[Nothing, Grouped, _]) :Boolean = expression match {
				case SQLLiteral(v :Boolean) => v
				case _ => false
			}

			override def &&[E <: RowProduct, O >: Grouped <: Single]
			               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = other

			override def ||[E <: RowProduct, O >: Grouped <: Single]
			               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = this

			override def ^ [E <: RowProduct, O >: Grouped <: Single]
			               (other :ColumnSQL[E, O, Boolean])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = !other

			override def toString = "True"
		}

		object False extends ColumnLiteral[Boolean] {
			override val value = true
			override val form :ColumnForm[Boolean] = ColumnForm[Boolean]

			/** This literal upcast to `GlobalBoolean[F]`, useful for better type inference in fold-like scenarios. */
			def apply[F <: RowProduct] :SingleBoolean[F] = this

			def unapply(expression :SQLExpression[Nothing, Grouped, _]) :Boolean = expression match {
				case SQLLiteral(v :Boolean) => !v
				case _ => false
			}

			override def &&[E <: RowProduct, O >: Grouped <: Single]
			               (other :SQLBoolean[E, O])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = this

			override def ||[E <: RowProduct, O >: Grouped <: Single]
			               (other :SQLBoolean[E, O])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = other

			override def ^ [E <: RowProduct, O >: Grouped <: Single]
			               (other :SQLBoolean[E, O])(implicit ev :Boolean =:= Boolean) :SQLBoolean[E, O] = other

			override def toString = "False"
		}
	}

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of `String` type and
	  * with [[net.noresttherein.oldsql.sql.SQLExpression.Single scope]] `S`.
	  */
	type SQLString[-F <: RowProduct, -S >: Grouped <: Single] = ColumnSQL[F, S, String]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of `String` type which can occur in any place
	  * of the SQL select represented by the ''from'' clause `F` or its enclosing selects (for subselect clauses).
	  * Such expressions can also be expanded to subselects of the clause `F` using the
	  * [[net.noresttherein.oldsql.sql.SQLExpression.basedOn basedOn]] method. This is in particular the expression type
	  * used in all ''where'' clauses.
	  * Note that `SingleString[F] <: `[[net.noresttherein.oldsql.sql.GroupedString GroupedString]]`[F]`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.Single]]
	  */
	type SingleString[-F <: RowProduct] = ColumnSQL[F, Single, String]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of `String` type which can be used solely
	  * within ''select'' and ''having'' clauses of the most deeply nested select represented by the ''from'' clause `F`.
	  * It is typically a derivative expression, containing an [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]]
	  * expression as its subexpression.
	  * Note that [[net.noresttherein.oldsql.sql.SingleString GlobalString]]`[F] <: LocalString[F]`.
	  */
	type GroupedString[-F <: RowProduct] = ColumnSQL[F, Grouped, String]



	/** A type wrapper for the type constructor of parameter mappings with string literals as names, present in their
	  * type signature. Importing it imports also a companion method `?:[X :SQLForm]` returning a new relation
	  * for parameter of type `X` as well as an implicit conversion adding a `?:[X]` factory method to string literals.
	  */
	@showAsInfix
	type ?:[N <: Label, X] = NamedParamRelation[N, X]

	/** Creates a dedicated [[net.noresttherein.oldsql.schema.Relation Relation]] for the synthetic parameter mapping
	  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, _]`. It represents a query/statement
	  * parameter of type `X`, the value of which is unspecified at this point, to be provided when executing
	  * the statement. The relation can only be used in [[net.noresttherein.oldsql.sql.ParamClause ParamClause]]
	  * join types. Importing this symbol imports also an overloaded factory method accepting a
	  * [[net.noresttherein.oldsql.schema.bits.LabeledMapping.Label Label]] type parameter for the parameter name,
	  * as well as an implicit conversion adding a [[net.noresttherein.oldsql.sql.NamedParamRelationFromLabel.?: ?:]]
	  * extension method to `String` literals, creating parameter relations for labeled mappings.
	  * @see [[net.noresttherein.oldsql.sql.?:[N<:Label:ValueOf,X:SQLForm] ?:]]
	  */
	def ?:[X :SQLForm] :ParamRelation[X] = ParamRelation()

	/** Creates a dedicated [[net.noresttherein.oldsql.schema.Relation Relation]] for a synthetic parameter mapping
	  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, _]`. It represents a query/statement
	  * parameter of type `X`, the value of which is unspecified at this point, to be provided when executing
	  * the statement. When used to create a [[net.noresttherein.oldsql.sql.ParamClause ParamClause]],
	  * it will automatically add an [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause with the parameter name
	  * (as a string literal type `N`) to the result. Importing this symbol imports also an overloaded factory method
	  * without the label, as well as an implicit conversion adding
	  * a [[net.noresttherein.oldsql.sql.NamedParamRelationFromLabel.?: ?:]] extension method to `String` literals,
	  * creating parameter relations for labeled mappings.
	  * @see [[net.noresttherein.oldsql.sql.?:[X:SQLForm] ?:]]
	  */
	def ?:[N <: Label :ValueOf, X :SQLForm] :NamedParamRelation[N, X] = NamedParamRelation()

	/** Adds a [[net.noresttherein.oldsql.sql.NamedParamRelationFromLabel.?: ?:]] method to `String` literals
	  * for creating labeled synthetic parameter relations. Code `"level".?:[Int]` will produce
	  * a [[net.noresttherein.oldsql.sql.ParamClause.NamedParamRelation NamedParamRelation]]`["level", Int]`.
	  */
	implicit def extension_?:[N <: Label](name :N) = new NamedParamRelationFromLabel[N](name)

	/** Extension method [[net.noresttherein.oldsql.sql.NamedParamRelationFromLabel.?: ?:]] for `String` literals,
	  * creating a dedicated [[net.noresttherein.oldsql.schema.Relation Relation]] for a parameter mapping
	  * labeled with this literal type.
	  */
	class NamedParamRelationFromLabel[N <: Label](private val name :N) extends AnyVal {
		/** Creates a synthetic [[net.noresttherein.oldsql.schema.Relation.StaticRelation StaticRelation]]
		  * using this `String` literal as relation the name, SQL statement parameter name and mapping label
		  * for access from [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]].
		  */
		def ?:[X :SQLForm] :NamedParamRelation[N, X] = NamedParamRelation(name)
	}

//  problem with overloads... currently just a method on SQLForm
//	/** Adds a [[net.noresttherein.oldsql.sql.NamedParamRelationFromForm.?: ?:]] method to
//	  * [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] for creating labeled synthetic parameter relations.
//	  */
//	implicit def ?:[X](form :SQLForm[X])(implicit erasureGuard :Boolean = false) = new NamedParamRelationFromForm(form)
//
//	/** Extension method [[net.noresttherein.oldsql.sql.NamedParamRelationFromForm.?: ?:]]
//	  * for [[net.noresttherein.oldsql.schema.SQLForm SQLForm]], creating a dedicated
//	  * [[net.noresttherein.oldsql.schema.Relation Relation]] for a parameter mapping labeled with a literal type.
//	  */
//	class NamedParamRelationFromForm[X](private val form: SQLForm[X]) extends AnyVal {
//		/** Creates a synthetic [[net.noresttherein.oldsql.schema.Relation.StaticRelation StaticRelation]]
//		  * for an unbound SQL [[net.noresttherein.oldsql.sql.ParamClause.NamedParamRelation parameter]]
//		  * using the (left) argument `String` literal as relation the name, SQL statement parameter name
//		  * and mapping label for access from [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]].
//		  */
//		def ?:[N <: Label](name :N) :NamedParamRelation[N, X] = NamedParamRelation[N, X](name)(form)
//	}



	/** An alias for `JoinParam` accepting the parameter type as the second (right) argument, hiding the
	  * `UnboundParam[X, _]` from the type signature.
	  */ //not F <: TopFromSome so it can be used in Generalized types
	@showAsInfix
	type WithParam[+F <: FromSome, X] = JoinParam[F, ParamRelation[X]#Param] //todo: rename ParamVal, JoinVal, AndParam - conflicts with param for WithClause

	/** A pseudo companion object to type alias [[net.noresttherein.oldsql.sql.WithParam! WithParam]],
	  * containing standard type aliases provided by companions of various
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] clauses.
	  */
	object WithParam {
		/** An existential upper bound of all [[net.noresttherein.oldsql.sql.WithParam WithParam]] instances
		  * which can be used in casting or pattern matching without generating compiler warnings about erasure.
		  */
		type __ = JoinParam.__

		/** The least upper bound of all [[net.noresttherein.oldsql.sql.WithParam WithParam]] instances
		  * introducing an [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam unbound]] parameter of type `X`
		  * by using mapping [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, _]`.
		  */
		type LUB[X] = FromSome WithParam X

		/** A type alias for [[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] type defined by all
		  * `_ WithParam X`.
		  */
		type Last[X] = RowProduct AndFrom ParamRelation[X]#Param

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.WithParam WithParam]] instances, accepting
		  * the left [[net.noresttherein.oldsql.sql.FromSome FromSome]] type parameter,
		  * and returning a type with a member type `F` accepting the parameter type.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithLeft[L <: FromSome] = { type F[X] = L WithParam X }

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.WithParam WithParam]] instances, accepting
		  * the parameter type used as the subject of synthetic mapping
		  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]],
		  * with a member type `F`, accepting the left [[net.noresttherein.oldsql.sql.FromSome FromSome]] type.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithRight[X] = { type F[L <: FromSome] = L WithParam X }
	}

	/** The least upper bound of all ungrouped ''from'' clauses ending with an unbound parameter using
	  * a synthetic parameter mapping [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, _]`.
	  */
	type AndFromParam[+L <: RowProduct, X] = L AndFrom ParamRelation[X]#Param


	/** An alias for `GroupParam` accepting the parameter type as the second (right) argument, hiding the
	  * `UnboundParam[X, _]` from the type signature.
	  */
	@showAsInfix
	type ByParam[+F <: GroupByClause, X] = GroupParam[F, ParamRelation[X]#Param]

	/** A pseudo companion of [[net.noresttherein.oldsql.sql.ByParam! ByParam]] type alias,
	  * grouping standard type constructors provided for all [[net.noresttherein.oldsql.sql.Adjoin Adjoin]]
	  * subtypes.
	  */
	object ByParam {
		/** An existential upper bound of all [[net.noresttherein.oldsql.sql.ByParam ByParam]] instances
		  * which can be used in casting or pattern matching without generating compiler warnings about erasure.
		  */
		type __ = GroupParam.__

		/** The least upper bound of all [[net.noresttherein.oldsql.sql.ByParam ByParam]] instances
		  * introducing an [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam unbound]] parameter of type `X`
		  * by using mapping [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, _]`.
		  */
		type LUB[X] = GroupByClause ByParam X

		/** A type alias for [[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] type defined by all
		  * `_ ByParam X`.
		  */
		type Last[X] = RowProduct AndBy ParamRelation[X]#Param

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.ByParam ByParam]] instances, accepting
		  * the left [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] type parameter,
		  * and returning a type with a member type `F` accepting the parameter type.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithLeft[L <: GroupByClause] = { type F[X] = L ByParam X }

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.ByParam ByParam]] instances, accepting
		  * the parameter type used as the subject of synthetic mapping
		  * [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]],
		  * with a member type `F`, accepting the left [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] type.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithRight[X] = { type F[L <: GroupByClause] = L ByParam X }
	}

	/** The least upper bound of all ''group by'' clauses ending with an unbound parameter using
	  * a synthetic parameter mapping [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]`[X, _]`.
	  */
	@showAsInfix
	type AndByParam[+L <: RowProduct, X] = L AndBy ParamRelation[X]#Param


//	type LastParam[P[O] <: ParamAt[O]] = FromSome JoinParam P
	//consider: currently unused; we could hijack the name for JoinParam, and use JoinParam for WithParam,
	// JoinParam for ParamClause and ParamClause for ParamsRow
/*
	type FromParam[P[O] <: ParamAt[O]] = ParamsRow[@~] JoinParam P

	object FromParam {
		type Of[X] = FromParam[ParamRelation[X]#Param]

		/** Create a [[net.noresttherein.oldsql.sql.RowProduct.ParamsRow ParamsRow]] declaring a single
		  * unbound parameter `X` represented by special pseudo relation `param`.
		  * @param param the last relation of the created ''from'' clause, using the
		  *              [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
		  * @return [[net.noresttherein.oldsql.sql.FromParam FromParam]]`[`[[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]]`[X]#Param]`.
		  */
		def apply[X](param :ParamRelation[X]) :Of[X] =
			FromParam[X](param, True)

		/** Create a [[net.noresttherein.oldsql.sql.RowProduct.ParamsRow ParamsRow]] declaring a single
		  * unbound parameter `X` represented by special pseudo relation `param`.
		  * @param param the last relation of the created ''from'' clause, using the
		  *              [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
		  * @param filter an optional join condition filtering the clause based on the value of `X`.
		  * @return [[net.noresttherein.oldsql.sql.FromParam FromParam]]`[`[[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]]`[X]#Param]`.
		  */
		def apply[X](param :ParamRelation[X], filter :GlobalBoolean[FromSome JoinParam ParamRelation[X]#Param]) :Of[X] =
			FromParam[ParamRelation[X]#Param, X, Nothing](LastParam(param), None)(filter)

		/** Create a [[net.noresttherein.oldsql.sql.RowProduct.ParamsRow ParamsRow]] declaring a single
		  * unbound parameter `X` represented by special pseudo relation `param`.
		  * @param param the last relation of the created ''from'' clause,
		  *              using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
		  * @return [[net.noresttherein.oldsql.sql.FromParam FromParam]]`[`[[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]]`[X]#Param] `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` N`.
		  */
		def apply[N <: Label, X](param :NamedParamRelation[N, X]) :Of[X] As N =
			FromParam[UnboundParam.Of[X]#P, X, N](LastParam(param), Some(param.name))(True)

		/** Create a [[net.noresttherein.oldsql.sql.RowProduct.ParamsRow ParamsRow]] declaring a single
		  * unbound parameter `X` represented by special pseudo relation `param`.
		  * @param param  the last relation of the created ''from'' clause,
		  *               using the [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam[X, _] ]] `Mapping` type.
		  * @param filter an optional join condition filtering the clause based on the value of `X`.
		  * @return [[net.noresttherein.oldsql.sql.FromParam FromParam]]`[`[[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]]`[X]#Param] `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` N`.
		  */
		def apply[N <: Label, X]
		         (param :NamedParamRelation[N, X], filter :GlobalBoolean[FromSome JoinParam ParamRelation[X]#Param])
				:Of[X] As N =
			FromParam[UnboundParam.Of[X]#P, X, N](LastParam(param), Some(param.name))(filter)

		private[sql] def apply[M[O] <: UnboundParam[X, O], X, A <: Label]
		                      (param :LastRelation[M, X], asOpt :Option[A])
		                      (cond :GlobalBoolean[FromSome JoinParam M]) : FromParam[M] As A =
			JoinParam[NoParams, M, X, A](NoParams, param, asOpt)(cond)
	}
*/



	/** A type alias for [[net.noresttherein.oldsql.sql.AndBy AndBy]], the supertype of all ''group by'' clauses
	  * which add an arbitrary expression with value type `S`,
	  * represented by a [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]`[S, _]`.
	  */
	@showAsInfix
	type AndByVal[+F <: RowProduct, S] = F AndBy Group[S]#M

	/** A pseudo companion of [[net.noresttherein.oldsql.sql.AndByVal! AndByVal]] type alias,
	  * grouping standard type constructors provided for all [[net.noresttherein.oldsql.sql.Adjoin Adjoin]]
	  * subtypes.
	  */
	object AndByVal {
		/** An existential upper bound of all [[net.noresttherein.oldsql.sql.AndByVal AndByVal]] instances
		  * which can be used in casting or pattern matching without generating compiler warnings about erasure.
		  */
		type __ = AndBy.__

		/** The least upper bound of all [[net.noresttherein.oldsql.sql.AndByVal AndByVal]] instances grouping
		  * any clause by an expression represented by synthetic mapping
		  * [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]`[X, _]`.
		  */
		type LUB[X] = RowProduct AndByVal X

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.AndByVal AndByVal]] instances, accepting
		  * the left [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] type parameter
		  * and returning a type with a member type `F` accepting the grouping expression's value type to use
		  * as the subject of the right relation.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithLeft[L <: RowProduct] = { type F[X] = L AndBy Group[X]#M }

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.AndByVal AndByVal]] instances, accepting
		  * the grouping expression's value type to use as the right mapping's subject type, and returning a type
		  * with a member type `F`, accepting the left [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] type.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithRight[X] = { type F[L <: RowProduct] = L AndBy Group[X]#M }
	}



	/** The first grouping expression of a [[net.noresttherein.oldsql.sql.GroupBy group by]] clause,
	  * represented by a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[S, _]`.
	  */ //consider: or GroupBySome/GroupByMany?
	@showAsInfix
	type GroupByVal[+F <: FromSome, S] = F GroupBy Group[S]#M

	/** A pseudo companion and factory of [[net.noresttherein.oldsql.sql.GroupByVal! GroupByVal]] type alias,
	  * grouping standard type constructors provided for all [[net.noresttherein.oldsql.sql.Adjoin Adjoin]]
	  * subtypes.
	  */
	object GroupByVal {
		def apply[F <: FromSome { type Generalized <: G }, G <: FromSome, S]
		         (from :F, group :SQLExpression[G, Single, S], filter :GroupedBoolean[G GroupByVal S] = True)
				:F GroupByVal S =
		{
			val grouping = GroupingRelation(group.anchor(from.generalized))
			val last = GroupingSQL[G, RowProduct AndByVal S, Group[S]#M, S, RowProduct AndByVal S](
				grouping.upcast, 0
			)
			GroupBy[F, Group[S]#M, S, Nothing](from, None)(last, filter)
		}

		/** An existential upper bound of all [[net.noresttherein.oldsql.sql.GroupByVal! GroupByVal]] instances
		  * which can be used in casting or pattern matching without generating compiler warnings about erasure.
		  */
		type __ = GroupBy.__

		/** The least upper bound of all [[net.noresttherein.oldsql.sql.GroupByVal! GroupByVal]] instances, grouping
		  * any clause by an expression represented by synthetic mapping
		  * [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]`[X, _]`.
		  */
		type LUB[X] = FromSome GroupByVal X

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.GroupByVal! GroupByVal]] instances, accepting
		  * the left [[net.noresttherein.oldsql.sql.FromSome FromSome]] type parameter
		  * and returning a type with a member type `F` accepting the grouping expression's value type to use
		  * as the subject of the right relation.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithLeft[L <: FromSome] = { type F[X] = L GroupByVal X }

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.GroupByVal GroupByVal]] instances, accepting
		  * the grouping expression's value type to use as the right mapping's subject type, and returning a type
		  * with a member type `F`, accepting the left [[net.noresttherein.oldsql.sql.FromSome FromSome]] type.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithRight[X] = { type F[L <: FromSome] = L GroupByVal X }
	}



	/** A [[net.noresttherein.oldsql.sql.By following]] grouping expression of a ''group by'' clause,
	  * represented by a [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]`[S, _]`.
	  */
	@showAsInfix
	type ByVal[+F <: GroupByClause, S] = F By Group[S]#M

	/** A pseudo companion and factory of [[net.noresttherein.oldsql.sql.ByVal! ByVal]] type alias,
	  * grouping standard type constructors provided for all [[net.noresttherein.oldsql.sql.Adjoin Adjoin]]
	  * subtypes.
	  */
	object ByVal {
		def apply[F <: FromSome,
		          L <: GroupByClause { type Generalized = G; type GeneralizedDiscrete >: Discrete <: F; type Discrete <: F },
		          G <: GroupByClause, S]
		         (from :L, group :SQLExpression[F, Single, S], filter :GroupedBoolean[G AndByVal S]) :L ByVal S =
			By[L, Group[S]#M, S, Nothing](from, None)(GroupingSQL[F, S](group.anchor(from.fromClause)), filter)

		def apply[F <: FromSome, L <: GroupingOfGeneralized[F], S]
		         (from :L, group :SQLExpression[F, Single, S]) :L ByVal S =
			By[L, Group[S]#M, S, Nothing](from, None)(GroupingSQL[F, S](group.anchor(from.fromClause)), True)

		/** An existential upper bound of all [[net.noresttherein.oldsql.sql.ByVal ByVal]] instances which can be used
		  * in casting or pattern matching without generating compiler warnings about erasure.
		  */
		type __ = By.__

		/** The least upper bound of all [[net.noresttherein.oldsql.sql.ByVal ByVal]] instances grouping any clause
		  * by an expression represented by synthetic mapping
		  * [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]`[X, _]`.
		  */
		type LUB[X] = GroupByClause ByVal X

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.ByVal ByVal]] instances, accepting
		  * the left [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] type parameter
		  * and returning a type with a member type `F` accepting the grouping expression's value type to use
		  * as the subject of the right relation.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithLeft[L <: GroupByClause] = { type F[X] = L By Group[X]#M }

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.ByVal ByVal]] instances, accepting
		  * the grouping expression's value type to use as the right mapping's subject type, and returning a type
		  * with a member type `F`, accepting the left [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] type.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithRight[X] = { type F[L <: GroupByClause] = L By Group[X]#M }
	}



	/** A type alias for [[net.noresttherein.oldsql.sql.AndBy AndBy]], the supertype of all ''group by'' clauses
	  * which add an arbitrary single column expression with value type `S`,
	  * represented by a [[net.noresttherein.oldsql.sql.ColumnSQLMapping ColumnSQLMapping]]`[S, _]`.
	  */
	@showAsInfix
	type AndByOne[+F <: RowProduct, S] = F AndBy Group[S]#Column

	/** A pseudo companion of [[net.noresttherein.oldsql.sql.AndByOne! AndByOne]] type alias,
	  * grouping standard type constructors provided for all [[net.noresttherein.oldsql.sql.Adjoin Adjoin]]
	  * subtypes.
	  */
	object AndByOne {
		/** An existential upper bound of all [[net.noresttherein.oldsql.sql.AndByOne! AndByOne]] instances,
		  * which can be used in casting or pattern matching without generating compiler warnings about erasure.
		  */
		type __ = AndBy.__

		/** The least upper bound of all [[net.noresttherein.oldsql.sql.AndByOne! AndByOne]] instances grouping,
		  * any clause by a single column expression represented by synthetic mapping
		  * [[net.noresttherein.oldsql.sql.ColumnSQLMapping ColumnSQLMapping]]`[X, _]`.
		  */
		type LUB[X] = RowProduct AndByOne X

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.AndByOne! AndByOne]] instances, accepting
		  * the left [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] type parameter,
		  * and returning a type with a member type `F` accepting the grouping expression's value type to use
		  * as the subject of the right relation.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithLeft[L <: RowProduct] = { type F[X] = L AndByOne X }

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.AndByOne! AndByOne]] instances, accepting
		  * the grouping expression's value type to use as the right mapping's subject type, and returning a type
		  * with a member type `F`, accepting the left [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] type.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithRight[X] = { type F[L <: RowProduct] = L AndByOne X }
	}



	/** A [[net.noresttherein.oldsql.sql.GroupBy group by]] clause consisting of a single, arbitrary,
	  * single column expression of type `S`,
	  * represented by a [[net.noresttherein.oldsql.sql.ColumnSQLMapping ColumnSQLMapping]]`[S, _]`.
	  */
	@showAsInfix
	type GroupByOne[+F <: FromSome, S] = F GroupBy Group[S]#Column

	/** A pseudo companion and a factory of [[net.noresttherein.oldsql.sql.GroupByOne! GroupByOne]] type alias,
	  * grouping standard type constructors provided for all [[net.noresttherein.oldsql.sql.Adjoin Adjoin]]
	  * subtypes.
	  */
	object GroupByOne {
		def apply[F <: FromSome { type Generalized <: G }, G <: FromSome, S]
		         (from :F, group :ColumnSQL[G, Single, S], filter :GroupedBoolean[G GroupByOne S] = True)
				:F GroupByOne S =
		{
			val grouping = GroupingRelation(group.anchor(from.generalized))
			val last = GroupingSQL[from.Generalized, RowProduct AndByOne S, Group[S]#C, S, RowProduct AndByOne S](
				grouping.upcast, 0
			)
			GroupBy[F, Group[S]#C, S, Nothing](from, None)(last, filter)
		}

		/** An existential upper bound of all [[net.noresttherein.oldsql.sql.GroupByOne! GroupByOne]] instances,
		  * which can be used in casting or pattern matching without generating compiler warnings about erasure.
		  */
		type __ = GroupBy.__

		/** The least upper bound of all [[net.noresttherein.oldsql.sql.GroupByOne! GroupByOne]] instances grouping
		  * any clause by an expression represented by synthetic mapping
		  * [[net.noresttherein.oldsql.sql.ColumnSQLMapping ColumnSQLMapping]]`[X, _]`.
		  */
		type LUB[X] = FromSome GroupByOne X

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.GroupByOne! GroupByOne]] instances, accepting
		  * the left [[net.noresttherein.oldsql.sql.FromSome FromSome]] type parameter
		  * and returning a type with a member type `F` accepting the grouping expression's value type to use
		  * as the subject of the right relation.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithLeft[L <: FromSome] = { type F[X] = L GroupByOne X }

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.GroupByOne! GroupByOne]] instances, accepting
		  * the grouping expression's value type to use as the right mapping's subject type, and returning a type
		  * with a member type `F`, accepting the left [[net.noresttherein.oldsql.sql.FromSome FromSome]] type.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithRight[X] = { type F[L <: FromSome] = L GroupByOne X }
	}



	/** A [[net.noresttherein.oldsql.sql.By following]] grouping expression in a ''group by'' clause,
	  * represented by a [[net.noresttherein.oldsql.sql.ColumnSQLMapping ColumnSQLMapping]]`[S, _]`.
	  */
	@showAsInfix
	type ByOne[+F <: GroupByClause, S] = F By Group[S]#Column

	/** A pseudo companion and a factory of [[net.noresttherein.oldsql.sql.ByOne! ByOne]] type alias,
	  * grouping standard type constructors provided for all [[net.noresttherein.oldsql.sql.Adjoin Adjoin]]
	  * subtypes.
	  */
	object ByOne {
		def apply[F <: FromSome,
		          L <: GroupByClause { type Generalized = G; type GeneralizedDiscrete >: Discrete <: F; type Discrete <: F },
		          G <: GroupByClause, S]
		         (from :L, group :ColumnSQL[F, Single, S], filter :GroupedBoolean[G AndByOne S]) :L ByOne S =
			By[L, Group[S]#C, S, Nothing](from, None)(GroupingSQL[F, S](group.anchor(from.fromClause)), filter)

		def apply[F <: FromSome, L <: GroupingOfGeneralized[F], S]
		         (from :L, group :ColumnSQL[F, Single, S]) :L ByOne S =
			By[L, Group[S]#C, S, Nothing](from, None)(GroupingSQL[F, S](group.anchor(from.fromClause)), True)

		/** An existential upper bound of all [[net.noresttherein.oldsql.sql.ByOne! ByOne]] instances,
		  * which can be used in casting or pattern matching without generating compiler warnings about erasure.
		  */
		type __ = By.__

		/** The least upper bound of all [[net.noresttherein.oldsql.sql.ByOne! ByOnE]] instances grouping
		  * any clause by an expression represented by synthetic mapping
		  * [[net.noresttherein.oldsql.sql.ColumnSQLMapping ColumnSQLMapping]]`[X, _]`.
		  */
		type LUB[X] = GroupByClause ByOne X

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.ByOne! ByOne]] instances, accepting
		  * the left [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] type parameter
		  * and returning a type with a member type `F` accepting the grouping expression's value type to use
		  * as the subject of the right relation.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithLeft[L <: GroupByClause] = { type F[X] = L ByOne X }

		/** A curried type constructor for [[net.noresttherein.oldsql.sql.ByOne! ByOne]] instances, accepting
		  * the grouping expression's value type to use as the right mapping's subject type, and returning a type
		  * with a member type `F`, accepting the left [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] type.
		  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
		  */
		type WithRight[X] = { type F[L <: GroupByClause] = L ByOne X }
	}







	/** A type alias for [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]] of any expression type,
	  * which in most of its use cases is not needed. When used in an infix manner,
	  * `T := F` (for some `T, F <: RowProduct)`, it represents an assignment
	  * of an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, GlobalScope, Value]` to
	  * a [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]]`[T, GlobalScope, Value]`.
	  * The latter is a supertype of [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expressions -
	  * representations of column sets of some table in `T` and their SQL-transparent conversions (used to unify
	  * the types of both ''lvalue'' and ''rvalue'' by applying available implicit conversions to the expressions,
	  * in order to bring them both to some common type - for example by number type promotion). The former
	  * is any non-aggregate expression based on ''from'' clause `F`. Currently, `T` is always
	  * [[net.noresttherein.oldsql.sql.From From]]`[M]`, enforcing that `lvalue` of `:=` is a component
	  * of a table with row [[net.noresttherein.oldsql.schema.Mapping mapping]] `M`. `F` can also be `From[M]` -
	  * for [[net.noresttherein.oldsql.sql.Update updates]]
	  * of table [[net.noresttherein.oldsql.schema.RelVar RelVar]]`[M]` which do not depend on any outside
	  * parameters (which use only to the updated table itself and expressions with concrete values -
	  * [[net.noresttherein.oldsql.sql.ast.SQLLiteral literals]] or
	  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters); it can however also take many more
	  * forms, in practice some combination of `From[M]` and [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]
	  * pseudo joins introducing expressions for statement parameters with values not known at their creation.
	  * For example, `From[M] := (From[M] WithParam X)` is an assignment usable by ''update'' statements
	  * parameterized with `X`: it assigns to some updated component of `M` a value of an expression depending
	  * on `M` itself and a parameter of type `X`. In [[net.noresttherein.oldsql.sql.Insert Insert]] statements
	  * the inserted values cannot refer to the inserted row, as no preexisting values are present;
	  * thus, `From[M] := RowProduct` is used by parameterless statements (enforcing that the assigned values
	  * are self-contained), or `From[M] := (FromSome WithParam X)` for statements parameterized with type `X`,
	  * as in the previous case. For both of these statement types, additional parameters can be added by subsequent use
	  * of [[net.noresttherein.oldsql.sql.WithParam WithParam]].
	  *
	  * Instances can be created with overloaded [[net.noresttherein.oldsql.sql.ast.LValueSQL.:= :=]]
	  * method available on [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expressions,
	  * as well as other, related assignment operators with more traditional forms
	  * such as [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.+= +=]]. Additionally,
	  * most of these operators have a version ending with `?`- for example,
	  * [[net.noresttherein.oldsql.sql.ast.LValueSQL.:=? :=?]]
	  * or [[net.noresttherein.oldsql.sql.ast.ColumnLValueSQL.+=? +=?]] - which accept any scala value
	  * of a compatible type with an [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] type class,
	  * and use it, wrapping it in a [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]]
	  * (rather than a [[net.noresttherein.oldsql.sql.ast.SQLLiteral SQLLiteral]] to which it would be otherwise
	  * implicitly converted). This will render the SQL for the expression as the JDBC parameter placeholder `"?"`,
	  * which is important in order to not flood the cache of the driver and the database with countless versions
	  * inlining their value.
	  *
	  * Note that `Mapping` subtypes `M[F]` - with an evident type constructor `M[O] <: Mapping { type Origin = O }`
	  * including an implicit [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] type class -
	  * are implicitly convertible to [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]`[F, M]`
	  * when used in the ''lvalue'' position of operator `:=` and its kin (just as invoking other operator-methods
	  * of `SQLExpression`, including in particular comparisons
	  * such as [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]]
	  * and arithmetic operators such as [[net.noresttherein.oldsql.sql.ColumnSQL.+ +]], will cause
	  * this implicit conversion).
	  */ //it's :=, not <- for example, because def := has assignment precedence
	type :=[-L <: RowProduct, -R <: RowProduct] = ComponentSetter[L, R]

	object := {
		def unapply[L <: RowProduct, R <: RowProduct](setter :ComponentSetter[L, R])
				:Opt[(LValueSQL[L, setter.Component, setter.Value], SQLExpression[R, Single, setter.Value])] =
			Got((setter.lvalue, setter.rvalue))
	}

	type |:=[-L <: RowProduct, -R <: RowProduct] = ColumnSetter[L, R]

	object |:= {
		def unapply[L <: RowProduct, R <: RowProduct](setter :ColumnSetter[L, R])
				:Opt[(ColumnLValueSQL[L, setter.Component, setter.Value], SQLExpression[R, Single, setter.Value])] =
			Got((setter.lvalue, setter.rvalue))

		def unapply[L <: RowProduct, R <: RowProduct](setter :ComponentSetter[L, R])
				:Opt[(ColumnLValueSQL[L, M, setter.Value], SQLExpression[R, Single, setter.Value]) forSome {
					type M[A] <: BaseColumn[setter.Value, A] with setter.Component[A]
				}] =
			setter match {
				case set :(ColumnSetter[L, R] {
					type Value = setter.Value
					type Component[A] = BaseColumn[setter.Value, A] with setter.Component[A]
				}) @unchecked =>
					Got((set.lvalue, set.rvalue))
			}
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
	//todo: desanitize

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



	/** An implicit argument used to 'seal' methods to package `sql`. A method `private[scope]` or `protected[scope]`
	  * cannot be used outside package/class `scope`, but, unless final, it can be overriden by extending classes
	  * with a matching `public` definition. In some cases we can't declare the method `final` in all open classes,
	  * but we don't want it to leak in any form, including overriding, to client code. Accepting an implicit parameter
	  * of `Seal` (a value of which is always available) ensures that the method cannot be overriden, as `Seal`
	  * cannot be referenced outside this package. Note that this should not be dependent upon in the context
	  * of security, as all such declarations are public in the bytecode and can thus be easily accessed from `Java`.
	  */
	private[sql] final class Seal

	private[sql] object Seal {
		@inline def apply() :Seal = instance
		implicit final val instance = new Seal
	}

	/** A value wrapper with visibility restricted to package `sql`, ensuring that any definition including it
	  * can neither be used nor overriden by extending classes from outside this package. A declaration
	  * of `private[sql] val x :Int` is usable only within the specified scope, but an extending class from any
	  * package can always override it with a `val x :Int`. Adding a `final` modifier solves this source
	  * of interface leak but is not always possible if other classes from withing the package override/implement
	  * the field. Declaring it as `protected[sql] val x :Sealed[Int]` makes overriding impossible, as class `Sealed`
	  * can be neither extended, nor referenced from outside the package. Inlined implicit boxing and unboxing reduces
	  * the syntax cost of this pattern. Note that this should not be dependent upon in the context of security,
	  * as all such declarations are public in the bytecode and can thus be easily accessed from `Java`.
	  */
	private[sql] class Sealed[+T](val value :T) extends AnyVal

	private[sql] object Sealed {
		@inline def apply[T](value :T) :Sealed[T] = new Sealed(value)

		@inline implicit def unseal[T](value :Sealed[T]) :T = value.value
		@inline implicit def seal[T](value :T) :T = new Sealed(value)
	}
}

