package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.{OldSQLVer => ver}
import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Opt.Lack
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnWriteForm.SingletonColumnWriteForm
import net.noresttherein.oldsql.schema.SQLForm.AbstractMappedForm
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.forms.ChainForm
import net.noresttherein.oldsql.sql.Call.{DirectCallFunction, DirectCallProcedure}
import net.noresttherein.oldsql.sql.Call.CallFunction.{CallFunctionNamedParamDecl, CallFunctionParamDecl}
import net.noresttherein.oldsql.sql.Call.CallProcedure.{CallProcedureNamedParamDecl, CallProcedureParamDecl}
import net.noresttherein.oldsql.sql.RowProduct.ParamsRow
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, Single}
import net.noresttherein.oldsql.sql.ast.{BoundParam, ChainTuple, FunctionColumnSQL, FunctionSQL}
import net.noresttherein.oldsql.sql.mechanics.{ProcedureSignature, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}

//here be implicits
import net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL






/** Common interface of stored [[net.noresttherein.oldsql.sql.StoredProcedure procedures]] and
  * [[net.noresttherein.oldsql.sql.StoredFunction functions]]. At the same time, it serves also as an integration
  * point for future or custom extensions.
  * @tparam Params type(s) of parameter or parameters of this function.
  * @tparam Res    the return type of this function.
  */
trait SQLExecutable[Params, +Res] extends Serializable {
	/** The name of this stored procedure/function. */
	val name :String

	/** The form responsible for setting the parameters of [[java.sql.CallableStatement CallableStatement]] instances
	  * invoking this stored procedure/function.
	  */
	val paramForm :SQLWriteForm[Params]

	/** Executes this function in the application memory for the given arguments, if its value is known.
	  * Defaults to `None`, but standard SQL functions for date formatting and such can implement this functionality.
	  * This method is used to provide [[net.noresttherein.oldsql.sql.ast.FunctionSQL.groundValue groundValue]]
	  * of SQL [[net.noresttherein.oldsql.sql.ast.FunctionSQL expressions]] invoking this function/procedure.
	  */
	def eval(args :Params) :Opt[Res] = Lack

	/** Creates a [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] executing this stored procedure.
	  * The parameters of the procedure become the parameters of the statement.
	  */
	def call :Call[Params, Res]


	/** Creates SQL text of an SQL expression invoking this function using
	  * [[java.sql.PreparedStatement PreparedStatement]]'s parameters directly as the arguments
	  * (i.e., the function is applied to a sequence of '?' placeholders).
	  * This formats only the function's signature itself, not a whole expression and is intended to be used
	  * as a subexpression of a larger SQL/DML expression, and not a standalone call. For that, see
	  * `this.`[[net.noresttherein.oldsql.sql.SQLExecutable.call call]]`.`[[net.noresttherein.oldsql.sql.Call.chant chant]].
	  * Note that for actual stored procedures (not [[net.noresttherein.oldsql.sql.StoredFunction functions]]),
	  * which do not return any value, this would be an invalid expression. It is the responsibility of the caller
	  * to ensure that only subclasses of `StoredProcedure` which can form legal SQL subexpressions invoke this method.
	  * It exists to open up the hierarchy for implementations representing stored functions returning non-standard
	  * values, unable to conform to `StoredFunction`, or whose usage must be restricted with respect to 'normal'
	  * functions returning distinct types.
	  */
	protected def paramSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Params] = {
		val sql = spelling.function(name) + "(" + paramForm.inlineParam + ")"
		SpelledSQL(sql, paramForm, spelling.newContext)
	}

	private[sql] final def `->paramSpelling`(spelling :SQLSpelling) :SpelledSQL[Params] =
		paramSpelling(spelling)

	/** Creates SQL text of an SQL expression invoking this function with arguments listed by the tuple expression
	  * `args`. It is intended to be used as a subexpression of a larger SQL/DML expression, and not a standalone call.
	  * For that, see [[net.noresttherein.oldsql.sql.SQLExecutable.call call]]`(args)`.
	  */
	protected def defaultSpelling[P, F <: RowProduct](args :SQLExpression[F, Grouped, Params])
	                                                 (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		(spelling.function(name) +  "(") +: (spelling.inCall(args)(from, context, params) + ")")

	private[sql] final def defaultSpelling[P, F <: RowProduct]
	                                      (spelling :SQLSpelling)(args :SQLExpression[F, Grouped, Params])
	                                      (from :F, context :SQLContext[P], params :Parameterization[P, F])
			:SpelledSQL[P] =
		defaultSpelling(args)(from, context, params)(spelling)


	/** Creates a [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] executing this stored procedure.
	  * The parameters of the procedure become the parameters of the statement.
	  */
	def chant(implicit dialect :Maybe[SQLDialect]) :Incantation[Params, Res] =
		this.call.chant(dialect.opt getOrElse StandardSQL)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLExecutable[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :SQLExecutable[_, _] if canEqual(other) && other.canEqual(this) =>
			name == other.name && paramForm == other.paramForm
		case _ => false
	}
	override def hashCode :Int = name.hashCode

	override def toString :String = name
}






/** A signature of an SQL stored procedure for use in SQL [[net.noresttherein.oldsql.sql.DMLStatement statements]].
  * This is the most generic variant, which accepts arguments as a polymorphic
  * [[net.noresttherein.oldsql.sql.ast.ChainTuple tuple]]; most implementations will be instances
  * of one of the subclasses specialized for concrete parameter lists. The elements of the chain typically correspond
  * one-to-one to the parameter's of the procedure, but this not needs be the case, and it can contain multi column
  * types, which map to a consecutive sequence of parameters. The argument chain can contain elements
  * of type [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[T]` to denote that the given parameter
  * (or, more precisely, all procedure's parameters mapped
  * by the [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]]`[T]`) is an ''OUT'' parameter.
  * Such parameters are returned as a consecutive chain from [[net.noresttherein.oldsql.sql.Call Call]] statements
  * invoking this procedure - as long as the arguments to the call are given as `Out(x)`/`SQLExpression[_,_Out[X]]`
  * rather than `x`/`SQLExpression[_,_,X]`, which in most cases is possible).
  *
  * @tparam Params a [[net.noresttherein.oldsql.collection.Chain Chain]] listing the types of all arguments of this function.
  * @see [[net.noresttherein.oldsql.sql.Call]]
  * @author Marcin Mościcki
  */
trait StoredProcedure[Params <: Chain] extends SQLExecutable[Params, Any] {
	/** The form responsible for setting the parameters of [[java.sql.CallableStatement CallableStatement]] instances
	  * invoking this stored procedure.
	  */
	override val paramForm :ChainForm[Params]

	/** Creates a [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] executing this stored procedure.
	  * The parameters of the procedure become the parameters of the statement.
	  */
	override def call :DirectCallProcedure[Params] = Call(this)

	/** Creates an executable SQL command which invokes this procedure with arbitrary SQL expressions `args`
	  * as the arguments. The [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters of the domain `F`
	  * of the tuple expression become the parameters of the command. This is a shorter version of
	  * to `this.`[[net.noresttherein.oldsql.sql.StoredProcedure.call]][[net.noresttherein.oldsql.sql.Call.DirectCallProcedure.apply[F<:RowProduct,Xs<:Chain]* (args)]]`.`[[net.noresttherein.oldsql.sql.Call.chant chant]],
	  * except the ''OUT'' parameters are not returned.
	  */
	def chant[F <: FromClause, Xs <: Chain]
	         (args :ChainTuple[F, Single, Xs])
	         (implicit signature :ProcedureSignature[Params, Xs] { type Domain <: F }, dialect :Maybe[SQLDialect])
			:Incantation[signature.CallParams, signature.Out] =
		signature(this, args).chant(dialect getOrElse StandardSQL)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[StoredProcedure[_]]
}




object StoredProcedure {
	def apply[X <: Chain :ChainForm](name :String) :StoredProcedure[X] = new BaseStoredProcedure[X](name)

	def of0(name :String) :StoredProcedure0 = new BaseStoredProcedure(name)

	def of1[A :SQLForm](name :String) :StoredProcedure1[A] = new BaseStoredProcedure(name)

	def of2[A :SQLForm, B :SQLForm](name :String) :StoredProcedure2[A, B] = new BaseStoredProcedure(name)

	def of3[A :SQLForm, B :SQLForm, C :SQLForm](name :String) :StoredProcedure3[A, B, C] =
		new BaseStoredProcedure(name)

	def of4[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm](name :String) :StoredProcedure4[A, B, C, D] =
		new BaseStoredProcedure(name)

	def of5[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm](name :String) :StoredProcedure5[A, B, C, D, E] =
		new BaseStoredProcedure(name)

	def of6[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm](name :String)
			:StoredProcedure6[A, B, C, D, E, F] =
		new BaseStoredProcedure(name)

	def of7[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm, G :SQLForm](name :String)
			:StoredProcedure7[A, B, C, D, E, F, G] =
		new BaseStoredProcedure(name)

	def of8[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm, G :SQLForm, H :SQLForm]
	       (name :String)
			:StoredProcedure8[A, B, C, D, E, F, G, H] =
		new BaseStoredProcedure(name)

	def of9[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm, G :SQLForm, H :SQLForm, I :SQLForm]
	       (name :String)
			:StoredProcedure9[A, B, C, D, E, F, G, H, I] =
		new BaseStoredProcedure(name)

	def of10[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm, G :SQLForm, H :SQLForm, I :SQLForm,
	         J :SQLForm]
	        (name :String)
			:StoredProcedure10[A, B, C, D, E, F, G, H, I, J] =
		new BaseStoredProcedure(name)



	/** Extension methods available for all stored procedures. These are all `apply` methods for creating
	  * [[net.noresttherein.oldsql.sql.Call Call]] expressions and similar, as `apply` methods with signatures
	  * accepting individual arguments individually instead of as a single chain must be extension methods,
	  * and declaring any methods with the same name in the class directly would cause overloading clashes.
	  */
	implicit class StoredProcedureExtension[Args <: Chain](private val procedure :StoredProcedure[Args]) extends AnyVal {

		/** Initiates the preparation of a parameterized stored procedure call ending with the creation of
		  * a [[net.noresttherein.oldsql.sql.Call.CallProcedure CallProcedure]] statement with `P` as its first -
		  * or only - parameter. Subsequent, named an anonymous parameters can be introduced by chaining `apply` calls
		  * with only type parameter lists, each introducing a single parameter: `this[P][Q][R]`, `this[P]["Q", Q][R]`.
		  * At any point, the process can be finalized by passing a
		  * [[net.noresttherein.oldsql.sql.ast.ChainTuple tuple]] SQL expression with a value of `X`, either
		  * [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply(args:JoinedMappings[F]=>ChainTuple[F,GlobalScope,X])* apply(domain => args)]]
		  * or [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply(args:ChainTuple[F,GlobalScope,X]) apply(args)]].
		  * The tuple expression must be based on a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting only
		  * of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins - in the case of the last examples,
		  * `FromSome WithParam P WithParam Q WithParam R` and `FromSome WithParam P WithParam Q As "Q" WithParam R`.
		  * The function creating the tuple accepts an argument of
		  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]], providing access to call parameters,
		  * and is analogous to the one passed to [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]].
		  *
		  * @return an object implicitly convertible to
		  *         [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams CallProcedureParams]]`[FromSome WithParam P, @~ ~ P, X]`
		  *         when one of the non-argument `apply` is invoked: `procedure[P1][P2]` or `procedure[P1]["P2", P2]`.
		  *         This step of indirection exists purely for technical reasons due to the need of an implicit parameter
		  *         of [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[P]`.
		  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
		  *           used by the future SQL expression with arguments for the procedure.
		  */
		def apply[P] :CallProcedureParamDecl[FromSome, @~, P, Args] =
			new CallProcedureParamDecl[FromSome, @~, P, Args](From.template, procedure)

		/** Initiates the preparation of a parameterized stored procedure call ending with the creation of
		  * a [[net.noresttherein.oldsql.sql.Call Call]] statement with `P` as its first - or only -
		  * parameter. The string literal passed as the first type argument is used as the name of the parameter, becoming
		  * an alias in the ''from'' clause `FromSome WithParam P As N` for the domain over which the expression
		  * with procedure's arguments are created. Subsequent, named an anonymous parameters can be introduced by chaining
		  * `apply` calls with only type parameter lists, each introducing a single parameter: `this[N, P][Q][R]`,
		  * `this[N, P]["Q", Q][R]`. At any point, the process can be finalized by passing a
		  * [[net.noresttherein.oldsql.sql.ast.ChainTuple tuple]] SQL expression with a value of `X`, either
		  * [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply(args:JoinedMappings[F]=>ChainTuple[F,GlobalScope,X])* apply(domain => args)]]
		  * or [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply(args:ChainTuple[F,GlobalScope,X]) apply(args)]].
		  * The tuple expression must be based on a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting only
		  * of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins - in the case of the last examples,
		  * `FromSome WithParam P As N WithParam Q WithParam R` and
		  * `FromSome WithParam P As N WithParam Q As "Q" WithParam R`. The function creating the tuple accepts
		  * an argument of [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]], providing access
		  * to call parameters, and is analogous to the one passed to
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]].
		  *
		  * @return an object implicitly convertible to
		  *         [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams CallProcedureParams]]`[FromSome WithParam P As N, @~ ~ P, X]`
		  *         when one of the non-argument `apply` is invoked: `procedure[P1][P2]` or `procedure[P1]["P2", P2]`.
		  *         This step of indirection exists purely for technical reasons due to the need of implicit parameters
		  *         of [[scala.ValueOf ValueOf]]`[N]` and [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[P]`.
		  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
		  *           used by the future SQL expression with arguments for the procedure.
		  * @tparam N a `String` literal type with the name of the parameter, which can be used to access it
		  *           from [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] for the whole domain
		  *           when creating the SQL expression with the arguments for the procedure.
		  */
		def apply[N <: Label, P] :CallProcedureNamedParamDecl[FromSome, @~, N, P, Args] =
			new CallProcedureNamedParamDecl[FromSome, @~, N, P, Args](From.template, procedure)
	}

	type StoredProcedure0 = StoredProcedure[@~]
	type StoredProcedure1[A] = StoredProcedure[@~ ~A]
	type StoredProcedure2[A, B] = StoredProcedure[@~ ~A~B]
	type StoredProcedure3[A, B, C] = StoredProcedure[@~ ~A~B~C]
	type StoredProcedure4[A, B, C, D] = StoredProcedure[@~ ~A~B~C~D]
	type StoredProcedure5[A, B, C, D, E] = StoredProcedure[@~ ~A~B~C~D~E]
	type StoredProcedure6[A, B, C, D, E, F] = StoredProcedure[@~ ~A~B~C~D~E~F]
	type StoredProcedure7[A, B, C, D, E, F, G] = StoredProcedure[@~ ~A~B~C~D~E~F~G]
	type StoredProcedure8[A, B, C, D, E, F, G, H] = StoredProcedure[@~ ~A~B~C~D~E~F~G~H]
	type StoredProcedure9[A, B, C, D, E, F, G, H, I] = StoredProcedure[@~ ~A~B~C~D~E~F~G~H~I]
	type StoredProcedure10[A, B, C, D, E, F, G, H, I, J] = StoredProcedure[@~ ~A~B~C~D~E~F~G~H~I~J]

	

	/** A wrapper over arbitrary type/value used to mark ''OUT'' parameters among formal parameters of
	  * stored [[net.noresttherein.oldsql.sql.StoredProcedure procedures]]. From the point of view of generated
	  * SQL/DML it is completely equivalent to the wrapped type and
	  * any SQL [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, S, X]` can be lift to
	  * `SQLExpression[F, S, Out[X]]`; similarly, there are implicit conversions in both directions.
	  * When used as an element of a type `X &lt;: `[[net.noresttherein.oldsql.collection.Chain Chain]]
	  * in an `StoredProcedure[X]`, it marks the procedure's parameter (or parameters) set by its corresponding form
	  * as ''OUT'' parameters. After execution, values of all such parameters are read by the same forms and combined
	  * in another `Chain` type and returned by the statement. Any ''OUT'' parameter can always be treated as
	  * ''IN''-only by providing the argument as the wrapped value `T` instead of `OUT[T]`, in which case it will
	  * not be included in the return type of the statement.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature]]
	  * @see [[net.noresttherein.oldsql.sql.StoredProcedure]]
	  */
	@SerialVersionUID(ver) //todo: make this an opaque type in Scala 3
	case class Out[T](param :T) extends Serializable

	object Out {
		@inline implicit def out[T](param :Out[T]) :T = param.param
		@inline implicit def in[T](value :T) :Out[T] = Out(value)

		implicit def form[T](implicit form :SQLForm[T]) :SQLForm[Out[T]] = new OutForm[T](form)
		implicit def columnForm[T :ColumnForm] :ColumnForm[Out[T]] = new OutColumnForm[T]

		private[sql] case class OutForm[T](override val form :SQLForm[T])
			extends AbstractMappedForm[T, Out[T]]()(form)
		{
			protected override def map(s :T) :Out[T] = new Out(s)
			protected override def unmap(t :Out[T]) :T = t.param

			override def toString :String = "Out[" + form + "]"
		}

		private[sql] class OutColumnForm[T](implicit override val form :ColumnForm[T])
			extends OutForm[T](form) with ColumnForm[Out[T]] with SingletonColumnWriteForm[Out[T]]
		{
			override def sqlType = form.sqlType
		}
	}
	
	
	
	@SerialVersionUID(ver)
	protected[sql] class BaseStoredProcedure[X <: Chain](override val name :String)
	                                                    (implicit override val paramForm :ChainForm[X])
		extends StoredProcedure[X]

}






/** A signature of an SQL function for use in SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]].
  * This is the most generic variant, which accepts arguments as a polymorphic
  * [[net.noresttherein.oldsql.sql.ast.ChainTuple tuple]]; most implementations will be instances
  * of the single column variant, [[net.noresttherein.oldsql.sql.ColumnFunction ColumnFunction]].
  * @tparam Params a [[net.noresttherein.oldsql.collection.Chain Chain]] listing the types of all arguments of this function.
  * @tparam Res    the return type of this function.
  * @see [[net.noresttherein.oldsql.sql.Call]]
  * @author Marcin Mościcki
  */
trait StoredFunction[Params <: Chain, Res] extends SQLExecutable[Params, Res] {

	/** Create an SQL expression invoking this function, which can be used as a part of larger SQL expressions
	  * in an SQL query or a DML statement. If the result is a ground expression - based on `RowProduct` itself,
	  * without any dependencies - the result will be convertible to
	  * a [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]] statement.
	  * The types of the arguments need not match exactly the formal parameter types listed by `X` type parameter
	  * of this function: any occurrence of [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]`
	  * can be replaced with a value of `X` directly.
	  * There is an `apply` extension method with the same signature forwarding to this method, allowing
	  * to to provide the arguments in the function application notation.
	  */
	def sql[F <: RowProduct, S >: Grouped <: Single, Xs <: Chain]
	       (args :ChainTuple[F, S, Xs])(implicit signature :ProcedureSignature[Params, Xs]) :FunctionSQL[F, S, Params, Res] =
		FunctionSQL(this, signature(args, paramForm))

	override val paramForm :ChainForm[Params]

	/** The form used to read the value returned by the function as an ''OUT'' parameter of the executed
	  * [[java.sql.PreparedStatement PreparedStatement]].
	  */
	val readForm :SQLReadForm[Res]

	override def call :DirectCallFunction[Params, Res] = Call(this)
	//dialect here doesn't have a default value, which creates an inconsistency. We'd have to switch to Maybe
//	override def chant[F <: FromSome, Ps <: Chain](args :ChainTuple[F, GlobalScope, X])
//	                                              (implicit domain :JoinedParams[F, Ps], dialect :Maybe[SQLDialect])
//			:Incantation[Ps, Y] =
//		Call.ParamCallFunction(domain())(this, args).chant(dialect.opt getOrElse StandardSQL)

//	/** Creates an executable SQL command which invokes this procedure with arbitrary SQL expressions `args`
//	  * as the arguments. The [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters of the domain `F`
//	  * of the tuple expression become the parameters of the command. This is a shorter version of
//	  * to `this.`[[net.noresttherein.oldsql.sql.StoredProcedure.StoredProcedureExtension.call]][[net.noresttherein.oldsql.sql.Call.DirectCallProcedure.apply[F<:RowProduct,Xs<:Chain]* (args)]]`.`[[net.noresttherein.oldsql.sql.Call.chant chant]],
//	  * except the ''OUT'' parameters are not returned.
//	  */
//	def chant[F <: FromClause, Xs <: Chain]
//	         (args :ChainTuple[F, GlobalScope, Xs])
//	         (implicit signature :ProcedureSignature[Params, Xs] { type Domain <: F }, dialect :Maybe[SQLDialect])
//			:Incantation[signature.CallParams, Any] =
//		signature(this, args).returning[Unit](StatementResult.NoResult).chant(dialect getOrElse StandardSQL)

	/** Creates an executable SQL command which invokes this procedure with arbitrary SQL expressions `args`
	  * as the arguments. The [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters of the domain `F`
	  * of the tuple expression become the parameters of the command. This is a shorter version of
	  * to `this.`[[net.noresttherein.oldsql.sql.StoredProcedure.call]][[net.noresttherein.oldsql.sql.Call.DirectCallProcedure.apply[F<:RowProduct,Xs<:Chain]* (args)]]`.`[[net.noresttherein.oldsql.sql.Call.chant chant]],
	  * except the ''OUT'' parameters are not returned.
	  */
	def chant[F <: FromClause, Xs <: Chain]
	         (args :ChainTuple[F, Single, Xs])
	         (implicit signature :ProcedureSignature[Params, Xs] { type Domain <: F }, dialect :Maybe[SQLDialect])
			:Incantation[signature.CallParams, signature.FunctionResult[Res]] =
		signature(this, args).chant(dialect getOrElse StandardSQL)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[StoredFunction[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :StoredFunction[_, _] if other canEqual that => name == other.name && readForm == other.readForm
		case _ => false
	}
	override def hashCode :Int = name.hashCode * 31 + readForm.hashCode

	override def toString :String = name + "[" + readForm + "]"
}




object StoredFunction {
	def apply[X <: Chain :ChainForm, Y :SQLReadForm](name :String) :StoredFunction[X, Y] = new BaseFunction(name)

	implicit class StoredFunctionExtension[X <: Chain, Y]
	                                      (private val function :StoredFunction[X, Y]) extends AnyVal
	{
		/** Initiates the preparation of a parameterized stored procedure call ending with the creation of
		  * a [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]] statement with `P` as its first - or only -
		  * parameter. Subsequent, named or anonymous parameters can be introduced by chaining `apply` calls with only
		  * type parameter lists, each introducing a single parameter: `this[P][Q][R]`, `this[P]["Q", Q][R]`.
		  * At any point, the process can be finalized by passing a
		  * [[net.noresttherein.oldsql.sql.ast.ChainTuple tuple]] SQL expression with a value of `X`,
		  * either [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:JoinedMappings[F]=>ChainTuple[F,GlobalScope,X])* apply(domain => args)]]
		  * or [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:ChainTuple[F,GlobalScope,X]) apply(args)]].
		  * The tuple expression must be based on a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting only
		  * of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins - in the case of the last examples,
		  * `FromSome WithParam P WithParam Q WithParam R` and `FromSome WithParam P WithParam Q As "Q" WithParam R`.
		  * The function creating the tuple accepts an argument of
		  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]], providing access to call parameters,
		  * and is analogous to the one passed to [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]].
		  *
		  * @return an object implicitly convertible
		  *         to [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams CallProcedureParams]]`[FromSome WithParam P, @~ ~ P, X]`
		  *         when one of the non-argument `apply` is invoked: `procedure[P1][P2]` or `procedure[P1]["P2", P2]`.
		  *         This step of indirection exists purely for technical reasons due to the need of an implicit parameter
		  *         of [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[P]`.
		  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
		  *           used by the future SQL expression with arguments for the procedure.
		  */
		def apply[P] :CallFunctionParamDecl[ParamsRow[@~], @~, P, X, Y] =
			CallFunctionParamDecl(function)

		/** Initiates the preparation of a parameterized stored procedure call ending with the creation of
		  * a [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]] statement with `P` as its first - or only -
		  * parameter. The string literal passed as the first type argument is used as the name of the parameter,
		  * becoming an alias in the ''from'' clause `FromSome WithParam P As N` for the domain over which
		  * the expression with procedure's arguments are created. Subsequent, named an anonymous parameters can
		  * be introduced by chaining `apply` calls with only type parameter lists, each introducing a single parameter:
		  * `this[N, P][Q][R]`, `this[N, P]["Q", Q][R]`. At any point, the process can be finalized by passing
		  * a [[net.noresttherein.oldsql.sql.ast.ChainTuple tuple]] SQL expression with a value of `X`, either
		  * [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:JoinedMappings[F]=>ChainTuple[F,GlobalScope,X])* apply(domain => args)]]
		  * or [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:ChainTuple[F,GlobalScope,X]) apply(args)]].
		  * The tuple expression must be based on a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting only
		  * of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins - in the case of the last examples,
		  * `FromSome WithParam P As N WithParam Q WithParam R` and
		  * `FromSome WithParam P As N WithParam Q As "Q" WithParam R`. The function creating the tuple accepts
		  * an argument of [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]], providing access
		  * to call parameters, and is analogous to the one passed to
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]].
		  *
		  * @return an object implicitly convertible to
		  *         [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams CallFunctionParams]]`[FromSome WithParam P As N, @~ ~ P, X]`
		  *         when one of the non-argument `apply` is invoked: `procedure[P1][P2]` or `procedure[P1]["P2", P2]`.
		  *         This step of indirection exists purely for technical reasons due to the need of implicit parameters
		  *         of [[scala.ValueOf ValueOf]]`[N]` and [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[P]`.
		  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
		  *           used by the future SQL expression with arguments for the procedure.
		  * @tparam N a `String` literal type with the name of the parameter, which can be used to access it
		  *           from [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] for the whole domain
		  *           when creating the SQL expression with the arguments for the procedure.
		  */
		def apply[N <: Label, P] :CallFunctionNamedParamDecl[ParamsRow[@~], @~, N, P, X, Y] =
			CallFunctionNamedParamDecl(function)

		/** Create an SQL expression invoking this function, which can be used as part of larger SQL expressions
		  * in an SQL query or a DML statement. If the result is a ground expression - based on `RowProduct` itself,
		  * without any dependencies - the result will be convertible to
		  * a [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]] statement.
		  * The types of the arguments need not match exactly the formal parameter types listed by `X` type parameter
		  * of this function: any occurrence of [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]`
		  * can be replaced with a value of `X` directly.
		  * This method forwards to [[net.noresttherein.oldsql.sql.StoredFunction.sql sql]] method
		  * of [[net.noresttherein.oldsql.sql.StoredFunction StoredFunction]] with the same signature.
		  */
		def apply[F <: RowProduct, S >: Grouped <: Single, Xs <: Chain]
                 (args :ChainTuple[F, S, Xs])(implicit signature :ProcedureSignature[X, Xs])
				:FunctionSQL[F, S, X, Y] =
			function.sql(args)

	}

	type StoredFunction0[Y] = StoredFunction[@~, Y]
	type StoredFunction1[A, Y] = StoredFunction[@~ ~A, Y]
	type StoredFunction2[A, B, Y] = StoredFunction[@~ ~A~B, Y]
	type StoredFunction3[A, B, C, Y] = StoredFunction[@~ ~A~B~C, Y]
	type StoredFunction4[A, B, C, D, Y] = StoredFunction[@~ ~A~B~C~D, Y]
	type StoredFunction5[A, B, C, D, E, Y] = StoredFunction[@~ ~A~B~C~D~E, Y]
	type StoredFunction6[A, B, C, D, E, F, Y] = StoredFunction[@~ ~A~B~C~D~E~F, Y]
	type StoredFunction7[A, B, C, D, E, F, G, Y] = StoredFunction[@~ ~A~B~C~D~E~F~G, Y]
	type StoredFunction8[A, B, C, D, E, F, G, H, Y] = StoredFunction[@~ ~A~B~C~D~E~F~G~H, Y]
	type StoredFunction9[A, B, C, D, E, F, G, H, I, Y] = StoredFunction[@~ ~A~B~C~D~E~F~G~H~I, Y]
	type StoredFunction10[A, B, C, D, E, F, G, H, I, J, Y] = StoredFunction[@~ ~A~B~C~D~E~F~G~H~I~J, Y]

	
	
	@SerialVersionUID(ver)
	protected[sql] class BaseFunction[X <: Chain, Y](override val name :String)
	                                                (implicit override val paramForm :ChainForm[X], 
	                                                          override val readForm :SQLReadForm[Y])
		extends StoredFunction[X, Y]

}






/** A signature of a single column SQL function for use in SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]].
  * This is generic base class, which accepts arguments as a polymorphic
  * [[net.noresttherein.oldsql.sql.ast.ChainTuple tuple]]; in most cases it is more convenient to use one of
  * its subclasses dedicated to functions of a fixed arity. They can be created using `ofX` methods from
  * the [[net.noresttherein.oldsql.sql.ColumnFunction$ companion]] object to this trait:
  * {{{
  *    val SYSDATE = ColumnFunction.of0[LocalDateTime]("SYSDATE")
  *    val now = SYSDATE()
  *    val TO_DATE = ColumnFunction.of2[String, String, LocalDate]("TO_DATE")
  *    val AprilFool = TO_DATE("01.10.2020".?, "DD.MM.YYYY")
  * }}}
  * @tparam X a [[net.noresttherein.oldsql.collection.Chain Chain]] listing the types of all arguments of this function.
  * @tparam Y the return type of this function.
  * @author Marcin Mościcki
  */
trait ColumnFunction[X <: Chain, Y] extends StoredFunction[X, Y] {
	override val readForm :ColumnReadForm[Y]

	override def sql[F <: RowProduct, S >: Grouped <: Single, Xs <: Chain]
                    (args :ChainTuple[F, S, Xs])(implicit signature :ProcedureSignature[X, Xs])
			:FunctionColumnSQL[F, S, X, Y] =
		FunctionColumnSQL(this, signature(args, paramForm))
}



object ColumnFunction {

	def apply[X <: Chain :ChainForm, Y :ColumnReadForm](name :String) :ColumnFunction[X, Y] =
		new BaseColumnFunction(name)

	
	def of0[Y :ColumnReadForm](name :String) :ColumnFunction0[Y] = new BaseColumnFunction(name)

	def of1[A :SQLForm, Y :ColumnReadForm](name :String) :ColumnFunction1[A, Y] = new BaseColumnFunction(name)

	def of2[A :SQLForm, B :SQLForm, Y :ColumnReadForm](name :String) :ColumnFunction2[A, B, Y] =
		new BaseColumnFunction(name)

	def of3[A :SQLForm, B :SQLForm, C :SQLForm, Y :ColumnReadForm](name :String) :ColumnFunction3[A, B, C, Y] =
		new BaseColumnFunction(name)

	def of4[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, Y :ColumnReadForm](name :String) 
			:ColumnFunction4[A, B, C, D, Y] =
		new BaseColumnFunction(name)

	def of5[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, Y :ColumnReadForm](name :String) 
			:ColumnFunction5[A, B, C, D, E, Y] =
		new BaseColumnFunction(name)

	def of6[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm, Y :ColumnReadForm](name :String)
			:ColumnFunction6[A, B, C, D, E, F, Y] =
		new BaseColumnFunction(name)

	def of7[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm, G :SQLForm, Y :ColumnReadForm]
	       (name :String)
			:ColumnFunction7[A, B, C, D, E, F, G, Y] =
		new BaseColumnFunction(name)

	def of8[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm, G :SQLForm, H :SQLForm, 
	        Y :ColumnReadForm]
	       (name :String)
			:ColumnFunction8[A, B, C, D, E, F, G, H, Y] =
		new BaseColumnFunction(name)

	def of9[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm, G :SQLForm, H :SQLForm, I :SQLForm, 
	        Y :ColumnReadForm]
	       (name :String)
			:ColumnFunction9[A, B, C, D, E, F, G, H, I, Y] =
		new BaseColumnFunction(name)

	def of10[A :SQLForm, B :SQLForm, C :SQLForm, D :SQLForm, E :SQLForm, F :SQLForm, G :SQLForm, H :SQLForm, I :SQLForm,
	         J :SQLForm, Y :ColumnReadForm]
	        (name :String)
			:ColumnFunction10[A, B, C, D, E, F, G, H, I, J, Y] =
		new BaseColumnFunction(name)


	type ColumnFunction0[Y] = ColumnFunction[@~, Y]
	type ColumnFunction1[A, Y] = ColumnFunction[@~ ~A, Y]
	type ColumnFunction2[A, B, Y] = ColumnFunction[@~ ~A~B, Y]
	type ColumnFunction3[A, B, C, Y] = ColumnFunction[@~ ~A~B~C, Y]
	type ColumnFunction4[A, B, C, D, Y] = ColumnFunction[@~ ~A~B~C~D, Y]
	type ColumnFunction5[A, B, C, D, E, Y] = ColumnFunction[@~ ~A~B~C~D~E, Y]
	type ColumnFunction6[A, B, C, D, E, F, Y] = ColumnFunction[@~ ~A~B~C~D~E~F, Y]
	type ColumnFunction7[A, B, C, D, E, F, G, Y] = ColumnFunction[@~ ~A~B~C~D~E~F~G, Y]
	type ColumnFunction8[A, B, C, D, E, F, G, H, Y] = ColumnFunction[@~ ~A~B~C~D~E~F~G~H, Y]
	type ColumnFunction9[A, B, C, D, E, F, G, H, I, Y] = ColumnFunction[@~ ~A~B~C~D~E~F~G~H~I, Y]
	type ColumnFunction10[A, B, C, D, E, F, G, H, I, J, Y] = ColumnFunction[@~ ~A~B~C~D~E~F~G~H~I~J, Y]


	implicit class ColumnFunctionExtension[X <: Chain, Y](protected val function :ColumnFunction[X, Y]) {
		//repeated from StoredFunctionExtension because of overload issues
		/** Initiates the preparation of a parameterized stored procedure call ending with the creation of
		  * a [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]] statement with `P` as its first - or only -
		  * parameter. Subsequent, named an anonymous parameters can be introduced by chaining `apply` calls with only
		  * type parameter lists, each introducing a single parameter: `this[P][Q][R]`, `this[P]["Q", Q][R]`.
		  * At any point, the process can be finalized by passing a
		  * [[net.noresttherein.oldsql.sql.ast.ChainTuple tuple]] SQL expression with a value of `X`,
		  * either [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:JoinedMappings[F]=>ChainTuple[F,GlobalScope,X])* apply(domain => args)]]
		  * or [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:ChainTuple[F,GlobalScope,X]) apply(args)]].
		  * The tuple expression must be based on a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting only
		  * of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins - in the case of the last examples,
		  * `FromSome WithParam P WithParam Q WithParam R` and `FromSome WithParam P WithParam Q As "Q" WithParam R`.
		  * The function will creating the tuple accepts an argument of
		  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]], providing access to call parameters,
		  * and is analogous to the one passed to [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]].
		  *
		  * @return an object implicitly convertible
		  *         to [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams CallProcedureParams]]`[FromSome WithParam P, @~ ~ P, X]`
		  *         when one of the non-argument `apply` is invoked: `procedure[P1][P2]` or `procedure[P1]["P2", P2]`.
		  *         This step of indirection exists purely for technical reasons due to the need of an implicit parameter
		  *         of [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[P]`.
		  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
		  *           used by the future SQL expression with arguments for the procedure.
		  */
		def apply[P] :CallFunctionParamDecl[ParamsRow[@~], @~, P, X, Y] =
			CallFunctionParamDecl(function)

		/** Initiates the preparation of a parameterized stored procedure call ending with the creation of
		  * a [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]] statement with `P` as its first - or only -
		  * parameter. The string literal passed as the first type argument is used as the name of the parameter,
		  * becoming an alias in the ''from'' clause `FromSome WithParam P As N` for the domain over which
		  * the expression with procedure's arguments are created. Subsequent, named an anonymous parameters can
		  * be introduced by chaining `apply` calls with only type parameter lists, each introducing a single parameter:
		  * `this[N, P][Q][R]`, `this[N, P]["Q", Q][R]`. At any point, the process can be finalized by passing
		  * a [[net.noresttherein.oldsql.sql.ast.ChainTuple tuple]] SQL expression with a value of `X`, either
		  * [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:JoinedMappings[F]=>ChainTuple[F,GlobalScope,X])* apply(domain => args)]]
		  * or [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:ChainTuple[F,GlobalScope,X]) apply(args)]].
		  * The tuple expression must be based on a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting only
		  * of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins - in the case of the last examples,
		  * `FromSome WithParam P As N WithParam Q WithParam R` and
		  * `FromSome WithParam P As N WithParam Q As "Q" WithParam R`. The function creating the tuple accepts
		  * an argument of [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]], providing access
		  * to call parameters, and is analogous to the one passed to
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]].
		  *
		  * @return an object implicitly convertible to
		  *         [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams CallFunctionParams]]`[FromSome WithParam P As N, @~ ~ P, X]`
		  *         when one of the non-argument `apply` is invoked: `procedure[P1][P2]` or `procedure[P1]["P2", P2]`.
		  *         This step of indirection exists purely for technical reasons due to the need of implicit parameters
		  *         of [[scala.ValueOf ValueOf]]`[N]` and [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[P]`.
		  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
		  *           used by the future SQL expression with arguments for the procedure.
		  * @tparam N a `String` literal type with the name of the parameter, which can be used to access it
		  *           from [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] for the whole domain
		  *           when creating the SQL expression with the arguments for the procedure.
		  */
		def apply[N <: Label, P] :CallFunctionNamedParamDecl[ParamsRow[@~], @~, N, P, X, Y] =
			CallFunctionNamedParamDecl(function)

		/** Create an SQL expression invoking this function, which can be used as part of larger SQL expressions
		  * in an SQL query or a DML statement. If the result is a ground expression - based on `RowProduct` itself,
		  * without any dependencies - the result will be convertible to
		  * a [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]] statement.
		  * The types of the arguments need not match exactly the formal parameter types listed by `X` type parameter
		  * of this function: any occurrence of [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]`
		  * can be replaced with a value of `X` directly.
		  * This method forwards to [[net.noresttherein.oldsql.sql.StoredFunction.sql sql]] method
		  * of [[net.noresttherein.oldsql.sql.StoredFunction StoredFunction]] with the same signature.
		  */
		def apply[F <: RowProduct, S >: Grouped <: Single, Xs <: Chain]
		         (args :ChainTuple[F, S, Xs])(implicit signature :ProcedureSignature[X, Xs])
				:FunctionColumnSQL[F, S, X, Y] =
			function.sql(args)

		@inline implicit final private[sql] def lastForm[I <: Chain, L](implicit form :ChainForm[I ~ L]) :SQLForm[L] =
			form.last
	}

	implicit class ColumnFunction0Extension[Y](self :ColumnFunction0[Y]) extends ColumnFunctionExtension(self) {
		def apply() :FunctionColumnSQL[_, _, _, Y] = function(ChainTuple())
	}

	implicit class ColumnFunction1Extension[A, Y](self :ColumnFunction1[A, Y]) extends ColumnFunctionExtension(self) {
		def apply(a :A) :FunctionColumnSQL[RowProduct, Single, @~ ~A, Y] =
			function(BoundParam[A, A](a)(function.paramForm.last))

		def apply[X <: RowProduct, Sc >: Grouped <: Single](a :SQLExpression[X, Sc, A])
				:FunctionColumnSQL[X, Sc, @~ ~A, Y] =
			function(ChainTuple(a))
	}

	implicit class ColumnFunction2Extension[A, B, Y](self :ColumnFunction2[A, B, Y])
		extends ColumnFunctionExtension(self)
	{
		def apply(a :A, b :B) :FunctionColumnSQL[RowProduct, Single, @~ ~A~B, Y] = {
			implicit val bf = function.paramForm
			implicit val af = bf.init
			function(a.?, b.?)
		}

		def apply[X <: RowProduct, Sc >: Grouped <: Single]
		         (a :SQLExpression[X, Sc, A], b :SQLExpression[X, Sc, B])
				:FunctionColumnSQL[X, Sc, @~ ~A~B, Y] =
			function(a.chain ~ b)
	}

	implicit class ColumnFunction3Extension[A, B, C, Y](self :ColumnFunction3[A, B, C, Y]) 
		extends ColumnFunctionExtension(self)
	{
		def apply(a :A, b :B, c :C) :FunctionColumnSQL[RowProduct, Single, @~ ~A~B~C, Y] = {
			implicit val cf = function.paramForm
			implicit val bf = cf.init
			implicit val af = bf.init
			function(a.?, b.?, c.?)
		}

		def apply[X <: RowProduct, Sc >: Grouped <: Single]
		         (a :SQLExpression[X, Sc, A], b :SQLExpression[X, Sc, B], c :SQLExpression[X, Sc, C])
				:FunctionColumnSQL[X, Sc, @~ ~A~B~C, Y] =
			function(a.chain~b~c)
	}

	implicit class ColumnFunction4Extension[A, B, C, D, Y](self :ColumnFunction4[A, B, C, D, Y])
		extends ColumnFunctionExtension(self)
	{
		def apply(a :A, b :B, c :C, d :D) :FunctionColumnSQL[RowProduct, Single, @~ ~A~B~C~D, Y] = {
			implicit val df = function.paramForm
			implicit val cf = df.init
			implicit val bf = cf.init
			implicit val af = bf.init
			function(a.?, b.?, c.?, d.?)
		}

		def apply[X <: RowProduct, Sc >: Grouped <: Single]
		         (a :SQLExpression[X, Sc, A], b :SQLExpression[X, Sc, B], c :SQLExpression[X, Sc, C], d :SQLExpression[X, Sc, D])
				:FunctionColumnSQL[X, Sc, @~ ~A~B~C~D, Y] =
			function(a.chain~b~c~d)
	}

	implicit class ColumnFunction5Extension[A, B, C, D, E, Y](self :ColumnFunction5[A, B, C, D, E, Y])
		extends ColumnFunctionExtension(self)
	{
		def apply(a :A, b :B, c :C, d :D, e :E) :FunctionColumnSQL[RowProduct, Single, @~ ~A~B~C~D~E, Y] = {
			implicit val ef = function.paramForm
			implicit val df = ef.init
			implicit val cf = df.init
			implicit val bf = cf.init
			implicit val af = bf.init
			function(a.?, b.?, c.?, d.?, e.?)
		}

		def apply[X <: RowProduct, Sc >: Grouped <: Single]
		         (a :SQLExpression[X, Sc, A], b :SQLExpression[X, Sc, B], c :SQLExpression[X, Sc, C], d :SQLExpression[X, Sc, D],
		          e :SQLExpression[X, Sc, E])
				:FunctionColumnSQL[X, Sc, @~ ~A~B~C~D~E, Y] =
			function(a.chain~b~c~d~e)
	}

	implicit class ColumnFunction6Extension[A, B, C, D, E, F, Y](self :ColumnFunction6[A, B, C, D, E, F, Y])
		extends ColumnFunctionExtension(self)
	{
		def apply(a :A, b :B, c :C, d :D, e :E, f :F)
				:FunctionColumnSQL[RowProduct, Single, @~ ~A~B~C~D~E~F, Y] =
		{
			implicit val ff = function.paramForm
			implicit val ef = ff.init
			implicit val df = ef.init
			implicit val cf = df.init
			implicit val bf = cf.init
			implicit val af = bf.init
			function(a.?, b.?, c.?, d.?, e.?, f.?)
		}

		def apply[X <: RowProduct, Sc >: Grouped <: Single]
		         (a :SQLExpression[X, Sc, A], b :SQLExpression[X, Sc, B], c :SQLExpression[X, Sc, C], d :SQLExpression[X, Sc, D],
		          e :SQLExpression[X, Sc, E], f :SQLExpression[X, Sc, F])
				:FunctionColumnSQL[X, Sc, @~ ~A~B~C~D~E~F, Y] =
			function(a.chain~b~c~d~e~f)
	}

	implicit class ColumnFunction7Extension[A, B, C, D, E, F, G, Y]
	                                       (self :ColumnFunction7[A, B, C, D, E, F, G, Y])
		extends ColumnFunctionExtension(self)
	{
		def apply(a :A, b :B, c :C, d :D, e :E, f :F, g :G)
				:FunctionColumnSQL[RowProduct, Single, @~ ~A~B~C~D~E~F~G, Y] =
		{
			implicit val gf = function.paramForm
			implicit val ff = gf.init
			implicit val ef = ff.init
			implicit val df = ef.init
			implicit val cf = df.init
			implicit val bf = cf.init
			implicit val af = bf.init
			function(a.?, b.?, c.?, d.?, e.?, f.?, g.?)
		}

		def apply[X <: RowProduct, Sc >: Grouped <: Single]
		         (a :SQLExpression[X, Sc, A], b :SQLExpression[X, Sc, B], c :SQLExpression[X, Sc, C], d :SQLExpression[X, Sc, D],
		          e :SQLExpression[X, Sc, E], f :SQLExpression[X, Sc, F], g :SQLExpression[X, Sc, G])
				:FunctionColumnSQL[X, Sc, @~ ~A~B~C~D~E~F~G, Y] =
			function(a.chain~b~c~d~e~f~g)
	}

	implicit class ColumnFunction8Extension[A, B, C, D, E, F, G, H, Y]
	                                       (self :ColumnFunction8[A, B, C, D, E, F, G, H, Y])
		extends ColumnFunctionExtension(self)
	{
		def apply(a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H)
				:FunctionColumnSQL[RowProduct, Single, @~ ~A~B~C~D~E~F~G~H, Y] =
		{
			implicit val hf = function.paramForm
			implicit val gf = hf.init
			implicit val ff = gf.init
			implicit val ef = ff.init
			implicit val df = ef.init
			implicit val cf = df.init
			implicit val bf = cf.init
			implicit val af = bf.init
			function(a.?, b.?, c.?, d.?, e.?, f.?, g.?, h.?)
		}

		def apply[X <: RowProduct, Sc >: Grouped <: Single]
		         (a :SQLExpression[X, Sc, A], b :SQLExpression[X, Sc, B], c :SQLExpression[X, Sc, C], d :SQLExpression[X, Sc, D],
		          e :SQLExpression[X, Sc, E], f :SQLExpression[X, Sc, F], g :SQLExpression[X, Sc, G], h :SQLExpression[X, Sc, H])
				:FunctionColumnSQL[X, Sc, @~ ~A~B~C~D~E~F~G~H, Y] =
			function(a.chain~b~c~d~e~f~g~h)
	}

	implicit class ColumnFunction9Extension[A, B, C, D, E, F, G, H, I, Y]
	                                       (self :ColumnFunction9[A, B, C, D, E, F, G, H, I, Y])
		extends ColumnFunctionExtension(self)
	{
		def apply(a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I)
				:FunctionColumnSQL[RowProduct, Single, @~ ~A~B~C~D~E~F~G~H~I, Y] =
		{
			implicit val ifm = function.paramForm
			implicit val hf = ifm.init
			implicit val gf = hf.init
			implicit val ff = gf.init
			implicit val ef = ff.init
			implicit val df = ef.init
			implicit val cf = df.init
			implicit val bf = cf.init
			implicit val af = bf.init
			function(a.?, b.?, c.?, d.?, e.?, f.?, g.?, h.?, i.?)
		}

		def apply[X <: RowProduct, Sc >: Grouped <: Single]
		         (a :SQLExpression[X, Sc, A], b :SQLExpression[X, Sc, B], c :SQLExpression[X, Sc, C], d :SQLExpression[X, Sc, D],
		          e :SQLExpression[X, Sc, E], f :SQLExpression[X, Sc, F], g :SQLExpression[X, Sc, G], h :SQLExpression[X, Sc, H],
		          i :SQLExpression[X, Sc, I])
				:FunctionColumnSQL[X, Sc, @~ ~A~B~C~D~E~F~G~H~I, Y] =
			function(a.chain~b~c~d~e~f~g~h~i)
	}

	implicit class ColumnFunction10Extension[A, B, C, D, E, F, G, H, I, J, Y]
	                                        (self :ColumnFunction10[A, B, C, D, E, F, G, H, I, J, Y])
		extends ColumnFunctionExtension(self)
	{
		def apply(a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J)
				:FunctionColumnSQL[RowProduct, Single, @~ ~A~B~C~D~E~F~G~H~I~J, Y] =
		{
			implicit val jf = function.paramForm
			implicit val ifm = jf.init
			implicit val hf = ifm.init
			implicit val gf = hf.init
			implicit val ff = gf.init
			implicit val ef = ff.init
			implicit val df = ef.init
			implicit val cf = df.init
			implicit val bf = cf.init
			implicit val af = bf.init
			function(a.?, b.?, c.?, d.?, e.?, f.?, g.?, h.?, i.?, j.?)
		}

		def apply[X <: RowProduct, Sc >: Grouped <: Single]
		         (a :SQLExpression[X, Sc, A], b :SQLExpression[X, Sc, B], c :SQLExpression[X, Sc, C], d :SQLExpression[X, Sc, D],
		          e :SQLExpression[X, Sc, E], f :SQLExpression[X, Sc, F], g :SQLExpression[X, Sc, G], h :SQLExpression[X, Sc, H],
		          i :SQLExpression[X, Sc, I], j :SQLExpression[X, Sc, J])
				:FunctionColumnSQL[X, Sc, @~ ~A~B~C~D~E~F~G~H~I~J, Y] =
			function(a.chain~b~c~d~e~f~g~h~i~j)
	}



	@SerialVersionUID(ver)
	private class BaseColumnFunction[X <: Chain, Y](override val name :String)
	                                               (implicit override val paramForm :ChainForm[X],
	                                                implicit override val readForm :ColumnReadForm[Y])
		extends ColumnFunction[X, Y]

}





//todo:
trait CursorFunction[Args <: Chain, Y] extends StoredProcedure[Args] {

}






object GenericFunction {
	def of1(name :String) :GenericFunction1 = new GenericFunction1(name)
	def of2(name :String) :GenericFunction2 = new GenericFunction2(name)
	def of3(name :String) :GenericFunction3 = new GenericFunction3(name)

	@SerialVersionUID(ver)
	class GenericFunction1(val name :String) extends Serializable {
		def apply[F <: RowProduct, S >: Grouped <: Single, X :SQLForm](arg :ColumnSQL[F, S, X])
				:ColumnSQL[F, S, X] =
			ColumnFunction.of1[X, X](name)(SQLForm[X], arg.selectForm)(arg.chain)

		override def toString :String = name
	}

	@SerialVersionUID(ver)
	class GenericFunction2(val name :String) extends Serializable {
		def apply[F <: RowProduct, S >: Grouped <: Single, X :SQLForm]
		         (arg1 :ColumnSQL[F, S, X], arg2 :ColumnSQL[F, S, X]) :ColumnSQL[F, S, X] =
			ColumnFunction.of2[X, X, X](name)(SQLForm[X], SQLForm[X], arg1.selectForm)(arg1.chain ~ arg2)

		override def toString :String = name
	}

	@SerialVersionUID(ver)
	class GenericFunction3(val name :String) extends Serializable {
		def apply[F <: RowProduct, S >: Grouped <: Single, X :SQLForm]
		         (arg1 :ColumnSQL[F, S, X], arg2 :ColumnSQL[F, S, X], arg3 :ColumnSQL[F, S, X]) :ColumnSQL[F, S, X] =
			ColumnFunction.of3(name)(SQLForm[X], SQLForm[X], SQLForm[X], arg1.selectForm)(arg1.chain ~ arg2 ~ arg3)
	}

}
