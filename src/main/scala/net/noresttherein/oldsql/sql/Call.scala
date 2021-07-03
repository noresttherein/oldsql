package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{ColumnWriteForm, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.Call.CallFunction.{CallFunctionNamedParamDecl, CallFunctionParamDecl, CaseFunction, FunctionVisitor}
import net.noresttherein.oldsql.sql.Call.CallProcedure.{CallProcedureNamedParamDecl, CallProcedureParamDecl, CaseProcedure, GroundCallProcedure, ParamCallProcedure, ParamCallProcedureImpl, ProcedureVisitor}
import net.noresttherein.oldsql.sql.Call.InOutCallFunction.{CaseInOutFunction, InOutFunctionVisitor}
import net.noresttherein.oldsql.sql.Call.InOutCallProcedure.{CaseInOutProcedure, InOutProcedureVisitor}
import net.noresttherein.oldsql.sql.DML.{BoundDML, ComposedDML, DMLAPI, RepeatedDML}
import net.noresttherein.oldsql.sql.DMLStatement.{AlteredResultStatement, BoundStatement, ComposedStatement, DMLStatementAPI, StatementResult, StatementVisitor}
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult.NoResult
import net.noresttherein.oldsql.sql.RowProduct.{As, JoinedMappings, ParameterizedFrom, ParameterizedWith, ParamlessFrom, PureParamFrom}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.mechanics.{ProcedureSignature, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.ProcedureInOutSignature
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization






/** A DML statement which invokes, possibly a number of times with different parameters,
  * a [[net.noresttherein.oldsql.sql.StoredProcedure stored procedure]]
  * (or a [[net.noresttherein.oldsql.sql.StoredFunction function]]). As with the base trait
  * [[net.noresttherein.oldsql.sql.DML DML]], there are three main subclasses:
  *   1. [[net.noresttherein.oldsql.sql.Call Call]] - a [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]],
  *      parameterized or not, which executes a stored procedure/function once passing a chain of arguments given
  *      as an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]];
  *      2. [[net.noresttherein.oldsql.sql.DML.RepeatedDML RepeatedDML with CallDML]] - a JDBC batch statement,
  *      keeping a parameterized `Call` statement and a sequence of matching parameter sets;
  *      3. [[net.noresttherein.oldsql.sql.DMLStatement.BoundStatement BoundStatement with Call]] - a parameterized
  *      `Call[X, Y]` applied to a concrete value of `X`. This is different from the first case in that the value
  *      of the arguments for the call is stored as a plain Scala object and the expression arguments are based
  *      on a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] with
  *      [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters, while in a `Call[(), Y]` the values would be
  *      built in into the argument expressions themselves as
  *      [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters.
  */
trait CallDML[-Args, +Res] extends DML[Args, Res] with DMLAPI[Args, Res, CallDML] {
	/** Types of the formal parameters of the called procedure in a [[net.noresttherein.oldsql.collection.Chain Chain]].
	  * These are in principle unrelated to the parameters of this DML.
	  */
	type Params <: Chain

	/** The type returned by the function. This is the 'true' return type, not taking into account
	  * any ''OUT'' parameters. Procedures define it as `Any` for compatibility.
	  */
	type Result

	/** Invoked stored procedure (or function). */
	val procedure :SQLExecutable[Params, Result]

	override def compose[X](f :X => Args) :CallDML[X, Res] =
		new ComposedDML.Base[X, Args, Res, CallDML](this, f) with DerivedDML[X, Res]
			with ComposedDML[X, Args, Res] with ComposedDML.Impl[X, Args, Res, CallDML]

	override def bind(args :Args) :CallDML[Unit, Res] =
		new BoundDML.Base[Args, Res, CallDML](this, args) with DerivedDML[Any, Res]
			with BoundDML[Args, Res] with BoundDML.Impl[Args, Res, CallDML]

	protected[sql] trait DerivedDML[-X, +Y] extends CallDML[X, Y] {
		override type Params = CallDML.this.Params
		override type Result = CallDML.this.Result
		override val procedure = CallDML.this.procedure
	}
}






/** A statement for a single stored [[net.noresttherein.oldsql.sql.StoredProcedure procedure]]
  * (or [[net.noresttherein.oldsql.sql.StoredFunction function]]) call. It is the root of the hierarchy of statements
  * executing a single procedure/function once. All standard implementations currently extend either of its subtypes:
  *   1. [[net.noresttherein.oldsql.sql.Call.CallProcedure CallProcedure]],
  *   1. [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]],
  *   1. [[net.noresttherein.oldsql.sql.Call.InOutCallProcedure InOutCallProcedure]] and
  *   1. [[net.noresttherein.oldsql.sql.Call.InOutCallFunction InOutCallFunction]],
  * but it is also a point of integration for custom or future extensions.
  * @tparam Args the type(s) of the parameter or parameters of the call statement itself, translating
  *              ot the parameters of the [[net.noresttherein.oldsql.sql.Incantation Incantation]] created based
  *              on this instance. These need not be the same as the formal parameters of the procedure/function:
  *              [[net.noresttherein.oldsql.sql.CallDML.Params Params]].
  * @tparam Res  the result of the call. This can be the return type of the called function
  *              as in [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]], but may also/instead include
  *              a selection of ''OUT'' parameters.
  */
trait Call[-Args, +Res]
	extends CallDML[Args, Res] with DMLStatement[Args, Res] with DMLStatementAPI[Args, Res, Call]
{
	protected override def returns[Y](result :StatementResult[Nothing, Y]) :Call[Args, Y] =
		new AlteredResultStatement.Base[Args, Y, Call](this, result) with Call[Args, Y] with DerivedDML[Args, Y]
			with AlteredResultStatement[Args, Y] with AlteredResultStatement.Impl[Args, Y, Call]

	override def compose[X](f :X => Args) :Call[X, Res] =
		new ComposedDML.Base[X, Args, Res, Call](this, f) with Call[X, Res] with DerivedDML[X, Res]
			with ComposedStatement[X, Args, Res] with ComposedStatement.Impl[X, Args, Res, Call]

	override def bind(args :Args) :Call[Unit, Res] =
		new BoundDML.Base[Args, Res, Call](this, args) with Call[Any, Res] with DerivedDML[Any, Res]
			with BoundStatement[Args, Res] with BoundStatement.Impl[Args, Res, Call]

	override def batch :CallDML[Seq[Args], Seq[Res]] =
		new RepeatedDML.Base(this)
			with CallDML[Seq[Args], Seq[Res]] with DerivedDML[Seq[Args], Seq[Res]] with RepeatedDML[Args, Res]

	protected override def doChant(implicit dialect :SQLDialect) :Incantation[Args, Res] = dialect(this)
}







/** Factory of statements calling stored [[net.noresttherein.oldsql.sql.StoredProcedure procedures]]
  * and [[net.noresttherein.oldsql.sql.StoredFunction function]].
  */
object Call {
	/** Creates an SQL statement invoking a stored [[net.noresttherein.oldsql.sql.StoredProcedure procedure]].
	  * The statement is parameterized with a [[net.noresttherein.oldsql.collection.Chain Chain]] containing
	  * all the parameters of the procedure, which are passed directly. It translates directly to the basic
	  * JDBC procedure call syntax:
	  * {{{
	  *     connection.prepareCall("{call count_the_bodies(?, ?, ?)}")
	  * }}}
	  * Apart from being a statement convertible to an [[net.noresttherein.oldsql.sql.Incantation Incantation]],
	  * it is also a factory of more complex calls to the procedure:
	  * {{{
	  *     valCall(count_the_bodies)["Race", String]["Slayer", String] {
	  *         params => params["Race"].chain ~ params["Slayer"] ~ "Sword Coast"
	  *     }
	  *     Call(count_the_bodies)[Race](_.last.name.chain ~ "Minsc" ~ "Sword Coast")
	  * }}}
	  * The parameterless `apply` calls declaring named and unnamed parameters can be chained and mixed freely
	  * in order to produce a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] consisting only of
	  * [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters of the created
	  * [[net.noresttherein.oldsql.sql.Call]]`[Params, Any]` (note that the parameter types of the call do not
	  * coincide with the parameters `Args` of the invoked procedure). It is then used to create a tuple expression
	  * for the SQL of the passed arguments using [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]
	  * facade, the same way that join conditions are created with
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]].
	  * ''OUT'' parameters of the procedure are returned by the call if the arguments are passed
	  * as [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` and ignored if given as unboxed values `X`
	  * @param procedure a stored procedure or an SQL function which will be invoked for side effects only.
	  * @see [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams]]
	  */
	def apply[Args <: Chain](procedure :StoredProcedure[Args]) :DirectCallProcedure[Args] =
		new DirectCallProcedure(procedure)

	/** Creates an SQL statement invoking a built in SQL function or a custom stored
	  * [[net.noresttherein.oldsql.sql.StoredFunction function]]. The statement is parameterized with a
	  * [[net.noresttherein.oldsql.collection.Chain Chain]] containing
	  * all the parameters of the procedure, which are passed directly. It translates directly to the basic
	  * JDBC procedure call syntax:
	  * {{{
	  *     connection.prepareCall("{call count_the_bodies(?, ?, ?)}")
	  * }}}
	  * Apart from being a statement convertible to an [[net.noresttherein.oldsql.sql.Incantation Incantation]],
	  * it is also a factory of more complex calls to the procedure:
	  * {{{
	  *     Call(count_the_bodies)["Race", String]["Slayer", String] {
	  *         params => params["Race"].chain ~ params["Slayer"] ~ "Sword Coast"
	  *     }
	  *     Call(count_the_bodies)[Race](_.last.name.chain ~ "Minsc" ~ "Sword Coast")
	  * }}}
	  * @param function an SQL function to execute.
	  * @see [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams]]
	  */
	def apply[Args <: Chain, Res](function :StoredFunction[Args, Res]) :DirectCallFunction[Args, Res] =
		new DirectCallFunction(function)



	implicit class Call0Extension[Y](private val self :Call[@~, Y]) extends AnyVal {
		@inline def applyUnit :Call[Unit, Y] = self.bind(@~)
	}
	implicit class Call1Extension[A, Y](private val self :Call[@~ ~A, Y]) extends AnyVal {
		@inline def apply(a :A) :Call[Unit, Y] = self.bind(@~ ~a)
	}
	implicit class Call2Extension[A, B, Y](private val self :Call[@~ ~A~B, Y]) extends AnyVal {
		@inline def apply(a :A, b :B) :Call[Unit, Y] = self.bind(@~ ~a~b)
	}
	implicit class Call3Extension[A, B, C, Y](private val self :Call[@~ ~A~B~C, Y]) extends AnyVal {
		@inline def apply(a :A, b :B, c :C) :Call[Unit, Y] = self.bind(@~ ~a~b~c)
	}
	implicit class Call4Extension[A, B, C, D, Y](private val self :Call[@~ ~A~B~C~D, Y]) extends AnyVal {
		@inline def apply(a :A, b :B, c :C, d :D) :Call[Unit, Y] = self.bind(@~ ~a~b~c~d)
	}
	implicit class Call5Extension[A, B, C, D, E, Y](private val self :Call[@~ ~A~B~C~D~E, Y]) extends AnyVal {
		@inline def apply(a :A, b :B, c :C, d :D, e :E) :Call[Unit, Y] = self.bind(@~ ~a~b~c~d~e)
	}
	implicit class Call6Extension[A, B, C, D, E, F, Y](private val self :Call[@~ ~A~B~C~D~E~F, Y]) extends AnyVal {
		@inline def apply(a :A, b :B, c :C, d :D, e :E, f :F) :Call[Unit, Y] = self.bind(@~ ~a~b~c~d~e~f)
	}
	implicit class Call7Extension[A, B, C, D, E, F, G, Y](private val self :Call[@~ ~A~B~C~D~E~F~G, Y]) extends AnyVal {
		@inline def apply(a :A, b :B, c :C, d :D, e :E, f :F, g :G) :Call[Unit, Y] = self.bind(@~ ~a~b~c~d~e~f~g)
	}
	implicit class Call8Extension[A, B, C, D, E, F, G, H, Y](private val self :Call[@~ ~A~B~C~D~E~F~G~H, Y])
		extends AnyVal
	{
		@inline def apply(a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H) :Call[Unit, Y] = self.bind(@~ ~a~b~c~d~e~f~g~h)
	}
	implicit class Call9Extension[A, B, C, D, E, F, G, H, I, Y](private val self :Call[@~ ~A~B~C~D~E~F~G~H~I, Y])
		extends AnyVal
	{
		@inline def apply(a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I) :Call[Unit, Y] =
			self.bind(@~ ~a~b~c~d~e~f~g~h~i)
	}
	implicit class Call10Extension[A, B, C, D, E, F, G, H, I, J, Y](private val self :Call[@~ ~A~B~C~D~E~F~G~H~I~J, Y])
		extends AnyVal
	{
		@inline def apply(a :A, b :B, c :C, d :D, e :E, f :F, g :G, h :H, i :I, j :J) :Call[Unit, Y] =
			self.bind(@~ ~a~b~c~d~e~f~g~h~i~j)
	}




	/** A parameterized statement invoking a [[net.noresttherein.oldsql.sql.StoredProcedure stored procedure]].
	  * Note that the parameter(s) `Args` of this class need not correspond in general to the parameters
	  * of the stored procedure: the latter can be arbitrary SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
	  * based on a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] with `Args` as
	  * [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] parameters. The result type is specified as `Any`
	  * rather than `Unit` for better interoperability with subclasses.
	  * @tparam Args the parameter or parameters of this statement.
	  * @see [[net.noresttherein.oldsql.sql.Call.CallFunction]]
	  */
	trait CallProcedure[-Args] extends Call[Args, Any] {
		override type Result = Any
		override val procedure :StoredProcedure[Params]
		override def result :StatementResult[Nothing, Any] = NoResult

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Any] =
			visitor.procedure(this)
	}


	/** A parameterized statement invoking a [[net.noresttherein.oldsql.sql.StoredFunction stored function]]
	  * (or a standard SQL function). Note that the parameter(s) `Args` of this class need not correspond in general
	  * to the parameters of the function: the latter can be arbitrary
	  * SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
	  * based on a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] with `Args` as
	  * [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] parameters.
	  * @tparam Args the parameter or parameters of this statement.
	  * @tparam Res  the return type of the called function and the result type of the statement.
	  * @see [[net.noresttherein.oldsql.sql.Call.InOutCallFunction]]
	  */
	trait CallFunction[-Args, Res]
		extends Call[Args, Res]// with CallFunctionDML[Args, Res] with DMLStatementAPI[Args, Res, CallFunction]
	{
		override type Result = Res
		override val procedure :StoredFunction[Params, Result]
		/** Invoked function - an alias for [[net.noresttherein.oldsql.sql.Call.CallFunction.procedure procedure]]. */
		def function           :StoredFunction[Params, Result] = procedure

		override def result :StatementResult[Nothing, Res] =
			StatementResult.ProcedureResult[Res](CallFunction.returnIndex)(function.readForm)

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] =
			visitor.function(this)

		protected override def doChant(implicit dialect :SQLDialect) :Incantation[Args, Res] = dialect(this)
	}


	/** A parameterized statement invoking a [[net.noresttherein.oldsql.sql.StoredProcedure stored procedure]]
	  * and returning (some of) its ''OUT'' parameters.
	  * Note that the parameter(s) `Args` of this class need not correspond in general to the parameters
	  * of the stored procedure: the latter can be arbitrary SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
	  * based on a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] with `Args` as
	  * [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] parameters.
	  * @tparam Args the parameter or parameters of this statement.
	  * @tparam Out  all ''OUT'' parameters returned by the call as a single value.
	  *              A [[net.noresttherein.oldsql.collection.Chain Chain]] subtype in the standard implementation.
	  * @see [[net.noresttherein.oldsql.sql.StoredProcedure.Out]]
	  * @see [[net.noresttherein.oldsql.sql.Call.InOutCallFunction]]
	  */
	trait InOutCallProcedure[-Args, +Out] extends CallProcedure[Args] with Call[Args, Out] {
		override val result :StatementResult[Nothing, Out] = StatementResult.ProcedureResult(outParamIndices)

		//consider: setting result in the constructor and getting rid of these two
		/** Indices of all ''OUT'' parameters returned as `Out` type parameter. These use the same indexing
		  * as JDBC parameters, that is statement parameters in `Args` used as arguments for the procedure.
		  * The index of the first parameter is 1.
		  */
		def outParamIndices :Seq[Int]

		/** A form reading registered ''OUT'' parameters from the underlying
		  * [[java.sql.CallableStatement CallableStatement]], as if they were columns
		  * of a [[java.sql.ResultSet ResultSet]].
		  */
		implicit protected def outParamsForm :SQLReadForm[Out]

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Out] =
			visitor.inOutProcedure(this)
	}


	/** A parameterized statement invoking a [[net.noresttherein.oldsql.sql.StoredProcedure stored function]]
	  * (or a standard SQL function) and returning its return value together with (some of) its ''OUT'' parameters.
	  * Note that the parameter(s) `Args` of this class need not correspond in general to the parameters
	  * of the stored function: the latter can be arbitrary SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
	  * based on a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] with `Args` as
	  * [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] parameters. The result type is a chain
	  * `Out ~ Y` following the types of ''OUT'' parameters with the actual return type of the function.
	  * @tparam Args the parameter or parameters of this statement.
	  * @tparam Out  all ''OUT'' parameters returned by the call as a `Chain`.
	  * @tparam Y    the return type of the invoked function.
	  * @see [[net.noresttherein.oldsql.sql.StoredProcedure.Out]]
	  * @see [[net.noresttherein.oldsql.sql.Call.InOutCallProcedure]]
	  */
	trait InOutCallFunction[-Args, +Out <: Chain, Y]
		extends Call[Args, Out ~ Y]// with CallFunctionDML[In, Out ~ Y]
	{
		override type Result = Y
		override val procedure :StoredFunction[Params, Result]
		/** Invoked function - an alias for [[net.noresttherein.oldsql.sql.Call.CallFunction.procedure procedure]]. */
		def function           :StoredFunction[Params, Result] = procedure

		override val result :StatementResult[Nothing, Out ~ Y] =
			StatementResult.FunctionResult(outParamIndices)(outParamsForm, procedure.readForm)

		/** Indices of all ''OUT'' parameters returned as `Out` type parameter. These use the same indexing
		  * as JDBC parameters, that is statement parameters in `Args` used as arguments for the procedure.
		  * The index of the special 'parameter' with the returned value is 1 and the index
		  * of the actual first parameter is 2.
		  */
		protected def outParamIndices :Seq[Int]

		/** A form reading registered ''OUT'' parameters from the underlying
		  * [[java.sql.CallableStatement CallableStatement]], as if they were columns
		  * of a [[java.sql.ResultSet ResultSet]].
		  */
		implicit protected def outParamsForm :SQLReadForm[Out]

		protected override def applyTo[R[-_, +_]](visitor :StatementVisitor[R]) :R[Args, Out ~ Y] =
			visitor.inOutFunction(this)
	}




	/** The simplest call of a [[net.noresttherein.oldsql.sql.StoredProcedure stored procedure]],
	  * passing JDBC statement parameters directly as arguments to the procedure using syntax:
	  * {{{
	  *     "{call <procedure>(?, ?, ?, ?)}
	  * }}}
	  * (with the number of parameters equaling the number of columns in the procedure's
	  * [[net.noresttherein.oldsql.sql.StoredProcedure.paramForm parameter form]].
	  * No values are returned by this call, which is executed solely for side effects.
	  *
	  * At the same time, this class serves as a builder/factory of more complex
	  * [[net.noresttherein.oldsql.sql.Call Call]]s.
	  * @tparam Args formal parameter types of the procedure.
	  * @param procedure the called procedure.
	  */
	class DirectCallProcedure[Args <: Chain](override val procedure :StoredProcedure[Args]) extends CallProcedure[Args] {
		/** Initiates the preparation of a parameterized stored procedure call ending with the creation of
		  * a [[net.noresttherein.oldsql.sql.Call.CallProcedure CallProcedure]] statement with `P` as its first -
		  * or only - parameter. Subsequent, named an anonymous parameters can be introduced by chaining `apply` calls
		  * with only type parameter lists, each introducing a single parameter: `this[P][Q][R]`, `this[P]["Q", Q][R]`.
		  * At any point, the process can be finalized by passing a
		  * [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple tuple]] SQL expression with a value of `X`, either
		  * [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply(args:JoinedMappings[F]=>ChainTuple[F,GlobalScope,X])* apply(domain => args)]]
		  * or [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply(args:ChainTuple[F,GlobalScope,X]) apply(args)]].
		  * The tuple expression must be based on a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting
		  * only of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins - in the case of the last examples,
		  * `FromSome WithParam P WithParam Q WithParam R` and `FromSome WithParam P WithParam Q As "Q" WithParam R`.
		  * The function creating the tuple accepts an argument of
		  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]], providing access to call parameters,
		  * and is analogous to the one passed to [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]].
		  *
		  * @return an object implicitly convertible to
		  *         [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams CallProcedureParams]]`[FromSome WithParam P, @~ ~ P, X]`
		  *         when one of the non-argument `apply` is invoked: `this[P1][P2]` or `this[P1]["P2", P2]`.
		  *         This step of indirection exists purely for technical reasons due to the need
		  *         of an implicit parameter of [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[P]`.
		  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
		  *           used by the future SQL expression with arguments for the procedure.
		  */
		def apply[P] :CallProcedureParamDecl[FromSome, @~, P, Args] =
			new CallProcedureParamDecl(From.template, procedure)

		/** Initiates the preparation of a parameterized stored procedure call ending with the creation of
		  * a [[net.noresttherein.oldsql.sql.Call Call]] statement with `P` as its first - or only -
		  * parameter. The string literal passed as the first type argument is used as the name of the parameter, becoming
		  * an alias in the ''from'' clause `FromSome WithParam P As N` for the domain over which the expression
		  * with procedure's arguments are created. Subsequent, named an anonymous parameters can be introduced by chaining
		  * `apply` calls with only type parameter lists, each introducing a single parameter: `this[N, P][Q][R]`,
		  * `this[N, P]["Q", Q][R]`. At any point, the process can be finalized by passing a
		  * [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple tuple]] SQL expression with a value of `X`, either
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
		  *         when one of the non-argument `apply` is invoked: `this[P1][P2]` or `this[P1]["P2", P2]`.
		  *         This step of indirection exists purely for technical reasons due to the need of implicit parameters
		  *         of [[scala.ValueOf ValueOf]]`[N]` and [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[P]`.
		  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
		  *           used by the future SQL expression with arguments for the procedure.
		  * @tparam N a `String` literal type with the name of the parameter, which can be used to access it
		  *           from [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] for the whole domain
		  *           when creating the SQL expression with the arguments for the procedure.
		  */
		def apply[N <: Label, P] :CallProcedureNamedParamDecl[FromSome, @~, N, P, Args] =
			new CallProcedureNamedParamDecl(From.template, procedure)

		/** Factory method for statements invoking this procedure and returning some combination of the values
		  * of its declared ''OUT'' parameters. All ''OUT'' parameters of a procedure are declared as type
		  * [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` rather than simply `X` in `Params`.
		  * The choice of which to return is made by similarly marking the provided arguments by wrapping them
		  * in `Out` (see [[net.noresttherein.oldsql.sql.SQLExpression.out SQLExpression.out]] method): any argument
		  * given in `In` as `Out[X]` - as in the formal parameters of the procedure - has its value returned.
		  * Passing an argument as `X` instead of `Out[X]` omits it from this list, treating it as another ''IN'' parameter.
		  * If the type of a parameter (an element type of the argument tuple) is not known to be `Out`, it is similarly
		  * treated as an ''IN'' parameter, even if its dynamic type is in fact an `Out` subtype. Additionally,
		  * a tuple matching the formal parameter types `Args` of this procedure will always be accepted, even if
		  * `Args` is not a fully instantiated type (including having a wildcard `Chain` as its beginning or being
		  * an abstract type); the return type becomes `Any` in that case, with no ''OUT'' parameters being returned.
		  * This method relies on implicit evidence `signature` matching the formal parameter types of the procedure
		  * `Args` with the types of the provided arguments `In` of the tuple expression `args`. The result type
		  * `signature.`[[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.Out Out]]
		  * is a [[net.noresttherein.oldsql.collection.Chain Chain]] listing all parameters `X` declared as `Out[X]`
		  * in `In`, in the order of their appearance.
		  * @tparam F        a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] subtype consisting solely of
		  *                  [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins declaring the parameters
		  *                  used by the argument expression `args`. The number and order of the parameters must match
		  *                  the chain with the arguments `In`:
		  *                  `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params Params]]` =:= In`.
		  *                  Serves as the domain for the arguments, that is the argument tuple `args` depends
		  *                  on the values of parameters it declares.
		  * @tparam In       types of the arguments given as a tuple.
		  * @param args      a tuple expression listing the values of all arguments for the procedure.
		  * @param signature the signature of the call, that is matching of the parameters to their arguments,
		  *                  providing the return type
		  *                  [[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.ProcedureResult ProcedureResult]]
		  *                  - the result type of the created `Call`.
		  * @return a statement invoking the procedure using the JDBC procedure call syntax.
		  */
		def apply[F <: RowProduct, In <: Chain]
		         (args :ChainTuple[F, GlobalScope, In])
		         (implicit signature :ProcedureSignature[Args, In] { type Domain <: F })
				:Call[signature.CallParams, signature.Out] =
			signature(procedure, args)

		/** Same as [[net.noresttherein.oldsql.sql.DML.bind bind]] - creates a parameterless `Call` which invokes
		  * this procedure with the given arguments.
		  */
		def apply(args :Args) :Call[Unit, Any] = bind(args)


		override type Params = Args

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Any] =
			visitor.directProcedure(this)

		protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args] =
			"{call " +: (procedure.paramSpellingForwarder(spelling) + "}")


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[DirectCallProcedure[_]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :DirectCallProcedure[_] if canEqual(other) && other.canEqual(this) =>
				procedure == other.procedure
			case _ => false
		}
		protected override def initHashCode :Int = procedure.hashCode

		protected override def initToString :String = procedure.name + "(" + procedure.paramForm.inlineParam + ")"
	}


	/** The simplest call of a [[net.noresttherein.oldsql.sql.StoredFunction stored function]],
	  * passing JDBC statement parameters directly as arguments to the function using syntax:
	  * {{{
	  *     "{? = call <function>(?, ?, ?, ?)}
	  * }}}
	  * (with the number of parameters equaling the number of columns in the function's
	  * [[net.noresttherein.oldsql.sql.StoredProcedure.paramForm parameter form]].
	  * No values are returned by this call, which is executed solely for side effects.
	  *
	  * At the same time, this class serves as a builder/factory of more complex
	  * [[net.noresttherein.oldsql.sql.Call Call]]s.
	  * @tparam Args formal parameter types of the function.
	  * @tparam Res  the return type of the function.
	  * @param procedure the called function.
	  */
	class DirectCallFunction[Args <: Chain, Res](override val procedure :StoredFunction[Args, Res])
		extends CallFunction[Args, Res]
	{
		/** Initiates the preparation of a parameterized SQL function call ending with the creation of
		  * a [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]] statement with `P` as its first -
		  * or only - parameter. Subsequent, named an anonymous parameters can be introduced by chaining `apply` calls
		  * with only type parameter lists, each introducing a single parameter: `this[P][Q][R]`, `this[P]["Q", Q][R]`.
		  * At any point, the process can be finalized by passing a
		  * [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple tuple]] SQL expression with a value of `X`, either
		  * [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:JoinedMappings[F]=>ChainTuple[F,GlobalScope,X])* apply(domain => args)]]
		  * or [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:ChainTuple[F,GlobalScope,X]) apply(args)]].
		  * The tuple expression must be based on a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting
		  * only of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins - in the case of the last examples,
		  * `FromSome WithParam P WithParam Q WithParam R` and `FromSome WithParam P WithParam Q As "Q" WithParam R`.
		  * The function creating the tuple accepts an argument of
		  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]], providing access to call parameters,
		  * and is analogous to the one passed to [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]].
		  *
		  * @return an object implicitly convertible to
		  *         [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams CallFunctionParams]]`[FromSome WithParam P, @~ ~ P, X]`
		  *         when one of the non-argument `apply` is invoked: `this[P1][P2]` or `this[P1]["P2", P2]`.
		  *         This step of indirection exists purely for technical reasons due to the need
		  *         of an implicit parameter of [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[P]`.
		  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
		  *           used by the future SQL expression with arguments for the procedure.
		  */
		def apply[P] :CallFunctionParamDecl[FromSome, @~, P, Args, Res] =
			new CallFunctionParamDecl(From.template, procedure)

		/** Initiates the preparation of a parameterized stored procedure call ending with the creation of
		  * a [[net.noresttherein.oldsql.sql.Call Call]] statement with `P` as its first - or only -
		  * parameter. The string literal passed as the first type argument is used as the name of the parameter, becoming
		  * an alias in the ''from'' clause `FromSome WithParam P As N` for the domain over which the expression
		  * with procedure's arguments are created. Subsequent, named an anonymous parameters can be introduced by chaining
		  * `apply` calls with only type parameter lists, each introducing a single parameter: `this[N, P][Q][R]`,
		  * `this[N, P]["Q", Q][R]`. At any point, the process can be finalized by passing a
		  * [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple tuple]] SQL expression with a value of `X`, either
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
		  *         when one of the non-argument `apply` is invoked: `this[P1][P2]` or `this[P1]["P2", P2]`.
		  *         This step of indirection exists purely for technical reasons due to the need of implicit parameters
		  *         of [[scala.ValueOf ValueOf]]`[N]` and [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[P]`.
		  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
		  *           used by the future SQL expression with arguments for the procedure.
		  * @tparam N a `String` literal type with the name of the parameter, which can be used to access it
		  *           from [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] for the whole domain
		  *           when creating the SQL expression with the arguments for the procedure.
		  */
		def apply[N <: Label, P] :CallFunctionNamedParamDecl[FromSome, @~, N, P, Args, Res] =
			new CallFunctionNamedParamDecl(From.template, procedure)


		/** Factory method for statements invoking this function and returning its result and some combination
		  * of the values of its declared ''OUT'' parameters. All ''OUT'' parameters of a procedure are declared as type
		  * [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` rather than simply `X` in `Params`.
		  * The choice of which to return is made by similarly marking the provided arguments by wrapping them
		  * in `Out` (see [[net.noresttherein.oldsql.sql.SQLExpression.out SQLExpression.out]] method): any argument
		  * given in `In` as `Out[X]` - as in the formal parameters of the function - has its value returned. Passing
		  * an argument as `X` instead of `Out[X]` omits it from this list, treating it as another ''IN'' parameter.
		  * If the type of a parameter (an element type of the argument tuple) is not known to be `Out`, it is similarly
		  * treated as an ''IN'' parameter, even if its dynamic type is in fact an `Out` subtype. Additionally,
		  * a tuple matching the formal parameter types `Args` of this function will always be accepted, even if
		  * `Args` is not a fully instantiated type (including having a wildcard `Chain` as its beginning or being
		  * an abstract type); the return type becomes `Any` in that case, with no ''OUT'' parameters being returned.
		  * This method relies on implicit evidence `signature` matching the formal parameter types of the function
		  * `Args` with the types of the provided arguments `In` of the tuple expression `args`. The result type
		  * `signature.`[[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.Out Out]]` ~ Res`
		  * is a [[net.noresttherein.oldsql.collection.Chain Chain]] listing all parameters `X` declared as `Out[X]`
		  * in `In`, in the order of their appearance, followed by the value returned by the function itself.
		  * @tparam F        a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] subtype consisting solely of
		  *                  [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins declaring the parameters
		  *                  used by the argument expression `args`. The number and order of the parameters must match
		  *                  the chain with the arguments `In`:
		  *                  `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params Params]]` =:= In`.
		  *                  Serves as the domain for the arguments, that is the argument tuple `args` depends
		  *                  on the values of parameters it declares.
		  * @tparam In       types of the arguments given as a tuple.
		  * @param args      a tuple expression listing the values of all arguments for the procedure.
		  * @param signature the signature of the call, that is matching of the parameters to their arguments,
		  *                  providing the return type
		  *                  [[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.FunctionResult FunctionResult]]`[Res]`
		  *                  - the result type of the created `Call`.
		  * @return a statement invoking the procedure using the JDBC procedure call syntax.
		  */
		def apply[F <: RowProduct, In <: Chain]
		         (args :ChainTuple[F, GlobalScope, In])
		         (implicit signature :ProcedureSignature[Args, In] { type Domain <: F })
				:Call[signature.CallParams, signature.FunctionResult[Res]] =
			signature(procedure, args)

		/** Same as [[net.noresttherein.oldsql.sql.DML.bind bind]] - creates a parameterless `Call` which invokes
		  * this function with the given arguments.
		  */
		def apply(args :Args) :Call[Unit, Res] = bind(args)


		override type Params = Args

		protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] =
			visitor.directFunction(this)

		protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args] = {
			val sql = procedure.paramSpellingForwarder(spelling)
			SpelledSQL("{?= call " +: (sql.sql + "}"), sql.context, ColumnWriteForm.gap + sql.setter)
		}


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[DirectCallProcedure[_]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :DirectCallProcedure[_] if canEqual(other) && other.canEqual(this) =>
				procedure == other.procedure
			case _ => false
		}
		protected override def initHashCode :Int = procedure.hashCode

		protected override def initToString :String = procedure.name + "(" + procedure.paramForm.inlineParam + ")"
	}




	/** A mix-in trait for `Call` implementations passing arbitrary SQL expressions not requiring statement parameters.
	  * This refers only to [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters of the
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] the argument tuple expression is based on; the argument
	  * can still include [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameter expressions
	  * translating to JDBC statement parameters, meaning that created [[java.sql.CallableStatement CallableStatement]]
	  * can be cached by the driver, but the values for the parameters are included in this instance.
	  * It stores the tuple expression [[net.noresttherein.oldsql.sql.Call.GroundCall.args args]], matching
	  * the formal parameter types declared by the invoked procedure/function. Note that this relates to
	  * [[net.noresttherein.oldsql.sql.StoredProcedure StoredProcedure]] and
	  * [[net.noresttherein.oldsql.sql.StoredFunction StoredFunction]] classes, rather than their database counterparts,
	  * which can contain complex types mapping to several actual parameters of the invoked procedure.
	  */
	trait GroundCall[+Res] extends Call[Unit, Res] {
		/** An SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] for a tuple with the arguments
		  * for the called procedure.
		  */
		val args :ChainTuple[RowProduct, GlobalScope, Params]

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroundCall[_]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :GroundCall[_] if canEqual(other) && other.canEqual(this) =>
				procedure == other.procedure && args == other.args
			case _ => false
		}
		protected override def initHashCode :Int = (procedure.hashCode * 31 + args.hashCode) * 31 + result.hashCode

		protected override def initToString :String = procedure.name + args + ":" + result
	}


	/** A mix-in trait for `Call` implementations passing arbitrary SQL expressions parameterized by
	  * [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters. It stores the tuple expression
	  * [[net.noresttherein.oldsql.sql.Call.ParamCall.args args]], matching the formal parameter types declared
	  * by the invoked procedure/function. Note that this relates to
	  * [[net.noresttherein.oldsql.sql.StoredProcedure StoredProcedure]] and
	  * [[net.noresttherein.oldsql.sql.StoredFunction StoredFunction]] classes, rather than their database counterparts,
	  * which can contain complex types mapping to several actual parameters of the invoked procedure.
	  * @tparam Args the type(s) of the parameter or parameters of the call statement itself,
	  *              the [[net.noresttherein.oldsql.sql.RowProduct.Params parameters]] of
	  *              the [[net.noresttherein.oldsql.sql.Call.ParamCall.Domain Domain]] type on which the
	  *              argument expression `args` is based. These need not be the same as the formal parameters
	  *              of the procedure/function: [[net.noresttherein.oldsql.sql.CallDML.Params Params]].
	  * @tparam Res  the result of the call. This can be the return type of the called function
	  *              as in [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]], but may also/instead include
	  *              a selection of ''OUT'' parameters.
	  */
	trait ParamCall[-Args <: Chain, +Res] extends Call[Args, Res] {
		override type Params <: Chain

		/** A [[net.noresttherein.oldsql.collection.Chain chain]] with the types of all parameters of this statement
		  * as used by the [[net.noresttherein.oldsql.sql.Call.ParamCall.args argument]]
		  * expressions provided for the procedure. It is the type parameter `Args` provided when this instance was
		  * created (due to contra variance).
		  */
		type Bound >: Args <: Chain

		/** A [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] type consisting solely of
		  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] 'joins' with parameter (or parameters)
		  * of this statement, on which the SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
		  * of the arguments for the procedure are based on. It can be completely unrelated to the
		  * [[net.noresttherein.oldsql.sql.Call.ParamCall.Params types of parameters]] of the procedure.
		  */
		type Domain <: PureParamFrom[Bound]

		/** A join or joins of mappings for unbound parameters used by the SQL
		  * [[net.noresttherein.oldsql.sql.SQLExpression expressions]] provided as the arguments for the procedure.
		  * It contains all parameters of this statement.
		  */
		val domain :Domain

		/** An SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] for a tuple with the arguments
		  * for the called procedure.
		  */
		def args :ChainTuple[domain.Self, GlobalScope, Params]

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamCall[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :ParamCall[_, _] if canEqual(other) && other.canEqual(this) =>
				procedure == other.procedure && domain == other.domain && args == other.args && result == other.result
			case _ => false
		}
		protected override def initHashCode :Int =
			((procedure.hashCode * 31 + domain.hashCode) * 31 + args.hashCode) * 31 + result.hashCode

		protected override def initToString :String = procedure.name + args + ":" + result
	}






	object CallProcedure {
		/** Factory method for statements invoking `procedure` and returning no result.
		  * This method variant requires the values for all the arguments to be known (to be literals,
		  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters or expressions built on them)
		  * and creates a parameterless `Call` instance.
		  * @tparam Params   types of the formal parameters of the called procedure.
		  * @param procedure the invoked stored procedure or function.
		  * @param args      a ground tuple expression listing the values of all arguments for the procedure.
		  *                  The types of the tuple elements must match exactly formal parameter types of the procedure.
		  * @return a statement invoking the procedure using the JDBC procedure call syntax.
		  */
		def GroundCallProcedure[Params <: Chain]
		                       (procedure :StoredProcedure[Params], args :ChainTuple[RowProduct, GlobalScope, Params])
				:GroundCallProcedure =
			new GroundCallProcedureImpl(procedure, args)

		/** A parameterless statement invoking a [[net.noresttherein.oldsql.sql.StoredProcedure stored procedure]].
		  * Note that ''parameterless'' here means that no arguments need be provided to execute this statement;
		  * the actual JDBC [[java.sql.CallableStatement CallableStatement]] may still be parameterized using
		  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameter expressions.
		  */
		trait GroundCallProcedure extends CallProcedure[Unit] with GroundCall[Any] {
			protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Unit, Any] =
				visitor.groundProcedure(this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Unit] =
				(("{call " + spelling.function(procedure.name) + '(') +:
					spelling.inline(args)(Dual, spelling.newContext, Parameterization.paramless[ParamlessFrom])) + ")}"

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroundCallProcedure]
		}

		private[Call] class GroundCallProcedureImpl[Xs <: Chain]
		                                           (override val procedure :StoredProcedure[Xs],
		                                            override val args :ChainTuple[RowProduct, GlobalScope, Xs])
			extends GroundCallProcedure// with CallProcedure[Any] with GroundDML.Impl[Any]
		{
			override type Params = Xs
		}



		/** Factory method for statements invoking `procedure` and returning no result.
		  * @tparam Args     types of unbound parameters of the argument expression given for the call.
		  * @tparam Params   types of the formal parameters of the called procedure.
		  * @param domain    a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] consisting solely of
		  *                  [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins declaring the parameters
		  *                  used by the argument expression `args`. It ''cannot'' contain any tables, as it would
		  *                  result in an invalid SQL being generated at a later point.
		  * @param procedure the invoked stored procedure or function.
		  * @param args      a tuple expression based on `domain`'s [[net.noresttherein.oldsql.sql.RowProduct.Self self]]
		  *                  type, listing the values of all arguments for the procedure. The types of the tuple elements
		  *                  must match exactly formal parameter types of the procedure.
		  * @return a statement invoking the procedure using the JDBC procedure call syntax.
		  */
		def ParamCallProcedure[Args <: Chain, Params <: Chain]
		                      (domain :PureParamFrom[Args]) //todo: type which enforces only JoinParam joins
		                      (procedure :StoredProcedure[Params], args :ChainTuple[domain.Self, GlobalScope, Params])
				:ParamCallProcedure[Args] =
			new ParamCallProcedureImpl[domain.type, domain.Self, Args, Params](domain, procedure, args)

		/** A parameterized call of a [[net.noresttherein.oldsql.sql.StoredProcedure stored procedure]]
		  * providing arbitrary SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] as arguments.
		  */
		trait ParamCallProcedure[-Args <: Chain] extends CallProcedure[Args] with ParamCall[Args, Any] {
			protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Any] =
				visitor.paramProcedure(this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args] =
				(("{call " + spelling.function(procedure.name) + '(') +:
					spelling.inline(args)(domain.self, domain.spellingContext, domain.parameterization)) + ")}"

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamCallProcedure[_]]
		}

		private[Call] class ParamCallProcedureImpl[D <: PureParamFrom[Args] { type Self <: F },
		                                           F <: ParameterizedFrom[Args], Args <: Chain, Xs <: Chain]
		                                          (override val domain :D, override val procedure :StoredProcedure[Xs],
		                                           override val args :ChainTuple[F, GlobalScope, Xs])
			extends ParamCallProcedure[Args]
		{
			override type Bound = Args
			override type Params = Xs
			override type Domain = D
		}




		/** An intermediate object adding a new [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter `P`
		  * to the `F &lt;: `[[net.noresttherein.oldsql.sql.RowProduct RowProduct]] domain serving as a base
		  * for the [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple parameter set]] of the call,
		  * implicitly convertible to
		  * [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams CallProcedureParams]]`[F WithParam P, Ps ~ P, Xs]`
		  * It has the following (extension) methods for adding additional parameters and creating an expression using them:
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply[P]* this]]`[Q]` -
		  *      adds a new parameter of type `Q` to the expression's domain: `F WithParam P WithParam Q`;
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply[N<:Label,P]* this]]`["Q", Q]` -
		  *   adds a new parameter named "Q" and of type `Q` to the expression's domain:
		  *   `F WithParam P WithParam Q As "Q"`;
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply(args:ChainTuple[F,GlobalScope,Xs])* this]]`(argsTuple)`
		  *      creates a new expression using a constructor function accepting
		  *      [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F WithParam P]`.
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply(args:JoinedMappings[F]=>ChainTuple[F,GlobalScope,Xs] this]]`(params => argsTuple)` -
		  *      creates a new expression using a constructor function accepting
		  *      [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F WithParam P]`.
		  *
		  * This class exists as Scala 2 would not allow chaining of methods with explicit type parameters
		  * but only implicit parameter lists.
		  * @tparam F  a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting ''solely'' of
		  *            [[net.noresttherein.oldsql.sql.WithParam WithParam]] expressions - each possibly with an
		  *            [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause. It is the ''from'' clause on which
		  *            created expressions are based.
		  * @tparam Ps a chain consisting of types of all unbound parameters of `F`, forming the parameters of the
		  *            created `Call` statement. Any wildcard prefix type of `F` is ignored and only unbound parameters
		  *            following it are included in `Ps`, which always starts with `@~`.
		  * @tparam P  the type of the new parameter.
		  */
		class CallProcedureParamDecl[F <: FromSome, Ps <: Chain, P, Xs <: Chain]
		                            (protected val domain :F ParameterizedWith Ps,
		                             protected val procedure :StoredProcedure[Xs])

		object CallProcedureParamDecl {
			implicit def domain[F <: PureParamFrom[Ps], Ps <: Chain, P :SQLForm, Xs <: Chain]
			                   (param :CallProcedureParamDecl[F, Ps, P, Xs])
					:CallProcedureParams[F WithParam P, Ps ~ P, Xs] =
				new CallProcedureParams(JoinParam(param.domain, ?:[P]), param.procedure)
		}

		/** An intermediate object adding a new [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter `P`
		  * to the `F &lt;: `[[net.noresttherein.oldsql.sql.RowProduct RowProduct]] domain serving as a base
		  * for the [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple parameter set]] of the call,
		  * implicitly convertible to
		  * [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams CallProcedureParams]]`[F WithParam P As N, Ps ~ P, Xs]`
		  * It has the following (extension) methods for adding additional parameters and creating an expression using them:
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply[P]* this]]`[Q]` -
		  *      adds a new parameter of type `Q` to the expression's domain: `F WithParam P WithParam Q`;
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply[N<:Label,P]* this]]`["Q", Q]` -
		  *   adds a new parameter named "Q" and of type `Q` to the expression's domain:
		  *   `F WithParam P WithParam Q As "Q"`;
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply(args:ChainTuple[F,GlobalScope,Xs])* this]]`(argsTuple)`
		  *      creates a new expression using a constructor function accepting
		  *      [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F WithParam P As N]`.
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply(args:JoinedMappings[F]=>ChainTuple[F,GlobalScope,Xs] this]]`(params => argsTuple)` -
		  *      creates a new expression using a constructor function accepting
		  *      [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F WithParam P As N]`.
		  *
		  * This class exists as Scala 2 would not allow chaining of methods with explicit type parameters
		  * but only implicit parameter lists.
		  * @tparam F  a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting ''solely'' of
		  *            [[net.noresttherein.oldsql.sql.WithParam WithParam]] expressions - each possibly with an
		  *            [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause. It is the ''from'' clause on which
		  *            created expressions are based.
		  * @tparam Ps a chain consisting of types of all unbound parameters of `F`, forming the parameters of the
		  *            created Call statement. Any wildcard prefix type of `F` is ignored and only unbound parameters
		  *            following it are included in `Ps`, which always starts with `@~`.
		  * @tparam N  a `String` literal with the name of the new parameter.
		  * @tparam P  the type of the new parameter.
		  */
		class CallProcedureNamedParamDecl[F <: FromSome, Ps <: Chain, N <: Label, P, Xs <: Chain]
		                                 (protected val domain :F ParameterizedWith Ps,
		                                  protected val procedure :StoredProcedure[Xs])

		object CallProcedureNamedParamDecl {
			implicit def domain[F <: PureParamFrom[Ps], Ps <: Chain, N <: Label :ValueOf, P :SQLForm, Xs <: Chain]
			                   (param :CallProcedureNamedParamDecl[F, Ps, N, P, Xs])
					:CallProcedureParams[F WithParam P As N, Ps ~ P, Xs] =
				new CallProcedureParams(JoinParam(param.domain, ?:[N, P]), param.procedure)
		}

		/** A factory of [[net.noresttherein.oldsql.sql.Call.CallProcedure CallProcedure]] SQL statements which execute
		  * this stored procedure. It accepts an SQL
		  * [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple ChainTuple]]`[F, GlobalScope, Ps]` expression
		  * containing all arguments for the procedure - either directly, or as a function of
		  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F]`.
		  * It also allows to introduce additional parameters by chaining parameterless calls of
		  * [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply[P] this]]`[P]` and
		  * [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams.apply[N<:Label,P] this]]`[N, P]`
		  * behaving identically to the counterpart methods of the executed `StoredProcedure`.
		  * @tparam F  a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting ''solely'' of
		  *            [[net.noresttherein.oldsql.sql.WithParam WithParam]] expressions - each possibly with an
		  *            [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause. It is the ''from'' clause on which
		  *            S
		  * @tparam Ps a chain consisting of types of all unbound parameters of `F`, forming the parameters of the
		  *            created SQL statement. Any wildcard prefix type of `F` is ignored and only unbound parameters
		  *            following it are included in `Ps`, which always starts with `@~`.
		  *            This is not related to the chain with types of the actual parameters of
		  *            the procedure itself - rather, the arguments for the procedure can be any SQL
		  *            [[net.noresttherein.oldsql.sql.SQLExpression expressions]] using `F` as their base.
		  * @tparam Xs a chain listing the types of all formal type parameters of the executed procedure.
		  */
		class CallProcedureParams[F <: PureParamFrom[Ps], Ps <: Chain, Xs <: Chain]
		                         (domain :F ParameterizedWith Ps, procedure :StoredProcedure[Xs])
		{
			/** Add another unbound parameter to the parameters of the created
			  * [[net.noresttherein.oldsql.sql.Call.CallProcedure CallProcedure]].
			  * @return an object implicitly convertible to a
			  *         [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams CallProcedureParams]]`[F With Param P, Ps ~ P, Xs]`.
			  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
			  *           used by the future SQL expression with arguments for the procedure.
			  * @see [[net.noresttherein.oldsql.sql.StoredProcedure.apply[P]]]
			  */
			def apply[P] :CallProcedureParamDecl[F, Ps, P, Xs] =
				new CallProcedureParamDecl[F, Ps, P, Xs](domain, procedure)

			/** Add another named unbound parameter to the parameters of the created
			  * [[net.noresttherein.oldsql.sql.Call.CallProcedure CallProcedure]].
			  * @return an object implicitly convertible to a
			  *         [[net.noresttherein.oldsql.sql.Call.CallProcedure.CallProcedureParams CallProcedureParams]]`[F With Param P, Ps ~ P, Xs]`.
			  * @see [[net.noresttherein.oldsql.sql.StoredProcedure.apply[N<:Label,P]]]
			  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
			  *           used by the future SQL expression with arguments for the procedure.
			  * @tparam N a `String` literal type with the name of the parameter, which can be used to access it
			  *           from [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] for the whole
			  *           domain when creating the SQL expression with the arguments for the procedure.
			  */
			def apply[N <: Label, P] :CallProcedureNamedParamDecl[F, Ps, N, P, Xs] =
				new CallProcedureNamedParamDecl[F, Ps, N, P, Xs](domain, procedure)

			/** Creates an SQL statement for a procedure call of this stored procedure using the arguments listed
			  * by the tuple expression created by the passed function from the facade to the domain `F` with all
			  * previously declared unbound parameters.
			  */
			def apply(args :JoinedMappings[F] => ChainTuple[F, GlobalScope, Xs]) :CallProcedure[Ps] =
				apply(args(new JoinedMappings(domain)))

			/** Creates an SQL statement for a procedure call of this stored procedure using the arguments listed
			  * by the tuple expression.
			  */
			def apply(args :ChainTuple[F, GlobalScope, Xs]) :CallProcedure[Ps] =
				ParamCallProcedure(domain)(procedure, args)

			/** Creates an SQL statement for a procedure call of this stored procedure and returning the values
			  * of its ''OUT'' parameters. The tuple expression returned by the passed function is used
			  * for the arguments of the procedure, and any parameters of type
			  * [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` can be substituted with `X`.
			  * Those which are not are included in type
			  * `signature.`[[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.Out Out]] provided
			  * by the implicit parameter matching the formal parameter types `Xs` to the arguments tuple type `In`.
			  * @param args a function accepting as its argument a facade to the domain type `F` with all previously
			  *             declared parameters used in creation of the call argument(s).
			  */
			def out[In <: Chain](args :JoinedMappings[F] => ChainTuple[F, GlobalScope, In])
			                    (implicit signature :ProcedureSignature[Xs, In]) :Call[Ps, signature.Out] =
				out(args(new JoinedMappings(domain)))

			/** Creates an SQL statement for a procedure call of this stored procedure and returning the values
			  * of its ''OUT'' parameters. The tuple expression `args` is used for the arguments of the procedure,
			  * and any parameters of type [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]`
			  * can be substituted with `X`. Those which are not are included in type
			  * `signature.`[[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.Out Out]] provided
			  * by the implicit parameter matching the formal parameter types `Xs` to the arguments tuple type `In`.
			  */
			def out[In <: Chain](args :ChainTuple[F, GlobalScope, In])(implicit signature :ProcedureSignature[Xs, In])
					:Call[Ps, signature.Out] =
				signature(domain, procedure)(args)
		}


		/** Fragment of the [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] ''visitor''
		  * covering the cases of [[net.noresttherein.oldsql.sql.Call.CallProcedure CallProcedure]] statements
		  * ''which, as an exception do not extend'' [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]],
		  * [[net.noresttherein.oldsql.sql.Call.InOutCallProcedure InOutCallProcedure]]'' or ''
		  * [[net.noresttherein.oldsql.sql.Call.InOutCallFunction InOutCallFunction]] - that is, 'true' procedure calls
		  * not returning any ''OUT'' parameters.
		  * This includes empty methods for every implementation type in the hierarchy as well as `CallProcedure` itself.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  * @see [[net.noresttherein.oldsql.sql.Call.CallProcedure.CaseProcedure]]
		  * @see [[net.noresttherein.oldsql.sql.Call.CallVisitor]]
		  */
		trait ProcedureVisitor[R[-X, +Y]] {
			def procedure[X](stmt :CallProcedure[X])                                    :R[X, Any]
			def directProcedure[X <: Chain](stmt :DirectCallProcedure[X])               :R[X, Any]
			def paramProcedure[X <: Chain](stmt :ParamCallProcedure[X])                 :R[X, Any]
			def groundProcedure(stmt :GroundCallProcedure)                              :R[Unit, Any]
		}

		/** An alias for [[net.noresttherein.oldsql.sql.Call.CallProcedure.ProcedureVisitor ProcedureVisitor]]
		  * introduced for consistency with the 'assembled' ''visitor'' pattern, as there are no intermediate
		  * types between [[net.noresttherein.oldsql.sql.Call.CallProcedure CallProcedure]] and its full
		  * (if not concrete) implementations.
		  */
		type MatchProcedure[R[-X, +Y]] = ProcedureVisitor[R]

		/** A mix-in trait for [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]]
		  * ''visitors'' of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] type hierarchy. It implements
		  * all methods of [[net.noresttherein.oldsql.sql.Call.CallProcedure.ProcedureVisitor ProcedureVisitor]]
		  * for concrete [[net.noresttherein.oldsql.sql.Call.CallProcedure CallProcedure]] implementations
		  * ''which, as an exception, do not extend any of ''
		  * [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]],
		  * [[net.noresttherein.oldsql.sql.Call.InOutCallProcedure InOutCallProcedure]] ''and''
		  * [[net.noresttherein.oldsql.sql.Call.InOutCallFunction InOutCallFunction]] ''traits'', by delegating them
		  * to method for `CallProcedure` itself, leaving it as the only open case.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  */
		trait CaseProcedure[R[-X, +Y]] extends ProcedureVisitor[R] {
			override def directProcedure[X <: Chain](stmt :DirectCallProcedure[X]) :R[X, Any]  = procedure(stmt)
			override def paramProcedure[X <: Chain](stmt :ParamCallProcedure[X])   :R[X, Any]  = procedure(stmt)
			override def groundProcedure(stmt :GroundCallProcedure)                :R[Unit, Any] = procedure(stmt)
		}
	}




	object CallFunction {
		/** Factory method for statements invoking `function` and returning its result.
		  * This method variant requires the values for all the arguments to be known (to be literals,
		  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters or expressions built on them)
		  * and creates a parameterless `Call` instance.
		  * @tparam Params   types of the formal parameters of the called function.
		  * @tparam Y        return type of the called function.
		  * @param function  the invoked stored function or SQL built-in function.
		  * @param args      a ground tuple expression listing the values of all arguments for the function.
		  *                  The types of the tuple elements must match exactly formal parameter types of the function.
		  * @return a statement invoking the function using the JDBC function call syntax.
		  */
		def GroundCallFunction[Params <: Chain, Y]
		                      (function :StoredFunction[Params, Y], args :ChainTuple[RowProduct, GlobalScope, Params])
				:GroundCallFunction[Y] =
			new GroundCallFunctionImpl(function, args)

		/** A parameterless statement invoking a [[net.noresttherein.oldsql.sql.StoredProcedure stored function]]
		  * (or a standard SQL function), returning its result.
		  * Note that ''parameterless'' here means that no arguments need be provided to execute this statement;
		  * the actual JDBC [[java.sql.CallableStatement CallableStatement]] may still be parameterized using
		  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameter expressions.
		  */
		trait GroundCallFunction[Res] extends CallFunction[Unit, Res] with GroundCall[Res] {
			protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Unit, Res] =
				visitor.groundFunction(this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Unit] = {
				val gap = Parameterization.paramless[ParamlessFrom] :+ ColumnWriteForm.gap
				val call = spelling.inline(args)(Dual, spelling.newContext, gap)
				val sql = ("{?= call " + spelling.function(procedure.name) + '(') +: (call.sql + ")}")
				SpelledSQL(sql, call.context, call.setter)
			}

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroundCallFunction[_]]
		}

		private class GroundCallFunctionImpl[Args <: Chain, Res]
		                                    (override val procedure :StoredFunction[Args, Res],
		                                     override val args :ChainTuple[RowProduct, GlobalScope, Args])
			extends GroundCallFunction[Res] //with CallFunction[Any, Res] with GroundDML.Impl[Res]
		{
			override type Params = Args
			override val result = super.result
		}



		/** Factory method for statements invoking `function` and returning its result.
		  * @tparam Args     types of unbound parameters of the argument expression given for the call.
		  * @tparam Params   types of the formal parameters of the called function.
		  * @tparam Y        return type of the called function.
		  * @param domain    a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] consisting solely of
		  *                  [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins declaring the parameters
		  *                  used by the argument expression `args`. It ''cannot'' contain any tables, as it would
		  *                  result in an invalid SQL being generated at a later point.
		  * @param function  the invoked stored function or a built-in SQL function.
		  * @param args      a tuple expression based on `domain`'s [[net.noresttherein.oldsql.sql.RowProduct.Self self]]
		  *                  type, listing the values of all arguments for the function. The types of the tuple elements
		  *                  must match exactly formal parameter types of the function.
		  * @return a statement invoking the function using the JDBC function call syntax.
		  */
		def ParamCallFunction[Args <: Chain, Params <: Chain, Y]
		                     (domain :PureParamFrom[Args])
		                     (function :StoredFunction[Params, Y], args :ChainTuple[domain.Self, GlobalScope, Params])
				:ParamCallFunction[Args, Y] =
			new ParamCallFunctionImpl[domain.type, domain.Self, Args, Params, Y](domain, function, args)

		/** A parameterized call of a [[net.noresttherein.oldsql.sql.StoredFunction stored function]]
		  * providing arbitrary SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] as arguments
		  * and returning its return value.
		  */
		trait ParamCallFunction[-Args <: Chain, Res] extends CallFunction[Args, Res] with ParamCall[Args, Res] {
			protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] =
				visitor.paramFunction(this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args] = {
				val call = spelling.inline(args)(domain.self, domain.spellingContext, domain.parameterization)
				val sql = (("{?= call " + spelling.function(procedure.name) + '(') +: call.sql) + ")}"
				SpelledSQL(sql, call.context, ColumnWriteForm.gap + call.setter)
			}

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamCallFunction[_, _]]
		}

		private class ParamCallFunctionImpl[D <: PureParamFrom[Args] { type Self <: F },
		                                    F <: ParameterizedFrom[Args], Args <: Chain, Xs <: Chain, Res]
		                                   (override val domain :D, override val procedure :StoredFunction[Xs, Res],
		                                    override val args :ChainTuple[F, GlobalScope, Xs])
			extends ParamCallFunction[Args, Res]
		{
			override type Bound = Args
			override type Params = Xs
			override type Domain = D
			override val result = super.result
		}




		private val returnIndex = 1::Nil

		/** An intermediate object adding a new [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter `P`
		  * to the `F &lt;: `[[net.noresttherein.oldsql.sql.RowProduct RowProduct]] domain serving as a base
		  * for the [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple parameter set]] of the call,
		  * implicitly convertible to
		  * [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams CallFunctionParams]]`[F WithParam P, Ps ~ P, Xs, Y]`
		  * It has the following (extension) methods for adding additional parameters and creating
		  * an expression using them:
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply[P]* this]]`[Q]` -
		  *      adds a new parameter of type `Q` to the expression's domain: `F WithParam P WithParam Q`;
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply[N<:Label,P]* this]]`["Q", Q]` -
		  *   adds a new parameter named "Q" and of type `Q` to the expression's domain:
		  *   `F WithParam P WithParam Q As "Q"`;
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:ChainTuple[F,GlobalScope,Xs])* this]]`(argsTuple)`
		  *      creates a new expression using a constructor function accepting
		  *      [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F WithParam P]`.*
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:JoinedMappings[F]=>ChainTuple[F,GlobalScope,Xs] this]]`(params => argsTuple)` -
		  *      creates a new expression using a constructor function accepting
		  *      [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F WithParam P]`.
		  *
		  * This class exists as Scala 2 would not allow chaining of methods with explicit type parameters
		  * but only implicit parameter lists.
		  * @tparam F  a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting ''solely'' of
		  *            [[net.noresttherein.oldsql.sql.WithParam WithParam]] expressions - each possibly with an
		  *            [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause. It is the ''from'' clause on which
		  *            created expressions are based.
		  * @tparam Ps a chain consisting of types of all unbound parameters of `F`, forming the parameters of the
		  *            created `Call` statement. Any wildcard prefix type of `F` is ignored and only unbound parameters
		  *            following it are included in `Ps`, which always starts with `@~`.
		  * @tparam P  the type of the new parameter.
		  */
		class CallFunctionParamDecl[F <: FromSome, Ps <: Chain, P, Xs <: Chain, Y]
		                           (protected val domain :F ParameterizedWith Ps,
		                            protected val function :StoredFunction[Xs, Y])

		object CallFunctionParamDecl {
			def apply[P, Xs <: Chain, Y](function :StoredFunction[Xs, Y])
					:CallFunctionParamDecl[PureParamFrom[@~], @~, P, Xs, Y] =
				new CallFunctionParamDecl[PureParamFrom[@~], @~, P, Xs, Y](
					PureParamFrom.empty.asInstanceOf[PureParamFrom[@~] ParameterizedWith @~], function
				)

			implicit def domain[F <: PureParamFrom[Ps], Ps <: Chain, P :SQLForm, Xs <: Chain, Y]
			                   (param :CallFunctionParamDecl[F, Ps, P, Xs, Y])
					:CallFunctionParams[F WithParam P, Ps ~ P, Xs, Y] =
				new CallFunctionParams(JoinParam(param.domain, ?:[P]), param.function)
		}

		/** An intermediate object adding a new [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameter `P`
		  * to the `F &lt;: `[[net.noresttherein.oldsql.sql.RowProduct RowProduct]] domain serving as a base
		  * for the [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple parameter set]] of a stored function call,
		  * implicitly convertible to
		  * [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams CallFunctionParams]]`[F WithParam P As N, Ps ~ P, Xs, Y]`
		  * It has the following (extension) methods for adding additional parameters and creating
		  * an expression using them:
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply[P]* this]]`[Q]` -
		  *      adds a new parameter of type `Q` to the expression's domain: `F WithParam P WithParam Q`;
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply[N<:Label,P]* this]]`["Q", Q]` -
		  *   adds a new parameter named "Q" and of type `Q` to the expression's domain:
		  *   `F WithParam P WithParam Q As "Q"`;
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:ChainTuple[F,GlobalScope,Xs])* this]]`(argsTuple)`
		  *      creates a new expression using a constructor function accepting
		  *      [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F WithParam P As N]`.*
		  *   1. [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply(args:JoinedMappings[F]=>ChainTuple[F,GlobalScope,Xs] this]]`(params => argsTuple)` -
		  *      creates a new expression using a constructor function accepting
		  *      [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F WithParam P As N]`.
		  *
		  * This class exists as Scala 2 would not allow chaining of methods with explicit type parameters
		  * but only implicit parameter lists.
		  * @tparam F  a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting ''solely'' of
		  *            [[net.noresttherein.oldsql.sql.WithParam WithParam]] expressions - each possibly with an
		  *            [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause. It is the ''from'' clause on which
		  *            created expressions are based.
		  * @tparam Ps a chain consisting of types of all unbound parameters of `F`, forming the parameters of the
		  *            created `Call` statement. Any wildcard prefix type of `F` is ignored and only unbound parameters
		  *            following it are included in `Ps`, which always starts with `@~`.
		  * @tparam N  a `String` literal with the name of the new parameter.
		  * @tparam P  the type of the new parameter.
		  */
		class CallFunctionNamedParamDecl[F <: FromSome, Ps <: Chain, N <: Label, P, Xs <: Chain, Y]
		                                (protected val domain :F ParameterizedWith Ps,
		                                 protected val function :StoredFunction[Xs, Y])

		object CallFunctionNamedParamDecl {
			def apply[N <: Label, P, Xs <: Chain, Y](function :StoredFunction[Xs, Y])
					:CallFunctionNamedParamDecl[PureParamFrom[@~], @~, N, P, Xs, Y] =
				new CallFunctionNamedParamDecl[PureParamFrom[@~], @~, N, P, Xs, Y](
					PureParamFrom.empty.asInstanceOf[PureParamFrom[@~] ParameterizedWith @~], function
				)

			implicit def domain[F <: PureParamFrom[Ps], Ps <: Chain, N <: Label :ValueOf, P :SQLForm, Xs <: Chain, Y]
			                   (param :CallFunctionNamedParamDecl[F, Ps, N, P, Xs, Y])
					:CallFunctionParams[F WithParam P As N, Ps ~ P, Xs, Y] =
				new CallFunctionParams(JoinParam(param.domain, ?:[N, P]), param.function)
		}

		/** A factory of [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]] SQL statements which execute
		  * a stored function carried by this object. It accepts an SQL
		  * [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple ChainTuple]]`[F, GlobalScope, Ps]` expression
		  * containing all arguments for the function - either directly, or as a function of
		  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[F]`.
		  * It also allows to introduce additional parameters by chaining parameterless calls of
		  * [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply[P] this[P]]] and
		  * [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams.apply[N<:Label,P] this]]`[N, P]`
		  * behaving identically to the counterpart methods of the methods of `StoredFunction` which initiate
		  * the construction of this instance.
		  * @tparam F  a [[net.noresttherein.oldsql.sql.FromClause FromClause]] consisting ''solely'' of
		  *            [[net.noresttherein.oldsql.sql.WithParam WithParam]] expressions - each possibly with an
		  *            [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause.
		  * @tparam Ps a chain consisting of types of all unbound parameters of `F`, forming the parameters of the
		  *            created SQL statement. This is not related to the chain with types of the actual parameters of
		  *            the Function itself - rather, the arguments for the Function can be any SQL
		  *            [[net.noresttherein.oldsql.sql.SQLExpression expressions]] using `F` as their base.
		  * @tparam Xs a chain listing the types of all formal parameters of the invoked function
		  * @tparam Y  return type of the function.
		  */
		class CallFunctionParams[F <: PureParamFrom[Ps], Ps <: Chain, Xs <: Chain, Y]
		                        (domain :F ParameterizedWith Ps, procedure :StoredFunction[Xs, Y])
		{
			/** Add another unbound parameter to the parameters of the created
			  * [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]].
			  * @return an object implicitly convertible to a
			  *         [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams CallFunctionParams]]`[F With Param P, Ps ~ P, Xs, Y]`.
			  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
			  *           used by the future SQL expression with arguments for the function.
			  * @see [[net.noresttherein.oldsql.sql.StoredFunction.apply[P]]]
			  */
			def apply[P] :CallFunctionParamDecl[F, Ps, P, Xs, Y] =
				new CallFunctionParamDecl[F, Ps, P, Xs, Y](domain, procedure)

			/** Add another named unbound parameter to the parameters of the created
			  * [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]].
			  * @return an object implicitly convertible to a
			  *         [[net.noresttherein.oldsql.sql.Call.CallFunction.CallFunctionParams CallFunctionParams]]`[F With Param P, Ps ~ P, Xs, Y]`.
			  * @see [[net.noresttherein.oldsql.sql.StoredFunction.apply[N<:Label,P]]]
			  * @tparam P the type of the - currently last - parameter of the `RowProduct` with the unbound parameters
			  *           used by the future SQL expression with arguments for the function.
			  * @tparam N a `String` literal type with the name of the parameter, which can be used to access it
			  *           from [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] for the whole
			  *           domain when creating the SQL expression with the arguments for the function.
			  */
			def apply[N <: Label, P] :CallFunctionNamedParamDecl[F, Ps, N, P, Xs, Y] =
				new CallFunctionNamedParamDecl[F, Ps, N, P, Xs, Y](domain, procedure)

			/** Creates an SQL statement for a Function call of this stored function using the arguments listed
			  * by the tuple expression created by the passed function from the facade to the domain `F` with all
			  * previously declared unbound parameters.
			  */
			def apply(args :JoinedMappings[F] => ChainTuple[F, GlobalScope, Xs]) :CallFunction[Ps, Y] =
				apply(args(new JoinedMappings(domain)))

			/** Creates an SQL statement for a function call of this stored Function using the arguments listed
			  * by the tuple expression.
			  */
			def apply(args :ChainTuple[F, GlobalScope, Xs]) :CallFunction[Ps, Y] =
				ParamCallFunction(domain)(procedure, args)

			/** Creates an SQL statement for a call of this stored function, returning its return value and the values
			  * of its ''OUT'' parameters. The tuple expression returned by the passed function is used
			  * for the arguments of the function, and any parameters of type
			  * [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` can be substituted with `X`.
			  * Those which are not are included in type
			  * `signature.`[[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.Out Out]] provided
			  * by the implicit parameter matching the formal parameter types `Xs` to the arguments tuple type `In`.
			  * @param args a function accepting as its argument a facade to the domain type `F` with all previously
			  *             declared parameters used in creation of the call argument(s).
			  */
			def out[In <: Chain](args :JoinedMappings[F] => ChainTuple[F, GlobalScope, In])
			                    (implicit signature :ProcedureSignature[Xs, In]) :Call[Ps, signature.FunctionResult[Y]] =
				out(args(new JoinedMappings(domain)))

			/** Creates an SQL statement for a call of this stored function, returning its return value and the values
			  * of its ''OUT'' parameters. The tuple expression `args` is used for the arguments of the function,
			  * and any parameters of type [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]`
			  * can be substituted with `X`. Those which are not are included in type
			  * `signature.`[[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.Out Out]] provided
			  * by the implicit parameter matching the formal parameter types `Xs` to the arguments tuple type `In`.
			  */
			def out[In <: Chain](args :ChainTuple[F, GlobalScope, In])
			                    (implicit signature :ProcedureSignature[Xs, In]) :Call[Ps, signature.FunctionResult[Y]] =
				signature(domain, procedure)(args)
		}


		/** Fragment of the [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] ''visitor''
		  * covering the cases of [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]] statements -
		  * calls of a [[net.noresttherein.oldsql.sql.StoredFunction stored function]] returning only its value
		  * and not not any ''OUT'' parameters.
		  * This includes empty methods for every implementation type in the hierarchy as well as `CallFunction` itself.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  * @see [[net.noresttherein.oldsql.sql.Call.CallFunction.CaseFunction]]
		  * @see [[net.noresttherein.oldsql.sql.Call.CallVisitor]]
		  */
		trait FunctionVisitor[R[-X, +Y]] {
			def function[X, Y](stmt :CallFunction[X, Y])                      :R[X, Y]
			def directFunction[X <: Chain, Y](stmt :DirectCallFunction[X, Y]) :R[X, Y]
			def paramFunction[X <: Chain, Y](stmt :ParamCallFunction[X, Y])   :R[X, Y]
			def groundFunction[Y](stmt :GroundCallFunction[Y])                :R[Unit, Y]
		}

		/** An alias for [[net.noresttherein.oldsql.sql.Call.CallFunction.FunctionVisitor FunctionVisitor]]
		  * introduced for consistency with the 'assembled' ''visitor'' pattern, as there are no intermediate
		  * types between [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]] and its full
		  * (if not concrete) implementations.
		  */
		type MatchFunction[R[-X, +Y]] = FunctionVisitor[R]

		/** A mix-in trait for [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]]
		  * ''visitors'' of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] type hierarchy. It implements
		  * all methods of [[net.noresttherein.oldsql.sql.Call.CallFunction.FunctionVisitor FunctionVisitor]]
		  * for concrete [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]] implementations -
		  * statements calling a [[net.noresttherein.oldsql.sql.StoredFunction stored function]] and returning only
		  * the value it returns - by delegating them to method for `CallFunction` itself,
		  * leaving it as the only open case.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  */
		trait CaseFunction[R[-X, +Y]] extends MatchFunction[R] {
			override def directFunction[X <: Chain, Y](stmt :DirectCallFunction[X, Y]) :R[X, Y]  = function(stmt)
			override def paramFunction[X <: Chain, Y](stmt :ParamCallFunction[X, Y])   :R[X, Y]  = function(stmt)
			override def groundFunction[Y](stmt :GroundCallFunction[Y])                :R[Unit, Y] = function(stmt)
		}
	}




	object InOutCallProcedure {
		/** Factory method for statements invoking `procedure` and returning some combination of the values
		  * of its declared ''OUT'' parameters. All ''OUT'' parameters of a procedure are declared as type
		  * [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` rather than simply `X` in `Params`.
		  * The choice of which to return is made by similarly marking the provided arguments by wrapping them
		  * in `Out` (see [[net.noresttherein.oldsql.sql.SQLExpression.out SQLExpression.out]] method): any argument
		  * given in `In` as `Out[X]` - as in the formal parameters of the procedure - has its value returned.
		  * Passing an argument as `X` instead of `Out[X]` omits it from this list, treating it as another
		  * ''IN'' parameter. If the type of a parameter (an element type of the argument tuple) is not known
		  * to be `Out`, it is similarly treated as an ''IN'' parameter, even if its dynamic type is in fact
		  * an `Out` subtype. Additionally, a tuple matching the formal parameter types `Params` of this procedure
		  * will always be accepted, even if `Args` is not a fully instantiated type (including having a wildcard `Chain`
		  * as its beginning or being an abstract type); the return type becomes `Any` in that case,
		  * with no ''OUT'' parameters being returned. This method relies on implicit evidence `signature`
		  * matching the formal parameter types of the procedure `Params` with the types of the provided arguments `In`
		  * of the tuple expression `args`. The result type `signature.Out`
		  * is a [[net.noresttherein.oldsql.collection.Chain Chain]] listing all parameters `X` declared as `Out[X]`
		  * in `In`, in the order of their appearance.
		  * This method variant requires the values for all the arguments to be known (to be literals,
		  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters or expressions built on them)
		  * and creates a parameterless `Call` instance.
		  * @tparam Params   types of the formal parameters of the called procedure.
		  * @tparam In       types of the arguments given as a tuple.
		  * @param procedure the invoked stored procedure or function.
		  * @param args      a ground tuple expression listing the values of all arguments for the procedure.
		  * @param signature the signature of the call, that is matching of the parameters to their arguments,
		  *                  providing the return type
		  *                  [[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.ProcedureResult ProcedureResult]]
		  *                  - the result type of the created `Call`.
		  * @return a statement invoking the procedure using the JDBC procedure call syntax.
		  */
		def GroundInOutCallProcedure[Params <: Chain, In <: Chain]
		                            (procedure :StoredProcedure[Params], args :ChainTuple[RowProduct, GlobalScope, In])
		                            (implicit signature :ProcedureInOutSignature[Params, In])
				:GroundInOutCallProcedure[signature.Out] =
			new GroundInOutCallProcedureImpl(procedure, signature(args), signature.outParamIndices(procedure))(
				signature.outForm(procedure.paramForm)
			)

		/** A parameterless statement invoking a [[net.noresttherein.oldsql.sql.StoredProcedure stored procedure]],
		  * returning the values of (some of) its ''OUT'' parameters.
		  * Note that ''parameterless'' here means that no arguments need be provided to execute this statement;
		  * the actual JDBC [[java.sql.CallableStatement CallableStatement]] may still be parameterized using
		  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameter expressions.
		  */
		trait GroundInOutCallProcedure[+Res]
			extends InOutCallProcedure[Unit, Res] with GroundCallProcedure with GroundCall[Res]
		{
			protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Unit, Res] =
				visitor.inOutGroundProcedure(this)

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroundInOutCallProcedure[_]]
		}

		private class GroundInOutCallProcedureImpl[Xs <: Chain, Res]
		                                          (override val procedure :StoredProcedure[Xs],
		                                           override val args :ChainTuple[RowProduct, GlobalScope, Xs],
		                                           override val outParamIndices :Seq[Int])
		                                          (implicit override val outParamsForm :SQLReadForm[Res])
			extends GroundInOutCallProcedure[Res]// with InOutCallProcedure[Any, Res] with GroundDML.Impl[Res]
		{
			override type Params = Xs
		}



		/** Factory method for statements invoking `procedure` and returning some combination of the values
		  * of its declared ''OUT'' parameters. All ''OUT'' parameters of a procedure are declared as type
		  * [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` rather than simply `X` in `Params`.
		  * The choice of which to return is made by similarly marking the provided arguments by wrapping them
		  * in `Out` (see [[net.noresttherein.oldsql.sql.SQLExpression.out SQLExpression.out]] method): any argument
		  * given in `In` as `Out[X]` - as in the formal parameters of the procedure - has its value returned.
		  * Passing an argument as `X` instead of `Out[X]` omits it from this list, treating it as another
		  * ''IN'' parameter. If the type of a parameter (an element type of the argument tuple) is not known
		  * to be `Out`, it is similarly treated as an ''IN'' parameter, even if its dynamic type is in fact
		  * an `Out` subtype. Additionally, a tuple matching the formal parameter types `Params` of this procedure
		  * will always be accepted, even if `Args` is not a fully instantiated type (including having a wildcard `Chain`
		  * as its beginning or being an abstract type); the return type becomes `Any` in that case,
		  * with no ''OUT'' parameters being returned. This method relies on implicit evidence `signature`
		  * matching the formal parameter types of the procedure `Params` with the types of the provided arguments `In`
		  * of the tuple expression `args`. The result type `signature.Out`
		  * is a [[net.noresttherein.oldsql.collection.Chain Chain]] listing all parameters `X` declared as `Out[X]`
		  * in `In`, in the order of their appearance.
		  * @tparam Args     types of unbound parameters of the argument expression given for the call.
		  * @tparam Params   types of the formal parameters of the called procedure.
		  * @tparam In       types of the arguments given as a tuple.
		  * @param domain    a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] consisting solely of
		  *                  [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins declaring the parameters
		  *                  used by the argument expression `args`. It ''cannot'' contain any tables (which is enforced
		  *                  by the `signature` implicit evidence).
		  * @param procedure the invoked stored procedure or function.
		  * @param args      a tuple expression based on `domain`'s [[net.noresttherein.oldsql.sql.RowProduct.Self self]]
		  *                  type, listing the values of all arguments for the procedure.
		  * @param signature the signature of the call, that is matching of the parameters to their arguments,
		  *                  providing the return type
		  *                  [[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.ProcedureResult ProcedureResult]]
		  *                  - the result type of the created `Call`.
		  * @return a statement invoking the procedure using the JDBC procedure call syntax.
		  */
		def ParamInOutCallProcedure[Args <: Chain, Params <: Chain, In <: Chain]
		                           (domain :PureParamFrom[Args])
		                           (procedure :StoredProcedure[Params], args :ChainTuple[domain.Self, GlobalScope, In])
		                           (implicit signature :ProcedureInOutSignature[Params, In])
				:ParamInOutCallProcedure[Args, signature.Out] =
			new ParamInOutCallProcedureImpl[domain.type, domain.Self, Args, Params, signature.Out](
				domain, procedure, signature(args), signature.outParamIndices(procedure)
			)(signature.outForm(procedure.paramForm))

		/** A parameterized call of a [[net.noresttherein.oldsql.sql.StoredProcedure stored procedure]]
		  * providing arbitrary SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] as arguments
		  * and returning its ''OUT'' parameters as `Res`.
		  */
		trait ParamInOutCallProcedure[-Args <: Chain, +Res]
			extends ParamCallProcedure[Args] with InOutCallProcedure[Args, Res]
		{
			protected override def applyTo[R[-X, +Y]](visitor :StatementVisitor[R]) :R[Args, Res] =
				visitor.inOutParamProcedure(this)

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamInOutCallProcedure[_, _]]
		}

		private class ParamInOutCallProcedureImpl[D <: PureParamFrom[Args] { type Self <: F; type Params = Args },
		                                          F <: ParameterizedFrom[Args], Args <: Chain, Xs <: Chain, Res <: Chain]
		                                         (domain :D, procedure :StoredProcedure[Xs],
		                                          params :ChainTuple[F, GlobalScope, Xs],
		                                          override val outParamIndices :Seq[Int])
		                                         (implicit override val outParamsForm :SQLReadForm[Res])
			extends ParamCallProcedureImpl[D, F, Args, Xs](domain, procedure, params)
			   with ParamInOutCallProcedure[Args, Res]




		/** Fragment of the [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] ''visitor''
		  * covering the cases of [[net.noresttherein.oldsql.sql.Call.InOutCallProcedure InOutCallProcedure]] statements -
		  * calls of a [[net.noresttherein.oldsql.sql.StoredProcedure stored procedure]] returning
		  * (some of) its ''OUT'' parameters. This includes empty methods for every implementation type in the hierarchy
		  * as well as `InOutCallProcedure` itself.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  * @see [[net.noresttherein.oldsql.sql.Call.InOutCallProcedure.CaseInOutProcedure]]
		  * @see [[net.noresttherein.oldsql.sql.Call.CallVisitor]]
		  */
		trait InOutProcedureVisitor[R[-X, +Y]] {
			def inOutProcedure[X, Y](stmt :InOutCallProcedure[X, Y])                    :R[X, Y]
			def inOutParamProcedure[X <: Chain, Y](stmt :ParamInOutCallProcedure[X, Y]) :R[X, Y]
			def inOutGroundProcedure[Y](stmt :GroundInOutCallProcedure[Y])              :R[Unit, Y]
		}

		/** An alias for [[net.noresttherein.oldsql.sql.Call.InOutCallProcedure.InOutProcedureVisitor InOutProcedureVisitor]]
		  * introduced for consistency with the 'assembled' ''visitor'' pattern, as there are no intermediate
		  * types between [[net.noresttherein.oldsql.sql.Call.InOutCallProcedure InOutCallProcedure]] and its full
		  * (if not concrete) implementations.
		  */
		type MatchInOutProcedure[R[-X, +Y]] = InOutProcedureVisitor[R]

		/** A mix-in trait for [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]]
		  * ''visitors'' of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] type hierarchy. It implements
		  * all methods of [[net.noresttherein.oldsql.sql.Call.InOutCallProcedure.InOutProcedureVisitor InOutProcedureVisitor]]
		  * for concrete [[net.noresttherein.oldsql.sql.Call.InOutCallProcedure InOutCallProcedure]] implementations -
		  * statements calling a [[net.noresttherein.oldsql.sql.StoredProcedure stored procedure]] and returning
		  * its ''OUT'' parameters - by delegating them to method for `InOutCallProcedure` itself, leaving it
		  * as the only open case.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  */
		trait CaseInOutProcedure[R[-X, +Y]] extends InOutProcedureVisitor[R] {
			override def inOutParamProcedure[X <: Chain, Y](stmt :ParamInOutCallProcedure[X, Y]) :R[X, Y] =
				inOutProcedure(stmt)
			override def inOutGroundProcedure[Y](stmt :GroundInOutCallProcedure[Y])              :R[Unit, Y] =
				inOutProcedure(stmt)
		}
	}




	object InOutCallFunction {
		/** Factory method for statements invoking `function` and returning its result together with some combination
		  * of the values of its declared ''OUT'' parameters. All ''OUT'' parameters of a procedure/function
		  * are declared as type [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` rather than simply `X`
		  * in `Params`. The choice of which to return is made by similarly marking the provided arguments
		  * by wrapping them in `Out` (see [[net.noresttherein.oldsql.sql.SQLExpression.out SQLExpression.out]] method):
		  * any argument given in `In` as `Out[X]` - as in the formal parameters of the function -
		  * has its value returned. Passing an argument as `X` instead of `Out[X]` omits it from this list,
		  * treating it as another ''IN'' parameter. If the type of a parameter (an element type of the argument tuple)
		  * is not known to be `Out`, it is similarly treated as an ''IN'' parameter, even if its dynamic type is in fact
		  * an `Out` subtype. Additionally, a tuple matching the formal parameter types `Params` of this procedure
		  * will always be accepted, even if `Args` is not a fully instantiated type (including having a wildcard `Chain`
		  * as its beginning or being an abstract type); the return type becomes `Any` in that case,
		  * with no ''OUT'' parameters being returned. This method relies on implicit evidence `signature`
		  * matching the formal parameter types of the function `Params` with the types of the provided arguments `In`
		  * of the tuple expression `args`. The result type `signature.Out ~ Y`
		  * is a [[net.noresttherein.oldsql.collection.Chain Chain]] listing all parameters `X` declared as `Out[X]`
		  * in `In`, in the order of their appearance, followed by the value returned by the whole function.
		  * This method variant requires the values for all the arguments to be known (to be literals,
		  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters or expressions built on them)
		  * and creates a parameterless `Call` instance.
		  * @tparam Params   types of the formal parameters of the called function.
		  * @param function  the invoked stored function.
		  * @param args      a tuple expression based on `domain`'s [[net.noresttherein.oldsql.sql.RowProduct.Self self]]
		  *                  type, listing the values of all arguments for the function.
		  * @param signature the signature of the call, that is matching of the parameters to their arguments,
		  *                  providing the return type
		  *                  [[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.FunctionResult FunctionResult]]`[Y]`
		  *                  - the result type of the created `Call`.
		  * @return a statement invoking the function using the JDBC function call syntax.
		  */
		def GroundInOutCallFunction[Params <: Chain, In <: Chain, Y]
		                           (function :StoredFunction[Params, Y], args :ChainTuple[RowProduct, GlobalScope, In])
		                           (implicit signature :ProcedureInOutSignature[Params, In])
				:GroundInOutCallFunction[signature.Out, Y] =
			new GroundInOutCallFunctionImpl(function, signature(args), signature.outParamIndices(function))(
				signature.outForm(function.paramForm)
			)

		/** A parameterless statement invoking a [[net.noresttherein.oldsql.sql.StoredProcedure stored function]],
		  * returning its result together with the ''OUT'' parameters declared by the function.
		  * Note that ''parameterless'' here means that no arguments need be provided to execute this statement;
		  * the actual JDBC [[java.sql.CallableStatement CallableStatement]] may still be parameterized using
		  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameter expressions.
		  */
		trait GroundInOutCallFunction[+Out <: Chain, Y] extends InOutCallFunction[Unit, Out, Y] with GroundCall[Out ~ Y] {
			protected override def applyTo[R[-_, +_]](visitor :StatementVisitor[R]) :R[Unit, Out ~ Y] =
				visitor.inOutGroundFunction(this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Unit] = {
				val gap = Parameterization.paramless[ParamlessFrom] :+ ColumnWriteForm.gap
				val call = spelling.inline(args)(Dual, spelling.newContext, gap)
				val sql = ("{?= call " + spelling.function(procedure.name) + '(') +: (call.sql + ")}")
				SpelledSQL(sql, call.context, call.setter)
			}

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroundInOutCallFunction[_, _]]
		}

		private class GroundInOutCallFunctionImpl[Args <: Chain, Out <: Chain, Res]
		                                         (override val procedure :StoredFunction[Args, Res],
		                                          override val args :ChainTuple[RowProduct, GlobalScope, Args],
		                                          override val outParamIndices :Seq[Int])
		                                         (implicit override val outParamsForm :SQLReadForm[Out])
			extends GroundInOutCallFunction[Out, Res] //with InOutCallFunction[Any, Out, Res] with GroundDML.Impl[Out ~ Res]
		{
			override type Params = Args
		}



		/** Factory method for statements invoking `function` and returning its result together with some combination
		  * of the values of its declared ''OUT'' parameters. All ''OUT'' parameters of a procedure/function
		  * are declared as type [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` rather than simply `X`
		  * in `Params`. The choice of which to return is made by similarly marking the provided arguments
		  * by wrapping them in `Out` (see [[net.noresttherein.oldsql.sql.SQLExpression.out SQLExpression.out]] method):
		  * any argument given in `In` as `Out[X]` - as in the formal parameters of the function -
		  * has its value returned. Passing an argument as `X` instead of `Out[X]` omits it from this list,
		  * treating it as another ''IN'' parameter. If the type of a parameter (an element type of the argument tuple)
		  * is not known to be `Out`, it is similarly treated as an ''IN'' parameter, even if its dynamic type is in fact
		  * an `Out` subtype. Additionally, a tuple matching the formal parameter types `Params` of this procedure
		  * will always be accepted, even if `Args` is not a fully instantiated type (including having a wildcard `Chain`
		  * as its beginning or being an abstract type); the return type becomes `Any` in that case,
		  * with no ''OUT'' parameters being returned. This method relies on implicit evidence `signature`
		  * matching the formal parameter types of the function `Params` with the types of the provided arguments `In`
		  * of the tuple expression `args`. The result type `signature.Out ~ Y`
		  * is a [[net.noresttherein.oldsql.collection.Chain Chain]] listing all parameters `X` declared as `Out[X]`
		  * in `In`, in the order of their appearance, followed by the value returned by the whole function.
		  * @tparam Args     types of unbound parameters of the argument expression given for the call.
		  * @tparam Params   types of the formal parameters of the called function.
		  * @param domain    a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] consisting solely of
		  *                  [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo joins declaring the parameters
		  *                  used by the argument expression `args`. It ''cannot'' contain any tables (which is enforced
		  *                  by the `signature` implicit evidence).
		  * @param function  the invoked stored function.
		  * @param args      a tuple expression based on `domain`'s [[net.noresttherein.oldsql.sql.RowProduct.Self self]]
		  *                  type, listing the values of all arguments for the function.
		  * @param signature the signature of the call, that is matching of the parameters to their arguments,
		  *                  providing the return type
		  *                  [[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.FunctionResult FunctionResult]]`[Y]`
		  *                  - the result type of the created `Call`.
		  * @return a statement invoking the function using the JDBC function call syntax.
		  */
		def ParamInOutCallFunction[Args <: Chain, Params <: Chain, In <: Chain, Y]
		                          (domain :PureParamFrom[Args])
		                          (function :StoredFunction[Params, Y], args :ChainTuple[domain.Self, GlobalScope, In])
		                          (implicit signature :ProcedureInOutSignature[Params, In])
				:ParamInOutCallFunction[Args, signature.Out, Y] =
			new ParamInOutCallFunctionImpl[domain.type, domain.Self, Args, Params, signature.Out, Y](
				domain, function, signature(args), signature.outParamIndices(function)
			)(signature.outForm(function.paramForm))

		/** A parameterized call of a [[net.noresttherein.oldsql.sql.StoredFunction stored function]]
		  * providing arbitrary SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] as arguments
		  * and returning its return value as well as its ''OUT'' parameters.
		  */
		trait ParamInOutCallFunction[-Args <: Chain, +Out <: Chain, Y]
			extends InOutCallFunction[Args, Out, Y] with ParamCall[Args, Out ~ Y]
		{
			protected override def applyTo[R[-_, +_]](visitor :StatementVisitor[R]) :R[Args, Out ~ Y] =
				visitor.inOutParamFunction(this)

			protected override def defaultSpelling(implicit spelling :SQLSpelling) :SpelledSQL[Args] = {
				val call = spelling.inline(args)(domain.self, domain.spellingContext, domain.parameterization)
				val sql = (("{?= call " + spelling.function(procedure.name) + '(') +: call.sql) + ")}"
				SpelledSQL(sql, call.context, ColumnWriteForm.gap + call.setter)
			}

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[ParamInOutCallFunction[_, _, _]]
		}

		private class ParamInOutCallFunctionImpl[D <: PureParamFrom[Args] { type Self <: F },
		                                         F <: ParameterizedFrom[Args], Args <: Chain, Xs <: Chain, Out <: Chain, Res]
		                                        (override val domain :D, override val procedure :StoredFunction[Xs, Res],
		                                         override val args :ChainTuple[F, GlobalScope, Xs],
		                                         override val outParamIndices :Seq[Int])
		                                        (implicit override val outParamsForm :SQLReadForm[Out])
			extends ParamInOutCallFunction[Args, Out, Res]
		{
			override type Bound = Args
			override type Params = Xs
			override type Domain = D
		}




		/** Fragment of the [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] ''visitor''
		  * covering the cases of [[net.noresttherein.oldsql.sql.Call.InOutCallFunction InOutCallFunction]] statements -
		  * calls of a [[net.noresttherein.oldsql.sql.StoredFunction stored function]] returning its result together
		  * with (some of) its ''OUT'' parameters. This includes empty methods for every implementation type
		  * in the hierarchy as well as `InOutCallFunction` itself.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  * @see [[net.noresttherein.oldsql.sql.Call.InOutCallFunction.CaseInOutFunction]]
		  * @see [[net.noresttherein.oldsql.sql.Call.CallVisitor]]
		  */
		trait InOutFunctionVisitor[R[-X, +Y]] {
			def inOutFunction[X, Out <: Chain, Y](stmt :InOutCallFunction[X, Out, Y])                    :R[X, Out ~ Y]
			def inOutParamFunction[X <: Chain, Out <: Chain, Y](stmt :ParamInOutCallFunction[X, Out, Y]) :R[X, Out ~ Y]
			def inOutGroundFunction[Out <: Chain, Y](stmt :GroundInOutCallFunction[Out, Y])              :R[Unit, Out ~ Y]
		}

		/** An alias for [[net.noresttherein.oldsql.sql.Call.InOutCallFunction.InOutFunctionVisitor InOutFunctionVisitor]]
		  * introduced for consistency with the 'assembled' ''visitor'' pattern, as there are no intermediate
		  * types between [[net.noresttherein.oldsql.sql.Call.InOutCallFunction InOutCallFunction]] and its full
		  * (if not concrete) implementations.
		  */
		type MatchInOutFunction[R[-X, +Y]] = InOutFunctionVisitor[R]

		/** A mix-in trait for [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]]
		  * ''visitors'' of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] type hierarchy. It implements
		  * all methods of [[net.noresttherein.oldsql.sql.Call.InOutCallFunction.InOutFunctionVisitor InOutFunctionVisitor]]
		  * for concrete [[net.noresttherein.oldsql.sql.Call.InOutCallFunction InOutCallFunction]] implementations -
		  * statements calling a [[net.noresttherein.oldsql.sql.StoredFunction stored function]] and returning
		  * its result together with (some of) its ''OUT'' parameters - by delegating them to method
		  * for `InOutCallFunction` itself, leaving it as the only open case.
		  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
		  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
		  *           and its return type (the `Res` argument of the visited statement).
		  */
		trait CaseInOutFunction[R[-X, +Y]] extends MatchInOutFunction[R] {
			override def inOutParamFunction[X <: Chain, Out <: Chain, Y](stmt :ParamInOutCallFunction[X, Out, Y]) :R[X, Out ~ Y] =
				inOutFunction(stmt)
			override def inOutGroundFunction[Out <: Chain, Y](stmt :GroundInOutCallFunction[Out, Y]) :R[Unit, Out ~ Y] =
				inOutFunction(stmt)
		}
	}




	/** Fragment of the [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] ''visitor''
	  * covering the cases of all ''call'' JDBC statements. This includes empty methods for every existing type
	  * in the hierarchy, abstract or concrete.
	  * Note that instances of standard adapter classes,
	  * such as [[net.noresttherein.oldsql.sql.DMLStatement.BoundStatement BoundStatement]], which extend also
	  * [[net.noresttherein.oldsql.sql.Call Call]], will not be handled by any of these methods, but rather
	  * into the more generic ones declared by `StatementVisitor` itself to better reflect their nature -
	  * in their case, conforming to `Call` serves only a declarative purpose.
	  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
	  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
	  *           and its return type (the `Res` argument of the visited statement).
	  * @see [[net.noresttherein.oldsql.sql.Call.MatchCall]]
	  * @see [[net.noresttherein.oldsql.sql.Call.CaseCall]]
	  */
	trait CallVisitor[R[-X, +Y]]
		extends ProcedureVisitor[R] with InOutProcedureVisitor[R] with FunctionVisitor[R] with InOutFunctionVisitor[R]
	{
		def call[X, Y](call :Call[X, Y]) :R[X, Y]
	}

	/** A mix-in trait for [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] ''visitors''
	  * of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] type hierarchy. It implements all methods
	  * of [[net.noresttherein.oldsql.sql.Call.CallVisitor CallVisitor]] for concrete
	  * [[net.noresttherein.oldsql.sql.Call Call]] implementations by delegating them to the methods for their
	  * base traits, leaving unimplemented only the cases for the abstract types
	  * [[net.noresttherein.oldsql.sql.Call.CallProcedure CallProcedure]],
	  * [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]],
	  * [[net.noresttherein.oldsql.sql.Call.InOutCallProcedure InOutCallProcedure]],
	  * [[net.noresttherein.oldsql.sql.Call.InOutCallFunction InOutCallFunction]] and `Call` itself
	  * (for custom extensions not derived from any of the existing implementations).
	  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
	  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
	  *           and its return type (the `Res` argument of the visited statement).
	  * @see [[net.noresttherein.oldsql.sql.Call.CaseCall]]
	  */
	trait MatchCall[R[-X, +Y]]
		extends CallVisitor[R]
		   with CaseProcedure[R] with CaseInOutProcedure[R] with CaseFunction[R] with CaseInOutFunction[R]

	/** A mix-in trait for [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor StatementVisitor]] ''visitors''
	  * of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]] type hierarchy. It expands on
	  * [[net.noresttherein.oldsql.sql.Call.MatchCall MatchCall]] by further delegating the remaining open cases
	  * to the method for [[net.noresttherein.oldsql.sql.Call Call]] trait itself. Note that this scheme means
	  * that overriding [[net.noresttherein.oldsql.sql.Call.CaseCall.procedure procedure]] method will not 'catch'
	  * the cases of any of the not-really-a-procedure-call, which instead dispatch directly
	  * to root [[net.noresttherein.oldsql.sql.Call.call call]] method:
	  * [[net.noresttherein.oldsql.sql.Call.InOutCallProcedure InOutCallProcedure]]
	  * [[net.noresttherein.oldsql.sql.Call.CallFunction CallFunction]] and
	  * [[net.noresttherein.oldsql.sql.Call.InOutCallFunction InOutCallFunction]]. This multi-step dispatch allows
	  * overriding on the level of arbitrary nodes in the type hierarchy.
	  * @tparam R the return type of this visitor, parameterized with the type of the parameters of the statement
	  *           (the `Args` argument of [[net.noresttherein.oldsql.sql.DMLStatement DMLStatement]])
	  *           and its return type (the `Res` argument of the visited statement).
	  */
	trait CaseCall[R[-X, +Y]] extends MatchCall[R] {
		override def procedure[X](stmt :CallProcedure[X])                                  :R[X, Any]     = call(stmt)
		override def function[X, Y](stmt :CallFunction[X, Y])                              :R[X, Y]       = call(stmt)
		override def inOutFunction[X, Out <: Chain, Y](stmt :InOutCallFunction[X, Out, Y]) :R[X, Out ~ Y] = call(stmt)
		override def inOutProcedure[X, Y](stmt :InOutCallProcedure[X, Y])                  :R[X, Y]       = call(stmt)
	}

}




