package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{SQLForm, Table}
import net.noresttherein.oldsql.schema.bits.ConstantMapping
import net.noresttherein.oldsql.schema.forms.ChainForm
import net.noresttherein.oldsql.sql.{Call, Dual, From, JoinParam, RowProduct, SQLExpression, StoredFunction, StoredProcedure, WithParam}
import net.noresttherein.oldsql.sql.Call.{CallFunction, CallProcedure}
import net.noresttherein.oldsql.sql.Call.CallFunction.{GroundCallFunction, ParamCallFunction}
import net.noresttherein.oldsql.sql.Call.CallProcedure.{GroundCallProcedure, ParamCallProcedure}
import net.noresttherein.oldsql.sql.Call.InOutCallFunction.ParamInOutCallFunction
import net.noresttherein.oldsql.sql.Call.InOutCallProcedure.ParamInOutCallProcedure
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult.NoResult
import net.noresttherein.oldsql.sql.FromSome.TopFromSome
import net.noresttherein.oldsql.sql.ParamClause.ParamRelation
import net.noresttherein.oldsql.sql.RowProduct.{PartOf, PureParamFrom, SelfTyped}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.StoredProcedure.Out
import net.noresttherein.oldsql.sql.StoredProcedure.Out.OutForm
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.{ProcedureInOutSignature, ProcedureInSignature}
import net.noresttherein.oldsql.sql.JoinParam.FromParam.EmptyClause






/** The signature of an invocation of a stored procedure
  * [[net.noresttherein.oldsql.sql.StoredProcedure StoredProcedure]]`[Params]`, passing an
  * SQL [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[_, _Args]` as its arguments.
  * Implicit values exist for all valid combinations, which include direct application to `Params`,
  * as well as chains substituting any [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` type marking
  * an ''OUT'' parameter of the procedure with `X`. The return type of the created
  * [[net.noresttherein.oldsql.sql.Call Call]] DML is specified as
  * [[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.ProcedureResult ProcedureResult]] for a procedure call
  * (including invocations of stored functions using the procedure call syntax) and
  * [[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.FunctionResult FunctionResult]]`[Y]` for a call of
  * [[net.noresttherein.oldsql.sql.StoredFunction StoredFunction]]`[Params, Y]`. It is determined based on which
  * arguments have been passed in ''IN/OUT'' mode: any occurrence of type
  * [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` within `Params` marks an `OUT` parameter
  * of the procedure/function and passing an argument value conforming to this type appends it to type
  * [[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.Out Out]] with returned ''OUT'' parameters.
  * It is however also possible to pass a value of `X` instead of `Out[X]`, in which case the call treats
  * the parameter as an ''IN'' parameter and does not include it in the returned type. In the most common use
  * cases, the chains `Params` and `Args` are of the same length and equal on any non-''OUT'' parameters,
  * with the only allowed variation being a substitution of `Out[X]` in `Params` for `X`. In principle however,
  * the two type arguments of this class need not be related at all, with the value of the `Args` being a completely
  * arbitrary type from which the value for `Params` can be inferred. One example of such an exception to this rule
  * is in the default implicit value of `ProcedureSignature[Params, Params]`
  * for any abstract [[net.noresttherein.oldsql.collection.Chain Chain]] subtype, which always allows passing
  * an expression matching the types of procedure's formal parameters. In this case ''OUT'' parameters are not returned,
  * and the [[net.noresttherein.oldsql.sql.Call Call]] returns an unspecified value for procedures or the
  * declared return type for functions.
  * @tparam Params a [[net.noresttherein.oldsql.collection.Chain Chain]] listing the types of all formal parameters
  *                of a stored procedure/function.
  * @tparam Args   a `Chain` listing the types of all supplied arguments, which can be converted to `Params`,
  *                both as `Args => Params` and `ChainTuple[F, S, Args] => ChainTuple[F, S, Params]`.
  */
sealed trait ProcedureSignature[Params <: Chain, Args <: Chain] {
	/** The types of all ''OUT'' parameters of the procedure, that is all members of the `Param` chain of type
	  * `Out[X]`, listed in the same order as they appear in `Params`. */
	type Out <: ProcedureResult

	/** The type returned by the created [[net.noresttherein.oldsql.sql.Call Call]] statement when a procedure
	  * with parameters `Params` is applied to arguments `Args`. */
	type ProcedureResult

	/** The type returned by the created [[net.noresttherein.oldsql.sql.Call Call]] statement when a
	  * [[net.noresttherein.oldsql.sql.StoredFunction StoredFunction]]`[Params, Y]` is applied to arguments `Args`.
	  * By default, it combines [[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.Out Out]] parameter chain
	  * with `Y` in a single value.
	  */
	type FunctionResult[Y]// <: ProcedureResult

	/** An enumeration of all argument types in `Args` chain as [[net.noresttherein.oldsql.sql.JoinParam unbound]]
	  * statement parameters. It mirrors the `Args` type, with a [[net.noresttherein.oldsql.sql.WithParam WithParam]]` X`
	  * pseudo join applied to an initial wildcard ''from'' clause for every
	  * [[net.noresttherein.oldsql.collection.Chain.~ ~]]` X` link in the argument chain.
	  * This is the type on which SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] with the arguments
	  * are based on (parameterized with).
	  */
	type Domain <: RowProduct

	/** The type of the argument(s) of created [[net.noresttherein.oldsql.sql.Call Call]] statements. */
	type CallParams

	def domain(form :ChainForm[Params]) :Domain
	def result(procedure :StoredProcedure[Params]) :StatementResult[CallProcedure[Out], Out]
	def result[Y](function :StoredFunction[Params, Y]) :StatementResult[CallFunction[Out, Y], FunctionResult[Y]]

	def apply(in :Args) :Params
	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](in :ChainTuple[F, S, Args]) :ChainTuple[F, S, Params]

	def apply(procedure :StoredProcedure[Params]) :Call[Args, Out]

	def apply(procedure :StoredProcedure[Params], args :Args) :Call[Unit, Out] =
		apply(procedure).bind(args)

	def apply(procedure :StoredProcedure[Params], args :ChainTuple[Domain, GlobalScope, Args]) :Call[CallParams, Out]

	def apply[Ps <: Chain](domain :PureParamFrom[Ps], procedure :StoredProcedure[Params])
	                      (args :ChainTuple[domain.Self, GlobalScope, Args]) :Call[Ps, Out]

	def apply[Y](function :StoredFunction[Params, Y]) :Call[Args, FunctionResult[Y]]

	def apply[Y](function :StoredFunction[Params, Y], args :Args) :Call[Unit, FunctionResult[Y]] =
		apply(function).bind(args)

	def apply[Y](function :StoredFunction[Params, Y], args :ChainTuple[Domain, GlobalScope, Args])
			:Call[CallParams, FunctionResult[Y]]

	def apply[Ps <: Chain, Y](domain :PureParamFrom[Ps], function :StoredFunction[Params, Y])
	                         (args :ChainTuple[domain.Self, GlobalScope, Args]) :Call[Ps, FunctionResult[Y]]
}






sealed abstract class Level1ProcedureSignatureImplicits {
	implicit def direct[Xs <: Chain] :ProcedureInSignature[Xs] = new ProcedureInSignature
}



sealed abstract class ProcedureInOutSignatureDefaultImplicit extends Level1ProcedureSignatureImplicits {
	implicit def param[Params <: Chain, Args <: Chain, X](implicit init :ProcedureInOutSignature[Params, Args])
			:ProcedureInOutSignature[Params ~ X, Args ~ X] {
				type Out = init.Out
				type ProcedureResult = init.ProcedureResult
				type Domain = init.Domain WithParam X
			} =
		new ProcedureInOutSignature[Params ~ X, Args ~ X] {
			override type Out = init.Out
			override type ProcedureResult = init.ProcedureResult
			override type Domain = init.Domain WithParam X

			override def outForm(inOut :ChainForm[Params ~ X]) :ChainForm[Out] = init.outForm(inOut.init)

			override def reverseOutParams(inOut :ChainForm[Params ~ X], offset :Int) :List[Int] =
				init.reverseOutParams(inOut.init, offset)

			override def domain(form :ChainForm[Params ~ X]) :SelfTyped[Domain] = {
				val left = init.domain(form.init)
				JoinParam[SelfTyped[init.Domain], X](left, ParamRelation[X]()(form.last))
			}

			override def params[F <: RowProduct](domain :Domain, expansion :F)(implicit part :Domain PartOf F)
					:ChainTuple[F, GlobalScope, Args ~ X] =
				init.params(domain.left, expansion)(part.expandFront[init.Domain]) ~
					(domain.last :SQLExpression[Domain, GlobalScope, X]).basedOn(expansion)

			override def apply(in :Args ~ X) :Params ~ X = init(in.init) ~ in.last

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](in :ChainTuple[F, S, Args ~ X])
					:ChainTuple[F, S, Params ~ X] =
				init(in.init) ~ in.last

			override def apply[Ps <: Chain](domain :PureParamFrom[Ps], procedure :StoredProcedure[Params ~ X])
			                               (args :ChainTuple[domain.Self, GlobalScope, Args ~ X]) :Call[Ps, Out] =
				ParamInOutCallProcedure[Ps, Params ~ X, Args ~ X](domain)(procedure, args)(this)

			override def apply[Ps <: Chain, Y](domain :PureParamFrom[Ps], fun :StoredFunction[Params ~ X, Y])
			                                  (args :ChainTuple[domain.Self, GlobalScope, Args ~ X]) :Call[Ps, Out ~ Y] =
				ParamInOutCallFunction[Ps, Params ~ X, Args ~ X, Y](domain)(fun, args)(this)
		}
}



object ProcedureSignature extends ProcedureInOutSignatureDefaultImplicit {
	import StoredProcedure.{Out => OutParam}

	/** The signature of a call to a [[net.noresttherein.oldsql.sql.StoredProcedure StoredProcedure]]`[Xs]`
	  * accepting expressions of exactly `Xs` (without swapping any
	  * [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` parameters for `X`), which returns
	  * no values (all parameters are ''IN'' parameters - or ignored ''OUT'' parameters).
	  * This signature has the lowest precedence of all `ProcedureSignature` implicits and is used when no
	  * additional information is available for type `Xs` - ''OUT'' parameters can't be identified.
	  */
	class ProcedureInSignature[Params <: Chain] extends ProcedureSignature[Params, Params] {
		override type Out = Any //todo: ()
		override type ProcedureResult = Any
		override type FunctionResult[Y] = Y
		override type Domain = RowProduct
		override type CallParams = Unit

		override def domain(form :ChainForm[Params]) :Dual = Dual

		override def result(procedure :StoredProcedure[Params]) :StatementResult[CallProcedure[Any], Any] =
			NoResult

		override def result[Y](function :StoredFunction[Params, Y]) :StatementResult[CallFunction[Any, Y], Y] =
			StatementResult.ProcedureResult(Nil)(function.readForm)

		override def apply(in :Params) :Params = in

		override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](in :ChainTuple[F, S, Params])
				:ChainTuple[F, S, Params] =
			in

		override def apply(procedure :StoredProcedure[Params]) :Call[Params, Any] = Call(procedure)

		override def apply(procedure :StoredProcedure[Params], args :ChainTuple[RowProduct, GlobalScope, Params])
				:Call[Unit, Any] =
			GroundCallProcedure(procedure, args)

		override def apply[Ps <: Chain](domain :PureParamFrom[Ps], procedure :StoredProcedure[Params])
		                               (args :ChainTuple[domain.Self, GlobalScope, Params]) :Call[Ps, Any] =
			ParamCallProcedure[Ps, Params](domain)(procedure, args)

		override def apply[Y](function :StoredFunction[Params, Y]) :Call[Params, Y] = Call(function)

		override def apply[Y](function :StoredFunction[Params, Y], args :ChainTuple[RowProduct, GlobalScope, Params])
				:Call[Unit, Y] =
			GroundCallFunction(function, args)

		override def apply[Ps <: Chain, Y](domain :PureParamFrom[Ps], function :StoredFunction[Params, Y])
		                                  (args :ChainTuple[domain.Self, GlobalScope, Params]) :Call[Ps, Y] =
			ParamCallFunction[Ps, Params, Y](domain)(function, args)
	}


	/** A signature of a [[net.noresttherein.oldsql.sql.StoredProcedure procedure]] - or
	  * [[net.noresttherein.oldsql.sql.StoredFunction function]] - call involving ''OUT'' parameters.
	  * They are returned from created [[net.noresttherein.oldsql.sql.Call Call]] DML as
	  * [[net.noresttherein.oldsql.sql.mechanics.ProcedureSignature.Out Out]]
	  * [[net.noresttherein.oldsql.collection.Chain chain]]. A procedure can be invoked by supplying
	  * an [[net.noresttherein.oldsql.sql.SQLExpression expression]] for the tuple with its parameters
	  * as defined by the procedure's type parameter, but any ''OUT'' parameters - those of type
	  * [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` - can be replaced with simply expressions
	  * of value type `X`. All values of type `Out[X]` mark the out parameters returned by the call as `X` in a
	  * chain of `Out` member type; aArguments not wrapped in `Out` marker type are treated as ''IN'' parameters.
	  * @tparam Params types of the formal type parameters of the called procedure/function as a chain.
	  * @tparam Args   the type of the chain with provided arguments, corresponding to `Params`.
	  *                Valid types (those for which implicit instances are available) are `Params` itself
	  *                as well as any chain resulting from substituting any subset of elements of type
	  *                [[net.noresttherein.oldsql.sql.StoredProcedure.Out Out]]`[X]` with `X`.
	  */
	trait ProcedureInOutSignature[Params <: Chain, Args <: Chain] extends ProcedureSignature[Params, Args] { sig =>
		override type Out <: ProcedureResult
		override type ProcedureResult <: Chain
		override type FunctionResult[Y] = Out ~ Y
		override type Domain <: TopFromSome { type Self <: sig.Domain; type Params = Args; type ParamsOnly = true }
		override type CallParams = Args
		def outForm(inOut :ChainForm[Params]) :ChainForm[Out]
		def reverseOutParams(inOut :ChainForm[Params], offset :Int = 0) :List[Int]

		def outParamIndices(procedure :StoredProcedure[Params]) :Seq[Int] =
			reverseOutParams(procedure.paramForm).reverse

		def outParamIndices[Y](function :StoredFunction[Params, Y]) :Seq[Int] =
			reverseOutParams(function.paramForm, 1).reverse

		def params(domain :Domain) :ChainTuple[Domain, GlobalScope, Args] = params(domain, domain)

		def params[F <: RowProduct](domain :Domain, expansion :F)
		                           (implicit part :Domain PartOf F) :ChainTuple[F, GlobalScope, Args]

		override def result(procedure :StoredProcedure[Params]) :StatementResult[CallProcedure[Out], Out] =
			StatementResult.ProcedureResult(outParamIndices(procedure))(outForm(procedure.paramForm))

		override def result[Y](function :StoredFunction[Params, Y]) :StatementResult[CallFunction[Out, Y], Out ~ Y] =
			StatementResult.FunctionResult(outParamIndices(function))(outForm(function.paramForm), function.readForm)


		override def apply(procedure :StoredProcedure[Params]) :Call[Args, Out] = {
			val from = domain(procedure.paramForm)
			apply(from, procedure)(params(from))
		}

		override def apply(procedure :StoredProcedure[Params], args :ChainTuple[Domain, GlobalScope, Args])
				:Call[CallParams, Out] =
			apply(domain(procedure.paramForm), procedure)(args)

		override def apply[Y](function :StoredFunction[Params, Y]) :Call[Args, Out ~ Y] = {
			val from = domain(function.paramForm)
			apply(from, function)(params(from))
		}

		override def apply[Y](function :StoredFunction[Params, Y], args :ChainTuple[Domain, GlobalScope, Args])
				:Call[Args, Out ~ Y] =
			apply(domain(function.paramForm), function)(args)

	}


	implicit def noParams :ProcedureInOutSignature[@~, @~] { type Out = @~ } =
		NoParams

	//Extracted so that the Domain type returned by def domain is erased, as type Domain is impossible to define due to recursiveness
	private abstract class NoParams extends ProcedureInOutSignature[@~, @~] {
		override type Domain = EmptyClause
		override def domain(form :ChainForm[@~]) :Domain = EmptyClause
	}

	private object NoParams extends NoParams {
		override type Out = @~
		override type ProcedureResult = @~
		override type FunctionResult[Y] = @~ ~ Y
//		override type Domain = Nothing //PureParamFrom[@~] { type Implicit = RowProduct; type Self <: NoParams.Domain }
		override type CallParams = @~

		override def reverseOutParams(inOut :ChainForm[@~], offset :Int) :List[Int] = Nil
		override def outForm(in :ChainForm[@~]) :ChainForm[@~] = in

		override def params[F <: RowProduct](domain :Domain, expansion :F)
		                                    (implicit part :Domain PartOf F) :ChainTuple[F, GlobalScope, @~] =
			ChainTuple.EmptyChain

		override def result(procedure :StoredProcedure[@~]) :StatementResult[CallProcedure[@~], @~] =
			NoResult.chain

		override def result[Y](function :StoredFunction[@~, Y]) :StatementResult[CallFunction[@~, Y], @~ ~ Y] =
			StatementResult.FunctionResult(1::Nil)(ChainForm, function.readForm)

		override def apply(in : @~) : @~ = in

		override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](in :ChainTuple[F, S, @~])
				:ChainTuple[F, S, @~] =
			in

		override def apply[Ps <: Chain](domain :PureParamFrom[Ps], procedure :StoredProcedure[@~])
		                               (args :ChainTuple[domain.Self, GlobalScope, @~]) :Call[Ps, Out] =
			ParamInOutCallProcedure[Ps, @~, @~](domain)(procedure, args)(this)

		override def apply[Ps <: Chain, Y](domain :PureParamFrom[Ps], function :StoredFunction[@~, Y])
		                                  (args :ChainTuple[domain.Self, GlobalScope, @~]) :Call[Ps, @~ ~ Y] =
			ParamInOutCallFunction[Ps, @~, @~, Y](domain)(function, args)(this)
	}


	implicit def inParam[Params <: Chain, Args <: Chain, X](implicit init :ProcedureInOutSignature[Params, Args])
			:ProcedureInOutSignature[Params ~ Out[X], Args ~ X] {
				type Out = init.Out
				type ProcedureResult = init.ProcedureResult
				type Domain = init.Domain WithParam X
			} =
		new ProcedureInOutSignature[Params ~ Out[X], Args ~ X] {
			override type Out = init.Out
			override type ProcedureResult = init.ProcedureResult
			override type Domain = init.Domain WithParam X

			override def outForm(inOut :ChainForm[Params ~ OutParam[X]]) :ChainForm[Out] =
				init.outForm(inOut.init)

			override def reverseOutParams(inOut :ChainForm[Params ~ OutParam[X]], offset :Int) :List[Int] =
				init.reverseOutParams(inOut.init, offset)

			override def params[F <: RowProduct](domain :Domain, expansion :F)(implicit part :Domain PartOf F)
					:ChainTuple[F, GlobalScope, Args ~ X] =
				init.params(domain.left, expansion)(part.expandFront[init.Domain]) ~ domain.last.basedOn(expansion)

			override def domain(form :ChainForm[Params ~ OutParam[X]]) :SelfTyped[Domain] = {
				val left = init.domain(form.init)
				JoinParam[init.Domain, X](left, ParamRelation[X]()(paramForm(form.last)))
			}

			override def apply(in :Args ~ X) :Params ~ OutParam[X] = init(in.init) ~ in.last

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope](in :ChainTuple[F, S, Args ~ X])
					:ChainTuple[F, S, Params ~ OutParam[X]] =
				init(in.init) ~ in.last.out

			override def apply[Ps <: Chain](domain :PureParamFrom[Ps],
			                                procedure :StoredProcedure[Params ~ OutParam[X]])
			                               (args :ChainTuple[domain.Self, GlobalScope, Args ~ X]) :Call[Ps, Out] =
				ParamInOutCallProcedure(domain)(procedure, args)(this)

			override def apply[Ps <: Chain, Y](domain :PureParamFrom[Ps],
			                                   function :StoredFunction[Params ~ OutParam[X], Y])
			                                  (args :ChainTuple[domain.Self, GlobalScope, Args ~ X])
					:Call[Ps, Out ~ Y] =
				ParamInOutCallFunction(domain)(function, args)(this)
		}


	implicit def outParam[Params <: Chain, Args <: Chain, X](implicit init :ProcedureInOutSignature[Params, Args])
			:ProcedureInOutSignature[Params ~ Out[X], Args ~ Out[X]] {
				type Out = init.Out ~ X
				type ProcedureResult = init.ProcedureResult ~ X
				type Domain = init.Domain WithParam OutParam[X]
			} =
		new ProcedureInOutSignature[Params ~ Out[X], Args ~ Out[X]] {
			override type Out = init.Out ~ X
			override type ProcedureResult = init.ProcedureResult ~ X
			override type Domain = init.Domain WithParam OutParam[X]

			override def outForm(inOut :ChainForm[Params ~ OutParam[X]]) :ChainForm[init.Out ~ X] =
				init.outForm(inOut.init) ~ paramForm(inOut.last)

			override def reverseOutParams(inOut :ChainForm[Params ~ OutParam[X]], shift :Int) :List[Int] =
				(inOut.size + shift) :: init.reverseOutParams(inOut.init)

			override def domain(form :ChainForm[Params ~ OutParam[X]]) :SelfTyped[Domain] = {
				val left = init.domain(form.init)
				JoinParam[init.Domain, OutParam[X]](left, ParamRelation[OutParam[X]]()(form.last))
			}

			override def params[F <: RowProduct](domain :Domain, expansion :F)(implicit part :Domain PartOf F)
					:ChainTuple[F, GlobalScope, Args ~ OutParam[X]] =
				init.params(domain.left, expansion)(part.expandFront[init.Domain]) ~ domain.last.basedOn(expansion)


			override def apply(in :Args ~ OutParam[X]) :Params ~ OutParam[X] = init(in.init) ~ in.last

			override def apply[F <: RowProduct, S >: LocalScope <: GlobalScope]
			                  (in :ChainTuple[F, S, Args ~ OutParam[X]]) :ChainTuple[F, S, Params ~ OutParam[X]] =
				init(in.init) ~ in.last

			override def apply[Ps <: Chain](domain :PureParamFrom[Ps],
			                                procedure :StoredProcedure[Params ~ OutParam[X]])
			                               (args :ChainTuple[domain.Self, GlobalScope, Args ~ OutParam[X]])
					:Call[Ps, Out] =
				ParamInOutCallProcedure(domain)(procedure, args)(this)

			override def apply[Ps <: Chain, Y](domain :PureParamFrom[Ps],
			                                   function :StoredFunction[Params ~ OutParam[X], Y])
			                                  (args :ChainTuple[domain.Self, GlobalScope, Args ~ OutParam[X]])
					:Call[Ps, Out ~ Y] =
				ParamInOutCallFunction(domain)(function, args)(this)
		}

	private def paramForm[X](form :SQLForm[Out[X]]) :SQLForm[X] = form match {
		case OutForm(param) => param
		case other => other.nullBimap(_.param)(Out.apply)
	}


}
