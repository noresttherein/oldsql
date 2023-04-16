package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{IllegalExpressionException, InseparableExpressionException}
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.sql.{ColumnFunction, ColumnSQL, RowProduct, RowShape, SQLExpression, StoredFunction}
import net.noresttherein.oldsql.sql.Call.CallFunction.GroundCallFunction
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, ColumnReformingDefaults, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, Grouped, Single, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.FunctionColumnSQL.{AnyFunctionColumnVisitor, SpecificFunctionColumnVisitor}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.{AlignableColumns, Reform, ReformPermissions, SQLScribe, SQLTransformation, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.NoReform
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** An SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] invoking a stored
  * [[net.noresttherein.oldsql.sql.StoredFunction function]].
  * For zero argument functions, see [[net.noresttherein.oldsql.sql.ast.NativeTerm NativeTerm]]
  * and [[net.noresttherein.oldsql.sql.ast.NativeColumnTerm NativeColumnTerm]] instead.
  */
trait FunctionSQL[-F <: RowProduct, -S >: Grouped <: Single, X <: Chain, Y] extends CompositeSQL[F, S, Y] {
	/** An SQL tuple expression with the expressions for every argument of this function. */
	val args :ChainTuple[F, S, X]

	def function :StoredFunction[X, Y]

	override def selectForm :SQLReadForm[Y] = function.readForm

	override def parts :Seq[SQLExpression[F, S, _]] = args::Nil

	override def isSingleRow    :Boolean = args.isSingleRow
	override def groundValue :Opt[Y]  = args.groundValue.flatMap(function.eval)
	override def isGround    :Boolean = args.isGround
	override def isAnchored  :Boolean = args.isAnchored
	override def isAnchored(from :F) :Boolean = args.isAnchored(from)

	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, Y] = mapper(args) match {
		case tuple :ChainTuple[E @unchecked, S @unchecked, X @unchecked] =>
			function(tuple)
		case res =>
			throw new IllegalExpressionException(
				s"Cannot rephrase $this with $mapper: arguments rephrased to non-tuple: $res."
			)
	}

	protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                             (implicit leftResult  :SQLTransformation[Y, U],
	                                       rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:(leftResult.Expression[F, S, SQLExpression[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
		if (this eq other)
			(leftResult(this), rightResult(other))
		else if (passCount.firstTime)
			passReform(other)(reform, passCount)
		else
			reform.prohibitReformLeft(NativeTerm[Y]("")(selectForm), other)

	protected override def realign(reordering :Rearrangement)(implicit spelling :SQLSpelling) :SQLExpression[F, S, Y] =
		//We reported zero potentialColumns because we wouldn't be able to try to align them,
		// so we expect the reordering to also have 0 input columns. This check is by no way a guarantee that
		// alignment succeeded, but we will check that later through reform method.
		if (reordering.columnCount == 0 && reordering.underlyingColumnCount == columnCount)
			this
		else
			throw new InseparableExpressionException(this)

	protected override def potentialColumns(permissions :Permissions)(implicit spelling :SQLSpelling) :AlignableColumns =
		AlignableColumns.none(this, NoReform)

	protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int = 0

	@throws[InseparableExpressionException]("if this expression is not a ColumnSQL.")
	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] =
		throw new InseparableExpressionException(this)

	protected override def shape(implicit spelling :SQLSpelling) :RowShape = selectForm.shape
	protected override def effectiveForm(implicit spelling :SQLSpelling) :SQLForm[Y] =
		selectForm <> SQLWriteForm.nulls(selectForm.columnCount)

	protected override def columnCount(implicit spelling :SQLSpelling) :Int = selectForm.columnCount
	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = spelling.sqlParamCount(args)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling(function)(this.args)(from, context, params)

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		selectForm.columnCount match {
			case 0 => Nil
			case 1 => defaultSpelling(from, context, params)::Nil
			case n =>
				throw new InseparableExpressionException(
					this, s"Cannot split a multi($n) column function call '$this' into individual columns."
				)
		}


	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyExpressionVisitor[F, R]) :R[S, Y] =
		visitor.function(this)

	protected override def visit[R](visitor :SpecificExpressionVisitor[F, S, Y, R]) :R = visitor.function(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: SQLExpression[F, S, Y] <: SQLExpression[F_, S_, Y],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F, R]) :R[S_, Y, E] =
//		visitor.function(this)

	override def sameAs(other :CompositeSQL.__) :Boolean = other match {
		case fun :FunctionSQL[_, _, _, _] => fun.function == function
		case _ => false
	}

	override def canEqual(other :Any) :Boolean = other match {
		case fun :FunctionSQL[_, _, _, _] => fun.function == function
		case _ => false
	}

	override def toString :String = function.name + "(" + args + ")"
}






object FunctionSQL {

	def apply[F <: RowProduct, S >: Grouped <: Single, X <: Chain, Y]
	         (fun :StoredFunction[X, Y], args :ChainTuple[F, S, X])
	         (implicit form :SQLReadForm[Y] = fun.readForm) :FunctionSQL[F, S, X, Y] =
		new Impl(fun, args)


	def unapply[F <: RowProduct, S >: Grouped <: Single, X <: Chain, Y](e :FunctionSQL[F, S, X, Y])
			:Opt[(StoredFunction[X, Y], ChainTuple[F, S, X])] =
		Got(e.function, e.args)

	def unapply[F <: RowProduct, S >: Grouped <: Single, Y](e :SQLExpression[F, S, Y])
			:Opt[(StoredFunction[X, Y], ChainTuple[F, S, X]) forSome { type X <: Chain }] =
		e match {
			case fun :FunctionSQL[F @unchecked, S @unchecked, x, Y] =>
				Got((fun.function, fun.args))
			case _ => Lack
		}



	implicit def statement[X <: Chain, Y](sql :FunctionSQL[RowProduct, Single, X, Y]) :GroundCallFunction[Y] =
		GroundCallFunction(sql.function, sql.args)



//currently commented out as we don't accept arbitrary expressions as arguments for JDBC calls of stored functions
/*
	implicit class TopFunctionSQLExtension[X <: Chain, Y]
	               (private val self :FunctionSQL[RowProduct, GlobalScope, X, Y])
		extends AnyVal
	{
		/** Translates the abstract syntax tree of this SQL expression into textual SQL.
		  * The result is adapted to the DBMS at hand using implicitly available
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] strategy. Aside from the `String`
		  * representation, returned [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]] contains
		  * write form setting all bound parameters of the call.
		  */ //an extension method as TopSelect would inherit conflicting signatures from Select and SelectSQL
		def spell(implicit spelling :SQLSpelling = StandardSQL.spelling) :SpelledSQL[@~, RowProduct] =
			spelling.spell(self)

		/** Translates the abstract syntax tree of this SQL expression into an executable JDBC call statement invoking
		  * the function. The result is adapted to the DBMS at hand by the implicitly given
		  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]].
		  */
		def chant(implicit dialect :SQLDialect = StandardSQL) :Cantrip[Y] = dialect(self)

		/** Converts this SQL expression to a JDBC call statement for the same function and arguments.  */
		def call :Call[(), Y] = Call.GroundCallFunction(self.function, self.args)
	}
*/


	type __ = FunctionSQL[_ <: RowProduct, _ >: Grouped <: Single, _ <: Chain, _]

	private class Impl[-F <: RowProduct, -S >: Grouped <: Single, X <: Chain, Y]
	                  (override val function :StoredFunction[X, Y], override val args :ChainTuple[F, S, X])
	                  (implicit override val selectForm :SQLReadForm[Y] = function.readForm)
		extends FunctionSQL[F, S, X, Y]



	trait SpecificFunctionVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
		extends SpecificFunctionColumnVisitor[F, S, V, Y]
	{
		def function[X <: Chain](e :FunctionSQL[F, S, X, V]) :Y
	}
	trait MatchSpecificFunction[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
		extends SpecificFunctionVisitor[F, S, V, Y]
	{
		override def functionColumn[X <: Chain](e :FunctionColumnSQL[F, S, X, V]) :Y = function(e :FunctionSQL[F, S, X, V])
	}
	type CaseSpecificFunction[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] = MatchSpecificFunction[F, S, V, Y]

//
//	trait FunctionVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends FunctionColumnVisitor[F, R]
//	{
//		def function[S >: Grouped <: Single, X <: Chain, Y]
//		            (e :FunctionSQL[F, S, X, Y]) :R[S, Y, FunctionSQL[F, S, X, Y]]
//	}
//	trait MatchFunction[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends FunctionVisitor[F, R]
//	{
//		override def functionColumn[S >: Grouped <: Single, X <: Chain, Y]
//		                           (e :FunctionColumnSQL[F, S, X, Y]) :R[S, Y, FunctionColumnSQL[F, S, X, Y]] =
//			function(e)
//	}
//	type CaseFunction[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		MatchFunction[F, R]

	trait AnyFunctionVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyFunctionColumnVisitor[F, R]
	{
		def function[S >: Grouped <: Single, X <: Chain, Y](e :FunctionSQL[F, S, X, Y]) :R[S, Y]
	}
	trait MatchAnyFunction[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyFunctionVisitor[F, R]
	{
		override def functionColumn[S >: Grouped <: Single, X <: Chain, Y](e :FunctionColumnSQL[F, S, X, Y]) :R[S, Y] =
			function(e :FunctionSQL[F, S, X, Y])
	}
	type CaseAnyFunction[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] = MatchAnyFunction[F, R]

}






trait FunctionColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, X <: Chain, Y]
	extends FunctionSQL[F, S, X, Y]
	   with ColumnReformingDefaults[F, S, Y, ({ type E[v] = ColumnSQL[F, S, v] })#E]
       with CompositeColumnSQL[F, S, Y]
{
	override def function :ColumnFunction[X, Y]
	override def selectForm :ColumnReadForm[Y] = function.readForm

	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Y] = mapper(args) match {
		case tuple :ChainTuple[E @unchecked, S @unchecked, X @unchecked] =>
			function(tuple)
		case res =>
			throw new IllegalExpressionException(
				s"Cannot rephrase $this with $mapper: arguments rephrased to non-tuple: $res."
			)
	}

	protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                             (implicit leftResult  :SQLTransformation[Y, U],
	                                       rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:(leftResult.Expression[F, S, ColumnSQL[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
		if (this eq other)
			(leftResult(this), rightResult(other))
		else if (passCount.firstTime)
			passReform(other)(reform, passCount)
		else other match {
			case column :ColumnSQL[F2, S2, V2] if reform.compatible(this, column) =>
				(leftResult(this), rightResult(other))
			case _ =>
				reform.prohibitReformLeft(NativeColumnTerm[Y]("")(selectForm), other)
		}

	protected override def effectiveForm(implicit spelling :SQLSpelling) :ColumnForm[Y] =
		selectForm <> ColumnWriteForm.nulls(selectForm.sqlType)

	protected[sql] override def atomicSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                             (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling(this)(from, context, params)

	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, R]) :R[S, Y] =
		visitor.functionColumn(this)

	protected override def visit[R](visitor :SpecificColumnVisitor[F, S, Y, R]) :R = visitor.functionColumn(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Y] <: SQLExpression[F_, S_, Y],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Y, E] =
//		visitor.functionColumn(this)
}



object FunctionColumnSQL {

	def apply[F <: RowProduct, S >: Grouped <: Single, X <: Chain, Y]
	         (fun :ColumnFunction[X, Y], args :ChainTuple[F, S, X])
	         (implicit form :ColumnReadForm[Y] = fun.readForm) :FunctionColumnSQL[F, S, X, Y] =
		new Impl(fun, args)


	def unapply[F <: RowProduct, S >: Grouped <: Single, X <: Chain, Y](e :FunctionSQL[F, S, X, Y])
			:Opt[(ColumnFunction[X, Y], ChainTuple[F, S, X])] =
		e match {
			case fun :FunctionColumnSQL[F @unchecked, S @unchecked, X @unchecked, Y @unchecked] =>
				Got((fun.function, fun.args))
			case _ => Lack
		}

	def unapply[F <: RowProduct, S >: Grouped <: Single, Y](e :SQLExpression[F, S, Y])
			:Opt[(ColumnFunction[X, Y], ChainTuple[F, S, X]) forSome { type X <: Chain }] =
		e match {
			case fun :FunctionColumnSQL[F @unchecked, S @unchecked, x, Y @unchecked] =>
				Got((fun.function, fun.args))
			case _ => Lack
		}


	private class Impl[-F <: RowProduct, -S >: Grouped <: Single, X <: Chain, Y]
	                  (override val function :ColumnFunction[X, Y], override val args :ChainTuple[F, S, X])
	                  (implicit override val selectForm :ColumnReadForm[Y] = function.readForm)
		extends FunctionColumnSQL[F, S, X, Y]


	trait SpecificFunctionColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] {
		def functionColumn[X <: Chain](e :FunctionColumnSQL[F, S, X, V]) :Y
	}
	type MatchSpecificFunctionColumn[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
		SpecificFunctionColumnVisitor[F, S, V, Y]
	type CaseSpecificFunctionColumn[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
		SpecificFunctionColumnVisitor[F, S, V, Y]
//
//	trait FunctionColumnVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def functionColumn[S >: Grouped <: Single, X <: Chain, Y]
//		                  (e :FunctionColumnSQL[F, S, X, Y]) :R[S, Y, FunctionColumnSQL[F, S, X, Y]]
//	}
//	type MatchFunctionColumn[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		FunctionColumnVisitor[F, R]
//	type CaseFunctionColumn[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		FunctionColumnVisitor[F, R]

	trait AnyFunctionColumnVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] {
		def functionColumn[S >: Grouped <: Single, X <: Chain, Y](e :FunctionColumnSQL[F, S, X, Y]) :R[S, Y]
	}
	type MatchAnyFunctionColumn[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] =
		AnyFunctionColumnVisitor[F, R]
	type CaseAnyFunctionColumn[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] =
		AnyFunctionColumnVisitor[F, R]
}


