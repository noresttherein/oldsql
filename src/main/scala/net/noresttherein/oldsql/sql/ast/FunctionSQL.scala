package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.InseparableExpressionException
import net.noresttherein.oldsql.schema.{ColumnReadForm, SQLReadForm}
import net.noresttherein.oldsql.sql.{ColumnFunction, ColumnSQL, RowProduct, SQLExpression, StoredFunction}
import net.noresttherein.oldsql.sql.Call.CallFunction.GroundCallFunction
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionVisitor, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.CompositeColumnSQL
import net.noresttherein.oldsql.sql.ast.FunctionSQL.FunctionColumnSQL.FunctionColumnVisitor
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** An SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] invoking a stored
  * [[net.noresttherein.oldsql.sql.StoredFunction function]].
  */
trait FunctionSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X <: Chain, Y] extends CompositeSQL[F, S, Y] {
	/** An SQL tuple expression with the expressions for every argument of this function. */
	val args :ChainTuple[F, S, X]

	def function :StoredFunction[X, Y]

	override def readForm :SQLReadForm[Y] = function.readForm
	override def groundValue :Opt[Y] = args.groundValue.flatMap(function.eval)

	override def parts :Seq[SQLExpression[F, S, _]] = args::Nil

	override def isGlobal :Boolean = args.isGlobal
	override def isAnchored :Boolean = args.isAnchored

	override def anchor(from :F) :SQLExpression[F, S, Y] = args.anchor(from) match {
		case tuple :ChainTuple[F @unchecked, S @unchecked, X @unchecked] =>
			function(tuple)
		case res =>
			throw new IllegalArgumentException(s"Cannot anchor $this: arguments anchored to non-tuple: $res.")
	}

	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, Y] = mapper(args) match {
		case tuple :ChainTuple[E @unchecked, S @unchecked, X @unchecked] =>
			function(tuple)
		case res =>
			throw new IllegalArgumentException(
				s"Cannot rephrase $this with $mapper: arguments rephrased to non-tuple: $res."
			)
	}

	protected override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](visitor :ExpressionVisitor[F, R]) :R[S, Y] =
		visitor.function(this)

	@throws[InseparableExpressionException]("if this expression is not a ColumnSQL.")
	override def split(implicit scope :OperationType) :Seq[ColumnSQL[F, S, _]] =
		throw new InseparableExpressionException("Cannot split a non-column function call " + this + " into columns.")

	override def columnCount(implicit spelling :SQLSpelling) :Int = readForm.readColumns

	protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		spelling(function)(this.args :ChainTuple[E, S, X])(context, params)

	protected override def inlineSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, E]] =
		readForm.readColumns match {
			case 0 => Nil
			case 1 => defaultSpelling(context, params)::Nil
			case n =>
				throw new InseparableExpressionException(
					s"Cannot split a multi($n) column function call '$this' into individual columns."
				)
		}


	override def sameAs(other :CompositeSQL.*) :Boolean = other match {
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

	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, X <: Chain, Y]
	         (fun :StoredFunction[X, Y], args :ChainTuple[F, S, X])
	         (implicit form :SQLReadForm[Y] = fun.readForm) :FunctionSQL[F, S, X, Y] =
		new BaseFunctionSQL(fun, args)


	def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, X <: Chain, Y](e :FunctionSQL[F, S, X, Y])
			:Opt[(StoredFunction[X, Y], ChainTuple[F, S, X])] =
		Got(e.function, e.args)

	def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, Y](e :SQLExpression[F, S, Y])
			:Opt[(StoredFunction[X, Y], ChainTuple[F, S, X]) forSome { type X <: Chain }] =
		e match {
			case fun :FunctionSQL[F @unchecked, S @unchecked, x, Y] =>
				Got((fun.function, fun.args))
			case _ => Lack
		}



	implicit def statement[X <: Chain, Y](sql :FunctionSQL[RowProduct, GlobalScope, X, Y]) :GroundCallFunction[Y] =
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



	private[sql] class BaseFunctionSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X <: Chain, Y]
	                   (override val function :StoredFunction[X, Y], override val args :ChainTuple[F, S, X])
	                   (implicit override val readForm :SQLReadForm[Y] = function.readForm)
		extends FunctionSQL[F, S, X, Y]



	trait FunctionColumnSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X <: Chain, Y]
		extends FunctionSQL[F, S, X, Y] with CompositeColumnSQL[F, S, Y]
	{
		override def function :ColumnFunction[X, Y]
		override def readForm :ColumnReadForm[Y] = function.readForm

		override def anchor(from :F) :ColumnSQL[F, S, Y] = args.anchor(from) match {
			case tuple :ChainTuple[F @unchecked, S @unchecked, X @unchecked] =>
				function(tuple)
			case res =>
				throw new IllegalArgumentException(s"Cannot anchor $this: arguments anchored to non-tuple: $res.")
		}

		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Y] = mapper(args) match {
			case tuple :ChainTuple[E @unchecked, S @unchecked, X @unchecked] =>
				function(tuple)
			case res =>
				throw new IllegalArgumentException(s"Cannot rephrase $this with $mapper: arguments rephrased to non-tuple: $res.")
		}

		protected override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[F, R]) :R[S, Y] =
			visitor.function(this)

		override def inParens[P, E <: F](context :SQLContext, params :Parameterization[P, E])
		                                (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
			defaultSpelling(context, params)
	}



	object FunctionColumnSQL {

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, X <: Chain, Y]
		         (fun :ColumnFunction[X, Y], args :ChainTuple[F, S, X])
		         (implicit form :ColumnReadForm[Y] = fun.readForm) :FunctionColumnSQL[F, S, X, Y] =
			new BaseFunctionColumnSQL(fun, args)


		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, X <: Chain, Y](e :FunctionSQL[F, S, X, Y])
				:Opt[(ColumnFunction[X, Y], ChainTuple[F, S, X])] =
			e match {
				case fun :FunctionColumnSQL[F @unchecked, S @unchecked, X @unchecked, Y @unchecked] =>
					Got((fun.function, fun.args))
				case _ => Lack
			}

		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, Y](e :SQLExpression[F, S, Y])
				:Opt[(ColumnFunction[X, Y], ChainTuple[F, S, X]) forSome { type X <: Chain }] =
			e match {
				case fun :FunctionColumnSQL[F @unchecked, S @unchecked, x, Y @unchecked] =>
					Got((fun.function, fun.args))
				case _ => Lack
			}


		private class BaseFunctionColumnSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X <: Chain, Y]
		              (override val function :ColumnFunction[X, Y], override val args :ChainTuple[F, S, X])
		              (implicit override val readForm :ColumnReadForm[Y] = function.readForm)
			extends FunctionColumnSQL[F, S, X, Y]


		trait FunctionColumnVisitor[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]] {
			def function[S >: LocalScope <: GlobalScope, X <: Chain, Y](e :FunctionColumnSQL[F, S, X, Y]) :R[S, Y]
		}

		type MatchFunctionColumn[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]] = FunctionColumnVisitor[F, R]
		type CaseFunctionColumn[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]] = FunctionColumnVisitor[F, R]
	}




	trait FunctionVisitor[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]]
		extends FunctionColumnVisitor[F, R]
	{
		def function[S >: LocalScope <: GlobalScope, X <: Chain, Y](e :FunctionSQL[F, S, X, Y]) :R[S, Y]
	}

	trait MatchFunction[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]]
		extends FunctionVisitor[F, R]
	{
		override def function[S >: LocalScope <: GlobalScope, X <: Chain, Y](e :FunctionColumnSQL[F, S, X, Y]) :R[S, Y] =
			function(e :FunctionSQL[F, S, X, Y])
	}

	type CaseFunction[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]] = MatchFunction[F, R]

}


