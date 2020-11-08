package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.schema.{ColumnReadForm, SQLReadForm}
import net.noresttherein.oldsql.sql.{ColumnFunction, ColumnSQL, RowProduct, SQLExpression, SQLFunction}
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.ColumnSQL.{ColumnMatcher, CompositeColumnSQL}
import net.noresttherein.oldsql.sql.ast.FunctionSQL.FunctionColumn.FunctionColumnMatcher
import net.noresttherein.oldsql.sql.mechanics.SQLScribe






trait FunctionSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X <: Chain, Y] extends CompositeSQL[F, S, Y] {
	/** An SQL tuple expression with the expressions for every argument of this function. */
	def args :ChainTuple[F, S, X]


	def function :SQLFunction[X, Y]

	override def parts :Seq[SQLExpression[F, S, _]] = args::Nil

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
			throw new IllegalArgumentException(s"Cannot rephrase $this with $mapper: arguments rephrased to non-tuple: $res.")
	}

	override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, R]) :R[S, Y] =
		matcher.function(this)


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

	def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, X <: Chain, Y :SQLReadForm]
	         (fun :SQLFunction[X, Y], args :ChainTuple[F, S, X]) :FunctionSQL[F, S, X, Y] =
		new BaseFunctionSQL(fun, args)


	def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, X <: Chain, Y](e :FunctionSQL[F, S, X, Y])
			:(SQLFunction[X, Y], ChainTuple[F, S, X]) =
		(e.function, e.args)

	def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, Y](e :SQLExpression[F, S, Y])
			:Option[(SQLFunction[X, Y], ChainTuple[F, S, X]) forSome { type X <: Chain }] =
		e match {
			case fun :FunctionSQL[F @unchecked, S @unchecked, x, Y] =>
				Some((fun.function, fun.args))
			case _ => None
		}



	private[sql] class BaseFunctionSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X <: Chain, Y]
	                   (override val function :SQLFunction[X, Y], override val args :ChainTuple[F, S, X])
	                   (implicit override val readForm :SQLReadForm[Y])
		extends FunctionSQL[F, S, X, Y]






	trait FunctionColumn[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X <: Chain, Y]
		extends FunctionSQL[F, S, X, Y] with CompositeColumnSQL[F, S, Y]
	{
		override def function :ColumnFunction[X, Y]

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

		override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[F, R]) :R[S, Y] =
			matcher.function(this)

	}



	object FunctionColumn {

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, X <: Chain, Y :ColumnReadForm]
		         (fun :ColumnFunction[X, Y], args :ChainTuple[F, S, X]) :FunctionColumn[F, S, X, Y] =
			new BaseFunctionColumn(fun, args)


		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, X <: Chain, Y](e :FunctionSQL[F, S, X, Y])
				:Option[(ColumnFunction[X, Y], ChainTuple[F, S, X])] =
			e match {
				case fun :FunctionColumn[F @unchecked, S @unchecked, X @unchecked, Y @unchecked] =>
					Some((fun.function, fun.args))
				case _ =>
					None
			}

		def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, Y](e :SQLExpression[F, S, Y])
				:Option[(ColumnFunction[X, Y], ChainTuple[F, S, X]) forSome { type X <: Chain }] =
			e match {
				case fun :FunctionColumn[F @unchecked, S @unchecked, x, Y @unchecked] =>
					Some((fun.function, fun.args))
				case _ => None
			}



		private[FunctionSQL] class BaseFunctionColumn[-F <: RowProduct, -S >: LocalScope <: GlobalScope, X <: Chain, Y]
		                           (override val function :ColumnFunction[X, Y], override val args :ChainTuple[F, S, X])
		                           (implicit override val readForm :ColumnReadForm[Y])
			extends FunctionColumn[F, S, X, Y]



		trait FunctionColumnMatcher[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]] {
			def function[S >: LocalScope <: GlobalScope, X <: Chain, Y](e :FunctionColumn[F, S, X, Y]) :R[S, Y]
		}

		type MatchFunctionColumn[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]] = FunctionColumnMatcher[F, R]

		type CaseFunctionColumn[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]] = FunctionColumnMatcher[F, R]

	}





	trait FunctionMatcher[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]]
		extends FunctionColumnMatcher[F, R]
	{
		def function[S >: LocalScope <: GlobalScope, X <: Chain, Y](e :FunctionSQL[F, S, X, Y]) :R[S, Y]
	}

	trait MatchFunction[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]]
		extends FunctionMatcher[F, R]
	{
		override def function[S >: LocalScope <: GlobalScope, X <: Chain, Y](e :FunctionColumn[F, S, X, Y]) :R[S, Y] =
			function(e :FunctionSQL[F, S, X, Y])
	}

	type CaseFunction[+F <: RowProduct, +R[-_ >: LocalScope <: GlobalScope, _]] = MatchFunction[F, R]

}
