package net.noresttherein.oldsql.sql.ast

import scala.collection.immutable.Seq

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, Select, SQLExpression}
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, ExpandedBy, GroundFrom, NonEmptyFrom, TopFrom}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionVisitor, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SubselectSQL, TopSelectSQL}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/**
  * @author Marcin Mościcki
  */
case class ChainSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, I <: Chain, L]
                   (init :SQLExpression[F, S, I], last :SQLExpression[F, S, L])
	extends CompositeSQL[F, S, I ~ L]
{
	protected override def parts :Seq[SQLExpression[F, S, _]] = init::last::Nil

	override val readForm :SQLReadForm[I ~ L] = (init.readForm, last.readForm) match {
		case (i :SQLForm[I @unchecked], l :SQLForm[L @unchecked]) => i ~ l
		case _ => init.readForm ~ last.readForm
	}
	override def groundValue :Opt[I ~ L] = for { i <- init.groundValue; l <- last.groundValue } yield i ~ l

	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :SQLExpression[E, S, I ~ L] =
		mapper(init) ~ mapper(last)

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S) :SQLExpression[E, S, I ~ L] =
		init.expand(base) ~ last.expand(base)

	override def isGlobal :Boolean = last.isGlobal && init.isGlobal
	override def isAnchored :Boolean = last.isAnchored && init.isAnchored

	override def anchor(from :F) :ChainSQL[F, S, I, L] =
		(init.anchor(from), last.anchor(from)) match {
			case (i, l) if (i eq init) && (l eq last) => this
			case (i, l) => new ChainSQL(i, l)
		}


	override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectSQL[I ~ L] =
		SelectSQL(from, this)

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B]) :SubselectSQL[B, I ~ L] =
		SelectSQL.subselect[B, from.type, I ~ L](from, this)

	override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
			:Select[P, I ~ L] =
		Select(from)[I ~ L](this)


	override def split(implicit op :OperationType) :Seq[ColumnSQL[F, S, _]] = init.split :++ last.split


	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ExpressionVisitor[F, Y]) :Y[S, I ~ L] =
		visitor.chain(this)


	override def columnCount(implicit spelling :SQLSpelling) :Int = init.columnCount + last.columnCount

	protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		( "(" +: inlineSpelling(context, params).reduce(_.sql +: ", " +: _)) + ")"

	protected override def inlineSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P, E]] =
	{
		val first = spelling.explode(init :SQLExpression[E, LocalScope, I])(context, params)
		val second = first match {
			case Seq() => spelling.explode(last :SQLExpression[E, LocalScope, L])(context, params)
			case columns =>
				val prev = columns.last
				spelling.explode(last :SQLExpression[E, LocalScope, L])(prev.context, prev.params)
		}
		//this is not ideal, as most likely these concatenations will take O(n^2) if first and second are lists
		first ++: second
	}


	override def sameAs(other :CompositeSQL.*) :Boolean = other.isInstanceOf[ChainSQL[_, _, _, _]]


	override def toString :String = inOrder.mkString("(", " ~ ", ")")
}



object ChainSQL {

	val EmptyChain = ChainTuple.EmptyChain

	trait ChainVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		//todo: make a common mixin CaseXxx with MatchChainTuple
		def chain[S >: LocalScope <: GlobalScope, I <: Chain, L](e :ChainSQL[F, S, I, L]) :Y[S, I ~ L]
	}

	type CaseChain[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ChainVisitor[F, Y]

	type MatchChain[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ChainVisitor[F, Y]
}
