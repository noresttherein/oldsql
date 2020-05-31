package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, OuterFrom}
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple.{CaseChain, ChainHead, ChainMatcher}
import net.noresttherein.oldsql.sql.TupleSQL.SeqTuple.{CaseSeq, SeqMatcher}
import scala.annotation.tailrec
import scala.collection.immutable.Seq

import net.noresttherein.oldsql.sql.SelectSQL.{FreeSelectSQL, SubselectSQL}



/** `TupleSQL` represents an SQL expression consisting of several individual column values.
  * In its literal form it is written as `(col1, col2, ..., colN)`.
  */
trait TupleSQL[-F <: FromClause, T] extends CompositeSQL[F, T] {

	override def sameAs(other :CompositeSQL[Nothing, _]) :Boolean = other.isInstanceOf[TupleSQL[_, _]]


	override def selectFrom[S <: F with OuterFrom, O](from :S) :FreeSelectSQL[T, O] =
		SelectSQL(from, this)

	override def subselectFrom[S <: F, O](from :S) :SubselectSQL[from.Implicit, T, O] =
		SelectSQL.subselect[from.Implicit, from.type, T, O](from, this)
}






object TupleSQL {

	def unapply[F <: FromClause](expr: SQLExpression[F, _]): Option[Seq[SQLExpression[F, _]]] = expr match {
		case t: TupleSQL[_, _] => Some(t.inOrder.asInstanceOf[Seq[SQLExpression[F, _]]])
		case _ => None
	}






	case class SeqTuple[-F <: FromClause, T](override val parts :Seq[SQLExpression[F, T]]) extends TupleSQL[F, Seq[T]] {
		override def inOrder :Seq[SQLExpression[F, T]] = parts

		override def readForm :SQLReadForm[Seq[T]] = SQLReadForm.seq(inOrder.map(_.readForm))

		override def freeValue :Option[Seq[T]] =
			(Option(List.empty[T]) /: inOrder) {
				case (acc, e) => for (seq <- acc; v <- e.freeValue) yield v :: seq
			}.map(_.reverse)

//		override def get(values: RowValues[F]): Option[Seq[T]] =
//			(Option(List.empty[T]) /: inOrder) {
//				case (acc, e) => for (seq <- acc; v <- e.get(values)) yield v::seq
//			}.map(_.reverse)



		override def applyTo[Y[_]](matcher: ExpressionMatcher[F, Y]): Y[Seq[T]] = matcher.seq(this)

		override def map[S <: FromClause](mapper: SQLScribe[F, S]) =
			SeqTuple(inOrder.map(mapper(_)))


		override def toString :String = inOrder.mkString("Seq(", ",", ")")
	}



	object SeqTuple {
		implicit def literalSeq[T :SQLForm](items :Seq[T]) :SeqTuple[FromClause, T] =
			new SeqTuple[FromClause, T](items.map(SQLLiteral(_)))

		implicit def expressionSeq[R<:FromClause, T](items :Seq[SQLExpression[R, T]]) :SeqTuple[R, T] =
			new SeqTuple[R, T](items)


		trait SeqMatcher[+F <: FromClause, +Y[X]] {
			def seq[X](e :SeqTuple[F, X]) :Y[Seq[X]]
		}

		type MatchSeq[+F <: FromClause, +Y[X]] = SeqMatcher[F, Y]

		type CaseSeq[+F <: FromClause, +Y[X]] = SeqMatcher[F, Y]
	}






	sealed trait ChainTuple[-F <: FromClause, T <: Chain] extends TupleSQL[F, T] {
		def size :Int

		protected override def parts :Seq[SQLExpression[F, _]] = {
			@tailrec def rec(formula :ChainTuple[F, _], acc :List[SQLExpression[F, _]] = Nil) :Seq[SQLExpression[F, _]] =
				formula match {
					case ChainHead(tail, head) => rec(tail, head::acc)
					case _ => acc
				}
			rec(this)
		}

		def toSeq :Seq[SQLExpression[F, _]] = parts

		override def applyTo[Y[_]](matcher :ExpressionMatcher[F, Y]) :Y[T] = matcher.chain(this)

		override def map[S <: FromClause](mapper :SQLScribe[F, S]) :ChainTuple[S, T]


//		override def stretch[M[O] <: MappingFrom[O]] :ChainTuple[F With M, T] =
//			stretch[F, F With M]
//
//		override def stretch[U <: F, S <: FromClause](implicit ev :U ExtendedBy S) :ChainTuple[S, T] =
//			map(SQLScribe.stretcher(target))

		override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :ChainTuple[S, T] =
			map(SQLScribe.stretcher(base))

		def ~[S <: F, H](head :SQLExpression[S, H]) :ChainHead[S, T, H] = new ChainHead(this, head)

		override def toString :String = inOrder.mkString("(", " ~ ", ")")
	}



	object ChainTuple {

		trait ChainMatcher[+F <: FromClause, +Y[X]] {
			def chain[X <: Chain](e :ChainTuple[F, X]) :Y[X]
		}

		type CaseChain[+F <: FromClause, +Y[X]] = ChainMatcher[F, Y]

		trait MatchChain[+F <: FromClause, +Y[X]] extends ChainMatcher[F, Y] {
			def chainHead[T <: Chain, H](tail :ChainTuple[F, T], head :SQLExpression[F, H]) :Y[T ~ H]
			def emptyChain :Y[@~]

			override def chain[X <: Chain](e :ChainTuple[F, X]) :Y[X] = (e match {
				case ChainHead(tail, head) => chainHead(tail, head)
				case _ => emptyChain
			}).asInstanceOf[Y[X]]
		}



		case class ChainHead[-F <: FromClause, T <: Chain, H](init :ChainTuple[F, T], last :SQLExpression[F, H])
			extends ChainTuple[F, T ~ H]
		{
			override def size :Int = init.size + 1

			override def readForm :SQLReadForm[T ~ H] = SQLReadForm.ChainReadForm[T, H](init.readForm, last.readForm)

//			override def get(values :RowValues[F]) :Option[T ~ H] =
//				for (t <- tail.get(values); h <- head.get(values)) yield t ~ h

			override def freeValue :Option[T ~ H] =
				for (t <- init.freeValue; h <- last.freeValue) yield t ~ h

			override def map[S <: FromClause](mapper :SQLScribe[F, S]) :ChainTuple[S, T ~ H] =
				init.map(mapper) ~ mapper(last)

			override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :ChainTuple[S, T ~ H] =
				init.stretch(base) ~ last.stretch(base)
		}



		case object EmptyChain extends SQLTerm[@~] with ChainTuple[FromClause, @~] {
			override def size :Int = 0

			protected override def form :SQLForm[@~] = SQLForm.EmptyChainForm

			override def writeForm :SQLWriteForm[Unit] = SQLWriteForm.empty

			override def readForm :SQLReadForm[@~] = SQLReadForm.EmptyChainReadForm

			override val freeValue :Option[@~] = Some(@~)

			override def map[S <: FromClause](mapper :SQLScribe[FromClause, S]) :ChainTuple[S, @~] = this

//			override def stretch[M[O] <: MappingFrom[O]] = this
//
//			override def stretch[U <: FromClause, S <: FromClause](implicit ev :U ExtendedBy S) = this

			override def stretch[U <: FromClause, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) = this
		}
	}





	trait TupleMatcher[+F <: FromClause, +Y[X]] extends ChainMatcher[F, Y] with SeqMatcher[F, Y]

	trait MatchTuple[+F <: FromClause, +Y[X]] extends CaseChain[F, Y] with CaseSeq[F, Y]


	trait CaseTuple[+F <: FromClause, +Y[X]] extends TupleMatcher[F, Y] with MatchTuple[F, Y] {
		def tuple[X](e: TupleSQL[F, X]): Y[X]

		override def chain[X <: Chain](e :ChainTuple[F, X]) :Y[X] = tuple(e)

		override def seq[X](e: SeqTuple[F, X]): Y[Seq[X]] = tuple(e)

	}




}

