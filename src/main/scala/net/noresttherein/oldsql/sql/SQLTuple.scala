package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.sql.SQLFormula.{CompositeFormula, Formula, FormulaMatcher}
import net.noresttherein.oldsql.sql.SQLMapper.SQLRewriter
import net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral
import net.noresttherein.oldsql.sql.SQLTuple.ChainFormula.{CaseChain, ChainHead, ChainMatcher}
import net.noresttherein.oldsql.sql.SQLTuple.SeqFormula.{CaseSeq, SeqMatcher}

import scala.annotation.tailrec
import scala.collection.immutable.Seq



/** `SQLTuple` represents an SQL expression consisting of several individual values.
  * In its literal form it is written as `(col1, col2, ..., colN)`.
  */
trait SQLTuple[-F <: FromClause, T] extends CompositeFormula[F, T] {

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLTuple[_, _]]

	override def isomorphic(expression: Formula[_]): Boolean = expression match {
		case t :SQLTuple[_, _] => contentsIsomorphic(t)
		case _ => false
	}

	private[oldsql] override def equivalent(expression :Formula[_]) :Boolean = expression match {
		case t :SQLTuple[_, _] => contentsEquivalent(t)
		case _ => false
	}

}






object SQLTuple {

	def unapply[F <: FromClause](expr: SQLFormula[F, _]): Option[Seq[SQLFormula[F, _]]] = expr match {
		case t: SQLTuple[_, _] => Some(t.inOrder.asInstanceOf[Seq[SQLFormula[F, _]]])
		case _ => None
	}






	case class SeqFormula[-F <: FromClause, T](override val parts :Seq[SQLFormula[F, T]]) extends SQLTuple[F, Seq[T]] {
		override def inOrder :Seq[SQLFormula[F, T]] = parts

		override def readForm :SQLReadForm[Seq[T]] = SQLReadForm.seq(inOrder.map(_.readForm))

		override def freeValue :Option[Seq[T]] =
			(Option(List.empty[T]) /: inOrder) {
				case (acc, e) => for (seq <- acc; v <- e.freeValue) yield v :: seq
			}.map(_.reverse)

//		override def get(values: RowValues[F]): Option[Seq[T]] =
//			(Option(List.empty[T]) /: inOrder) {
//				case (acc, e) => for (seq <- acc; v <- e.get(values)) yield v::seq
//			}.map(_.reverse)



		override def applyTo[Y[+X]](matcher: FormulaMatcher[F, Y]): Y[Seq[T]] = matcher.seq(this)

		override def map[S <: FromClause](mapper: SQLRewriter[F, S]) =
			SeqFormula(inOrder.map(mapper(_)))


		override def toString :String = inOrder.mkString("Seq(", ",", ")")
	}



	object SeqFormula {
		implicit def literalSeq[T :SQLForm](items :Seq[T]) :SeqFormula[FromClause, T] =
			new SeqFormula[FromClause, T](items.map(SQLLiteral(_)))

		implicit def expressionSeq[R<:FromClause, T](items :Seq[SQLFormula[R, T]]) :SeqFormula[R, T] =
			new SeqFormula[R, T](items)


		trait SeqMatcher[+F <: FromClause, +Y[X]] {
			def seq[X](f :SeqFormula[F, X]) :Y[Seq[X]]
		}

		type MatchSeq[+F <: FromClause, +Y[X]] = SeqMatcher[F, Y]

		type CaseSeq[+F <: FromClause, +Y[X]] = SeqMatcher[F, Y]
	}






	sealed trait ChainFormula[-F <: FromClause, T <: Chain] extends SQLTuple[F, T] {
		def size :Int

		protected override def parts :Seq[SQLFormula[F, _]] = {
			@tailrec def rec(formula :ChainFormula[F, _], acc :List[SQLFormula[F, _]] = Nil) :Seq[SQLFormula[F, _]] =
				formula match {
					case ChainHead(tail, head) => rec(tail, head::acc)
					case _ => acc
				}
			rec(this)
		}

		def toSeq :Seq[SQLFormula[F, _]] = parts

		override def applyTo[Y[+X]](matcher :FormulaMatcher[F, Y]) :Y[T] = matcher.chain(this)

		override def map[S <: FromClause](mapper :SQLRewriter[F, S]) :ChainFormula[S, T]

		def ~[S <: F, H](head :SQLFormula[S, H]) :ChainHead[S, T, H] = new ChainHead(this, head)

		override def toString :String = inOrder.mkString("(", " ~ ", ")")
	}



	object ChainFormula {

		trait ChainMatcher[+F <: FromClause, +Y[X]] {
			def chain[X <: Chain](f :ChainFormula[F, X]) :Y[X]
		}

		type CaseChain[+F <: FromClause, +Y[X]] = ChainMatcher[F, Y]

		trait MatchChain[+F <: FromClause, +Y[X]] extends ChainMatcher[F, Y] {
			def chainHead[T <: Chain, H](tail :ChainFormula[F, T], head :SQLFormula[F, H]) :Y[T ~ H]
			def emptyChain :Y[@~]

			override def chain[X <: Chain](f :ChainFormula[F, X]) :Y[X] = (f match {
				case ChainHead(tail, head) => chainHead(tail, head)
				case _ => emptyChain
			}).asInstanceOf[Y[X]]
		}



		case class ChainHead[-F <: FromClause, T <: Chain, H](init :ChainFormula[F, T], last :SQLFormula[F, H])
			extends ChainFormula[F, T ~ H]
		{
			override def size :Int = init.size + 1

			override def readForm :SQLReadForm[T ~ H] = SQLReadForm.ChainReadForm[T, H](init.readForm, last.readForm)

//			override def get(values :RowValues[F]) :Option[T ~ H] =
//				for (t <- tail.get(values); h <- head.get(values)) yield t ~ h

			override def freeValue :Option[T ~ H] =
				for (t <- init.freeValue; h <- last.freeValue) yield t ~ h

			override def map[S <: FromClause](mapper :SQLRewriter[F, S]) :ChainFormula[S, T ~ H] =
				init.map(mapper) ~ mapper(last)
		}



		case object EmptyChain extends SQLTerm[@~] with ChainFormula[FromClause, @~] {
			override def size :Int = 0

			override def writeForm :SQLWriteForm[Unit] = SQLWriteForm.empty

			override def readForm :SQLReadForm[@~] = SQLReadForm.EmptyChainReadForm

			override val freeValue :Option[@~] = Some(@~)

			override def map[S <: FromClause](mapper :SQLRewriter[FromClause, S]) :ChainFormula[S, @~] = this
		}
	}





	trait TupleMatcher[+F <: FromClause, +Y[X]] extends ChainMatcher[F, Y] with SeqMatcher[F, Y]

	trait MatchTuple[+F <: FromClause, +Y[X]] extends CaseChain[F, Y] with CaseSeq[F, Y]


	trait CaseTuple[+F <: FromClause, +Y[X]] extends TupleMatcher[F, Y] with MatchTuple[F, Y] {
		def tuple[X](f: SQLTuple[F, X]): Y[X]

		override def chain[X <: Chain](f :ChainFormula[F, X]) :Y[X] = tuple(f)

		override def seq[X](f: SeqFormula[F, X]): Y[Seq[X]] = tuple(f)

	}




}

