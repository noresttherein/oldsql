package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.{Chain, LiteralIndex}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, OuterFrom}
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher}
import net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple.{CaseChain, ChainHead, ChainMatcher, EmptyChain}
import net.noresttherein.oldsql.sql.TupleSQL.SeqTuple.{CaseSeq, SeqMatcher}
import scala.annotation.tailrec
import scala.collection.immutable.Seq

import net.noresttherein.oldsql.collection.LiteralIndex.{:~, |~}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.ColumnSQL.AliasedColumn
import net.noresttherein.oldsql.sql.SelectSQL.{FreeSelectSQL, SubselectSQL}
import net.noresttherein.oldsql.sql.TupleSQL.IndexedChainTuple.{IndexedChainHead, IndexedChainMatcher, IndexedColumn, IndexedSQLExpression}



/** `TupleSQL` represents an SQL expression consisting of several individual column values.
  * In its literal form it is written as `(col1, col2, ..., colN)`. The member expressions of this tuple
  * may not be all instances of `ColumnSQL`, but any multi-column expressions are inlined in the resulting `SQL`.
  */
trait TupleSQL[-F <: FromClause, T] extends CompositeSQL[F, T] {

	override def sameAs(other :CompositeSQL[Nothing, _]) :Boolean = other.isInstanceOf[TupleSQL[_, _]]


	override def selectFrom[S <: F with OuterFrom, O](from :S) :FreeSelectSQL[T, O] =
		SelectSQL(from, this)

	override def subselectFrom[S <: F, O](from :S) :SubselectSQL[from.Implicit, T, O] =
		SelectSQL.subselect[from.Implicit, from.type, T, O](from, this)
}
//todo: conversion to MappingSQL





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

		override def rephrase[S <: FromClause](mapper: SQLScribe[F, S]) =
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






	/** A tuple implementation with flexible length, mapping nominally
	  * to a [[net.noresttherein.oldsql.collection.Chain Chain]] subtype containing the member expressions.
	  */
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

		override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :ChainTuple[S, T]


		override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :ChainTuple[S, T] =
			rephrase(SQLScribe.stretcher(base))

		def ~[S <: F, H](head :SQLExpression[S, H]) :ChainHead[S, T, H] = new ChainHead(this, head)

		override def toString :String = inOrder.mkString("(", " ~ ", ")")
	}



	object ChainTuple {

		trait ChainMatcher[+F <: FromClause, +Y[X]] {
			def chain[X <: Chain](e :ChainTuple[F, X]) :Y[X]
		}

		type CaseChain[+F <: FromClause, +Y[X]] = ChainMatcher[F, Y]

		trait MatchChain[+F <: FromClause, +Y[X]] extends ChainMatcher[F, Y] {
			def chainHead[I <: Chain, L](init :ChainTuple[F, I], last :SQLExpression[F, L]) :Y[I ~ L]
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

			override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :ChainTuple[S, T ~ H] =
				init.rephrase(mapper) ~ mapper(last)

			override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :ChainTuple[S, T ~ H] =
				init.stretch(base) ~ last.stretch(base)
		}



		case object EmptyChain
			extends SQLTerm[@~] with ChainTuple[FromClause, @~] with IndexedChainTuple[FromClause, @~]
		{
			override def size :Int = 0

			override def toSeq :List[Nothing] = Nil

			protected override def form :SQLForm[@~] = SQLForm.EmptyChainForm

			override def writeForm :SQLWriteForm[Unit] = SQLWriteForm.empty

			override def readForm :SQLReadForm[@~] = SQLReadForm.EmptyChainReadForm

			override val freeValue :Option[@~] = Some(@~)

			override def rephrase[S <: FromClause](mapper :SQLScribe[FromClause, S]) :this.type = this

			override def stretch[U <: FromClause, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) = this
		}
	}




	//does not extend ChainTuple because IndexedChainHead doesn't extend ChainHead and it would cause problems
	// in pattern matching a ChainTuple
	/** A variant of [[net.noresttherein.oldsql.sql.TupleSQL.ChainTuple ChainTuple]] which maps
	  * to a [[net.noresttherein.oldsql.collection.LiteralIndex LiteralIndex]] - a `Chain` subtype with key-value pairs
	  * in the form of `K :~ V` as its only elements. The keys exist only on the type level in the `T` type parameter;
	  * Member SQL expressions are not of the `K :~ V` type, but rather directly the value type `V`.
	  * The indexing has no special effect on the generated SQL other than an `IndexedColumn` being a subclass of
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.AliasedColumn AliasedColumn]]. Instead, it allows referencing
	  * the columns in a type safe way when mapping results of a select, if this expression appears
	  * in the ''select'' clause. Additionally, a ''select'' clause consisting solely of this type can be matched
	  * to any [[net.noresttherein.oldsql.schema.IndexedSchemaMapping IndexedSchemaMapping]] with a component chain
	  * containing exactly the same key-value pairs as the `T` type parameter, regardless of their order.
	  * Non-column components of the mapping must in that case recursively match a nested indexed tuple paired
	  * with the same key type in this `LiteralChain` as the component's label.
	  * An `IndexedChainTuple` can consist only of expressions implementing `IndexedSQLExpression` - a sealed type
	  * extended only by the [[net.noresttherein.oldsql.sql.TupleSQL.IndexedChainTuple.IndexedColumn IndexedColumn]]
	  * and this trait. This ensures that every column of the whole expression is assigned a unique key,
	  * which however may be a sequence of `Label`s, rather than a single one, if this expression contains
	  * another indexed tuples as its subexpressions.
	  * Note that, in order to provide clear cases when pattern matching, this type does not extend the standard
	  * `ChainTuple`.
	  */
	trait IndexedChainTuple[-F <: FromClause, T <: LiteralIndex] extends TupleSQL[F, T] with IndexedSQLExpression[F, T] {
		def size :Int

		protected override def parts :Seq[SQLExpression[F, _]] = {
			@tailrec def rec(e :IndexedChainTuple[F, _], acc :List[SQLExpression[F, _]] = Nil) :Seq[SQLExpression[F, _]] =
				e match {
					case IndexedChainHead(tail, head) => rec(tail, head::acc)
					case _ => acc
				}
			rec(this)
		}

		def toSeq :Seq[SQLExpression[F, _]] = parts


		override def applyTo[Y[_]](matcher :ExpressionMatcher[F, Y]) :Y[T] = matcher.indexedChain(this)

		override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :IndexedChainTuple[S, T]


		override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :IndexedChainTuple[S, T] =
			rephrase(SQLScribe.stretcher(base))


		def |~[S <: F, K <: Label :ValueOf, H](head :K :~ IndexedSQLExpression[S, H]) :IndexedChainHead[S, T, K, H] =
			new IndexedChainHead(this, head.value)

		def |~[S <: F, K <: Label, H](head :IndexedColumn[S, K, H]) :IndexedChainHead[S, T, K, H] =
			new IndexedChainHead(this, head)(new ValueOf(head.alias))

		def |~[S <: F, K <: Label :ValueOf, H](head :K :~ ColumnSQL[S, H]) :IndexedChainHead[S, T, K, H] =
			new IndexedChainHead(this, new IndexedColumn(head.value, valueOf[K]))

		override def toString :String = {
			def rec(e :IndexedChainTuple[_, _], res :StringBuilder) :StringBuilder = e match {
				case EmptyChain => res ++= "@~"
				case tuple :IndexedChainHead[_, _, _, _] =>
					rec(tuple.init, res) ++= " |~ " ++= tuple.key ++= ":~" ++= tuple.last.toString
			}
			rec(this, new StringBuilder).toString
		}

	}



	object IndexedChainTuple {
		final val EmptyIndexedChain = ChainTuple.EmptyChain

		sealed trait IndexedSQLExpression[-F <: FromClause, T] extends SQLExpression[F, T] {
			def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :IndexedSQLExpression[S, T]

			def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :IndexedSQLExpression[S, T]
		}

		class IndexedColumn[-F <: FromClause, N <: Label, V](column :ColumnSQL[F, V], override val alias :N)
			extends AliasedColumn[F, V](column, alias) with IndexedSQLExpression[F, V]
		{
			override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :IndexedColumn[S, N, V] =
				new IndexedColumn(mapper(column), alias)

			override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :IndexedColumn[S, N, V] =
				new IndexedColumn(column.stretch(base), alias)
		}


		case class IndexedChainHead[-F <: FromClause, T <: LiteralIndex, K <: Label :ValueOf, H]
		                           (init :IndexedChainTuple[F, T], last :IndexedSQLExpression[F, H])
			extends IndexedChainTuple[F, T |~ (K :~ H)]
		{
			def key :K = valueOf[K]

			override def size :Int = init.size + 1

			override def readForm :SQLReadForm[T |~ (K :~ H)] =
				SQLReadForm.LiteralIndexReadForm[T, K, H](init.readForm, implicitly[ValueOf[K]], last.readForm)


			override def freeValue :Option[T |~ (K :~ H)] =
				for (t <- init.freeValue; h <- last.freeValue) yield t |~ :~[K](h)

			override def rephrase[S <: FromClause](mapper :SQLScribe[F, S]) :IndexedChainTuple[S, T |~ (K :~ H)] =
				init.rephrase(mapper) |~ :~[K](last.rephrase(mapper))

			override def stretch[U <: F, S <: FromClause](base :S)(implicit ev :U ExtendedBy S) :IndexedChainTuple[S, T |~ (K :~ H)] =
				init.stretch(base) |~ :~[K](last.stretch(base))
		}



		trait IndexedChainMatcher[+F <: FromClause, +Y[X]] {
			def indexedChain[X <: LiteralIndex](e :IndexedChainTuple[F, X]) :Y[X]
		}

		type CaseIndexedChain[+F <: FromClause, +Y[X]] = IndexedChainMatcher[F, Y]

		trait MatchIndexedChain[+F <: FromClause, +Y[X]] extends IndexedChainMatcher[F, Y] {
			def emptyChain :Y[@~]

			def indexedChainHead[I <: LiteralIndex, K <: Label :ValueOf, L]
			                    (init :IndexedChainTuple[F, I], last :IndexedSQLExpression[F, L]) :Y[I |~ (K :~ L)]

			override def indexedChain[X <: LiteralIndex](e :IndexedChainTuple[F, X]) :Y[X] = (e match {
				case tuple @ IndexedChainHead(tail, head) => indexedChainHead(tail, head)(new ValueOf(tuple.key))
				case _ => emptyChain
			}).asInstanceOf[Y[X]]
		}
	}






	trait TupleMatcher[+F <: FromClause, +Y[X]]
		extends ChainMatcher[F, Y] with IndexedChainMatcher[F, Y] with SeqMatcher[F, Y]

	trait MatchTuple[+F <: FromClause, +Y[X]] extends CaseChain[F, Y] with CaseSeq[F, Y]


	trait CaseTuple[+F <: FromClause, +Y[X]] extends TupleMatcher[F, Y] with MatchTuple[F, Y] {
		def tuple[X](e: TupleSQL[F, X]): Y[X]

		override def chain[X <: Chain](e :ChainTuple[F, X]) :Y[X] = tuple(e)

		override def indexedChain[X <: LiteralIndex](e :IndexedChainTuple[F, X]) :Y[X] = tuple(e)

		override def seq[X](e: SeqTuple[F, X]): Y[Seq[X]] = tuple(e)

	}




}

