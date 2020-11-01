package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.{Chain, IndexedChain}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{SQLForm, SQLForms, SQLReadForm, SQLReadForms, SQLWriteForm}
import net.noresttherein.oldsql.sql.FromClause.{ExactSubselectOf, ExtendedBy, FreeFrom, NonEmptyFrom, PartOf}
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple.{CaseChain, ChainHead, ChainMatcher, EmptyChain}
import net.noresttherein.oldsql.sql.TupleSQL.SeqTuple.{CaseSeq, SeqMatcher}
import scala.annotation.tailrec
import scala.collection.immutable.Seq

import net.noresttherein.oldsql.collection.IndexedChain.{:~, |~}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.ColumnSQL.AliasedColumn
import net.noresttherein.oldsql.sql.SelectSQL.{FreeSelectSQL, SubselectSQL}
import net.noresttherein.oldsql.sql.TupleSQL.IndexedChainTuple.{IndexedChainHead, IndexedChainMatcher, IndexedColumn, IndexedSQLExpression}



/** `TupleSQL` represents an SQL expression consisting of several individual column values.
  * In its literal form it is written as `(col1, col2, ..., colN)`. The member expressions of this tuple
  * may not be all instances of `ColumnSQL`, but any multi-column expressions are inlined in the resulting `SQL`.
  */
trait TupleSQL[-F <: FromClause, -S >: LocalScope <: GlobalScope, T] extends CompositeSQL[F, S, T] {

	override def freeSelectFrom[E <: F with FreeFrom](from :E) :FreeSelectSQL[T] =
		SelectSQL(from, this)

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B]) :SubselectSQL[B, T] =
		SelectSQL.subselect[B, from.type, T](from, this)

	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, Y]) :Y[S, T] =
		matcher.tuple(this)

	override def sameAs(other :CompositeSQL.*) :Boolean = other.isInstanceOf[TupleSQL[_, _, _]]
}






object TupleSQL {

	def unapply[F <: FromClause, S >: LocalScope <: GlobalScope](expr: SQLExpression[F, S, _])
			:Option[Seq[SQLExpression[F, S, _]]] =
		expr match {
			case t: TupleSQL[_, _, _] => Some(t.inOrder.asInstanceOf[Seq[SQLExpression[F, S, _]]])
			case _ => None
		}



	case class SeqTuple[-F <: FromClause, -S >: LocalScope <: GlobalScope, T](override val parts :Seq[SQLExpression[F, S, T]])
		extends TupleSQL[F, S, Seq[T]]
	{
		override def inOrder :Seq[SQLExpression[F, S, T]] = parts

		override def readForm :SQLReadForm[Seq[T]] = SQLReadForm.seq(inOrder.map(_.readForm))

		override def anchor(from :F) :SQLExpression[F, S, Seq[T]] = new SeqTuple(parts.map(_.anchor(from)))

		override def rephrase[E <: FromClause](mapper: SQLScribe[F, E]) :SeqTuple[E, S, T] =
			SeqTuple(inOrder.map(mapper(_)))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher: ExpressionMatcher[F, Y]): Y[S, Seq[T]] =
			matcher.seq(this)


		override def toString :String = inOrder.mkString("Seq(", ",", ")")
	}



	object SeqTuple {
		implicit def literalSeq[T :SQLForm](items :Seq[T]) :SeqTuple[FromClause, GlobalScope, T] =
			new SeqTuple(items.map(SQLLiteral(_)))

		implicit def expressionSeq[F <: FromClause, S >: LocalScope <: GlobalScope, T]
		                          (items :Seq[SQLExpression[F, S, T]]) :SeqTuple[F, S, T] =
			new SeqTuple(items)


		trait SeqMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def seq[S >: LocalScope <: GlobalScope, X](e :SeqTuple[F, S, X]) :Y[S, Seq[X]]
		}

		type MatchSeq[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = SeqMatcher[F, Y]

		type CaseSeq[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = SeqMatcher[F, Y]
	}






	/** A tuple expression implementation with flexible length, mapping nominally
	  * to a [[net.noresttherein.oldsql.collection.Chain Chain]] subtype containing the member expressions.
	  */
	sealed trait ChainTuple[-F <: FromClause, -S >: LocalScope <: GlobalScope, T <: Chain] extends TupleSQL[F, S, T] {
		def size :Int

		protected override def parts :Seq[SQLExpression[F, S, _]] = {
			@tailrec def rec(formula :ChainTuple[F, S, _], acc :List[SQLExpression[F, S, _]] = Nil)
					:Seq[SQLExpression[F, S, _]] =
				formula match {
					case ChainHead(tail, head) => rec(tail, head::acc)
					case _ => acc
				}
			rec(this)
		}

		def toSeq :Seq[SQLExpression[F, S, _]] = parts


		override def asGlobal :Option[ChainTuple[F, GlobalScope, T]] =
			if (isGlobal) Some(this.asInstanceOf[ChainTuple[F, GlobalScope, T]])
			else None

		//overriden to narrow down the result type
		override def basedOn[U <: F, E <: FromClause]
		                    (base :E)(implicit ext :U PartOf E) :ChainTuple[E, S, T] =
			rephrase(SQLScribe.extend(base))

		//overriden to narrow down the result type
		override def extend[U <: F, E <: FromClause]
		                   (base :E)(implicit ext :U ExtendedBy E, global :GlobalScope <:< S) :ChainTuple[E, S, T] =
			rephrase(SQLScribe.extend(base))


		override def anchor(from :F) :ChainTuple[F, S, T]


		//overriden to narrow down the result type
		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ChainTuple[E, S, T]

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, Y]) :Y[S, T] =
			matcher.chain(this)


		def ~[E <: F, O >: LocalScope <: S, H](head :SQLExpression[E, O, H]) :ChainHead[E, O, T, H] =
			new ChainHead(this, head)

		override def toString :String = inOrder.mkString("(", " ~ ", ")")
	}



	object ChainTuple {

		trait ChainMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def chain[S >: LocalScope <: GlobalScope, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X]
		}

		type CaseChain[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = ChainMatcher[F, Y]

		trait MatchChain[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ChainMatcher[F, Y] {
			def emptyChain :Y[GlobalScope, @~]

			def chainHead[S >: LocalScope <: GlobalScope, I <: Chain, L]
			             (init :ChainTuple[F, S, I], last :SQLExpression[F, S, L]) :Y[S, I ~ L]

			override def chain[S >: LocalScope <: GlobalScope, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X] = (e match {
				case ChainHead(tail, head) => chainHead(tail, head)
				case _ => emptyChain
			}).asInstanceOf[Y[S, X]]
		}



		case class ChainHead[-F <: FromClause, -S >: LocalScope <: GlobalScope, T <: Chain, H]
		                    (init :ChainTuple[F, S, T], last :SQLExpression[F, S, H])
			extends ChainTuple[F, S, T ~ H]
		{
			override def size :Int = init.size + 1

			override val readForm :SQLReadForm[T ~ H] = (init.readForm, last.readForm) match {
				case (i :SQLForm[T @unchecked], l :SQLForm[H @unchecked]) =>
					SQLForms.ChainForm(i, l)
				case _ =>
					SQLReadForms.ChainReadForm[T, H](init.readForm, last.readForm)
			}

			override def isGlobal :Boolean = last.isGlobal && init.isGlobal
			override def isAnchored :Boolean = last.isAnchored && init.isAnchored

			override def anchor(from :F) = (init.anchor(from), last.anchor(from)) match {
				case (i, l) if (i eq init) && (l eq last) => this
				case (i, l) => new ChainHead(i, l)
			}

			override def extend[U <: F, E <: FromClause]
			                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< S) :ChainTuple[E, S, T ~ H] =
				init.extend(base) ~ last.extend(base)

			override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :ChainTuple[E, S, T ~ H] =
				init.rephrase(mapper) ~ mapper(last)
		}



		case object EmptyChain extends SQLTerm[@~]
			with ChainTuple[FromClause, GlobalScope, @~] with IndexedChainTuple[FromClause, GlobalScope, @~]
		{
			override def size :Int = 0

			override def toSeq :List[Nothing] = Nil

			protected override def form :SQLForm[@~] = SQLForm[@~]
			override def writeForm :SQLWriteForm[Unit] = SQLWriteForm.empty
			override def readForm :SQLReadForm[@~] = SQLReadForm[@~]

			override val freeValue :Option[@~] = Option(@~)
			override def isGlobal = true
			override def asGlobal :Option[EmptyChain.type] = Some(this)
			override def isAnchored = true
			override def anchor(from :FromClause) :this.type = this


			override def basedOn[U <: FromClause, E <: FromClause](base :E)(implicit ext :U PartOf E) :this.type =
				this

			override def extend[U <: FromClause, E <: FromClause]
			                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< GlobalScope) :this.type =
				this

			override def rephrase[E <: FromClause](mapper :SQLScribe[FromClause, E]) :this.type = this
		}
	}




	//does not extend ChainTuple because IndexedChainHead doesn't extend ChainHead and it would cause problems
	// in pattern matching a ChainTuple
	/** A variant of [[net.noresttherein.oldsql.sql.TupleSQL.ChainTuple ChainTuple]] which maps
	  * to a [[net.noresttherein.oldsql.collection.IndexedChain IndexedChain]] - a `Chain` subtype with key-value pairs
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
	trait IndexedChainTuple[-F <: FromClause, -S >: LocalScope <: GlobalScope, T <: IndexedChain]
		extends TupleSQL[F, S, T] with IndexedSQLExpression[F, S, T]
	{
		def size :Int

		protected override def parts :Seq[SQLExpression[F, S, _]] = {
			@tailrec def rec(e :IndexedChainTuple[F, S, _], acc :List[SQLExpression[F, S, _]] = Nil) :Seq[SQLExpression[F, S, _]] =
				e match {
					case IndexedChainHead(tail, head) => rec(tail, head::acc)
					case _ => acc
				}
			rec(this)
		}

		def toSeq :Seq[SQLExpression[F, S, _]] = parts


		override def asGlobal :Option[IndexedChainTuple[F, GlobalScope, T]] =
			if (isGlobal) Some(this.asInstanceOf[IndexedChainTuple[F, GlobalScope, T]])
			else None

		override def anchor(from :F) :IndexedChainTuple[F, S, T]

		//overriden to narrow down the result type
		override def basedOn[U <: F, E <: FromClause](base :E)(implicit ext :U PartOf E) :IndexedChainTuple[E, S, T] =
			rephrase(SQLScribe.extend(base))

		//overriden to narrow down the result type
		override def extend[U <: F, E <: FromClause]
		             (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< S) :IndexedChainTuple[E, S, T] =
			rephrase(SQLScribe.extend(base))

		//overriden to narrow down the result type
		override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :IndexedChainTuple[E, S, T]

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, Y]) :Y[S, T] =
			matcher.indexedChain(this)


		def |~[E <: F, O >: LocalScope <: S, K <: Label :ValueOf, H]
		      (head :K :~ IndexedSQLExpression[E, O, H]) :IndexedChainHead[E, O, T, K, H] =
			new IndexedChainHead(this, head.value)

		def |~[E <: F, O >: LocalScope <: S, K <: Label, H]
		      (head :IndexedColumn[E, O, K, H]) :IndexedChainHead[E, O, T, K, H] =
			new IndexedChainHead(this, head)(new ValueOf(head.alias))

		def |~[E <: F, O >: LocalScope <: S, K <: Label :ValueOf, H]
		       (head :K :~ ColumnSQL[E, O, H]) :IndexedChainHead[E, O, T, K, H] =
			new IndexedChainHead(this, new IndexedColumn(head.value, valueOf[K]))

		override def toString :String = {
			def rec(e :IndexedChainTuple[_, _, _], res :StringBuilder) :StringBuilder = e match {
				case EmptyChain => res ++= "@~"
				case tuple :IndexedChainHead[_, _, _, _, _] =>
					rec(tuple.init, res) ++= " |~ " ++= tuple.key ++= ":~" ++= tuple.last.toString
			}
			rec(this, new StringBuilder).toString
		}

	}



	object IndexedChainTuple {
		final val EmptyIndexedChain = ChainTuple.EmptyChain

		sealed trait IndexedSQLExpression[-F <: FromClause, -S >: LocalScope <: GlobalScope, T]
			extends SQLExpression[F, S, T]
		{
			override def asGlobal :Option[IndexedSQLExpression[F, GlobalScope, T]]

			override def anchor(from :F) :IndexedSQLExpression[F, S, T]

			override def basedOn[U <: F, E <: FromClause]
			                    (base :E)(implicit ext :U PartOf E) :IndexedSQLExpression[E, S, T]

			override def extend[U <: F, E <: FromClause]
			             (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< S) :IndexedSQLExpression[E, S, T]

			def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :IndexedSQLExpression[E, S, T]
		}


		class IndexedColumn[-F <: FromClause, -S >: LocalScope <: GlobalScope, N <: Label, V]
		                   (override val column :ColumnSQL[F, S, V], override val alias :N)
			extends AliasedColumn[F, S, V](column, alias) with IndexedSQLExpression[F, S, V]
		{
			override def asGlobal :Option[IndexedColumn[F, GlobalScope, N, V]] =
				if (column.isGlobal) Some(this.asInstanceOf[IndexedColumn[F, GlobalScope, N, V]])
				else None

			override def anchor(from :F) :IndexedColumn[F, S, N, V] = column.anchor(from) match {
				case same if same eq column => this
				case anchored => new IndexedColumn(anchored, alias)
			}

			override def basedOn[U <: F, E <: FromClause](base :E)(implicit ext :U PartOf E) :IndexedColumn[E, S, N, V] =
				new IndexedColumn(column.basedOn(base), alias)

			override def extend[U <: F, E <: FromClause]
			             (base :E)(implicit ext :U ExtendedBy E, global :GlobalScope <:< S) :IndexedColumn[E, S, N, V] =
				new IndexedColumn(column.extend(base), alias)

			override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :IndexedColumn[E, S, N, V] =
				new IndexedColumn(mapper(column), alias)

		}


		case class IndexedChainHead[-F <: FromClause, -S >: LocalScope <: GlobalScope, T <: IndexedChain, K <: Label :ValueOf, H]
		                           (init :IndexedChainTuple[F, S, T], last :IndexedSQLExpression[F, S, H])
			extends IndexedChainTuple[F, S, T |~ (K :~ H)]
		{
			def key :K = valueOf[K]

			override def size :Int = init.size + 1

			override val readForm :SQLReadForm[T |~ (K :~ H)] = (init.readForm, last.readForm) match {
				case (i :SQLForm[T @unchecked], l :SQLForm[H @unchecked]) =>
					SQLForms.IndexedChainForm[T, K, H](i, implicitly[ValueOf[K]], l)
				case _ =>
					SQLReadForms.IndexedChainReadForm[T, K, H](init.readForm, implicitly[ValueOf[K]], last.readForm)
			}


			override def isGlobal :Boolean = last.isGlobal && init.isGlobal
			override def isAnchored :Boolean = last.isAnchored && init.isAnchored

			override def anchor(from :F) = (init.anchor(from), last.anchor(from)) match {
				case (i, l) if (i eq init) && (l eq last) => this
				case (i, l) => new IndexedChainHead(i, l)
			}


			override def basedOn[U <: F, E <: FromClause]
			                    (base :E)(implicit ext :U PartOf E) :IndexedChainTuple[E, S, T |~ (K :~ H)] =
				init.basedOn(base) |~ :~[K](last.basedOn(base))

			override def extend[U <: F, E <: FromClause]
			                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< S)
					:IndexedChainTuple[E, S, T |~ (K :~ H)] =
				init.extend(base) |~ :~[K](last.extend(base))


			override def rephrase[E <: FromClause](mapper :SQLScribe[F, E]) :IndexedChainTuple[E, S, T |~ (K :~ H)] =
				init.rephrase(mapper) |~ :~[K](last.rephrase(mapper))
		}



		trait IndexedChainMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def indexedChain[S >: LocalScope <: GlobalScope, X <: IndexedChain](e :IndexedChainTuple[F, S, X]) :Y[S, X]
		}

		type CaseIndexedChain[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = IndexedChainMatcher[F, Y]

		trait MatchIndexedChain[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends IndexedChainMatcher[F, Y] {
			def emptyChain :Y[GlobalScope, @~]

			def indexedChainHead[S >: LocalScope <: GlobalScope, I <: IndexedChain, K <: Label :ValueOf, L]
			                    (init :IndexedChainTuple[F, S, I], last :IndexedSQLExpression[F, S, L]) :Y[S, I |~ (K :~ L)]

			override def indexedChain[S >: LocalScope <: GlobalScope, X <: IndexedChain](e :IndexedChainTuple[F, S, X]) :Y[S, X] =
				(e match {
					case tuple @ IndexedChainHead(tail, head) => indexedChainHead(tail, head)(new ValueOf(tuple.key))
					case _ => emptyChain
				}).asInstanceOf[Y[S, X]]
		}
	}






	trait TupleMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ChainMatcher[F, Y] with IndexedChainMatcher[F, Y] with SeqMatcher[F, Y]
	{
		def tuple[S >: LocalScope <: GlobalScope, X](e: TupleSQL[F, S, X]): Y[S, X]
	}

	trait MatchTuple[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends TupleMatcher[F, Y] with CaseChain[F, Y] with CaseSeq[F, Y]


	trait CaseTuple[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] extends TupleMatcher[F, Y] with MatchTuple[F, Y] {

		override def chain[S >: LocalScope <: GlobalScope, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X] = tuple(e)

		override def indexedChain[S >: LocalScope <: GlobalScope, X <: IndexedChain](e :IndexedChainTuple[F, S, X]) :Y[S, X] = tuple(e)

		override def seq[S >: LocalScope <: GlobalScope, X](e: SeqTuple[F, S, X]): Y[S, Seq[X]] = tuple(e)

	}




}

