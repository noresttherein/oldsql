package net.noresttherein.oldsql.sql.ast

import scala.annotation.tailrec
import scala.collection.immutable.Seq

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.collection.{Chain, Listing, Opt, ReversedList}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.forms.SQLForms
import net.noresttherein.oldsql.sql.{ast, ColumnSQL, IndexedMapping, ListingColumnSQLMapping, ListingSQLMapping, RowProduct, Select, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.AliasedColumn
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, ExpandedBy, GroundFrom, NonEmptyFrom, PartOf, TopFrom}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionVisitor, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectAs, TopSelectAs}
import net.noresttherein.oldsql.sql.ast.SelectColumnAs.{SubselectColumnAs, TopSelectColumnAs}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SubselectSQL, TopSelectSQL}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple.{CaseChainTuple, ChainEntry, ChainTupleVisitor, EmptyChain}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ListingSQL.{ListingColumn, ListingEntry, ListingValueSQL, ListingVisitor}
import net.noresttherein.oldsql.sql.ast.TupleSQL.SeqTuple.{CaseSeq, SeqVisitor}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** `TupleSQL` represents an SQL expression consisting of several individual column values.
  * In its literal form it is written as `(col1, col2, ..., colN)`. The member expressions of this tuple
  * may not be all instances of `ColumnSQL`, but any multi-column expressions are inlined in the resulting `SQL`.
  */
trait TupleSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T] extends CompositeSQL[F, S, T] {
	def toSeq :Seq[SQLExpression[F, S, _]] = parts

	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ExpressionVisitor[F, Y]) :Y[S, T] =
		visitor.tuple(this)

	override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectSQL[T] =
		SelectSQL(from, this)

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B]) :SubselectSQL[B, T] =
		SelectSQL.subselect[B, from.type, T](from, this)

	override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
			:Select[P, T] =
		Select(from)[T](this)


	override def columnCount(implicit spelling :SQLSpelling) :Int = (0 /: parts)(_ + _.columnCount)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		explodedSpelling(from, context, params) match {
			case Seq() => SpelledSQL("()", context)
			case columns => ( "(" +: columns.reduce(_ +: ", " +: _)) + ")"
		}

	protected override def explodedSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		inOrder.foldLeft(List.empty[SpelledSQL[P]]) {
			(parts, item) => //we must assemble left to right for params, but the result is reversed due to prepending
				val ctx = if (parts.isEmpty) context else parts.head.context
				spelling.explode(item)(from, ctx, parts).toList reverse_::: parts
		}.reverse


	override def sameAs(other :CompositeSQL.*) :Boolean = other.isInstanceOf[TupleSQL[_, _, _]]
}






object TupleSQL {
	//todo: true scala tuple expressions
	def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope](expr: SQLExpression[F, S, _])
			:Opt[Seq[SQLExpression[F, S, _]]] =
		expr match {
			case t: TupleSQL[_, _, _] => Got(t.inOrder.asInstanceOf[Seq[SQLExpression[F, S, _]]])
			case _ => Lack
		}



	case class SeqTuple[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T](override val parts :Seq[SQLExpression[F, S, T]])
		extends TupleSQL[F, S, Seq[T]]
	{
		override def inOrder :Seq[SQLExpression[F, S, T]] = parts

		override def readForm :SQLReadForm[Seq[T]] = SQLReadForm.seq(inOrder.map(_.readForm))

		override def groundValue :Opt[Seq[T]] = inOrder.flatMap(_.groundValue.toOption) match {
			case res if res.length == inOrder.length => Got(res)
			case _ => Lack
		}

		override def anchor(from :F) :SQLExpression[F, S, Seq[T]] = new SeqTuple(parts.map(_.anchor(from)))

		override def rephrase[E <: RowProduct](mapper: SQLScribe[F, E]) :SeqTuple[E, S, T] =
			SeqTuple(inOrder.map(mapper(_)))

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor: ExpressionVisitor[F, Y]): Y[S, Seq[T]] =
			visitor.seq(this)

		override def split(implicit scope :OperationType) :Seq[ColumnSQL[F, S, _]] = parts.flatMap(_.split)

		override def toString :String = inOrder.mkString("Seq(", ", ", ")")
	}



	object SeqTuple {
		implicit def literalSeq[T :SQLForm](items :Seq[T]) :SeqTuple[RowProduct, GlobalScope, T] =
			new SeqTuple(items.map(SQLLiteral(_)))

		implicit def expressionSeq[F <: RowProduct, S >: LocalScope <: GlobalScope, T]
		                          (items :Seq[SQLExpression[F, S, T]]) :SeqTuple[F, S, T] =
			new SeqTuple(items)


		trait SeqVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def seq[S >: LocalScope <: GlobalScope, X](e :SeqTuple[F, S, X]) :Y[S, Seq[X]]
		}

		type MatchSeq[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = SeqVisitor[F, Y]

		type CaseSeq[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = SeqVisitor[F, Y]
	}






	/** A tuple expression implementation with flexible length, mapping nominally
	  * to a [[net.noresttherein.oldsql.collection.Chain Chain]] subtype containing the member expressions.
	  */ //todo: conversion from tuples of SQLExpressions
	sealed trait ChainTuple[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T <: Chain] extends TupleSQL[F, S, T] {
		def size :Int

		@inline def init[I <: Chain](implicit nonEmpty :Chain.Init[T, I]) :ChainTuple[F, S, I] =
			this.asInstanceOf[ChainEntry[F, S, I, _]].init

		@inline def last[L](implicit nonEmpty :Chain.Last[T, L]) :SQLExpression[F, S, L] =
			this.asInstanceOf[ChainEntry[F, S, _, L]].last

		def ~[E <: F, O >: LocalScope <: S, H](head :SQLExpression[E, O, H]) :ChainEntry[E, O, T, H] =
			new ChainEntry(this, head)

		protected override def parts :Seq[SQLExpression[F, S, _]] = {
			@tailrec def rec(e :ChainTuple[F, S, _], acc :List[SQLExpression[F, S, _]] = Nil)
					:Seq[SQLExpression[F, S, _]] =
				e match {
					case ChainEntry(tail, head) => rec(tail, head::acc)
					case _ => acc
				}
			rec(this)
		}

//		def toSeq :Seq[SQLExpression[F, S, _]] = parts

		override def asGlobal :Option[ChainTuple[F, GlobalScope, T]] =
			if (isGlobal) Some(this.asInstanceOf[ChainTuple[F, GlobalScope, T]])
			else None

		override def anchor(from :F) :ChainTuple[F, S, T] = rephrase(SQLScribe.anchor(from))

		//overriden to narrow down the result type
		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ChainTuple[E, S, T]

		//overriden to narrow down the result type
		override def basedOn[U <: F, E <: RowProduct]
		                    (base :E)(implicit ext :U PartOf E) :ChainTuple[E, S, T] =
			rephrase(SQLScribe.expand(base))

		//overriden to narrow down the result type
		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ext :U ExpandedBy E, global :GlobalScope <:< S) :ChainTuple[E, S, T] =
			rephrase(SQLScribe.expand(base))

		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ExpressionVisitor[F, Y]) :Y[S, T] =
			visitor.chainTuple(this)


		override def toString :String = inOrder.mkString("(", " ~ ", ")")
	}



	object ChainTuple {

		def apply() :ChainTuple[RowProduct, GlobalScope, @~] = EmptyChain

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, T](e :SQLExpression[F, S, T]) :ChainTuple[F, S, @~ ~ T] =
			new ChainEntry(EmptyChain, e)


		class ChainEntry[-F <: RowProduct, -S >: LocalScope <: GlobalScope, I <: Chain, L]
		                (override val init :ChainTuple[F, S, I], override val last :SQLExpression[F, S, L])
			extends ChainSQL[F, S, I, L](init, last) with ChainTuple[F, S, I ~ L]
		{
			override def size :Int = init.size + 1

			override def groundValue :Opt[I ~ L] = for { i <- init.groundValue; l <- last.groundValue } yield i ~ l

			override def anchor(from :F) :ChainEntry[F, S, I, L] =
				(init.anchor(from), last.anchor(from)) match {
					case (i, l) if (i eq init) && (l eq last) => this
					case (i, l) => new ChainEntry(i, l)
				}

			override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ChainTuple[E, S, I ~ L] =
				init.rephrase(mapper) ~ mapper(last)

			override def expand[U <: F, E <: RowProduct]
			                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S) :ChainTuple[E, S, I ~ L] =
				init.expand(base) ~ last.expand(base)

			override def canEqual(that :Any) :Boolean = that.getClass == getClass
		}


		object ChainEntry {
			def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, I <: Chain, L]
			         (init :ChainTuple[F, S, I], last :SQLExpression[F, S, L]) :ChainEntry[F, S, I, L] =
				new ChainEntry(init, last)

			def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, C]
			           (e :SQLExpression[F, S, C]) :Option[(ChainTuple[F, S, _ <: Chain], SQLExpression[F, S, _])] =
				e match {
					case entry :ChainEntry[F, S, _, _] => Some((entry.init, entry.last))
					case _ => None
				}

			def unapply[F <: RowProduct, S >: LocalScope <: GlobalScope, I <: Chain, L]
			           (e :ChainTuple[F, S, I ~ L]) :Option[(ChainTuple[F, S, I], SQLExpression[F, S, L])] =
				e match {
					case entry :ChainEntry[F, S, I @unchecked, L @unchecked] => Some((entry.init, entry.last))
					case _ => None
				}
		}


		case object EmptyChain extends SQLTerm[@~]
			with ChainTuple[RowProduct, GlobalScope, @~] with ListingSQL[RowProduct, GlobalScope, @~]
		{
			override def size :Int = 0

			override def toSeq :List[Nothing] = Nil

			override def readForm :SQLReadForm[@~] = SQLReadForm[@~]

			override val groundValue :Opt[@~] = Got(@~)
			override def isGlobal = true
			override def asGlobal :Option[EmptyChain.type] = Some(this)
			override def isAnchored = true
			override def isAnchored(from :RowProduct) = true
			override def anchor(from :RowProduct) :this.type = this

			override def rephrase[E <: RowProduct](mapper :SQLScribe[RowProduct, E]) :this.type = this

			override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit ext :U PartOf E) :this.type = this

			override def expand[U <: RowProduct, E <: RowProduct]
			             (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :this.type = this


			override def split(implicit scope :OperationType) :Seq[ColumnSQL[RowProduct, GlobalScope, _]] =
				ReversedList.empty

			protected override def defaultSpelling[P]
			                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
			                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
				SpelledSQL(context)

			protected override def explodedSpelling[P]
			                       (from :RowProduct, context :SQLContext[P], params :Parameterization[P, RowProduct])
			                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
				Nil
		}


		trait ChainTupleVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def chainTuple[S >: LocalScope <: GlobalScope, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X]
		}

		type CaseChainTuple[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ChainTupleVisitor[F, Y]

		trait MatchChainTuple[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ChainTupleVisitor[F, Y] {
			def emptyChain :Y[GlobalScope, @~]

			def chainHead[S >: LocalScope <: GlobalScope, I <: Chain, L]
			             (init :ChainTuple[F, S, I], last :SQLExpression[F, S, L]) :Y[S, I ~ L]

			override def chainTuple[S >: LocalScope <: GlobalScope, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X] =
				(e match {
					case ChainEntry(tail, head) => chainHead(tail, head)
					case _ => emptyChain
				}).asInstanceOf[Y[S, X]]
		}

	}






	//does not extend ChainTuple because ListingEntry doesn't extend ChainEntry and it would cause problems
	// in pattern matching a ChainTuple
	/** A variant of [[ast.TupleSQL.ChainTuple ChainTuple]] which maps
	  * to a [[net.noresttherein.oldsql.collection.Listing Listing]] - a `Chain` subtype with key-value pairs
	  * in the form of `K :~ V` as its only elements. The keys exist only on the type level in the `T` type parameter;
	  * Member SQL expressions are not of the `K :~ V` type, but rather directly the value type `V`.
	  * The indexing has no special effect on the generated SQL other than an `ListingColumn` being a subclass of
	  * [[net.noresttherein.oldsql.sql.ColumnSQL.AliasedColumn AliasedColumn]]. Instead, it allows referencing
	  * the columns in a type safe way when mapping results of a select, if this expression appears
	  * in the ''select'' clause. Additionally, a ''select'' clause consisting solely of this type can be matched
	  * to any [[net.noresttherein.oldsql.schema.bits.IndexedSchemaMapping IndexedSchemaMapping]] with a component chain
	  * containing exactly the same key-value pairs as the `T` type parameter, regardless of their order.
	  * Non-column components of the mapping must in that case recursively match a nested indexed tuple paired
	  * with the same key type in this `LiteralChain` as the component's label.
	  * An `ListingSQL` can consist only of expressions implementing `ListingValueSQL` - a sealed type
	  * extended only by [[ast.TupleSQL.ListingSQL.ListingColumn ListingColumn]]
	  * and this trait. This ensures that every column of the whole expression is assigned a unique key,
	  * which however may be a sequence of `Label`s, rather than a single one, if this expression contains
	  * another indexed tuples as its subexpressions.
	  * Note that, in order to provide unambigous cases when pattern matching, this type does not extend the standard
	  * `ChainTuple`.
	  */
	trait ListingSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T <: Listing]
		extends TupleSQL[F, S, T] with ListingValueSQL[F, S, T]
	{
		def size :Int
		override def mapping[O] :ListingSQLMapping[F, S, T, O] = ListingSQLMapping(this)

		protected override def parts :Seq[ListingValueSQL[F, S, _]] = {
			@tailrec def rec(e :ListingSQL[F, S, _], acc :List[ListingValueSQL[F, S, _]] = Nil) :Seq[ListingValueSQL[F, S, _]] =
				e match {
					case ListingEntry(tail, head) => rec(tail, head::acc)
					case _ => acc
				}
			rec(this)
		}

		override def toSeq :Seq[ListingValueSQL[F, S, _]] = parts

		def toMap :Map[String, ListingValueSQL[F, S, _]] = {
			@tailrec def rec(e :ListingSQL[F, S, _], acc :Map[String, ListingValueSQL[F, S, _]])
					:Map[String, ListingValueSQL[F, S, _]] =
				e match {
					case entry :ListingEntry[F, S, _, _, _] => rec(entry.init, acc.updated(entry.key, entry.last))
					case _ => acc
				}
			rec(this, Map.empty)
		}

		override def asGlobal :Option[ListingSQL[F, GlobalScope, T]] =
			if (isGlobal) Some(this.asInstanceOf[ListingSQL[F, GlobalScope, T]])
			else None

		override def anchor(from :F) :ListingSQL[F, S, T] = rephrase(SQLScribe.anchor(from))

		//overriden to narrow down the result type
		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ListingSQL[E, S, T]

		//overriden to narrow down the result type
		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ListingSQL[E, S, T] =
			rephrase(SQLScribe.expand(base))

		//overriden to narrow down the result type
		override def expand[U <: F, E <: RowProduct]
		             (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S) :ListingSQL[E, S, T] =
			rephrase(SQLScribe.expand(base))


		protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](visitor :ExpressionVisitor[F, Y]) :Y[S, T] =
			visitor.listing(this)


		override def selectFrom(from :F) :SelectAs[from.Base, IndexedMapping.Of[T]#Projection] =
			if (from.isParameterized)
				throw new IllegalArgumentException(
					s"Cannot use a parameterized clause as a basis for select $this: $from."
				)
			else if (from.isSubselect) {
				subselectFrom(from.asInstanceOf[ExactSubselectOf[from.type, NonEmptyFrom]])
					.asInstanceOf[SelectAs[from.Base, IndexedMapping.Of[T]#Projection]]
			} else
				topSelectFrom(from.asInstanceOf[F with GroundFrom])

		override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectAs[IndexedMapping.Of[T]#Projection] =
			SelectSQL[E, T](from, this)

		override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
				:SubselectAs[B, IndexedMapping.Of[T]#Projection] =
			SelectSQL.subselect[B, ExactSubselectOf[F, B], T](from, this)

		override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
				:SelectMapping[P, IndexedMapping.Of[T]#Projection] =
			Select(from)(this)

//		override def paramSelectFrom[E <: F with TopFrom { type Params = P }, P <: Chain](from :E)
//				:SelectAs[P, IndexedMapping.Of[T]#Projection] =
//			Select(from, this)
//

		def |~[E <: F, O >: LocalScope <: S, K <: Label :ValueOf, H]
		      (head :K :~ ListingValueSQL[E, O, H]) :ListingEntry[E, O, T, K, H] =
			new ListingEntry(this, head.value)

		def |~[E <: F, O >: LocalScope <: S, K <: Label, H]
		      (head :ListingColumn[E, O, K, H]) :ListingEntry[E, O, T, K, H] =
			new ListingEntry(this, head)(new ValueOf(head.alias))

		def |~[E <: F, O >: LocalScope <: S, K <: Label :ValueOf, H]
		       (head :K :~ ColumnSQL[E, O, H]) :ListingEntry[E, O, T, K, H] =
			new ListingEntry(this, new ListingColumn(head.value, valueOf[K]))

		override def toString :String = {
			def rec(e :ListingSQL[_, _, _], res :StringBuilder) :StringBuilder = e match {
				case EmptyChain => res ++= "@~"
				case tuple :ListingEntry[_, _, _, _, _] =>
					rec(tuple.init, res) ++= " |~ " ++= tuple.key ++= ":~" ++= tuple.last.toString
			}
			rec(this, new StringBuilder).toString
		}

	}



	object ListingSQL {
		final val EmptyListing = ChainTuple.EmptyChain

		def apply() :ListingSQL[RowProduct, GlobalScope, @~] = EmptyListing

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, N <: Label, T]
		         (e :ListingColumn[F, S, N, T]) :ListingSQL[F, S, @~ |~ (N :~ T)] =
			new ListingEntry(EmptyChain, e)(new ValueOf(e.alias))

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, N <: Label :ValueOf, T]
		         (e :N :~ ColumnSQL[F, S, T]) :ListingSQL[F, S, @~ |~ (N :~ T)] =
			new ListingEntry(EmptyChain, new ListingColumn(e.value, valueOf[N]))

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, N <: Label, T]
		         (key :N, value :ColumnSQL[F, S, T]) :ListingSQL[F, S, @~ |~ (N :~ T)] =
			new ListingEntry(EmptyChain, key @: value)(new ValueOf(key))



		sealed trait ListingValueSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T]
			extends SQLExpression[F, S, T]
		{
			def mapping[O] :ListingSQLMapping[F, S, T, O]

			override def asGlobal :Option[ListingValueSQL[F, GlobalScope, T]]

			override def anchor(from :F) :ListingValueSQL[F, S, T]

			override def basedOn[U <: F, E <: RowProduct]
			                    (base :E)(implicit ext :U PartOf E) :ListingValueSQL[E, S, T]

			override def expand[U <: F, E <: RowProduct]
			             (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S) :ListingValueSQL[E, S, T]

			def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ListingValueSQL[E, S, T]

		}



		class ListingColumn[-F <: RowProduct, -S >: LocalScope <: GlobalScope, N <: Label, V]
		                   (override val column :ColumnSQL[F, S, V], override val alias :N)
			extends AliasedColumn[F, S, V](column, alias) with ListingValueSQL[F, S, V]
		{
			override def mapping[O] :ListingColumnSQLMapping[F, S, N, V, O] =
				ListingColumnSQLMapping(this)

			override def asGlobal :Option[ListingColumn[F, GlobalScope, N, V]] =
				if (column.isGlobal) Some(this.asInstanceOf[ListingColumn[F, GlobalScope, N, V]])
				else None

			override def anchor(from :F) :ListingColumn[F, S, N, V] = column.anchor(from) match {
				case same if same eq column => this
				case anchored => new ListingColumn(anchored, alias)
			}

			override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ListingColumn[E, S, N, V] =
				new ListingColumn(mapper(column), alias)

			override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ListingColumn[E, S, N, V] =
				new ListingColumn(column.basedOn(base), alias)

			override def expand[U <: F, E <: RowProduct]
			             (base :E)(implicit ext :U ExpandedBy E, global :GlobalScope <:< S) :ListingColumn[E, S, N, V] =
				new ListingColumn(column.expand(base), alias)


			override def selectFrom(from :F) :SelectColumnAs[from.Base, IndexedMapping.Of[V]#Column, V] =
				if (from.isParameterized)
					throw new IllegalArgumentException(
						s"Cannot use a parameterized clause as a basis for select $this: $from."
					)
				else if (from.isSubselect) {
					subselectFrom(from.asInstanceOf[ExactSubselectOf[from.type, NonEmptyFrom]])
						.asInstanceOf[SelectColumnAs[from.Base, IndexedMapping.Of[V]#Column, V]]
				} else
					topSelectFrom(from.asInstanceOf[F with GroundFrom])

			override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectColumnAs[IndexedMapping.Of[V]#Column, V] =
				SelectSQL(from, this)

			override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B])
					:SubselectColumnAs[B, IndexedMapping.Of[V]#Column, V] =
				SelectSQL.subselect[B, ExactSubselectOf[F, B], N, V](from, this)

			override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
					:SelectMapping[P, IndexedMapping.Of[V]#Column] =
				Select(from)[N, V](this)

		}



		case class ListingEntry[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T <: Listing, K <: Label :ValueOf, H]
		                       (init :ListingSQL[F, S, T], last :ListingValueSQL[F, S, H])
			extends ListingSQL[F, S, T |~ (K :~ H)]
		{
			def key :K = valueOf[K]

			override def size :Int = init.size + 1

			//todo: get rid of explicit references to SQLForms & SQLForms
			override val readForm :SQLReadForm[T |~ (K :~ H)] = (init.readForm, last.readForm) match {
				case (i :SQLForm[T @unchecked], l :SQLForm[H @unchecked]) =>
					SQLForms.ListingForm[T, K, H](i, implicitly[ValueOf[K]], l)
				case _ =>
					SQLForms.ListingReadForm[T, K, H](init.readForm, implicitly[ValueOf[K]], last.readForm)
			}

			override def groundValue :Opt[T |~ (K :~ H)] =
				for { i <- init.groundValue; l <- last.groundValue } yield i |~ :~[K](l)

			override def isGlobal :Boolean = last.isGlobal && init.isGlobal
			override def isAnchored :Boolean = last.isAnchored && init.isAnchored
			override def isAnchored(from :F) :Boolean = last.isAnchored(from) && init.isAnchored(from)

			override def anchor(from :F) :ListingSQL[F, S, T |~ (K :~ H)] =
				(init.anchor(from), last.anchor(from)) match {
					case (i, l) if (i eq init) && (l eq last) => this
					case (i, l) => new ListingEntry(i, l)
				}


			override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ListingSQL[E, S, T |~ (K :~ H)] =
				init.rephrase(mapper) |~ :~[K](last.rephrase(mapper))

			override def basedOn[U <: F, E <: RowProduct]
			                    (base :E)(implicit ext :U PartOf E) :ListingSQL[E, S, T |~ (K :~ H)] =
				init.basedOn(base) |~ :~[K](last.basedOn(base))

			override def expand[U <: F, E <: RowProduct]
			                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S)
					:ListingSQL[E, S, T |~ (K :~ H)] =
				init.expand(base) |~ :~[K](last.expand(base))


			override def split(implicit scope :OperationType) :Seq[ColumnSQL[F, S, _]] = init.split ++ last.split
		}



		trait ListingVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def listing[S >: LocalScope <: GlobalScope, X <: Listing](e :ListingSQL[F, S, X]) :Y[S, X]
		}

		type CaseListing[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ListingVisitor[F, Y]

		trait MatchListing[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ListingVisitor[F, Y] {
			def emptyChain :Y[GlobalScope, @~]

			def listingEntry[S >: LocalScope <: GlobalScope, I <: Listing, K <: Label :ValueOf, L]
			                (init :ListingSQL[F, S, I], last :ListingValueSQL[F, S, L]) :Y[S, I |~ (K :~ L)]

			override def listing[S >: LocalScope <: GlobalScope, X <: Listing](e :ListingSQL[F, S, X]) :Y[S, X] =
				(e match {
					case tuple @ ListingEntry(tail, head) => listingEntry(tail, head)(new ValueOf(tuple.key))
					case _ => emptyChain
				}).asInstanceOf[Y[S, X]]
		}
	}






	trait TupleVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ChainTupleVisitor[F, Y] with ListingVisitor[F, Y] with SeqVisitor[F, Y]
	{
		def tuple[S >: LocalScope <: GlobalScope, X](e: TupleSQL[F, S, X]): Y[S, X]
	}

	trait MatchTuple[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends TupleVisitor[F, Y] with CaseChainTuple[F, Y] with CaseSeq[F, Y]


	trait CaseTuple[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends TupleVisitor[F, Y] with MatchTuple[F, Y] {

		override def chainTuple[S >: LocalScope <: GlobalScope, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X] = tuple(e)

		override def listing[S >: LocalScope <: GlobalScope, X <: Listing](e :ListingSQL[F, S, X]) :Y[S, X] = tuple(e)

		override def seq[S >: LocalScope <: GlobalScope, X](e: SeqTuple[F, S, X]): Y[S, Seq[X]] = tuple(e)

	}




}

