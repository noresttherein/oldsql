package net.noresttherein.oldsql.sql.ast

import scala.annotation.tailrec
import scala.collection.immutable.Seq

import net.noresttherein.oldsql.collection.{Chain, Listing, Opt}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.ChunkedString
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.forms.SQLForms
import net.noresttherein.oldsql.sql.{ast, ColumnSQL, IndexedMapping, ListingColumnSQLMapping, ListingSQLMapping, ParamSelect, RowProduct, SQLExpression, SQLMapping}
import net.noresttherein.oldsql.sql.ColumnSQL.AliasedColumn
import net.noresttherein.oldsql.sql.ParamSelect.ParamSelectAs
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, ExpandedBy, GroundFrom, NonEmptyFrom, PartOf, TopFrom}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{CompositeSQL, ExpressionMatcher, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SelectAs, SelectColumnAs, SubselectAs, SubselectColumnAs, SubselectSQL, TopSelectAs, TopSelectColumnAs, TopSelectSQL}
import net.noresttherein.oldsql.sql.ast.SQLTerm.SQLLiteral
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple.{CaseChain, ChainEntry, ChainMatcher, EmptyChain}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ListingSQL.{ListingColumn, ListingEntry, ListingMatcher, ListingValueSQL}
import net.noresttherein.oldsql.sql.ast.TupleSQL.SeqTuple.{CaseSeq, SeqMatcher}
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** `TupleSQL` represents an SQL expression consisting of several individual column values.
  * In its literal form it is written as `(col1, col2, ..., colN)`. The member expressions of this tuple
  * may not be all instances of `ColumnSQL`, but any multi-column expressions are inlined in the resulting `SQL`.
  */
trait TupleSQL[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T] extends CompositeSQL[F, S, T] {

	override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, Y]) :Y[S, T] =
		matcher.tuple(this)

	override def topSelectFrom[E <: F with GroundFrom](from :E) :TopSelectSQL[T] =
		SelectSQL(from, this)

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B]) :SubselectSQL[B, T] =
		SelectSQL.subselect[B, from.type, T](from, this)

	override def paramSelectFrom[P <: Chain, G <: F](from :TopFrom { type Generalized <: G; type Params = P })
			:ParamSelect[P, T] =
		ParamSelect(from)[T](this)


	protected override def defaultSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		SpelledSQL("(", context, params) + (inlineSpelling(_, _)) + ")"

	protected override def inlineSpelling[P, E <: F](context :SQLContext, params :Parameterization[P, E])
	                                                (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
		if (parts.isEmpty)
			SpelledSQL(context, params)
		else
			inOrder.view.scanLeft(SpelledSQL(context, params)) {
				(sql, item) => spelling.inline(item :SQLExpression[E, S, _])(sql.context, sql.params)
			}.tail.reduce(_.sql +: ", " +: _)


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

		override def anchor(from :F) :SQLExpression[F, S, Seq[T]] = new SeqTuple(parts.map(_.anchor(from)))

		override def rephrase[E <: RowProduct](mapper: SQLScribe[F, E]) :SeqTuple[E, S, T] =
			SeqTuple(inOrder.map(mapper(_)))

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher: ExpressionMatcher[F, Y]): Y[S, Seq[T]] =
			matcher.seq(this)

		override def toString :String = inOrder.mkString("Seq(", ", ", ")")
	}



	object SeqTuple {
		implicit def literalSeq[T :SQLForm](items :Seq[T]) :SeqTuple[RowProduct, GlobalScope, T] =
			new SeqTuple(items.map(SQLLiteral(_)))

		implicit def expressionSeq[F <: RowProduct, S >: LocalScope <: GlobalScope, T]
		                          (items :Seq[SQLExpression[F, S, T]]) :SeqTuple[F, S, T] =
			new SeqTuple(items)


		trait SeqMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def seq[S >: LocalScope <: GlobalScope, X](e :SeqTuple[F, S, X]) :Y[S, Seq[X]]
		}

		type MatchSeq[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = SeqMatcher[F, Y]

		type CaseSeq[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = SeqMatcher[F, Y]
	}






	/** A tuple expression implementation with flexible length, mapping nominally
	  * to a [[net.noresttherein.oldsql.collection.Chain Chain]] subtype containing the member expressions.
	  */
	sealed trait ChainTuple[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T <: Chain] extends TupleSQL[F, S, T] {
		def size :Int

		protected override def parts :Seq[SQLExpression[F, S, _]] = {
			@tailrec def rec(formula :ChainTuple[F, S, _], acc :List[SQLExpression[F, S, _]] = Nil)
					:Seq[SQLExpression[F, S, _]] =
				formula match {
					case ChainEntry(tail, head) => rec(tail, head::acc)
					case _ => acc
				}
			rec(this)
		}

		def toSeq :Seq[SQLExpression[F, S, _]] = parts

		override def asGlobal :Option[ChainTuple[F, GlobalScope, T]] =
			if (isGlobal) Some(this.asInstanceOf[ChainTuple[F, GlobalScope, T]])
			else None

		override def anchor(from :F) :ChainTuple[F, S, T]

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

		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, Y]) :Y[S, T] =
			matcher.chain(this)


		def ~[E <: F, O >: LocalScope <: S, H](head :SQLExpression[E, O, H]) :ChainEntry[E, O, T, H] =
			new ChainEntry(this, head)

		override def toString :String = inOrder.mkString("(", " ~ ", ")")
	}



	object ChainTuple {

		def apply() :ChainTuple[RowProduct, GlobalScope, @~] = EmptyChain

		def apply[F <: RowProduct, S >: LocalScope <: GlobalScope, T](e :SQLExpression[F, S, T]) :ChainTuple[F, S, @~ ~ T] =
			new ChainEntry(EmptyChain, e)



		case class ChainEntry[-F <: RowProduct, -S >: LocalScope <: GlobalScope, T <: Chain, H]
		                     (init :ChainTuple[F, S, T], last :SQLExpression[F, S, H])
			extends ChainTuple[F, S, T ~ H]
		{
			override def size :Int = init.size + 1

			//todo: get rid of explicit references to SQLForms & SQLForms
			override val readForm :SQLReadForm[T ~ H] = (init.readForm, last.readForm) match {
				case (i :SQLForm[T @unchecked], l :SQLForm[H @unchecked]) =>
					SQLForms.ChainForm(i, l)
				case _ =>
					SQLForms.ChainReadForm[T, H](init.readForm, last.readForm)
			}

			override def isGlobal :Boolean = last.isGlobal && init.isGlobal
			override def isAnchored :Boolean = last.isAnchored && init.isAnchored

			override def anchor(from :F) = (init.anchor(from), last.anchor(from)) match {
				case (i, l) if (i eq init) && (l eq last) => this
				case (i, l) => new ChainEntry(i, l)
			}

			override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ChainTuple[E, S, T ~ H] =
				init.rephrase(mapper) ~ mapper(last)

			override def expand[U <: F, E <: RowProduct]
			                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S) :ChainTuple[E, S, T ~ H] =
				init.expand(base) ~ last.expand(base)
		}



		case object EmptyChain extends SQLTerm[@~]
			with ChainTuple[RowProduct, GlobalScope, @~] with ListingSQL[RowProduct, GlobalScope, @~]
		{
			override def size :Int = 0

			override def toSeq :List[Nothing] = Nil

			protected override def form :SQLForm[@~] = SQLForm[@~]
			override def writeForm :SQLWriteForm[Unit] = SQLWriteForm.empty
			override def readForm :SQLReadForm[@~] = SQLReadForm[@~]

			override val groundValue :Option[@~] = Option(@~)
			override def isGlobal = true
			override def asGlobal :Option[EmptyChain.type] = Some(this)
			override def isAnchored = true
			override def anchor(from :RowProduct) :this.type = this

			override def rephrase[E <: RowProduct](mapper :SQLScribe[RowProduct, E]) :this.type = this

			override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit ext :U PartOf E) :this.type = this

			override def expand[U <: RowProduct, E <: RowProduct]
			             (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :this.type = this

			protected override def defaultSpelling[P, E <: RowProduct]
			                                      (context :SQLContext, params :Parameterization[P, E])
			                                      (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
				SpelledSQL(ChunkedString.empty, context, params)

			protected override def inlineSpelling[P, E <: RowProduct]
			                                     (context :SQLContext, params :Parameterization[P, E])
			                                     (implicit spelling :SQLSpelling) :SpelledSQL[P, E] =
				defaultSpelling(context, params)
		}



		trait ChainMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def chain[S >: LocalScope <: GlobalScope, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X]
		}

		type CaseChain[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ChainMatcher[F, Y]

		trait MatchChain[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ChainMatcher[F, Y] {
			def emptyChain :Y[GlobalScope, @~]

			def chainHead[S >: LocalScope <: GlobalScope, I <: Chain, L]
			             (init :ChainTuple[F, S, I], last :SQLExpression[F, S, L]) :Y[S, I ~ L]

			override def chain[S >: LocalScope <: GlobalScope, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X] = (e match {
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

		def toSeq :Seq[ListingValueSQL[F, S, _]] = parts

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

		override def anchor(from :F) :ListingSQL[F, S, T]

		//overriden to narrow down the result type
		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ListingSQL[E, S, T]

		//overriden to narrow down the result type
		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ListingSQL[E, S, T] =
			rephrase(SQLScribe.expand(base))

		//overriden to narrow down the result type
		override def expand[U <: F, E <: RowProduct]
		             (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S) :ListingSQL[E, S, T] =
			rephrase(SQLScribe.expand(base))


		override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]](matcher :ExpressionMatcher[F, Y]) :Y[S, T] =
			matcher.listing(this)


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
				:ParamSelectAs[P, IndexedMapping.Of[T]#Projection] =
			ParamSelect(from)(this)

//		override def paramSelectFrom[E <: F with TopFrom { type Params = P }, P <: Chain](from :E)
//				:ParamSelectAs[P, IndexedMapping.Of[T]#Projection] =
//			ParamSelect(from, this)
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
					:ParamSelectAs[P, IndexedMapping.Of[V]#Column] =
				ParamSelect(from)[N, V](this)

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


			override def isGlobal :Boolean = last.isGlobal && init.isGlobal
			override def isAnchored :Boolean = last.isAnchored && init.isAnchored

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
		}



		trait ListingMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
			def listing[S >: LocalScope <: GlobalScope, X <: Listing](e :ListingSQL[F, S, X]) :Y[S, X]
		}

		type CaseListing[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ListingMatcher[F, Y]

		trait MatchListing[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends ListingMatcher[F, Y] {
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






	trait TupleMatcher[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends ChainMatcher[F, Y] with ListingMatcher[F, Y] with SeqMatcher[F, Y]
	{
		def tuple[S >: LocalScope <: GlobalScope, X](e: TupleSQL[F, S, X]): Y[S, X]
	}

	trait MatchTuple[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends TupleMatcher[F, Y] with CaseChain[F, Y] with CaseSeq[F, Y]


	trait CaseTuple[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends TupleMatcher[F, Y] with MatchTuple[F, Y] {

		override def chain[S >: LocalScope <: GlobalScope, X <: Chain](e :ChainTuple[F, S, X]) :Y[S, X] = tuple(e)

		override def listing[S >: LocalScope <: GlobalScope, X <: Listing](e :ListingSQL[F, S, X]) :Y[S, X] = tuple(e)

		override def seq[S >: LocalScope <: GlobalScope, X](e: SeqTuple[F, S, X]): Y[S, Seq[X]] = tuple(e)

	}




}

