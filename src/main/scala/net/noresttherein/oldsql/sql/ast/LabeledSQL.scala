package net.noresttherein.oldsql.sql.ast

import scala.annotation.{implicitAmbiguous, implicitNotFound, tailrec}
import scala.collection.immutable.SeqMap
import scala.collection.mutable.ListBuffer

import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainLength}
import net.noresttherein.oldsql.collection.{Chain, Listing, Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{Bug, IllegalExpressionException, MismatchedExpressionsException}
import net.noresttherein.oldsql.morsels.{generic, Extractor}
import net.noresttherein.oldsql.morsels.generic.=>:
import net.noresttherein.oldsql.schema.bits.{IndexedMapping, LabelPath}
import net.noresttherein.oldsql.schema.bits.LabelPath.{~/, Label, LabelPathPrefix, SplitLabelPath}
import net.noresttherein.oldsql.schema.SQLReadForm
import net.noresttherein.oldsql.slang.{cast2TypeParams, downcastTypeParam, mappingMethods, SeqExtension}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, RowShape, Select, SQLExpression, TypedListingColumnSQLMapping, TypedListingSQLMapping}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, GroundRow, NonEmptyRow, PartOf, ProperSubselectOf, TopRow}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, Grouped, Single, SpecificExpressionVisitor, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.ast.ChainTuple.{EmptyChain, EmptyChainSQL}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.GroundingCompositeSQLTemplate
import net.noresttherein.oldsql.sql.ast.LabeledSQL.{EmptyListing, LabeledColumnSQL, LabeledEntry, LabeledItem, LabeledValueSQL}
import net.noresttherein.oldsql.sql.ast.RecordSQL.{RecordItem, RecordProjection, RecordShuffle, SubRecord}
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectAs, TopSelectAs}
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.GenericColumnDecoratorTemplate
import net.noresttherein.oldsql.sql.ast.EmptySQL.{AnyEmptyVisitor, SpecificEmptyVisitor}
import net.noresttherein.oldsql.sql.ast.InlineSQL.InlineItem
import net.noresttherein.oldsql.sql.ast.LabeledSQL.LabeledColumnSQL.{AnyLabeledColumnVisitor, SpecificLabeledColumnVisitor}
import net.noresttherein.oldsql.sql.ast.SelectColumnAs.{SubselectColumnAs, TopSelectColumnAs}
import net.noresttherein.oldsql.sql.mechanics.{sql_=>, Reform, SQLAdaptation, SQLConversion, SQLScribe, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.SQLConversion.{ConvertRecord, ReorderRecord}






//does not extend ChainTuple because LabeledEntry doesn't extend ChainEntry and it would cause problems
// in pattern matching a ChainTuple
/** A variant of [[net.noresttherein.oldsql.sql.ast.ChainTuple ChainTuple]] which maps
  * to a [[net.noresttherein.oldsql.collection.Listing Listing]] - a `Chain` subtype with key-value pairs
  * in the form of `K :~ V` as its only elements. The keys exist only on the type level in the `T` type parameter;
  * Member SQL expressions are not of the `K :~ V` type, but rather directly the value type `V`.
  * The indexing has no special effect on the generated SQL other than `ListingColumn` being a subclass of
  * [[net.noresttherein.oldsql.sql.ast.AliasedSQL AliasedColumn]]. Instead, it allows referencing
  * the columns in a type safe way when mapping results of a select, if this expression appears
  * in the ''select'' clause. Additionally, a ''select'' clause consisting solely of this type can be matched
  * to any [[net.noresttherein.oldsql.schema.bits.IndexedSchemaMapping IndexedSchemaMapping]] with a component chain
  * containing exactly the same key-value pairs as the `T` type parameter, regardless of their order.
  * Non-column components of the mapping must in that case recursively match a nested indexed tuple paired
  * with the same key type in this `LiteralChain` as the component's label.
  * An `LabeledSQL` can consist only of expressions implementing `LabeledValueSQL` - a sealed type
  * extended only by [[net.noresttherein.oldsql.sql.ast.LabeledSQL.LabeledColumnSQL LabeledColumnSQL]]
  * and this trait. This ensures that every column of the whole expression is assigned a unique key,
  * which however may be a sequence of `Label`s, rather than a single one, if this expression contains
  * another indexed tuples as its subexpressions.
  * Note that, in order to provide unambiguous cases when pattern matching, this type does not extend the standard
  * `ChainTuple`.
  */
/*
trait ListingSQL[-F <: RowProduct, -S >: Grouped <: GlobalScope, V <: Listing] extends InlineSQL[F, S, V] {
	override type Item[-E <: RowProduct, -D >: Grouped <: GlobalScope, X] = LabeledItem[E, D, _ <: Label, X]

	def init[I <: Listing](implicit nonEmpty: V <:< (I |~ Listing.Item)) :ListingSQL[F, S, I]

	def last[L](implicit nonEmpty: V <:< (Listing |~ (Label :~ L))) :SQLExpression[F, S, L]

	def lastKey[K <: Label](implicit nonEmpty: V <:< (Listing |~ (K :~ Any))) :K

	def lastItem[K <: Label, L](implicit notEmpty :V <:< (Listing |~ (K :~ L))) :LabeledItem[F, S, K, L]

	def |~[E <: F, O >: Grouped <: S, K <: Label :ValueOf, H]
	      (entry :K :~ SQLExpression[E, O, H]) :ListingSQL[E, O, V |~ (K :~ H)] =
		new ListingEntry(this, entry.value)

	def |~[E <: F, O >: Grouped <: S, K <: Label, H]
	      (entry :(K, SQLExpression[E, O, H])) :ListingSQL[E, O, V |~ (K :~ H)] =
		new ListingEntry(this, entry._2)(new ValueOf[K](entry._1))

	//declared here due to overloading rules
	def |~[K <: Label :ValueOf, X :SQLForm](n :Null) :ListingLayout[V |~ (K :~ X)] =
		this |~ :~[K](SQLNull[X])


	def keys :Seq[Label] = {
		@tailrec def rec(e :ListingSQL[F, S, _], acc :List[Label] = Nil) :Seq[Label] =
			e match {
				case entry :ListingEntry[F, S, _, _, _] => rec(entry.prev, entry.key::acc)
				case _ => acc
			}
		rec(this)
	}


	protected override lazy val parts :Seq[SQLExpression[F, S, _]] = {
		@tailrec def rec(e :ListingSQL[F, S, _], acc :List[SQLExpression[F, S, _]] = Nil) :Seq[SQLExpression[F, S, _]] =
			e match {
				case entry :ListingEntry[F, S, _, _, _] => rec(entry.prev, entry.value::acc)
				case _ => acc
			}
		rec(this)
	}

	override def toSeq :Seq[SQLExpression[F, S, _]] = parts

	def toMap :Map[String, SQLExpression[F, S, _]] = {
		@tailrec def rec(e :ListingSQL[F, S, _], acc :Map[String, SQLExpression[F, S, _]])
				:Map[String, SQLExpression[F, S, _]] =
			e match {
				case entry :ListingEntry[F, S, _, _, _] => rec(entry.prev, acc.updated(entry.key, entry.value))
				case _ => acc
			}
		rec(this, Map.empty)
	}

	override def asGlobal :Option[ListingSQL[F, GlobalScope, V]] =
		if (isGlobal) Some(this.asInstanceOf[ListingSQL[F, GlobalScope, V]])
		else None

	override def anchor(from :F) :ListingSQL[F, S, V] = rephrase(SQLScribe.anchor(from))


	//overridden to narrow down the result type
	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ListingSQL[E, S, V]

	//overridden to narrow down the result type
	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ListingSQL[E, S, V] =
		rephrase(SQLScribe.expand(base))

	//overridden to narrow down the result type
	override def expand[U <: F, E <: RowProduct]
	             (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S) :ListingSQL[E, S, V] =
		rephrase(SQLScribe.expand(base))


	override def topSelectFrom[E <: F with GroundRow { type Complete <: E }](from :E)
			:TopSelectAs[IndexedMapping.of[V]#Mapping] =
		SelectSQL[E, V](from, this)

	override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :SubselectAs[B, IndexedMapping.of[V]#Mapping] =
		SelectSQL.subselect[B, F ProperSubselectOf B, V](from, this)

	override def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E)
			:SelectMapping[P, IndexedMapping.of[V]#Mapping] =
		Select(from)(this)


//		/** Creates a `ListingSQL` expression by reordering the items of this listing to match the argument key sequence,
//		  * and wraps it in a [[net.noresttherein.oldsql.sql.ast.ConversionSQL ConversionSQL]] which maps
//		  * the new order back to the value type of this listing.
//		  */
	@throws[IllegalArgumentException]("if keys is not a permutation of this listing's keys")
	@throws[IllegalStateException]("if this listing contains duplicate keys.")
	def reorder(keys :Unique[String]) :SQLExpression[F, S, V]

	protected override def visit[Y[-_ >: Grouped <: GlobalScope, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[S, V] =
		visitor.listing(this)

	protected override def visit[Y](visitor :ExpressionVisitor[F, S, V, Y]) :Y = visitor.listing(this)

	override def toString :String = {
		def rec(e :ListingSQL[_, _, _], res :StringBuilder) :StringBuilder = e match {
			case tuple :ListingEntry[_, _, _, _, _] =>
				rec(tuple.prev, res) ++= " |~ " ++= tuple.key ++= ":~" ++= tuple.value.toString
			case EmptyListing => res ++= "@~"
		}
		rec(this, new StringBuilder).toString
	}
}




object ListingSQL {
	def apply() :ListingSQL[RowProduct, GlobalScope, @~] = EmptyListing

	def apply[F <: RowProduct, S >: Grouped <: GlobalScope, N <: Label, T]
	         (e :LabeledColumnSQL[F, S, N, T]) :ListingSQL[F, S, @~ |~ (N :~ T)] =
		new ListingEntry(EmptyListing, e)(new ValueOf(e.alias))

	def apply[F <: RowProduct, S >: Grouped <: GlobalScope, N <: Label :ValueOf, T]
	         (e :N :~ SQLExpression[F, S, T]) :ListingSQL[F, S, @~ |~ (N :~ T)] =
		new ListingEntry(EmptyListing, e.value)

	def apply[F <: RowProduct, S >: Grouped <: GlobalScope, N <: Label, T]
	         (key :N, value :SQLExpression[F, S, T]) :ListingSQL[F, S, @~ |~ (N :~ T)] =
		new ListingEntry(EmptyListing, value)(new ValueOf(key))


	class ListingItem[-F <: RowProduct, -S >: Grouped <: GlobalScope, K <: Label, X]
	                 (override val index :Int, val key :K, override val value :SQLExpression[F, S, X])
		extends TupleItem[F, S, X]

//		/** A type alias of [[net.noresttherein.oldsql.sql.ast.InlineSQL.ListingSQL ListingSQL]] used in cases
//		  * when the actual value of the expression does not matter, but only its type and, in particular, structure.
//		  * It is closely related to both [[net.noresttherein.oldsql.sql.RowShape RowShape]]
//		  * and [[net.noresttherein.oldsql.schema.SQLForm SQLForm]], but enforces an additinal level of compatibility
//		  * coming from the `Listing` type, especially its keys, in order to prevent accidental shape matching
//		  * of otherwise incompatible expressions.
//		  */
	type ListingShape[V <: Listing] = ListingSQL[RowProduct, Grouped, V]



	//todo: make it package private after moving to ast
	private[ast] final class ListingEntry[-F <: RowProduct, -S >: Grouped <: GlobalScope, I <: Listing, K <: Label :ValueOf, L]
	                                     (val prev :ListingSQL[F, S, I], val value :SQLExpression[F, S, L])
		extends ListingSQL[F, S, I |~ (K :~ L)] with SQLShapeCache
	{
		override type Item[-A <: RowProduct, -B >: Grouped <: GlobalScope, V] = ListingItem[A, B, _ <: Label, V]

		def key :K = valueOf[K]
		override def size :Int = items.last.index + 1
		val item = new ListingItem(prev.size, key, value)
		override lazy val items :Seq[Item[F, S, _]] = prev.items :+ item

		override def init[A <: Listing](implicit nonEmpty: (I |~ (K :~ L)) <:< (A |~ Listing.Item)) =
			prev.asInstanceOf[ListingSQL[F, S, A]]

		override def last[A](implicit nonEmpty: (I |~ (K :~ L)) <:< (Listing |~ (Label :~ A))) =
			value.asInstanceOf[SQLExpression[F, S, A]]

		override def lastKey[A <: Label](implicit nonEmpty: (I |~ (K :~ L)) <:< (Listing |~ (A :~ Any))) =
			key.asInstanceOf[A]

		override def lastItem[A <: Label, B](implicit notEmpty :(I |~ (K :~ L)) <:< (Listing |~ (A :~ B))) =
			item.asInstanceOf[ListingItem[F, S, A, B]]

		override def extracts[E <: F, A >: Grouped <: S]
				:Seq[Assoc[SQLExpression.curry[E]#x[A]#E, Extractor.curry[I |~ (K :~ L)]#T, _]] =
			_extracts.asInstanceOf[Seq[Assoc[SQLExpression.curry[E]#x[A]#E, Extractor.curry[I |~ (K :~ L)]#T, _]]]

		private[this] lazy val _extracts :Seq[Assoc[SQLExpression.curry[F]#x[S]#E, Extractor.curry[I |~ (K :~ L)]#T, _]] = {
			def map[X](entry :Assoc[SQLExpression.curry[F]#x[S]#E, Extractor.curry[I]#T, X]) =
				Assoc[SQLExpression.curry[F]#x[S]#E, Extractor.curry[I |~ (K :~ L)]#T, X](
					entry._1, entry._2 compose Chain.init[I] _
				)
			prev.extracts.map {
				entry :Assoc[SQLExpression.curry[F]#x[S]#E, Extractor.curry[I]#T, _] => map(entry)
			} :+ Assoc[SQLExpression.curry[F]#x[S]#E, Extractor.curry[I |~ (K :~ L)]#T, L](
				value, { tuple :(I |~ (K :~ L)) => tuple.last.value }
			)
		}

		override def construct(items :({ type T[X] = ListingItem[F, S, _ <: Label, X] })#T =>: generic.Self)
				:I |~ (K :~ L) =
			prev.construct(items) |~ :~[K](items(item))

		override lazy val keys  :Seq[Label] = super.keys
		override lazy val toMap :Map[String, SQLExpression[F, S, _]] = prev.toMap.updated(key, value)

		if (toMap.size == prev.toMap.size)
			throw new IllegalArgumentException(
				"Cannot add " + value + " to " + this + ": Label" + key + " already used for " + toMap(key) + "."
			)

		override lazy val selectForm :SQLReadForm[I |~ (K :~ L)] = (prev.selectForm, value.selectForm) match {
			case (i :SQLForm[I @unchecked], l :SQLForm[L @unchecked]) => i |~ :~[K](l)
			case _ => prev.selectForm |~ :~[K](value.selectForm)
		}


		override def isGlobal   :Boolean = value.isGlobal && prev.isGlobal
		override def isGround   :Boolean = value.isGround && prev.isGround

		override def groundValue :Opt[I |~ (K :~ L)] =
			for { i <- prev.groundValue; l <- value.groundValue } yield i |~ :~[K](l)

		override def isAnchored :Boolean = value.isAnchored && prev.isAnchored
		override def isAnchored(from :F) :Boolean = value.isAnchored(from) && prev.isAnchored(from)

		override def anchor(from :F) :ListingSQL[F, S, I |~ (K :~ L)] =
			(prev.anchor(from), value.anchor(from)) match {
				case (i, l) if (i eq prev) && (l eq value) => this
				case (i, l) => new ListingEntry(i, l)
			}

		override def basedOn[U <: F, E <: RowProduct]
		                    (base :E)(implicit ext :U PartOf E) :ListingSQL[E, S, I |~ (K :~ L)] =
			prev.basedOn(base) |~ :~[K](value.basedOn(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< S)
				:ListingSQL[E, S, I |~ (K :~ L)] =
			prev.expand(base) |~ :~[K](value.expand(base))

		//this short-circuits the call, resulting in no-callbacks for the prefixes. Lets call it a feature.
		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ListingSQL[E, S, I |~ (K :~ L)] =
			prev.rephrase(mapper) |~ :~[K](mapper(value))


		override def reorder(keys :Unique[String]) :SQLExpression[F, S, I |~ (K :~ L)] = {
			if (items.view.map(_.key) == keys)
				this
			else if (items.length != keys.size)
				throw new IllegalArgumentException(
					"There are " + items.length + " items in listing " + this + ", but the key permutation given " +
						keys + " is counts " + keys.size + " elements."
				)
			else {
				val newKeys = keys.downcastParam[Label]
				val ourKeys = items.view.map(_.key :Label).to(Unique)
				if (ourKeys.size != items.length)
					throw new IllegalStateException(
						"Cannot reorder listing " + this + " to key order " + keys + " because it contains key duplicates."
					)
				if (ourKeys.toSet != keys.toSet)
					throw new IllegalArgumentException(
						"Cannot reorder listing " + this + " to key order " + keys + " because the keys do not match."
					)
				def reorder(itemNo :Int) :ListingSQL[F, S, _ <: Listing] =
					if (itemNo < 0)
						EmptyListing
					else {
						val init = reorder(itemNo - 1)
						val key = keys(itemNo)
						init |~ (key, toMap(key))
					}
				val reordered = reorder(ourKeys.size - 1).asInstanceOf[ListingSQL[F, S, Listing]]
				val convert = new ReorderListing[Listing, I |~ (K :~ L)](
					SublistingOf.untyped[I |~ (K :~ L), Listing](newKeys, ourKeys),
					SublistingOf.untyped[Listing, I |~ (K :~ L)](ourKeys, newKeys)
				)
				convert(reordered)
			}
		}


		//todo: reforming of expressions for different tuple sizes: a listing of columns vs a listing of components
		protected override def reformer[E <: RowProduct, C >: Grouped <: GlobalScope, X, U]
		                       (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
		                       (implicit leftResult :Lift[I |~ (K :~ L), U], rightResult :Lift[X, U], spelling :SQLSpelling)
				:ExpressionVisitor[E, C, X, (SQLExpression[F, S, U], SQLExpression[E, C, U])] =
			new BaseReformer[E, C, X, U](other)(reform, passesAllowed)
				with MatchListing[E, C, X, (SQLExpression[F, S, U], SQLExpression[E, C, U])]
			{
				override def emptyChain =
					throw new MismatchedExpressionsException(
						"Record " + ListingSQL.this + " matched with an empty record."
					)

				override def listingItem[OI <: Listing, OK <: Label, OL]
				                        (e :ListingSQL[E, C, OI |~ (OK :~ OL)],
				                         init :ListingSQL[E, C, OI], last :LabeledItem[E, C, OK, OL])
				                        (implicit isListing :X =:= (OI |~ (OK :~ OL))) =
				{
//						/*  We could conceivably accept listings with different key sets and add matching null columns
//						 *  if reform.mayAddNullLeft/reform.mayAddNullRight, but it would lead to bugs.
//						 *  Listings are created explicitly and are not represented by a higher abstraction
//						 *  such ComponentSQL, so one can always simply add null items for the missing keys explicitly.
//						 */
					implicit val rightRes = isListing.substituteCo[({ type T[A] = Lift[A, U] })#T](rightResult)
					if (!leftResult.isDerived || !rightResult.isDerived)
						reform(leftResult(ListingEntry.this), rightRes(e))
					else {
						//fixme: Doesn't handle reordered listings from above due to unsupported lifts.
						//  The only way to make it work is to have a reform method without lifts,
						//  which validates itself that the expression types are equal.
						//  This is definitely not worth it for simple reordering, as we can just expect
						//  the application to use consistent ordering. What would be useful though
						//  are missing items and inserting null columns.
						//  The least we could do is try to check if the shapes af the expressions with matching labels are equal
						//attempt reforming in the exact order, ignoring keys
						//Splits a Lift[W |~ (A :~ V), Z] into
						// Lift[W, X], Lift[V, Y] and Lift[W |~ (A :~ Y), Z] forSome {type X <: Listing; type Y}
						type Split[W <: Listing, A <: Label, V, Z] =
							(Lift[W, Listing], Lift[V, Any], Lift[Listing |~ (A :~ Any), Z])

						//split the lift we were given for the whole expression into lifts for init and last
						def split[W <: Listing, A <: Label, V, Z](lift :Lift[W |~ (A :~ V), Z]) :Split[W, A, V, Z] =
							lift match {
								case _ if lift.isIdentity =>
									(lift, lift, lift).asInstanceOf[Split[W, A, V, Z]]
								case chain :LiftListing[xi, xl, yi, yl, k] =>
									(chain.init, chain.last, Lift.self).asInstanceOf[Split[W, A, V, Z]]
								case composed :ComposedLift[W |~ (A :~ V), y, Z] =>
									try {
										val (init, last, combined) = split[W, A, V, y](composed.first)
										(init, last, combined andThen composed.second)
									} catch {
										case e :MismatchedExpressionsException =>
											e.addSuppressed(new RethrowContext("unsupported lift type " + lift + "."))
											throw e
									}
								case _ =>
									throw new MismatchedExpressionsException(
										ListingEntry.this, e, "unsupported lift type " + lift + "."
									)
							}
						//attempt to recreate a ListingSQL by casting down the expressions obtained
						// by reforming init and last
						def listing[G <: RowProduct, D >: Grouped <: GlobalScope, W <: Listing, A <: Label, Z]
						           (i :SQLExpression[G, D, W], key :A, l :SQLExpression[G, D, Z]) =
							i match {
								case listing :ListingSQL[G, D, W] => l match {
									case value :LabeledValueSQL[G, D, Z] =>
										(listing |~ :~[A](value))(new ValueOf(key))
									case _ =>
										throw new MismatchedExpressionsException(ListingEntry.this, e,
											"received a non-LabeledValueSQL " + l + " as one of the values"
										)
								}
								case _ =>
									throw new MismatchedExpressionsException(ListingEntry.this, e,
										"received a non-ListingSQL expression " + i +
											": " + i.getClass.getName + " as a partial result."
									)
							}
						var liftOk = false
						try {
							val (leftInit, leftLast, leftPost)    = split(leftResult)
							val (rightInit, rightLast, rightPost) = split(rightRes)
							implicit val initCompat = leftInit vs rightInit
							implicit val lastCompat = leftLast vs rightLast
							liftOk = true
							val (thisInit, otherInit) = reform(prev, e.init)
							val (thisLast, otherLast) = reform(value, e.last)
							val left  = listing(thisInit, key, thisLast).to(leftPost) //these should be ok
							val right = listing(otherInit, e.lastKey :OK, otherLast).to(rightPost)
							(left.to[U], right.to[U])
//									fallback[F, S](isListing.liftContra[({ type T[A] = SQLExpression[E, C, A] })#T](e))(left, right)
						} catch {
							case e1 @ (_ :MismatchedExpressionsException | _ :UnsupportedOperationException) =>
								try {
									//todo: skip unmatched columns if mayAddNullLeft/mayAddNullRight
									//todo: reorder:
									fallback
								} catch {
									case e2 :Exception =>
										e2.addSuppressed(e1)
										if (e2.getSuppressed.length != 0)
											throw e2
										else {
											e1.addSuppressed(e2); throw e1
										}
								}
						}
					}
				}
			}

		protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] =
			spelling.split(prev) ++ spelling.split(value)

		protected override def shape(implicit spelling :SQLSpelling) :RowShape =
			spelling.shape(init) + spelling.shape(value)

		override def equals(that :Any) :Boolean = that match {
			case self  :AnyRef if this eq self => true
			case other :ListingEntry[_, _, _, _, _] if other.canEqual(this) =>
				value == other.value && key == other.key && prev == other.prev
			case _ => false
		}
		override def hashCode :Int = (prev.hashCode * 31 + key.hashCode) * 31 + value.hashCode
	}
}
*/
object RecordSQL {
	trait RecordTransformation[X <: Listing, Ks <: Chain] extends Serializable  {
		type Out <: Listing
		def apply(record :X) :Out
//		def inverse(record :Out) :X
		def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X]) :LabeledSQL[F, S, Out]
//		def inverse[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, Out]) :LabeledSQL[F, S, X]
	}


	@implicitNotFound("${Ks} does not form a complete list of keys in ${X}.")
	sealed abstract class RecordShuffle[X <: Listing, Ks <: Chain] extends RecordTransformation[X, Ks]

	object RecordShuffle {

		implicit def reorder[X <: Listing, Ks <: Chain](implicit select :SubRecord[X, Ks], size :ChainLength[Ks, _])
				:RecordShuffle[X, Ks] { type Out = select.Out } =
			new RecordShuffle[X, Ks] {
				override type Out = select.Out
				override def apply(record :X) = select(record)
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X]) = select(record)
				override def toString = (select.toString(new StringBuilder ++= "RecordShuffle(") += ')').result()
			}
	}


	@implicitNotFound("${Ks} contains keys not present in ${X} (or at least one of the types is abstract).")
	@implicitAmbiguous("${Ks} contain duplicate keys or at least one of the keys appears in ${X} more than once.")
	sealed abstract class SubRecord[X <: Listing, Ks <: Chain] extends RecordTransformation[X, Ks] {
		def size :Int
		def andThen[C <: Chain](projection :SubRecord[Out, C]) :SubRecord[X, C] { type Out = projection.Out } =
			new SubRecord[X, C] {
				override type Out = projection.Out
				override def size = projection.size

				override def apply(record :X) :Out = record(SubRecord.this(record))
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X]) =
					record(SubRecord.this(record))

				override def toString(res :StringBuilder) = projection.toString(res)
			}

		def toString(res :StringBuilder) :StringBuilder
		override def toString :String = (toString(new StringBuilder ++= "SubRecord(") += ')').result()
	}

	object SubRecord {
		type SubRecordOf[Y <: Listing, X <: Listing] = SubRecord[X, _] { type Out = Y }

		implicit def emptySubRecord[X <: Listing] :SubRecord[X, @~] { type Out = @~ } =
			new SubRecord[X, @~] {
				override type Out = @~
				override def size = 0
				override def apply(record :X): @~ = @~
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X]) = EmptyListing
				override def toString(res :StringBuilder) :StringBuilder = res
			}
		implicit def nonEmptySubRecord[X <: Listing, Ks <: Chain, K <: Label]
		                              (implicit item :RecordItem[X, K], subRecord :SubRecord[X, Ks])
		        :SubRecord[X, Ks ~ K] { type Out = subRecord.Out |~ (K :~ item.Value) } =
			new SubRecord[X, Ks ~ K] {
				override type Out = subRecord.Out |~ (K :~ item.Value)
				override def size = subRecord.size + 1
				override def apply(record :X) = subRecord(record) |~ :~[K](item(record))

				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X]) =
					(subRecord(record) |~ :~[K](item(record)))(new ValueOf[K](item.path(record)))

				override def toString(res :StringBuilder) = subRecord.size match {
					case 0 => res += '0'
					case n => res += ',' ++= n.toString
				}
			}
		private[ast] def unsafe[X <: Listing, Ks <: Chain, Y <: Listing]
		                       (superOrder :Unique[String], subOrder :Unique[String]) :SubRecord[X, Ks] { type Out = Y } =
			new UntypedSubRecord[X, Ks, Y](superOrder, subOrder)

		private class UntypedSubRecord[X <: Listing, Ks <: Chain, Y <: Listing]
		                              (val oldOrder :Unique[String], val newOrder :Unique[String])
			extends SubRecord[X, Ks]
		{
			type Out = Y

			override def apply(index :X) :Y = {
				val entries = index.toIndexedSeq
				def rec(i :Int) :Listing =
					if (i < 0)
						@~
					else {
						val init = rec(i - 1)
						val key = newOrder(i)
						val entry = entries(oldOrder.sureIndexOf(key))
						init |~ entry
					}
				rec(newOrder.size - 1).asInstanceOf[Y]
			}

			override def apply[F <: RowProduct, S >: Grouped <: Single](listing :LabeledSQL[F, S, X]) = {
				val items = listing.toMap
				((EmptyListing :LabeledSQL[F, S, _]) /: newOrder) {
					(res, key) => (res |~ (key, items(key)))
				}.asInstanceOf[LabeledSQL[F, S, Y]]
			}

			//			override def inverse[F <: RowProduct, S >: Grouped <: Single](listing :LabeledSQL[F, S, X]) = {
			//				val items = listing.toMap
			//				(EmptyListing.asInstanceOf[LabeledSQL[F, S, Listing]] /: oldOrder) { (res, key) =>
			//					(items.get(key) match {
			//						case Some(value) => res |~ (key, value)
			//						case _ => res |~ LabeledColumnSQL(key, SQLNull[Any]())
			//					}).asInstanceOf[LabeledSQL[F, S, Listing]]
			//				}.asInstanceOf[LabeledSQL[F, S, Y]]
			//			}
			//
			//			override def compose[O <: Listing](other :Y SublistingOf O) :X SublistingOf O = other match {
			//				case untyped :UntypedSublistingOf[Y, O] =>
			//					if (untyped.newOrder != oldOrder)
			//						throw new IllegalArgumentException(
			//							"Cannot compose (" + this + ") with (" + untyped + "): output key order " +
			//								"of the second conversion does not match the input key order of the first one."
			//						)
			//					new UntypedSublistingOf(newOrder, untyped.oldOrder)
			//				case _ => super.compose(other)
			//			}
			//
			override def andThen[C <: Chain](projection :SubRecord[Y, C]) :SubRecord[X, C] { type Out = projection.Out } =
				projection match {
					case untyped :UntypedSubRecord[_, _] =>
						new UntypedSubRecord[X, C, projection.Out](oldOrder, untyped.newOrder)
					case _ =>
						super.andThen(projection)
				}

			override def toString(res :StringBuilder) =
				if (newOrder.isEmpty)
					res
				else {
					res append newOrder.head
					val i = newOrder.iterator
					i.next().foreach { res append ',' append _.toString }
					res
				}
		}
	}


	@implicitNotFound("Keys ${Ks} do not appear in ${X} in the same order (or contain absent keys). " +
	                  "Did you want to use SubRecord[${X}, ${Ks}] instead?")
	@implicitAmbiguous("Either ${Ks} or ${X} contain duplicate keys.")
	sealed abstract class RecordProjection[X <: Listing, Ks <: Chain] extends RecordTransformation[X, Ks]

	object RecordProjection {
		implicit def emptyProjection[X <: Listing] :RecordProjection[X, @~] { type Out = @~ } =
			new RecordProjection[X, @~] {
				override type Out = @~
				override def apply(record :X) = @~
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X]) = EmptyListing
			}
		implicit def includeLast[X <: Listing, Ks <: Chain, K <: Label, V](implicit preceding :RecordProjection[X, Ks])
				:RecordProjection[X |~ (K :~ V), Ks ~ K] { type Out = preceding.Out |~ (K :~ V) } =
			new RecordProjection[X |~ (K :~ V), Ks ~ K] {
				override type Out = preceding.Out |~ (K :~ V)
				override def apply(record :X |~ (K :~ V)) =
					preceding(record.init) |~ :~[K](record.last.value)

				override def apply[F <: RowProduct, S >: Grouped <: Single]
				                  (record :LabeledSQL[F, S, X |~ (K :~ V)]) :LabeledSQL[F, S, preceding.Out |~ (K :~ V)] =
					(preceding(record.init) |~ :~[K](record.last))(new ValueOf[K](record.lastKey))
			}
		implicit def excludeLast[X <: Listing, Ks <: Chain, K <: Label, V](implicit preceding :RecordProjection[X, Ks])
				:RecordProjection[X |~ (K :~ V), Ks ~ K] { type Out = preceding.Out } =
			new RecordProjection[X |~ (K :~ V), Ks ~ K] {
				override type Out = preceding.Out
				override def apply(record :X |~ (K :~ V)) = preceding(record.init)
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X |~ (K :~ V)]) =
					preceding(record.init)
			}
	}


	trait DoubleRecordTransformation[X <: Listing, Y <: Listing] extends Serializable {
		type Out <: Listing
		type Keys <: Chain
		val left  :RecordTransformation[X, Keys] { type Out = DoubleRecordTransformation.this.Out }
		val right :RecordTransformation[Y, Keys] { type Out = DoubleRecordTransformation.this.Out }
	}

	sealed abstract class DoubleRecordProjection[X <: Listing, Y <: Listing] extends DoubleRecordTransformation[X, Y]

	object DoubleRecordProjection {
	}
/*
	sealed abstract class RecordSum[X <: Listing, Y <: Listing] {
		type Out <: Listing
		type Keys <: Chain
		val left  :RecordTransformation[X, Keys] { type Out = RecordSum.this.Out }
		val right :RecordTransformation[Y, Keys] { type Out = RecordSum.this.Out }
	}

	private[RecordSQL] sealed abstract class SequentialRecordSum {
	}
	private[RecordSQL] sealed abstract class SubRecordSum {
		implicit def toRight[X <: Listing, Y <: Listing, Ks <: Chain]
		                    (implicit keys :RecordKeys[Y] { type Out = Ks }, subRecord :SubRecord[X, Ks] { type Out = Y })
				:RecordSum[X, Y] { type Out = Y; type Keys = Ks } =
			new RecordSum[X, Y] {
				override type Out  = Y
				override type Keys = Ks
				override val left  = subRecord
				override val right = identity.asInstanceOf[RecordTransformation[Y, Ks] { type Out = Y }]
			}
		implicit def toLeft[X <: Listing, Y <: Listing, Ks <: Chain]
		                   (implicit keys :RecordKeys[X] { type Out = Ks }, subRecord :SubRecord[Y, Ks] { type Out = X })
				:RecordSum[X, Y] { type Out = X; type Keys = Ks } =
			new RecordSum[X, Y] {
				override type Out  = X
				override type Keys = Ks
				override val left  = identity.asInstanceOf[RecordTransformation[X, Ks] { type Out = X }]
				override val right = subRecord
			}
		private val identity = new RecordTransformation[Listing, Chain] {
			override type Out = Listing
			override def apply(record :Listing) :Listing = record
			override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, Listing]) = record
			override def toString = "RecordTransformation(<identity>)"
		}
	}
	object RecordSum extends SubRecordSum {
		implicit def same[X <: Listing](implicit keys :RecordKeys[X])
				:RecordSum[X, X] { type Out = X; type Keys = keys.Out } =
			new RecordSum[X, X] {
				override type Out = X
				override type Keys = keys.Out
				override val left = new RecordTransformation[X, Keys] {
					override type Out = X
					override def apply(record :X) = record
					override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X]) = record
				}
				override val right = left
			}
	}
*/


	@implicitNotFound("Path ${P} does not appear in ${X}.")
	@implicitAmbiguous("Multiple entries with path ${P} present in ${X}.")
	sealed abstract class RecordItem[X <: Listing, P] extends Serializable {
		type Value
		type Updated[T] <: Listing
		def posFromTheRight :Int
		def isTopLevel :Boolean
		def apply(record :X) :Value
		def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X]) :LabeledValueSQL[F, S, Value]
		def path[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X]) :P

		def set(record :X, value :Value) :X

		def set[F <: RowProduct, S >: Grouped <: Single]
		       (record :LabeledSQL[F, S, X], value :LabeledValueSQL[F, S, Value]) :LabeledSQL[F, S, X]

		override def toString :String = "RecordItem(-" + posFromTheRight + ')'
	}

	object RecordItem {
		implicit def lastItem[X <: Listing, K <: Label, V]
				:RecordItem[X |~ (K :~ V), K] { type Value = V; type Updated[T] = X |~ (K :~ T) } =
			new RecordItem[X |~ (K :~ V), K] {
				override type Value = V
				override type Updated[T] = X |~ (K :~ T)
				override def posFromTheRight = 0
				override def isTopLevel = true

				override def apply(record :X |~ (K :~ V)) :V = record.last.value

				override def apply[F <: RowProduct, S >: Grouped <: Single]
				                  (record :LabeledSQL[F, S, X |~ (K :~ V)]) :LabeledValueSQL[F, S, V] =
					record.last

				override def path[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X |~ (K :~ V)]) :K =
					record.lastKey

				override def set(record :X |~ (K :~ V), value :V) :X |~ (K :~ V) = record.last.value

				override def set[F <: RowProduct, S >: Grouped <: Single]
				                (record :LabeledSQL[F, S, X |~ (K :~ V)], value :LabeledValueSQL[F, S, V]) =
					record.last
			}
		//todo: I don't know if Scala can infer these types.
		implicit def previousItem[X <: Listing, K <: Label, V, A <: Label, B](implicit preceding :RecordItem[X, K])
				:RecordItem[X |~ (A :~ B), K] {
					type Value = preceding.Value; type Updated[T] = preceding.Updated[T] |~ (A :~ B)
				} =
			new RecordItem[X |~ (A :~ B), K] {
				override type Value = preceding.Value
				override type Updated[T] = preceding.Updated[T] |~ (A :~ B)
				override def posFromTheRight = preceding.posFromTheRight + 1
				override def isTopLevel = true

				override def apply(record :X |~ (A :~ B)) :V = preceding(record.init)
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X |~ (A :~ B)]) =
					preceding(record.init)

				override def path[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X |~ (A :~ B)]) :K =
					preceding.path(record.init)

				override def set(record :X |~ (A :~ B), value :preceding.Value) :X |~ (A :~ B) =
					preceding.set(record.init, value) |~ record.last

				override def set[F <: RowProduct, S >: Grouped <: Single]
				                (record :LabeledSQL[F, S, X |~ (A :~ B)], value :LabeledValueSQL[F, S, preceding.Value]) =
					preceding.set(record.init, value) |~ record.lastItem
			}

		implicit def nestedItem[X <: Listing, P <: LabelPath[P], H <: Label, T, V <: Listing]
		                       (implicit split :SplitLabelPath[P] { type First = H; type Suffix = T },
		                                 topLevel :RecordItem[X, H] { type Value = V }, nested :RecordItem[V, T])
				:RecordItem[X, P] {
					type Value = nested.Value; type Updated[T] = topLevel.Updated[nested.Updated[T]]
				} =
			new RecordItem[X, P] {
				override type Value = nested.Value
				override type Updated[T] = topLevel.Updated[nested.Updated[T]]
				override def posFromTheRight = topLevel.posFromTheRight
				override def isTopLevel = false

				override def apply(record :X) :Value = nested(topLevel(record))
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X]) =
					nested(topLevel(record))

				override def path[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X]) =
					split.join(topLevel.path(record), nested.path(topLevel(record)))

				override def set(record :X, value :Value) = topLevel.set(record, nested.set(topLevel(record), value))
				override def set[F <: RowProduct, S >: Grouped <: Single]
				                (record :LabeledSQL[F, S, X], value :LabeledValueSQL[F, S, Value]) =
					topLevel.set(record, nested.set(topLevel(record), value))
			}
	}


	@implicitNotFound("Cannot determine the full list of keys for ${X} - is the type partially abstract?")
	sealed abstract class RecordKeys[X <: Listing] {
		type Out <: Chain
		def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, X]) :Out
	}

	object RecordKeys {
		implicit val emptyRecordKeys :RecordKeys[@~] { type Out = @~ } =
			new RecordKeys[@~] {
				override type Out = @~
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, @~]) = @~
			}
		implicit def nonEmptyRecordKeys[X <: Listing, K <: Label, V](implicit preceding :RecordKeys[X])
				:RecordKeys[@~] { type Out = preceding.Out ~ K } =
			new RecordKeys[@~] {
				override type Out = preceding.Out ~ K
				override def apply[F <: RowProduct, S >: Grouped <: Single](record :LabeledSQL[F, S, @~]) =
					preceding(record.init) ~ record.lastKey
			}
	}
}






//does not extend ChainTuple because LabeledEntry doesn't extend ChainEntry and it would cause problems
// in pattern matching a ChainTuple
/** A variant of [[net.noresttherein.oldsql.sql.ast.ChainTuple ChainTuple]] which maps
  * to a [[net.noresttherein.oldsql.collection.Listing Listing]] - a `Chain` subtype with key-value pairs
  * in the form of `K :~ V` as its only elements. The keys exist only on the type level in the `T` type parameter;
  * Member SQL expressions are not of the `K :~ V` type, but rather directly the value type `V`.
  * The indexing has no special effect on the generated SQL other than `ListingColumn` being a subclass of
  * [[net.noresttherein.oldsql.sql.ast.AliasedSQL AliasedColumn]]. Instead, it allows referencing
  * the columns in a type safe way when mapping results of a select, if this expression appears
  * in the ''select'' clause. Additionally, a ''select'' clause consisting solely of this type can be matched
  * to any [[net.noresttherein.oldsql.schema.bits.IndexedSchemaMapping IndexedSchemaMapping]] with a component chain
  * containing exactly the same key-value pairs as the `T` type parameter, regardless of their order.
  * Non-column components of the mapping must in that case recursively match a nested indexed tuple paired
  * with the same key type in this `LiteralChain` as the component's label.
  * An `LabeledSQL` can consist only of expressions implementing `LabeledValueSQL` - a sealed type
  * extended only by [[net.noresttherein.oldsql.sql.ast.LabeledSQL.LabeledColumnSQL LabeledColumnSQL]]
  * and this trait. This ensures that every column of the whole expression is assigned a unique key,
  * which however may be a sequence of `Label`s, rather than a single one, if this expression contains
  * another indexed tuples as its subexpressions.
  * Note that, in order to provide unambiguous cases when pattern matching, this type does not extend the standard
  * `ChainTuple`.
  */ //consider: renaming to IndexSQL or DeepRecordSQL
sealed trait LabeledSQL[-F <: RowProduct, -S >: Grouped <: Single, V <: Listing]
	extends InlineSQL[F, S, V] with LabeledValueSQL[F, S, V]
	   with SelectableMappingSQL[F, IndexedMapping.of[V]#Mapping]
	   with GroundingCompositeSQLTemplate[F, S, V, ({ type E[-f <: RowProduct] = LabeledSQL[f, S, V] })#E]
{
	override type Item[-E <: RowProduct, -D >: Grouped <: Single, X] <: LabeledItem[E, D, _ <: Label, X]

//	override def items :Seq[LabeledItem[F, S, _ <: Label, _]]

	override def mapping[O](columnPrefix :String) :TypedListingSQLMapping[F, S, V, O] =
		TypedListingSQLMapping(this, columnPrefix)

	def init[I <: Listing](implicit nonEmpty: V <:< (I |~ Listing.Item)) :LabeledSQL[F, S, I] =
		this.asInstanceOf[LabeledEntry[F, S, I, Label, Any]].left

	def last[L](implicit nonEmpty: V <:< (Listing |~ (Label :~ L))) :LabeledValueSQL[F, S, L] =
		this.asInstanceOf[LabeledEntry[F, S, Listing, Label, L]].right

	def lastKey[K <: Label](implicit nonEmpty: V <:< (Listing |~ (K :~ Any))) :K =
		this.asInstanceOf[LabeledEntry[F, S, Listing, K, Any]].key

	def lastItem[K <: Label, L](implicit notEmpty :V <:< (Listing |~ (K :~ L))) :LabeledItem[F, S, K, L] =
		this.asInstanceOf[LabeledEntry[F, S, Listing, K, L]].lastItem

	def |~[E <: F, O >: Grouped <: S, K <: Label :ValueOf, L]
	      (entry :K :~ LabeledValueSQL[E, O, L]) :LabeledSQL[E, O, V |~ (K :~ L)] =
		new LabeledEntry(this, entry.value)

	def |~[E <: F, O >: Grouped <: S, K <: Label, L]
	      (entry :LabeledColumnSQL[E, O, K, L]) :LabeledSQL[E, O, V |~ (K :~ L)] =
		new LabeledEntry(this, entry)(new ValueOf(entry.alias))

	def |~[E <: F, O >: Grouped <: S, K <: Label :ValueOf, L]
	       (entry :K :~ ColumnSQL[E, O, L]) :LabeledSQL[E, O, V |~ (K :~ L)] =
		new LabeledEntry(this, new LabeledColumnSQL(entry.value, valueOf[K]))

	def |~[E <: F, O >: Grouped <: S, K <: Label, L]
	      (entry :(K, LabeledValueSQL[E, O, L])) :LabeledSQL[E, O, V |~ (K :~ L)] =
		entry._2 match {
			case column :LabeledColumnSQL[_, _, _, _] if column.alias != entry._1 =>
				throw new IllegalArgumentException(
					"Attempted to append a LabeledColumnSQL " + column + " under a different key " + entry._1 +
					" to " + this + "."
				)
			case _ =>
				new LabeledEntry(this, entry._2)(new ValueOf[K](entry._1))
		}

	def |~[E <: F, O >: Grouped <: S, K <: Label, L](entry :LabeledItem[E, O, K, L]) :LabeledSQL[E, O, V |~ (K :~ L)] =
		new LabeledEntry(this, entry.value)(new ValueOf[K](entry.key))

	//declared here due to overloading rules
//	def |~[K <: Label :ValueOf, X :ColumnForm](n :Null) :LabeledShape[V |~ (K :~ X)] =
//		this |~ :~[K](SQLNull[X])

//		def |~[K <: Label :ValueOf, X <: Listing](entry :K :~ ListingLayout[X]) :ListingLayout[T |~ (K :~ X)] =
//			this |~[Nothing, Grouped, K, X] :~[K](entry.value)

	def apply[K <: Label](key :K)(implicit get :RecordItem[V, K])    :LabeledValueSQL[F, S, get.Value] = get(this)
	def apply[P](path :LabelPath[P])(implicit get :RecordItem[V, P]) :LabeledValueSQL[F, S, get.Value] = get(this)

	/** A list of (direct) keys/entry labels in this record, in order from left to right. */
	def keys :Seq[Label] = {
		@tailrec def rec(e :LabeledSQL[F, S, _], acc :List[Label] = Nil) :Seq[Label] =
			e match {
				case entry :LabeledEntry[F, S, _, _, _] => rec(entry.left, entry.key::acc)
				case _ => acc
			}
		rec(this)
	}

	override def toSeq   :Seq[LabeledValueSQL[F, S, _]] = inOrder
	override def inOrder :Seq[LabeledValueSQL[F, S, _]] = {
		@tailrec def rec(e :LabeledSQL[F, S, _], acc :List[LabeledValueSQL[F, S, _]] = Nil) :Seq[LabeledValueSQL[F, S, _]] =
			e match {
				case entry :LabeledEntry[F, S, _, _, _] => rec(entry.left, entry.right :: acc)
				case _ => acc
			}
		rec(this)
	}
	protected override def parts :Seq[SQLExpression[F, S, _]] = inOrder

	/** A list of paths to all, direct and indirect record columns contained in this expression, in their order
	  * of appearance from left to right. The result is the key set of
	  * `this.`[[net.noresttherein.oldsql.sql.ast.LabeledSQL.columns columns]].
	  */
	def paths :Seq[LabelPath[_]] = {
		def rec(path :LabelPathPrefix, e :LabeledSQL[F, S, _], acc :List[LabelPath[_]]) :List[LabelPath[_]] =
			e match {
				case entry :LabeledEntry[F, S, _, _, _] =>
					def dfs(path :LabelPath[_], e :LabeledValueSQL[F, S, _], res :List[LabelPath[_]]) :List[LabelPath[_]] =
						e match {
							case _ :LabeledColumnSQL[F, S, l, _] => path::res
							case tuple :LabeledSQL[F, S, _] => rec(Got(path), tuple, res)
						}
					val down = path / entry.key
					rec(path, entry.left, dfs(down, entry.right, acc))
				case _ => acc
			}
		rec(~/, this, Nil)
	}


//The implementation here is inconsistent with ChainTuple: ChainEntry defines parts as Seq(init, last).
//	protected override lazy val parts :Seq[LabeledValueSQL[F, S, _]] = {
//		@tailrec def rec(e :LabeledSQL[F, S, _], acc :List[LabeledValueSQL[F, S, _]] = Nil) :Seq[LabeledValueSQL[F, S, _]] =
//			e match {
//				case entry :LabeledEntry[F, S, _, _, _] => rec(entry.left, entry.right::acc)
//				case _ => acc
//			}
//		rec(this)
//	}


	/** Map of entries in this record: keys mapped to their values. The key set of the result equals
	  * `this.`[[net.noresttherein.oldsql.sql.ast.LabeledSQL.keys keys]]`.toSet`.
	  * @see [[net.noresttherein.oldsql.sql.ast.LabeledSQL.index pathValues]]
	  * @see [[net.noresttherein.oldsql.sql.ast.LabeledSQL.toSeq toSeq]]
	  */
	def toMap :Map[String, LabeledValueSQL[F, S, _]] = {
		@tailrec def rec(e :LabeledSQL[F, S, _], acc :Map[String, LabeledValueSQL[F, S, _]])
				:Map[String, LabeledValueSQL[F, S, _]] =
			e match {
				case entry :LabeledEntry[F, S, _, _, _] =>
					if (acc.contains(entry.key))
						throw new IllegalStateException("Duplicate keys in expression " + this + ".")
					rec(entry.left, acc.updated(entry.key, entry.right))
				case _ => acc
			}
		rec(this, Map.empty)
	}

	/** Map of all full paths in this expression, that is paths starting with a key in this record
	  * and ending with a direct or indirect record column. The key set of the result equals
	  * `this.`[[net.noresttherein.oldsql.sql.ast.LabeledSQL.paths paths]]`.toSet`.
	  * @see [[net.noresttherein.oldsql.sql.ast.LabeledSQL.paths paths]]
	  * @see [[net.noresttherein.oldsql.sql.ast.LabeledSQL.index pathValues]]
	  * @see [[net.noresttherein.oldsql.sql.ast.LabeledSQL.toMap toMap]]
	  */
	def columns :Map[LabelPath[_], LabeledColumnSQL[F, S, _ <: Label, _]] = {
		type Res = Map[LabelPath[_], LabeledColumnSQL[F, S, _ <: Label, _]]
		def rec(path :LabelPathPrefix, e :LabeledSQL[F, S, _ <: Listing], acc :Res) :Res =
			e match {
				case entry :LabeledEntry[F, S, _, _, _] =>
					def dfs(path :LabelPath[_], e :LabeledValueSQL[F, S, _], res :Res) :Res =
						e match {
							case column :LabeledColumnSQL[F, S, l, _] => res.updated(path, column)
							case tuple :LabeledSQL[F, S, _] => rec(path, tuple, res)
						}
					rec(path, entry.left, dfs(path / entry.key, entry.right, acc))
				case _ => acc
			}
		rec(~/, this, SeqMap.empty)
	}

	/** Map of all, direct and indirect record entries under this record, indexed by their paths,
	  * starting with a key from this record.
	  * @see [[net.noresttherein.oldsql.sql.ast.LabeledSQL.columns columns]]
	  */
	def index :Map[LabelPath[_], LabeledValueSQL[F, S, _]] = {
		type Res = Map[LabelPath[_], LabeledValueSQL[F, S, _]]
		def rec(path :LabelPathPrefix, e :LabeledSQL[F, S, _], acc :Res) :Res =
			e match {
				case entry :LabeledEntry[F, S, _, _, _] =>
					def dfs(path :LabelPath[_], e :LabeledValueSQL[F, S, _], res :Res) :Res =
						e match {
							case column :LabeledColumnSQL[F, S, l, _] => res.updated(path, column)
							case tuple :LabeledSQL[F, S, _] => rec(path, tuple, acc.updated(path, tuple))
						}
					rec(path, entry.left, dfs(path / entry.key, entry.right, acc))
				case _ => acc
			}
		rec(~/, this, SeqMap.empty)
	}


	override def asSingleRow :Option[LabeledSQL[F, Single, V]] =
		if (isSingleRow) Some(this.asInstanceOf[LabeledSQL[F, Single, V]])
		else None
//
//	//these methods are overridden in LabeledEntry, but we need to also implement them here
//	// because CompositeSQL implements them and we narrow the return type.
//	override def anchor(from :F) :LabeledSQL[F, S, V] = rephrase(SQLScribe.anchor(from))
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :LabeledSQL[E, S, V] =
//		rephrase(SQLScribe.expand(base))
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :LabeledSQL[E, S, V] =
//		rephrase(SQLScribe.expand(base))
//
	//overridden to narrow down the result type and because of a clash between LabeledValueSQL and CompositeSQL
	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :LabeledSQL[E, S, V]

	override def nullSQL   :LabeledSQL[RowProduct, Single, V]

	def reorder[K <: Chain](implicit order :RecordShuffle[V, K]) :LabeledSQL[F, S, order.Out] = order(this)
	def reorder[K <: Chain](keys :K)(implicit order :RecordShuffle[V, K]) :LabeledSQL[F, S, order.Out] = order(this)

	def project[K <: Chain](implicit project :RecordProjection[V, K]) :LabeledSQL[F, S, project.Out] = project(this)
	def project[K <: Chain](keys :K)(implicit project :RecordProjection[V, K]) :LabeledSQL[F, S, project.Out] =
		project(this)

	//todo: conjunction and disjunction of two expressions (one each, or ordered and non ordered?)
//	def reform[K <: Listing](implicit reform :RecordReform[V, K]) :LabeledSQL[F, S, reform.Out] = reform(this)
//	def reform[K <: Listing]()

	/** Creates a `LabeledSQL` expression by reordering the items of this listing to match the argument key sequence,
	  * and wraps it in a [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]] which maps
	  * the new order back to the value type of this listing.
	  */
	@throws[IllegalArgumentException]("if keys is not a permutation of this listing's keys")
	@throws[IllegalStateException]("if this listing contains duplicate keys.")
	def reorder(keys :Unique[String]) :SQLExpression[F, S, V]

	/** Creates a `LabeledSQL` expression with its values recursively reordered to follow the order of their paths
	  * specified by the argument. The argument must equal
	  * `record.`[[net.noresttherein.oldsql.sql.ast.LabeledSQL.paths paths]] for some `record :LabeledSQL[_, _, _]`.
	  * @param paths `this.paths` reordered in a grouped manner: for any path prefix length (in keys),
	  *              all paths with equal prefixes of that length form a consecutive subsequence in the argument.
	  */
	def reorder(paths :Seq[LabelPath[_]]) :SQLExpression[F, S, V]

	def project(paths :Seq[LabelPath[_]]) :SQLExpression[F, S, V]

	def reform[E <: F, A >: Grouped <: S](paths :Seq[(LabelPath[_], LabeledValueSQL[E, A, _])]) :SQLExpression[E, A, V]


	override def topSelectFrom[E <: F with GroundRow { type Complete <: E }](from :E)
			:TopSelectAs[IndexedMapping.of[V]#Mapping] =
		SelectSQL[E, V](from, this)

	override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B) :SubselectAs[B, IndexedMapping.of[V]#Mapping] =
		SelectSQL.subselect[B, F ProperSubselectOf B, V](from, this)

	override def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E)
			:SelectMapping[P, IndexedMapping.of[V]#Mapping] =
		Select(from)(this)

//		def reorder[I <: Listing](implicit up :I SublistingOf T, down :T SublistingOf I) :LabeledSQL[F, S, I] =
//			up(this)
//
//		def reform[I <: Listing](implicit up :I SublistingOf T) :LabeledSQL[F, S, I] = up(this)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[S, V] =
		visitor.labeled(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, S, V, Y]) :Y = visitor.labeled(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: SQLExpression[F, S, V] <: SQLExpression[F_, S_, V],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F, R]) :R[S_, V, E] =
//		visitor.labeled(this)

	override def toString :String = {
		def rec(e :LabeledSQL[_, _, _], res :StringBuilder) :StringBuilder = e match {
			case tuple :LabeledEntry[_, _, _, _, _] =>
				rec(tuple.left, res) ++= " |~ " ++= tuple.key ++= ":~" ++= tuple.right.toString
			case EmptyListing => res ++= "@~"
		}
		rec(this, new StringBuilder).toString
	}

}



object LabeledSQL {
	def apply() :LabeledSQL[RowProduct, Single, @~] = EmptyListing

	def apply[F <: RowProduct, S >: Grouped <: Single, N <: Label, T]
	         (item :LabeledColumnSQL[F, S, N, T]) :LabeledSQL[F, S, @~ |~ (N :~ T)] =
		new LabeledEntry(EmptyListing, item)(new ValueOf(item.alias))

	def apply[F <: RowProduct, S >: Grouped <: Single, N <: Label :ValueOf, T]
	         (item :N :~ ColumnSQL[F, S, T]) :LabeledSQL[F, S, @~ |~ (N :~ T)] =
		new LabeledEntry(EmptyListing, new LabeledColumnSQL(item.value, valueOf[N]))

	def apply[F <: RowProduct, S >: Grouped <: Single, N <: Label, T]
	         (item :LabeledItem[F, S, N, T]) :LabeledSQL[F, S, @~ |~ (N :~ T)] =
		new LabeledEntry(EmptyListing, new LabeledColumnSQL(item.value, item.key))

	def apply[F <: RowProduct, S >: Grouped <: Single, N <: Label, T]
	         (key :N, value :ColumnSQL[F, S, T]) :LabeledSQL[F, S, @~ |~ (N :~ T)] =
		new LabeledEntry(EmptyListing, key @: value)(new ValueOf(key))


	class LabeledItem[-F <: RowProduct, -S >: Grouped <: Single, K <: Label, X]
	                 (override val index :Int, val key :K, override val value :LabeledValueSQL[F, S, X])
		extends InlineItem[F, S, X]



	type __ = LabeledSQL[_ <: RowProduct, _ >: Grouped <: Single, _]

	private[ast] trait EmptyLabeled extends LabeledSQL[RowProduct, Single, @~]

	/** A type alias of [[net.noresttherein.oldsql.sql.ast.LabeledSQL LabeledSQL]] used in cases
	  * when the actual value of the expression does not matter, but only its type and, in particular, structure.
	  * It is closely related to both [[net.noresttherein.oldsql.sql.RowShape RowShape]]
	  * and [[net.noresttherein.oldsql.schema.SQLForm SQLForm]], but enforces an additinal level of compatibility
	  * coming from the `Listing` type, especially its keys, in order to prevent accidental shape matching
	  * of otherwise incompatible expressions.
	  */
	type LabeledShape[V <: Listing] = LabeledSQL[Nothing, Grouped, V]

	/** [[net.noresttherein.oldsql.sql.ast.EmptySQL EmptySQL]] as an instance of `LabeledSQL`. */
	val EmptyListing :LabeledSQL[RowProduct, Single, @~] = EmptySQL


	/** An SQL expression which may occur as a subexpression of
	  * a [[net.noresttherein.oldsql.sql.ast.LabeledSQL LabeledSQL]], which associates with it -
	  * the value of the SQL expression - a unique [[net.noresttherein.oldsql.schema.bits.LabelPath.Label Label]]
	  * identifying it among the elements of the tuple. It has only two subclasses: `LabeledSQL` itself and
	  * [[net.noresttherein.oldsql.sql.ast.LabeledSQL.LabeledColumnSQL ListingColumn]] - a decorator
	  * of any column expression with an associated label used as the leaf in the expression tree of the root
	  * `LabeledSQL`. Thus, every column in such an expression is uniquely identified
	  * by a [[net.noresttherein.oldsql.schema.bits.LabelPath LabelPath]] following the labels of all enclosing
	  * components. In contexts where an SQL expression is adapted
	  * to a [[net.noresttherein.oldsql.schema.Mapping Mapping]], that is in ''group by'' expressions and
	  * in ''select'' clauses, this expression is represented
	  * by a [[net.noresttherein.oldsql.sql.TypedListingSQLMapping TypedListingSQLMapping]] (or just its supertype
	  * [[net.noresttherein.oldsql.sql.ListingSQLMapping ListingSQLMapping]], which allows access to its individual
	  * components representing subexpressions of this expression by the use
	  * of a [[net.noresttherein.oldsql.schema.bits.LabelPath LabelPath]] listing labels of all enclosing
	  * subexpressions on the path to a particular subexpression. This allows safer use of the value
	  * of this expression than relying on hard-coded positioning information in a non indexed tuple expressions.
	  * Additionally, queries returning this expression type can be safely combined in ''compound selects''
	  * trough [[net.noresttherein.oldsql.sql.Select.SelectOperator set operators]] such as
	  * [[net.noresttherein.oldsql.sql.Select.Union union]] or [[net.noresttherein.oldsql.sql.Select.Minus minus]].
	  */
	sealed trait LabeledValueSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
		extends SQLExpression[F, S, V] //consider: a ConvertedLabeledValueSQL would help with reordering
		   with VariantGroundingTemplate[F, S, V, ({ type E[-f <: RowProduct] = LabeledValueSQL[f, S, V] })#E]
	{
		def mapping[O] :TypedListingSQLMapping[F, S, V, O] = mapping[O]("")
		def mapping[O](columnPrefix :String) :TypedListingSQLMapping[F, S, V, O]

		override def asSingleRow :Option[LabeledValueSQL[F, Single, V]]
//
//		override def anchor(from :F) :LabeledValueSQL[F, S, T]
//
//		override def basedOn[U <: F, E <: RowProduct]
//		                    (base :E)(implicit expansion :U PartOf E) :LabeledValueSQL[E, S, T]
//
//		override def expand[U <: F, E <: RowProduct]
//		             (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S) :LabeledValueSQL[E, S, T]

		def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :LabeledValueSQL[E, S, V]

		def nullSQL   :LabeledValueSQL[RowProduct, Single, V] //consider: moving it up to SQLExpression

		@throws[NullPointerException](
			"if the value type of any of the columns does not accept nulls or they are disallowed by their form.")
		def nullValue :Opt[V]
	}

	object LabeledValueSQL {
		trait SpecificLabeledValueVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
			extends SpecificLabeledVisitor[F, S, V, Y] with SpecificLabeledColumnVisitor[F, S, V, Y]

		type MatchSpecificLabeledValue[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
			SpecificLabeledValueVisitor[F, S, V, Y]

		trait CaseSpecificLabeledValue[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
			extends MatchSpecificLabeledValue[F, S, V, Y]
		{
			def labeledValue(e :LabeledValueSQL[F, S, V]) :Y

			override def labeled[X <: Listing](e :LabeledSQL[F, S, X])(implicit isListing :X =:= V) :Y = labeledValue(e)
			override def labeledColumn[N <: Label](e :LabeledColumnSQL[F, S, N, V]) :Y = labeledValue(e)
		}


		trait AnyLabeledValueVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
			extends AnyLabeledVisitor[F, Y] with AnyLabeledColumnVisitor[F, Y]

		type MatchAnyLabeledValue[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyLabeledValueVisitor[F, Y]

		trait CaseAnyLabeledValue[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
			extends MatchAnyLabeledValue[F, Y]
		{
			def labeledValue[S >: Grouped <: Single, V](e :LabeledValueSQL[F, S, V]) :Y[S, V]

			override def labeled[S >: Grouped <: Single, V <: Listing](e :LabeledSQL[F, S, V]) :Y[S, V] = labeledValue(e)
			override def labeledColumn[S >: Grouped <: Single, N <: Label, V](e :LabeledColumnSQL[F, S, N, V]) :Y[S, V] =
				labeledValue(e)
		}
	}


	//consider: we could make *all* columns LabeledValueSQL. We'd still need this class for single column selects.
	//  problem: LabeledValueSQL.rephrase is needed to rephrase LabeledEntry, but we can't assume
	/** A bottom expression in a [[net.noresttherein.oldsql.sql.ast.LabeledSQL LabeledSQL]] - a single column expression
	  * with an associated key value `alias`.
	  */
	final class LabeledColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, N <: Label, V] private[LabeledSQL]
	                            (override val value :ColumnSQL[F, S, V], override val alias :N)
		extends AliasedSQL[F, S, V](value, alias) with LabeledValueSQL[F, S, V]
		   with SelectableColumnMappingSQL[F, IndexedMapping.of[V]#Column, V]
		   with GenericColumnDecoratorTemplate[F, S, V, ({ type E[-f <: RowProduct, -s >: Grouped <: Single, v] =
		                                                                              LabeledColumnSQL[f, s, N, v] })#E]
	{
		override def mapping[O] :TypedListingColumnSQLMapping[F, S, N, V, O] =
			TypedListingColumnSQLMapping(this, alias)

		override def mapping[O](columnPrefix :String) :TypedListingColumnSQLMapping[F, S, N, V, O] =
			TypedListingColumnSQLMapping(this, columnPrefix)

		override def nullSQL   = new LabeledColumnSQL[RowProduct, Single, N, V](SQLNull(value.selectForm), alias)
		//Consider: we might want to differentiate from 'default value' (functional) and true 'null' value
		// (i.e, something that will map to all nulls), because LabeledColumnSQL currently uses form.nulls.nullValue
		// to generate null columns when aligning column sets of two expressions.
		override def nullValue :Opt[V] = value.selectForm.nulls.opt //what if it's NonNull?
//			override def layout :LabeledColumnSQL[Nothing, S, N, V] = new LabeledColumnSQL(value.layout, alias)

		//made public because the def in GenericColumnDecoratorTemplate is protected, but public in AdaptedSQL
		override def reapply[E <: RowProduct, A >: Grouped <: Single]
		                    (e :ColumnSQL[E, A, V]) :LabeledColumnSQL[E, A, N, V] =
			decorate(e)

		protected override def decorate[E <: RowProduct, A >: Grouped <: Single, X]
		                               (e :ColumnSQL[E, A, X]) :LabeledColumnSQL[E, A, N, X] =
			new LabeledColumnSQL[E, A, N, X](e, alias)


		override def topSelectFrom[E <: F with GroundRow { type Complete <: E }](from :E)
				:TopSelectColumnAs[IndexedMapping.of[V]#Column, V] =
			SelectSQL(from, this)

		override def subselectFrom[B <: NonEmptyRow](from :F ProperSubselectOf B)
				:SubselectColumnAs[B, IndexedMapping.of[V]#Column, V] =
			SelectSQL.subselect[B, F ProperSubselectOf B, N, V](from, this)

		override def paramSelectFrom[P, E <: F with TopRow { type Complete <: E; type Params = P }](from :E)
				:SelectMapping[P, IndexedMapping.of[V]#Column] =
			Select(from)[N, V](this)

		override def visit[R[-_]](visitor :SpecificColumnVisitor[F, S, V, R]) :R = visitor.labeledColumn(this)
		override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, R]) :R[S, V] =
			visitor.labeledColumn(this)
	}



	object LabeledColumnSQL {
		def apply[A <: Label, F <: RowProduct, S >: Grouped <: Single, V]
		         (label :A, column :ColumnSQL[F, S, V]) :LabeledColumnSQL[F, S, A, V] =
			new LabeledColumnSQL(column, label)

		def unapply[F <: RowProduct, S >: Grouped <: Single, V]
		           (e :SQLExpression[F, S, V]) :Opt[(_ <: Label, ColumnSQL[F, S, V])] =
			e match {
				case col :LabeledColumnSQL[F, S, l, V] => Got((col.alias, col.value))
				case _ => Lack
			}

		trait SpecificLabeledColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] {
			def labeledColumn[N <: Label](e :LabeledColumnSQL[F, S, N, V]) :Y
		}
		type MatchSpecificLabeledColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
			SpecificLabeledColumnVisitor[F, S, V, Y]
		type CaseSpecificLabeledColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
			SpecificLabeledColumnVisitor[F, S, V, Y]

		trait AnyLabeledColumnVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
			def labeledColumn[S >: Grouped <: Single, N <: Label, V](e :LabeledColumnSQL[F, S, N, V]) :Y[S, V]
		}
		type MatchAnyLabeledColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyLabeledColumnVisitor[F, Y]
		type CaseAnyLabeledColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]  = AnyLabeledColumnVisitor[F, Y]
	}



	private final class LabeledEntry[-F <: RowProduct, -S >: Grouped <: Single, I <: Listing, K <: Label :ValueOf, L]
	                                (val left :LabeledSQL[F, S, I], val right :LabeledValueSQL[F, S, L])
		extends LabeledSQL[F, S, I |~ (K :~ L)] with RowShapeCache
	{ self =>
		override type Item[-E <: RowProduct, -D >: Grouped <: Single, X] = LabeledItem[E, D, _ <: Label, X]

		if (init.keys.contains(key))
			throw new IllegalArgumentException("Duplicate key given in " + this + ".")

		def key :K = valueOf[K]
		override def size :Int = items.last.index + 1
		val item = new LabeledItem(left.size, key, right)
		override lazy val items :Seq[Item[F, S, _]] = left.items :+ item
		override lazy val inOrder = left.inOrder :+ right
		override val keys :Seq[Label] = init.keys :+ key
//		override protected def parts :Seq[SQLExpression[F, S, _]] =
		override lazy val paths   :Seq[LabelPath[_]] = super.paths
		override lazy val toMap   :Map[String, LabeledValueSQL[F, S, _]] = left.toMap.updated(key, right)
		override lazy val columns :Map[LabelPath[_], LabeledColumnSQL[F, S, _ <: Label, _]] = super.columns
		override lazy val index   :Map[LabelPath[_], LabeledValueSQL[F, S, _]] = super.index
//
//		override def init[A <: Listing](implicit nonEmpty: (I |~ (K :~ L)) <:< (A |~ Listing.Item)) =
//			left.asInstanceOf[LabeledSQL[F, S, A]]
//
//		override def last[A](implicit nonEmpty: (I |~ (K :~ L)) <:< (Listing |~ (Label :~ A))) =
//			right.asInstanceOf[LabeledValueSQL[F, S, A]]
//
//		override def lastKey[A <: Label](implicit nonEmpty: (I |~ (K :~ L)) <:< (Listing |~ (A :~ Any))) =
//			key.asInstanceOf[A]
//
//		override def lastItem[A <: Label, B](implicit notEmpty :(I |~ (K :~ L)) <:< (Listing |~ (A :~ B))) =
//			item.asInstanceOf[LabeledItem[F, S, A, B]]

		override def extracts[E <: F, A >: Grouped <: S]
				:Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[I |~ (K :~ L)]#to, _]] =
			_extracts.asInstanceOf[Seq[Assoc[SQLExpression.from[E]#rows[A]#E, Extractor.from[I |~ (K :~ L)]#to, _]]]

		private[this] lazy val _extracts
			:Seq[Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I |~ (K :~ L)]#to, _]] =
		{
			def map[X](entry :Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I]#to, X]) =
				Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I |~ (K :~ L)]#to, X](
					entry._1, entry._2 compose Chain.init[I] _
				)
			left.extracts.map {
				entry :Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I]#to, _] => map(entry)
			} :+ Assoc[SQLExpression.from[F]#rows[S]#E, Extractor.from[I |~ (K :~ L)]#to, L](
				right, { tuple :(I |~ (K :~ L)) => tuple.last.value }
			)
		}

		override def construct(items :({ type T[X] = LabeledItem[F, S, _ <: Label, X] })#T =>: generic.Self) :I |~ (K :~ L) =
			left.construct(items) |~ :~[K](items(item))

		if (toMap.size == left.toMap.size)
			throw new IllegalArgumentException(
				"Cannot add " + right + " to " + this + ": Label" + key + " already used for " + toMap(key) + "."
			)

		override lazy val selectForm :SQLReadForm[I |~ (K :~ L)] = left.selectForm |~ :~[K](right.selectForm)

//			override def layout     :LabeledEntry[Nothing, S, I, K, L] = new LabeledEntry(prev.layout, value.layout)

		override def reorder(keys :Unique[String]) :SQLExpression[F, S, I |~ (K :~ L)] = {
			if (items.view.map(_.key) == keys)
				this
			else if (items.length != keys.size)
				throw new IllegalArgumentException(
					"There are " + items.length + " items in listing " + this + ", but the key permutation given " +
						keys + " is counts " + keys.size + " elements."
				)
			else {
				val newKeys = keys.downcastParam[Label]
				val ourKeys = items.view.map(_.key :Label).to(Unique)
				if (ourKeys.size != items.length)
					throw new IllegalStateException(
						"Cannot reorder listing " + this + " to key order " + keys + " because it contains key duplicates."
					)
				if (ourKeys.toSet != keys.toSet)
					throw new IllegalArgumentException(
						"Cannot reorder listing " + this + " to key order " + keys + " because the keys do not match."
					)
				def reorder(itemNo :Int) :LabeledSQL[F, S, _ <: Listing] =
					if (itemNo < 0)
						EmptyListing
					else {
						val init = reorder(itemNo - 1)
						val key = keys(itemNo)
						init |~ (key, toMap(key))
					}
				val reordered = reorder(ourKeys.size - 1).asInstanceOf[LabeledSQL[F, S, Listing]]
				val convert = ReorderRecord[Listing, I |~ (K :~ L)](
					SubRecord.unsafe(newKeys, ourKeys), SubRecord.unsafe(ourKeys, newKeys)
				)
				convert(reordered)
			}
		}

		override def reorder(paths :Seq[LabelPath[_]]) :SQLExpression[F, S, I |~ (K :~ L)] =
			if (this.paths == paths)
				this
			else {
				val includedInThis = this.index.keySet
				if (!paths.forall(includedInThis))
					throw new IllegalArgumentException(
						"Record reordering " + paths + " contains paths not existing in " + this + " " + this.paths + "."
					)
				val coveredPaths = paths.view.map(_.toList) to Set
				@tailrec def coversPath(path :List[String]) :Boolean =
					coveredPaths(path) || path.nonEmpty && coversPath(path.tail)
				if (!this.paths.view.map(_.toList).forall(coversPath))
					throw new IllegalArgumentException(
						"Record reordering " + paths + " of expression " + this + " does not include all columns: " +
							paths.filterNot(this.columns.contains) + "."
					)
				reform("reorder", paths.map(path => (path, index(path))))
			}

		override def project(paths :Seq[LabelPath[_]]) :SQLExpression[F, S, I |~ (K :~ L)] =
			if (this.paths == paths)
				this
			else {
				val includedInThis = this.index.keySet
				if (!paths.forall(includedInThis))
					throw new IllegalArgumentException(
						"Record projection " + paths + " contains paths not existing in " + this + " " + this.paths + "."
					)
				reform("project", paths.map(path => (path, index(path))))
			}

		override def reform[E <: F, A >: Grouped <: S]
		                   (paths :Seq[(LabelPath[_], LabeledValueSQL[E, A, _])]) :SQLExpression[E, A, I |~ (K :~ L)] =
			reform("reform", paths)

		private def reform[E <: F, A >: Grouped <: S](name :String, paths :Seq[(LabelPath[_], LabeledValueSQL[E, A, _])])
				:SQLExpression[E, A, I |~ (K :~ L)] =
		{
			if (paths.isEmpty)
				throw new IllegalArgumentException("Cannot project record " + this + " to an empty column set.")
			else if (this.paths == paths)
				this
			else {
				type Index = List[(String, Either[LabeledValueSQL[E, A, _], List[Any]])]
				/* A function converting between recursive Listing types with the structure represented by the arguments.
				 * Both list arguments have a recursive structure, with second elements of the pair having the same type
				 * as the argument lists. Each list element represents an element of either the argument listing,
				 * or the returned listing. The first pair elements in the entries are top level keys in a listing,
				 * while the last elements are either placeholder expressions providing a default (null) value,
				 * for when a corresponding entry in the other record does not exist, or a list of the same type
				 * as the argument list, containing recursive structure of the record field under the associated key.
				 *
				 * If a value is needed for a path not present in srcKeys, then the Either provided
				 * in dstKeys under the same path must be a Left, and expr.nullValue is used (not selectForm.nullValue) -
				 * this is the case for all columns, as well as completely omitted values.
				 * If an entry in dstKeys contains a Right, then src keys must use Right for the same key,
				 * and the value in the argument src for this key must be also a Listing.
				 */
				def convert(//the index for src argument: tails of the paths in the paths argument grouped by the first path element
	                        srcKeys :List[(String, Either[LabeledValueSQL[E, A, _], List[Any]])],
				            //the index of the built record: each element represents a LabeledValueSQL under this LabeledSQL
				            dstKeys :List[(String, Either[LabeledValueSQL[E, A, _], List[Any]])]
				           )(src :Listing) :Listing =
				{
					val entries = src.toSeq
					assert(srcKeys.length == entries.length,
						"The number of elements in the argument record " + src +
							" does not match the number of elements in the pre-built index " + srcKeys
					)
					val topValues = srcKeys.view.zipMap(entries) {
						case ((key, subtree), entry) => (key, (entry.value, subtree))
					}.toMap
					((@~ :Listing) /: dstKeys) { case (acc, (key, subtree)) =>
						subtree match {
							//We either use the field from the argument verbatim, or a null value from the provided expr
							case Left(expr) => topValues.get(key) match {
								case Some((value, _)) => acc |~ :~[key.type](value)
								case _ => acc |~ :~[key.type](expr.nullValue)
							}
							case Right(dst :Index @unchecked) =>
								topValues.get(key) match {
									case Some((listing :Listing, Right(index :Index @unchecked))) =>
										acc |~ :~[key.type](convert(index, dst)(listing))
									case Some((value, index)) =>
										throw Bug(s"Expected a Listing and an index list of entries, got ($value, $index).")
									case _ =>
										throw new AssertionError(
											"No field " + key + " in the mapped record " + src + "."
										)
								}
						}
					}
				}

				/* Recursively converts a record, starting with this expression, to an index of all direct and indirect
				 * values, both columns and other records. The returned List is of the same length as this.keys,
				 * and contains the latter as the first elements in the list entries. The second elements
				 * are this.toMap(key).selectForm.nulls, while the third is either an empty list, if value.toMap(key)
				 * is a column, or a recursively built index for the latter.
				 */
				def buildDstIndex(value :LabeledSQL[F, S, _]) :List[(String, Either[LabeledValueSQL[E, A, _], List[Any]])] =
					value.keys.view.map { key =>
						value.toMap(key) match {
							case column :LabeledColumnSQL[F, S, _, _] => (key, Left(column))
							case value :LabeledSQL[F, S, _] => (key, Right(buildDstIndex(value)))
						}
					}.toList

				/* Converts the argument paths as srcPaths to a recursive tree index: the first element of the pair
				 * is the key to a node, the second element is NullValue.NotNull, used for paths not present
				 * in this record, and the third element is a list with subnodes, of the same type as the return type.
				 */
				def buildSrcIndex(srcPaths :List[(List[String], LabeledValueSQL[E, A, _])],
				                  //path prefix to the current level
					              prefix :LabelPathPrefix,
				                  //the head of the last non empty element in srcPaths
				                  First :String = null,
				                  //tails of preceding elements in srcPath which had First as their head
				                  current :ListBuffer[(List[String], LabeledValueSQL[E, A, _])] =
				                    ListBuffer.empty[(List[String], LabeledValueSQL[E, A, _])],
				                  //result accumulator
				                  res :ListBuffer[(String, Either[LabeledValueSQL[E, A, _], List[_]])] =
				                    ListBuffer.empty[(String, Either[LabeledValueSQL[E, A, _], List[_]])])
						:List[(String, Either[LabeledValueSQL[E, A, _], List[_]])] =
					//invariant: First == null iff current.isEmpty
				{
					type Entry = (List[String], LabeledValueSQL[E, A, _])
					/* Fetches the expression under the given path in this expression, or returns alternatives
					 * if the path is not present in this expression. */
					def pathEntry(path :LabelPath[_], alternative :LabeledValueSQL[F, S, _]) :LabeledValueSQL[F, S, _] =
						this.index.get(path) match {
							case Some(value) => value
							case _ => alternative match {
								case column :LabeledColumnSQL[F, S, _, _] if column.alias != path.last =>
									throw new IllegalArgumentException(
										"Cannot introduce column " + column + " into " + this + " under path " + path +
										" because column's alias does not match the last element of the path."
									)
								case _ =>
									alternative
							}
						}
					srcPaths match {
						//First recursion step on this level, srcPaths contains a single, empty, path.
						case (Nil, alternative)::Nil if current.isEmpty =>
							assert(prefix.isInstanceOf[LabelPath[_]],
								"An empty path prefix when projecting " + this + " to " + paths + "."
							)
							val path = prefix.asInstanceOf[LabelPath[_]]
							(path.last, Left(pathEntry(path, alternative)))::Nil
						//an empty 'top level' path included with other paths in original srcDst
						case (Nil, _)::_ =>
							throw new IllegalArgumentException(
								"Paths " + paths + " include " + prefix +
									" which is either a duplicate or a prefix or of another path."
							)
						//Also the first recursion step, there are non empty paths under prefix
						case (key::path, alternative)::tail if current.isEmpty =>
							buildSrcIndex(tail, prefix, key, current += (path, alternative), res)
						//The first label in the current path is the same as in the previous one
						case (First::path, alternative)::tail =>
							buildSrcIndex(tail, prefix, First, current += (path, alternative), res)
						//The current path diverges from the previous one on this level
						case (key::path, alternative)::tail =>
							if (res.lastIndexWhere(_._1 == key) >= 0)
								throw new IllegalArgumentException(s"Paths $paths are not grouped by their keys.")
							//recursively build the index for the finished node
							val lastValuePath = prefix / First
							val lastIndex = buildSrcIndex(current.toList, lastValuePath)
							val last = (First, Right(lastIndex))
							//and now continue the recursion on the same level
							buildSrcIndex(tail, prefix, key, ListBuffer.empty[Entry] += (path, alternative), res += last)
						case Nil =>
							res.toList
					}
				}
				def buildNewExpr(keys :List[(String, Either[LabeledValueSQL[E, A, _], List[_]])]) :LabeledSQL[E, A, _] =
					((EmptyListing :LabeledSQL[F, S, _]) /: keys) {
						case (acc, (key, index)) => index match {
							case Left(expr) => acc |~ (key, expr)
							case Right(tree :Index @unchecked) => acc |~ (key, buildNewExpr(tree))
						}
					}

				val pathList = paths.view.map { case (path, placeholder) => (path.toList, placeholder) }.toList
				val srcIndex = buildSrcIndex(pathList, ~/)
				val dstIndex = buildDstIndex(this)
				val result = buildNewExpr(srcIndex).asInstanceOf[LabeledSQL[E, A, Listing]]
				val name = this.paths.mkString("." + name + "(", ", ", ")")
				val conversion = SQLConversion(
					name, convert(srcIndex, dstIndex), convert(dstIndex, srcIndex)
				).castParam2[I |~ (K :~ L)]
				conversion(result)
			}
		}

		override def groundValue :Opt[I |~ (K :~ L)] =
			for { i <- left.groundValue; l <- right.groundValue } yield i |~ :~[K](l)

		override def anchor(from :F) :LabeledSQL[F, S, I |~ (K :~ L)] =
			(left.anchor(from), right.anchor(from)) match {
				case (i, l) if (i eq left) && (l eq right) => this
				case (i, l) => new LabeledEntry(i, l)
			}

		override def basedOn[U <: F, E <: RowProduct]
		                    (base :E)(implicit expansion :U PartOf E) :LabeledSQL[E, S, I |~ (K :~ L)] =
			left.basedOn(base) |~ :~[K](right.basedOn(base))

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< S)
				:LabeledSQL[E, S, I |~ (K :~ L)] =
			left.expand(base) |~ :~[K](right.expand(base))

		//this short-circuits the call, resulting in no-callbacks for the prefixes. Lets call it a feature.
		override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :LabeledSQL[E, S, I |~ (K :~ L)] =
			left.rephrase(mapper) |~ :~[K](right.rephrase(mapper))

		override def nullSQL :LabeledSQL[RowProduct, Single, I |~ (K :~ L)] =
			left.nullSQL |~ :~[K](right.nullSQL)

		override def nullValue :Opt[I |~ (K :~ L)] = (left.nullValue, right.nullValue) match {
			case (Got(init), Got(last)) => Got(init |~ :~[K](last))
			case _ => Lack
		}

		protected override def reformer[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                                EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                               (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                               (implicit leftResult  :SQLTransformation[I |~ (K :~ L), U],
		                                         rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
				:SpecificExpressionVisitor
				 [F2, S2, V2, (leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
			new BaseReformer[F1, S1, F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](other)(reform, passCount)
				with MatchSpecificLabeled[F2, S2, V2, (leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]],
				                                       rightResult.SQLResult[F2, S2, EC2[U]])]
			{
				override def empty(implicit isEmpty :V2 =:= @~) =
					if (passCount.secondTime && reform.mayAddNullRight) {
						val emptyConversion = SQLConversion("to[@~]", (_:(I |~ (K :~ L))) => @~ : @~, (_: @~) => nullValue)
						(left, rightResult(emptyConversion(nullSQL)))
					} else if (passCount.secondTime && reform.mayExcludeLeft) {
						val nullConversion = SQLConversion(keys.mkString(".to[", ",", "]"), (_: @~) => nullValue, (_:(I |~ (K :~ L))))
						(leftResult(EmptyListing))
					} else
						fallback

				//we could also reform with a LabeledColumnSQL, too
				override def labeled[V <: Listing](e :LabeledSQL[F2, S2, V])(implicit isListing :V2 =:= V)
						:(leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
					if (keys == e.keys)
						super.labeled(e)
					else {
						//Attempt to reorder both or either expression, either excluding or adding null columns,
						// but only if the column forms match exactly on shared columns, because we have no information
						// on how - if at all - I |~ (K :~ L) and V2 are related.
						val leftColumns = self.columns
						val rightColumns = e.columns
						val subsetOfRight = leftColumns.forall {
							case (path, col) => rightColumns.get(path).exists(_.selectForm == col.selectForm)
						}
						val leftPaths  = leftColumns.keySet
						val rightPaths = rightColumns.keySet
						val subsetOfLeft =
							if (subsetOfRight)
								leftPaths == rightPaths
							else
								rightColumns.forall {
									case (path, col) => rightColumns.get(path).exists(_.selectForm == col.selectForm)
								}
						def subseqOfRight = {
							val otherPaths = e.paths to Unique
							paths.isSortedBy(otherPaths.indexOf)
						}
						def subseqOfLeft = {
							val paths = self.paths to Unique
							e.paths.isSortedBy(paths.indexOf)
						}
						if (subsetOfRight && subsetOfLeft) //same structure, different order
							if (reform.mayReorderLeft) {
								val reordered = reorder(e.paths)
								(leftResult(reordered), rightResult(other))
							} else if (reform.mayReorderRight) {
								val reordered = e.reorder(paths).asInstanceOf[Argument]
								(leftResult(self), rightResult(reordered))
							} else
								fallback
						else if (subsetOfRight)
							if (reform.mayExcludeRight) //use the left path set, it is a subset the right one
								if  (reform.mayReorderRight || subseqOfRight) {
									val reordered = e.project(paths).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
									(leftResult(self), rightResult(reordered))
								} else if (reform.mayReorderLeft) {
									val order = e.paths.filterNot(leftColumns.keySet)
									val right = e.project(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
									(leftResult(reorder(order)), rightResult(right))
								} else
									fallback
							else if (reform.mayAddNullLeft) //use the right path set, it is a superset of the left one
								if (reform.mayReorderLeft || subseqOfRight) {
									val order = e.paths.map(path => (path, rightColumns(path).nullSQL))
									(leftResult(self.reform(order)), rightResult(other))
								} else if (reform.mayReorderRight) {
									val order = paths ++ e.paths.filterNot(leftPaths)
									val left  = self.reform(order.map(path => (path, rightColumns(path).nullSQL)))
									val right = e.project(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
									(leftResult(left), rightResult(right))
								} else
									fallback
							else
								fallback
						else if (subsetOfLeft) //symmetrical to the above, but extracting as a method would actually take more code
							if (reform.mayExcludeLeft) //use the right path set, it is a subset of the left one
								if (reform.mayReorderLeft || subseqOfLeft)
									(leftResult(project(e.paths)), rightResult(other))
								else if (reform.mayReorderRight) {
									val order = paths.filterNot(rightColumns.keySet)
									val right = e.reorder(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
									(leftResult(project(order)), rightResult(right))
								} else
									fallback
							else if (reform.mayAddNullRight) //use the left path set, it a superset of the right one
								if (reform.mayReorderRight || subseqOfLeft) {
									val order = paths.map(path => (path, leftColumns(path).nullSQL))
									val right = e.reform(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
									(leftResult(self), rightResult(right))
								} else if (reform.mayReorderLeft) {
									val order = e.paths ++ paths.filterNot(rightPaths)
									val right = e.reform(order.map(path => (path, leftColumns(path).nullSQL)))
										.asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
									(leftResult(project(order)), rightResult(right))
								} else
									fallback
							else
								fallback
						else {
							val sharedInLeftOrder  = paths.filter(rightPaths)
							val sharedInRightOrder = e.paths.filter(leftPaths)
							if (reform.mayExcludeLeft)
								if (reform.mayExcludeRight) { //user the shared path set, projecting both sides
									if (reform.mayReorderLeft || sharedInLeftOrder == sharedInRightOrder) {
										val left  = self.project(sharedInRightOrder)
										val right = e.project(sharedInRightOrder).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
										(leftResult(left), rightResult(right))
									} else if (reform.mayReorderRight) {
										val left  = self.project(sharedInLeftOrder)
										val right = e.project(sharedInLeftOrder).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
										(leftResult(left), rightResult(right))
									} else
										fallback
								} else if (reform.mayAddNullLeft) { //use the right path set
									if (reform.mayReorderLeft || sharedInLeftOrder == sharedInRightOrder) {
										val left  = self.reform(e.paths.map { path => (path, rightColumns(path).nullSQL) })
										(leftResult(left), rightResult(other))
									} else if (reform.mayReorderRight) {
										val order = sharedInLeftOrder ++ e.paths.filterNot(leftPaths)
										val left = self.reform(order.map { path => (path, rightColumns(path).nullSQL) })
										val right = e.reorder(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
										(leftResult(left), rightResult(right))
									} else
										fallback
								} else
									fallback
							else if (reform.mayAddNullRight)
								if (reform.mayExcludeRight) { //use the left path set
									if (reform.mayReorderRight || sharedInLeftOrder == sharedInRightOrder) {
										val right = e.reform(paths.map { path => (path, leftColumns(path).nullSQL) })
										(leftResult(self), rightResult(right.asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]))
									} else if (reform.mayReorderLeft) {
										val order = e.paths.filter(leftPaths) ++ paths.filterNot(rightPaths)
										val left  = self.reorder(order)
										val right = e.reform(order.map { path => (path, leftColumns(path).nullSQL) })
										(leftResult(left), rightResult(right.asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]))
									} else
										fallback
								} else if (reform.mayAddNullLeft)  //use the superset of both path sets
									if (reform.mayReorderLeft || sharedInLeftOrder == sharedInRightOrder) {
										val order =
											e.paths.map { path => (path, rightColumns(path).nullSQL) } ++
												paths.filterNot(rightPaths).map { path => (path, leftColumns(path).nullSQL) }
										val right = e.reform(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
										(leftResult(reform(order)), rightResult(right))
									} else if (reform.mayReorderRight) {
										val order =
											paths.map { path => (path, leftColumns(path).nullSQL) } ++
												e.paths.filterNot(leftPaths).map { path => (path, rightColumns(path).nullSQL) }
										val right = e.reform(order).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
										(leftResult(reform(order)), rightResult(right))
									} else
										fallback
								else
									fallback
						}
					}

				override def emptyLabeled =
					if (reform.mayAddNullRight) {
						val forceEmpty = SQLAdaptation(".return(@~)", (_ :I |~ (K :~ L)) => @~)
						(leftResult(thisExpression), //this cast can be removed by delegating this case to EmptyListing
						 rightResult(forceEmpty(nullSQL).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]))
					} else if (reform.mayExcludeLeft)
						try {
							val nullValue = LabeledEntry.this.nullValue
							val forceNull = SQLAdaptation(".return(" + nullValue + ")", (_: @~) => nullValue)
							(leftResult(forceNull(nullSQL)), rightResult(other))
						} catch {
							case e1 :NullPointerException => try {
								fallback
							} catch {
								case e2 :MismatchedExpressionsException =>
									e2.addSuppressed(e1)
									throw e2
							}
						}
					else
						fallback

				override def labeledItem[I2 <: Listing, K2 <: Label, L2]
				                        (e :LabeledSQL[F2, S2, I2 |~ (K2 :~ L2)])
				                        (implicit isListing :V2 =:= (I2 |~ (K2 :~ L2))) =
				{
					implicit val listingResult = isListing.substituteCo[RightResult](rightResult)

					//attempt to recreate a LabeledSQL by casting down the expressions obtained
					// by reforming init and last
					def listing[G <: RowProduct, D >: Grouped <: Single, W <: Listing, A <: Label, Z]
					           (i :SQLExpression[G, D, W], key :A, l :SQLExpression[G, D, Z]) =
						attempt(i, key, l) match {
							case Lack if !i.isInstanceOf[LabeledSQL[_, _, _]] =>
								throw new MismatchedExpressionsException(LabeledEntry.this, e,
									"received a non-LabeledSQL expression " + i +
										": " + i.getClass.getName + " as a partial result."
								)
							case Lack =>
								throw new MismatchedExpressionsException(LabeledEntry.this, e,
									"Reformed last expression " + l + " is neither a ColumnSQL nor a LabeledValueSQL."
								)
							case res => res
						}
					type LeftSplit[YI <: Listing, YL] =
						(I sql_=> YI, L sql_=> YL, SQLTransformation[YI |~ (K :~ YL), U]#Into[leftResult.SQLResult])
					type RightSplit[YI <: Listing, YL] =
						(I2 sql_=> YI, L2 sql_=> YL, SQLTransformation[YI |~ (K :~ YL), U]#Into[rightResult.SQLResult])

					(splitListingTransformation(leftResult), splitListingTransformation(listingResult)) match {
						case (Lack, _) =>
							throw new MismatchedExpressionsException(
								self, other, "unsupported left conversion type " + leftResult + "."
							)
						case (_, Lack) =>
							throw new MismatchedExpressionsException(
								self, other, "unsupported right conversion type " + rightResult + "."
							)
						//init and last are converted separately, no conversion applied on top of init |~ key :~ last
						case (left :Opt[LeftSplit[Listing, Any]]@unchecked, right :Opt[RightSplit[Listing, Any]]@unchecked) =>
	//					if left.get._3.isIdentity && right.get._3.isIdentity =>
							implicit val (leftInitResult, leftLastResult, leftPost) = left.get
							implicit val (rightInitResult, rightLastResult, rightPost) = right.get
							val (leftLast, rightLast) = reform(self.last, e.last)
							val (leftInit, rightInit) = reform(self.init, e.init)
							val leftReformed  = listing(leftInit, key, leftLast)
							val rightReformed = listing(rightInit, e.lastKey, rightLast)
							(leftPost(leftReformed), rightPost(rightReformed))
					}
				}
			}


		protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] =
			spelling.split(left) ++ spelling.split(right)

		protected override def shape(implicit spelling :SQLSpelling) :RowShape =
			spelling.shape(init) + spelling.shape(right)

		override def equals(that :Any) :Boolean = that match {
			case self  :AnyRef if this eq self => true
			case other :LabeledEntry[_, _, _, _, _] if other.canEqual(this) =>
				right == other.right && key == other.key && left == other.left
			case _ => false
		}
		override def hashCode :Int = (left.hashCode * 31 + key.hashCode) * 31 + right.hashCode
	}


//	//We cannot sadly have a single instance for an empty ChainTuple and an empty LabeledSQL
//	// because methods init, last have incompatible signatures.
//	case object EmptyListing
//		extends NativeTerm[@~]("") with LabeledSQL[RowProduct, Single, @~] with EmptyChainSQL
//	{
//		private def unsupported =
//			throw new UnsupportedOperationException(
//				"This method is impossible to call without casting: @~ is not a subtype of |~."
//			)
//
//		override def init[I <: Listing](implicit nonEmpty: @~ <:< (I |~ Listing.Item)) :Nothing =
//			unsupported
//		override def last[L](implicit nonEmpty: @~ <:< (Listing |~ (Label :~ L))) :Nothing =
//			unsupported
//		override def lastKey[K <: Label](implicit nonEmpty: @~ <:< (Listing |~ (K :~ Any))) :Nothing =
//			unsupported
//		override def lastItem[K <: Label, L](implicit notEmpty: @~ <:< (Listing |~ (K :~ L))) :Nothing =
//			unsupported
//
//		override def nullSQL   :LabeledSQL[RowProduct, Single, @~] = this
//		override def nullValue : @~ = @~
//
//		override def keys :Seq[Label] = PassedArray.empty
//
//		override def reorder(keys :Unique[String]) :SQLExpression[RowProduct, Single, @~] =
//			if (keys.isEmpty)
//				this
//			else
//				throw new IllegalArgumentException(
//					"Cannot reorder an empty listing to a non-empty key order: " + keys + "."
//				)
//		override def reorder(paths :Seq[LabelPath[_]]) :SQLExpression[RowProduct, Single, @~] =
//			if (paths.isEmpty)
//				this
//			else
//				throw new IllegalArgumentException("Attempted to reorder an empty record to " + paths + ".")
//
//		override def project(paths :Seq[LabelPath[_]]) :SQLExpression[RowProduct, Single, @~] = reorder(paths)
//
//		override def reform[E <: RowProduct, A >: Grouped <: Single]
//		                   (paths :Seq[(LabelPath[_], LabeledValueSQL[E, A, _])]) :SQLExpression[E, A, @~] =
//		{
//			def reformed(suffixes :Seq[(List[String], LabeledValueSQL[E, A, _])])
//					:LabeledValueSQL[E, A, _] =
//			{
//				val lastIndices = (Map.empty[String, Int] /: suffixes.zipWithIndex) {
//					case (lastIndex, ((key::_, _), i)) =>
//						if (lastIndex.get(key).exists(_ != i - 1))
//							throw new IllegalArgumentException(
//								"Cannot project an empty record to " + paths + " because the paths are not grouped by leading keys."
//							)
//							lastIndex.updated(key, i)
//				}
//				val keyOrder = lastIndices.toIndexedSeq.sortBy(_._2).map(_._1)
//				val fields = suffixes.groupBy(_._1.head)//.view.mapValues { case (path, expr) => (path.tail, expr) }
//
//				val orderedFields = keyOrder.map(key => (key, fields(key)))
//				((EmptyListing :LabeledSQL[E, A, _]) /: orderedFields) { case (acc, (key, element)) =>
//					val topLevel = element.filter(_._1.isEmpty)
//					if (topLevel.sizeIs >= 1 && element.sizeIs > 1)
//						throw new IllegalArgumentException("Duplicate paths or a prefix path present in " + paths + ".")
//					if (topLevel.sizeIs == 1)
//						acc |~ ((key, element.head._2))
//					else
//						acc |~ ((key, reformed(element.map { case (path, placeholder) => (path.tail, placeholder) })))
//				}
//			}
//			val expr = reformed(paths.map { case (path, placeholder) => (path.toList, placeholder) })
//				.asInstanceOf[LabeledValueSQL[E, A, Listing]]
//			val conversion = SQLConversion(".return(@~)", (_ :Listing) => @~, (_: @~) => expr.nullValue)
//			conversion(expr)
//		}
//	}





	//attempt to recreate a LabeledSQL by casting down the expressions obtained
	// by reforming init and last
	private[ast] def attempt[F <: RowProduct, S >: Grouped <: Single, I <: Listing, K <: Label, L]
	           (i :SQLExpression[F, S, I], key :K, l :SQLExpression[F, S, L]) :Opt[LabeledSQL[F, S, I |~ (K :~ L)]] =
		i match {
			case listing :LabeledSQL[F, S, I] => l match {
				case value :LabeledValueSQL[F, S, L] =>
					Got(listing |~ :~[K](value))(new ValueOf(key))
				case value :ColumnSQL[F, S, L] =>
					Got(listing |~ :~[K](value))(new ValueOf(key))
				case _ => Lack
//					throw new MismatchedExpressionsException(LabeledEntry.this, e,
//						"Reformed last expression " + l + " is neither a ColumnSQL nor a LabeledValueSQL."
//					)
			}
			case _ => Lack
//				throw new MismatchedExpressionsException(LabeledEntry.this, e,
//					"received a non-LabeledSQL expression " + i +
//						": " + i.getClass.getName + " as a partial result."
//				)
		}

	/** Splits a transformation of a non-empty chain into transformations for its `init` and `last`.
	  * If the method returns `Got`, then the last conversion is actually identity.
	  */
	private[ast] def splitListingTransformation[XI <: Listing, K <: Label, XL, Z]
	                                           (conversion :SQLTransformation[XI |~ (K :~ XL), Z])
			:Opt[(XI sql_=> YI, XL sql_=> YL, SQLTransformation[YI |~ (K :~ YL), Z]#Into[conversion.SQLResult]) forSome {
				type YI <: Listing; type YL
			}] =
	{
		type Result[Y] = SQLTransformation[Y, Z]#Into[conversion.SQLResult]
		type Composed[Y] = (SQLTransformation[XI |~ (K :~ XL), Y], Result[Y])
		conversion match {
			case _ if conversion.isIdentity =>
				Got(SQLConversion.toSelf[XI], SQLConversion.toSelf[XL], conversion)
			case chain :ConvertRecord[xi, xl, yi, yl, k] =>
				Got(chain.init, chain.last, SQLConversion.toSelf.asInstanceOf[Result[yi |~ (k :~ yl)]])
			//if conversion is composed of only ConvertChain, we can split each and compose each item individually
			case _ => SQLTransformation.Composition.unapply(conversion) match {
				//a way of ensuring the intermediate existential types match and we can recompose
				case composed :Opt[Composed[y]] if composed.isDefined =>
					type Y = y
					type SplitFirst[YI <: Listing, YL, Y] =
						(XI sql_=> YI, XL sql_=> YL, SQLTransformation[YI |~ (K :~ YL), Y])
					splitListingTransformation(composed.get._1) match {
						case splitFirst :Opt[SplitFirst[yi, yl, Y]] if splitFirst.isDefined =>
							type YI = yi; type YL = yl
							type SplitSecond[ZI, ZL] = (YI sql_=> ZI, YL sql_=> ZL, Result[ZI |~ (K :~ ZL)])
							assert(splitFirst.get._3.isIdentity, splitFirst.get._3 + " is not identity")
							splitListingTransformation(composed.get._2.asInstanceOf[Result[YI |~ (K :~ YL)]]) match {
								case splitSecond :Opt[SplitSecond[zi, zl]] if splitSecond.isDefined =>
									val initResult     = splitFirst.get._1 andThen splitSecond.get._1
									val lastResult     = splitFirst.get._2 andThen splitSecond.get._2
									val combinedResult = splitSecond.get._3
									Got(initResult, lastResult, combinedResult)
								case _ => Lack
							}
						case _ => Lack
					}
				case _ => Lack
			}
		}
	}




	trait SpecificLabeledVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] { //extends SpecificEmptyVisitor[X, Y] {
		def labeled[V <: Listing](e :LabeledSQL[F, S, V])(implicit isListing :X =:= V) :Y
	}
	trait MatchSpecificLabeled[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificLabeledVisitor[F, S, X, Y]
	{
		def emptyLabeled :Y
		def labeledItem[I <: Listing, K <: Label, L]
		               (e :LabeledSQL[F, S, I |~ (K :~ L)])(implicit isListing :X =:= (I |~ (K :~ L))) :Y

		override def labeled[V <: Listing](e :LabeledSQL[F, S, V])(implicit isListing :X =:= V) :Y =
			e match {
				case tuple :LabeledEntry[F, S, i, k, l] =>
					labeledItem(tuple)(isListing.asInstanceOf[X =:= (i |~ (k :~ l))])
				case _ => emptyLabeled
			}
	}
	type CaseSpecificLabeled[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificLabeledVisitor[F, S, X, Y]
//
//
//	trait LabeledVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def labeled[S >: Grouped <: Single, T <: Listing](e :LabeledSQL[F, S, T]) :R[S, T, LabeledSQL[F, S, T]]
//	}
//	trait MatchLabeled[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends LabeledVisitor[F, R]
//	{
//		def labeledItem[S >: Grouped <: Single, I <: Listing, K <: Label, L]
//		               (e :LabeledSQL[F, S, I |~ (K :~ L)]) :R[S, I ~ (K :~ L), LabeledSQL[F, S, I |~ (K :~ L)]]
//
//		def emptyLabeled :R[Single, @~, LabeledSQL[RowProduct, Single, @~]]
//
//		override def labeled[S >: Grouped <: Single, T <: Listing]
//		                    (e :LabeledSQL[F, S, T]) :R[S, T, LabeledSQL[F, S, T]] =
//			(e match {
//				case tuple :LabeledEntry[F, S, i, k, l] => labeledItem(tuple)
//				case _ => emptyLabeled
//			}).asInstanceOf[R[S, T, LabeledSQL[F, S, T]]]
//	}
//	type CaseLabeled[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		LabeledVisitor[F, R]


	trait AnyLabeledVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {//extends AnyEmptyVisitor[Y] {
		def labeled[S >: Grouped <: Single, V <: Listing](e :LabeledSQL[F, S, V]) :Y[S, V]
	}

	trait MatchAnyLabeled[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyLabeledVisitor[F, Y]
	{
		def emptyLabeled :Y[Single, @~]

        def labeledItem[S >: Grouped <: Single, I <: Listing, K <: Label, L]
		               (e :LabeledSQL[F, S, I |~ (K :~ L)]) :Y[S, I |~ (K :~ L)]

		override def labeled[S >: Grouped <: Single, V <: Listing](e :LabeledSQL[F, S, V]) :Y[S, V] =
			(e match {
				case tuple :LabeledEntry[F, S, i, k, l] => labeledItem(tuple)
				case _ => emptyLabeled
			}).asInstanceOf[Y[S, V]]
	}
	type CaseAnyLabeled[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyLabeledVisitor[F, Y]
}
