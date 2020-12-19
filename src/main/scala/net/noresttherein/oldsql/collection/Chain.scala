package net.noresttherein.oldsql.collection

import scala.annotation.{implicitNotFound, tailrec}

import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainApplication, UpperBound}
import net.noresttherein.oldsql.collection.ChainMap.&~
import net.noresttherein.oldsql.collection.Listing.{:~, method_:~, |~}
import net.noresttherein.oldsql.morsels.abacus.{Inc, NegativeInc, Numeral, Positive}
import net.noresttherein.oldsql.morsels.generic.{Const, GenericFun, Self}
import net.noresttherein.oldsql.morsels.LUB







/** A list-like collection of fixed length, with the types of all its elements encoded in its type.
  * This is a very limited variant of ''shapeless'' `HList`, but which is extended from the end, rather than the front,
  * in opposition to traditional immutable lists. This stems from the intent of using it in conjunction with
  * `RowProduct`, with `AndFrom` being likewise left-associative, which makes the elements of both appear in the exact same
  * order, rather than reversed as an ease-of-life feature.
  *
  * Chain instances are, like lists, typically built by extending the empty chain `@~`:
  * {{{
  *     val values = @~ ~ 1 ~ "next" ~ 4.2
  *     values :@~ ~ Int ~ String ~ Double
  * }}}
  *
  * Aside from the traditional empty and non-empty subclasses, there are also subtypes of `Chain` with specific type
  * bounds, making possible enforcing a desired invariant without resorting to implicit witnesses.
  * @see [[net.noresttherein.oldsql.collection.Chain.@~]]
  * @see [[net.noresttherein.oldsql.collection.Chain.~]]
  * @see [[net.noresttherein.oldsql.collection.Listing]]
  * @see [[net.noresttherein.oldsql.collection.Record]]
  * @author Marcin Mościcki
  */
sealed trait Chain { //todo: Array based implementation; with macros/lazy implicits of scala 3 this would be a big improvement
	def isEmpty :Boolean
	def length :Int

	type Cat[+P <: Chain] <: Chain
	protected[collection] def cat[P <: Chain](prefix :P) :Cat[P]
}






/** An implementation artifact required to enforce required precedence of implicit values.
  * @see [[net.noresttherein.oldsql.collection.ChainFactory]]
   */
sealed trait BaseChainFactory {

	/** Type of the companion class. */
	type Type >: @~ <: Chain
	/** Upper bound for all items in the chain. */
	type Item
	/** Non-singleton lowest upper bound of `Item`, or `Item` if it is not a singleton type nor does it contain one. */
	type NonSingleton >: Item
	/** The non-empty subtype of `Type`. */
	type Link[+I <: Type, +L <: Item] <: (I ~ L) with Type


	/** Factory method for non-empty chains of type `Type`. */
	protected def link[I <: Type, L <: Item](init :I, last :L) :I Link L

	private[this] final val noBound = new UpperBound[Type, Item]

	/** This is the lowest priority implicit which can infer any type `U >: Item` as the upper bound of any chain.
	  * It is used when both `C` is abstract (i.e, ends with `Type` rather than `@~`) and the bound is externally
	  * constrained - usually by the caller specifying it explicitly. One design quirk is that it ''can'' infer
	  * the singleton type `Item` (if it is, or contains a singleton type) as the bound, which the higher priority
	  * implicit defined in `ChainFactory` cannot. This comes from the scala's compiler which strongly avoids
	  * inferring a singleton type. Note that the seemingly superfluous `U >: Item` type parameter is required
	  * to make this implicit lower priority than the wider definition declared in the extending `ChainFactory`.
	  *
	  */
	implicit def noUpperBound[C <: Type, U >: Item] :UpperBound[C, U] = noBound.asInstanceOf[UpperBound[C, U]]
}



/** Base trait for `Chain` subclasses companion objects. In this minimal form, it contains only `UpperBound` implicits
  * and type declarations for both the companion class and its type parameter bounds.
  */
trait ChainFactory extends BaseChainFactory {
	private[this] final val noBound = new UpperBound[Type, NonSingleton]

	/** Fallback `UpperBound` implicit value used when the chain `C` is abstract, that is ends with `Chain`/`Type`
	  * rather than `@~`. As the name implies, the type inferred will actually use the non-singleton version of*/
	implicit def nonSingletonUpperBound[C <: Type] :UpperBound[C, NonSingleton] = noBound.asInstanceOf[UpperBound[C, NonSingleton]]



	implicit def ordering[I <: Type :Ordering, L <: Item :Ordering] :Ordering[I Link L] =
		(left :I Link L, right :I Link L) => Ordering[I].compare(left.init, right.init) match {
			case 0 => Ordering[L].compare(left.last, right.last)
			case n => n
		}

}






object Chain extends ChainFactory {
	override type Type = Chain
	override type Item = Any
	override type NonSingleton = Any
	override type Link[+I <: Chain, +L] = I ~ L

	protected override def link[I <: Chain, L <: Any](init :I, last :L) :I ~ L = new link(init, last)



	@inline def apply[I <: Chain, L](init :I, last :L) :I ~ L = new link(init, last)



	/** Non-empty `Chain` implementation consisting of the 'tail' `I` (or rather, `init`, considering its 'reversed'
	  * nature) and 'head' `L` (the last element of the chain). There is an implicit conversion available
	  * to `ChainOps` which actually defines the methods for all chain operations, in order for it to work both
	  * for empty and non-empty chains and be able to provide type invariant implementations.
	  * This is also the base class of all non-empty `Chain` subclasses (variants with different type bounds).
	  * @tparam I the type of the chain with all elements but the last element of this type.
	  * @tparam L the type of the last element in the chain.
	  */
	trait ~[+I <: Chain, +L] extends Chain {
		val init :I
		def last :L

		def get :(I, L) = init -> last

		override def isEmpty = false
		override def length :Int = init.length + 1

		override type Cat[+P <: Chain] <: init.Cat[P] ~ L

		def canEqual(that :Any) :Boolean = that.isInstanceOf[~[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other: ~[_, _] if other.canEqual(this) => other.last == last && other.init == init
			case _ => false
		}

		override def hashCode :Int = init.hashCode * 31 + last.hashCode


		override def toString :String = {
			def entry(sb :StringBuilder, e :Any) :StringBuilder = e match {
				case chain : Chain => rec(chain, sb append "(") append ")"
				case _ => sb append e
			}
			def rec(chain :Chain, prefix :StringBuilder) :StringBuilder = chain match {
				case t ~ h => entry(rec(t, prefix) append symbol, h)
				case _ => prefix append chain
			}
			rec(this, new StringBuilder).toString
		}

		protected def symbol :String = "~"
	}



	object ~ {
		@inline def apply[I <: Chain, L](init :I, last :L) :I ~ L = new link(init, last)

		@inline def unapply[I <: Chain, L](chain :I ~ L) :I ~ L = chain

		def unapply(chain :Chain) :Option[(Chain, Any)] = chain match {
			case link: ~[_, _] => Some(link.init -> link.last)
			case _ => None
		}
	}



	protected[collection] class link[I <: Chain, L](override val init :I, override val last :L) extends (I ~ L) {
		override type Cat[+P <: Chain] = init.Cat[P] ~ L

		protected[collection] override def cat[P <: Chain](prefix :P) :Cat[P] = init.cat(prefix) ~ last
	}






	/** The type of all empty chains, with a single member being its companion object
	  * [[net.noresttherein.oldsql.collection.Chain.@~$ @~]].
	  */
	sealed class @~ private[Chain] extends Record with LabeledChain {
		override def isEmpty = true
		override def length = 0

		override type Cat[+P <: Chain] = P
		override type IndexedCat[+P <: Listing] = P
		override type LabeledCat[+P <: LabeledChain] = P
		override type MapCat[+P <: ChainMap] = P
		override type RecordCat[+P <: Record] = P

		def ~[N](next :N): @~ ~ N = new link(this, next)
		def |~[N <: Listing.Item](next :N) = new Listing.link(this, next)
		def >~[N <: LabeledChain.Item](next :N) = new LabeledChain.link(this, next)
		def &~[N <: ChainMap.Item](next :N) = new ChainMap.link(this, next)
		def |#[N <: Record.Item](next :N) = new Record.link(this, next)

		protected[collection] override def cat[P <: Chain](prefix :P) :P = prefix
		protected[collection] override def indexedCat[P <: Listing](prefix :P) :P = prefix
		protected[collection] override def labeledCat[P <: LabeledChain](prefix :P) :P = prefix
		protected[collection] override def mapCat[P <: ChainMap](prefix :P) :P = prefix
		protected[collection] override def recordCat[P <: Record](prefix :P) :P = prefix

	}


	/** An empty `Chain`, which is also an instance of every `Chain` variants defined here. */
	case object @~ extends @~ {
		def unapply(chain :Chain) :Boolean = chain.isInstanceOf[@~]
	}


	type Knot = @~
	final val Knot = @~



	@inline implicit class ChainOps[C <: Chain](private val self :C) extends AnyVal {

		/** Adds a new element at the end of this chain. */
		@inline def ~[N](next :N) :C ~ N = new link(self, next)

		/** Returns the element at the given index. The index must be an `Int` literal; if it is non-negative,
		  * it must be less then the chain's length and counting goes from left to right, starting with zero.
		  * If it is negative, counting goes from right to left, starting with `-1`. For positive indices,
		  * the chain must be complete, without an abstract prefix; for negative indices, it suffices that the number
		  * of last known elements is greater or equal `-index`. Negative indexing is more efficient, especially
		  * for items close to the end of the chain, as positive indexing always needs to traverse the whole chain
		  * to rewind and count from `@~`.
		  */
		@inline def apply[N <: Numeral, X](index :N)(implicit get :ChainGet[C, N, X]) :X =
			get(self)

		/** Sets the element at the given index. The index must be an `Int` literal; if it is non-negative,
		  * it must be less then the chain's length and counting goes from left to right, starting with zero.
		  * If it is negative, counting goes from right to left, starting with `-1`. For positive indices,
		  * the chain must be complete, without an abstract prefix; for negative indices, it suffices that the number
		  * of last known elements is greater or equal `-index`. Negative indexing is more efficient, especially
		  * for items close to the end of the chain, as positive indexing always needs to traverse the whole chain
		  * to rewind and count from `@~`.
		  * @return the chain equal to this one on all positions except the index, where it contains the inserted item.
		  */
		@inline def updated[N <: Numeral, X, R <: Chain](index :N, item :X)(implicit set :ChainSet[C, N, X, R]) :R =
			set(self, item)

		/** Inserts the element into this chain. The index must be an `Int` literal; if it is non-negative,
		  * it must be less then or equal the chain's length and the element is inserted before the index
		  * (on the left side), with counting going from left to right, starting with zero. If it is negative,
		  * the element is inserted after the index (on the right side) and counting goes from right to left,
		  * starting with `-1`. For positive indices, the chain must be complete, without an abstract prefix;
		  * for negative indices, it suffices that the number of last known elements is greater or equal `-index`.
		  * Negative indexing is more efficient, especially for items close to the end of the chain,
		  * as positive indexing always needs to traverse the whole chain to rewind and count from `@~`.
		  */
		@inline def insert[N <: Numeral, X, R <: Chain](index :N, item :X)(implicit insert :ChainInsert[C, N, X, R]) :R =
			insert(self, item)

		/** Removes the element at the given index. The index must be an `Int` literal; if it is non-negative,
		  * it must be less then the chain's length and counting goes from left to right, starting with zero.
		  * If it is negative, counting goes from right to left, starting with `-1`. For positive indices, the
		  * chain must be complete, without an abstract prefix; for negative indices, it suffices that the number
		  * of last known elements is greater or equal `-index`. Negative indexing is more efficient, especially
		  * for items close to the end of the chain, as positive indexing always needs to traverse the whole chain
		  * to rewind and count from `@~`.
		  * @return the chain resulting from removing the item at index `N`.
		  */
		@inline def remove[N <: Numeral, R <: Chain](index :N)(implicit del :ChainDelete[C, N, R]) :R =
			del(self)



		/** Maps this chain using the given generic (polymorphic) function.
		  * @tparam XF type constructor which forms the type of every element in this chain. If this chain contains
		  *            elements without a common type constructor, [[net.noresttherein.oldsql.morsels.generic.Self Self]]
		  *            can be used which is an identity functor for types.
		  * @tparam YF the type constructor which is being applied to every argument of functor `XF` in this chain.
		  * @tparam Y the type of the transformed chain, which is the result of replacing every top-level application
		  *           of `XF` on every element of this chain with the type constructor `YF`.
		  */
		@inline def map[XF[T], YF[T], Y <: Chain](f :GenericFun[XF, YF])(implicit result :MapChain[XF, C, YF, Y]) :Y =
			result(f)(self)

		/** Applies the given function to every element of this chain for the side effects. */
		def foreach(f :GenericFun[Self, Const[Unit]#T]) :Unit = {
			def rec(chain :Chain) :Unit = chain match {
				case t ~ h => rec(t); f(h)
				case _ => ()
			}
			rec(self)
		}


		/** Appends the given chain to the end of this chain. */
		@inline def ++[S <: Chain](suffix :S) :suffix.Cat[C] = suffix.cat(self)

		/** Given a function `F` accepting the same number of arguments, of the same types and in the same order
		  * as the elements of this chain, apply it to the elements of this chain and return the result.
		  */
		@inline def feedTo[F, Y](f :F)(implicit application :ChainApplication[C, F, Y]) :Y = application(f, self)


		/** Returns the elements of this chain in a `Seq`, which type is the lowest upper bound of all type elements
		  * (unless another `UpperBound` instance is provided explicitly). This simply forwards to `toList`.
		  */
		@inline def toSeq[U](implicit ub :UpperBound[C, U]) :Seq[U] = toList

		/** Returns the elements of this chain in a list, which type is the lowest upper bound of all type elements
		  * (unless the type parameter `U` or another `UpperBound` instance is provided explicitly).
		  * If this chain contains only singleton elements, the singleton part of the type will be removed
		  * (i.e., `String with Singleton` is replaced with `String`), unless the singleton type bound is provided
		  * explicitly. The list contains the elements in the same order as this chain, meaning the top (last) element
		  * of this chain becomes the deepest (first after `Nil`) element of the list.
		  */
		def toList[U](implicit ub :UpperBound[C, U]) :List[U] = {
			@tailrec def rec(chain :Chain, res :List[U] = Nil) :List[U] = chain match {
				case t ~ h => rec(t, h.asInstanceOf[U]::res)
				case _ => res
			}
			rec(self)
		}

		/** If all elements of this chain are `Tuple2` instances, returns them in a `Map` with the key and value types
		  *  being the upper bounds of the first and second elements of the tuples, respectively. Unless the type
		  *  parameter or the `UpperBound` implicit parameter is provided explicitly, these will be the lowest upper
		  *  bounds which are ''not'' singleton types.
		  */
		@inline def toMap[K, V](implicit ub :UpperBound[C, (K, V)]) :Map[K, V] =
			self.toSeq[(K, V)].toMap

	}



	@inline def last[T](c :Chain ~ T) :T = c.last

	@inline def init[C <: Chain](c :C ~ Any) :C = c.init






	implicit val EmptyOrdering :Ordering[@~] = (x : @~, y : @~) => 0


	@implicitNotFound("Type ${U} is not an upper bound of elements in chain ${C}.")
	sealed class UpperBound[-C <: Chain, +U] private[collection] () {}

	implicit final val EmptyChainBound = new UpperBound[@~, Nothing]

	@inline implicit def upperBound[T <: Chain, H, U, X](implicit tail :UpperBound[T, U], lub :LUB[H, U, X]) :UpperBound[T ~ H, X] =
		tail.asInstanceOf[UpperBound[T ~ H, X]]




	@implicitNotFound("Can't prove that ${P}[X] for all X in chain ${C}.")
	class ForAllItems[C <: Chain, P[_]] private()

	object ForAllItems {
		private[this] val instance = new ForAllItems[Chain, List]

		implicit def empty[P[_]] :ForAllItems[@~, P] = instance.asInstanceOf[ForAllItems[@~, P]]

		implicit def induction[C <: Chain, X, P[_]](implicit init :ForAllItems[C, P], last :P[X]) :ForAllItems[C ~ X, P] =
			instance.asInstanceOf[ForAllItems[C ~ X, P]]
	}


	@implicitNotFound("Can't find an element X satisfying ${P}[X] in chain ${C}: implicit ${P}[${X}] not found.")
	class ItemExists[C <: Chain, P[_], X] private (val found :P[X]) extends AnyVal

	object ItemExists {
		implicit def last[C <: Chain, P[_], X](implicit pred :P[X]) :ItemExists[C ~ X, P, X] =
			new ItemExists[C ~ X, P, X](pred)

		implicit def earlier[C <: Chain, T, P[_], X](implicit exists :ItemExists[C, P, X]) :ItemExists[C ~ T, P, X] =
			new ItemExists[C ~ T, P, X](exists.found)
	}


	type ChainContains[C <: Chain, X] = ItemExists[C, ({ type EQ[T] = X =:= T })#EQ, X]






	@implicitNotFound("Can't calculate the length of chain ${C}: either it is unknown " +
	                  "(the chain starts with abstract Chain), it not equal ${N}, or we ran out of natural numbers.")
	class ChainLength[-C <: Chain, N <: Numeral] private ()

	object ChainLength {
		implicit val ZeroLength = new ChainLength[@~, 0]

		implicit def oneLinkLonger[C <: Chain, M <: Numeral, N <: Numeral]
		                          (implicit prefix :ChainLength[C, M], plus :Inc[M, N]) :ChainLength[C ~ Any, N] =
			ZeroLength.asInstanceOf[ChainLength[C ~ Any, N]]
	}



	@implicitNotFound("Can't get ${N}-th element of chain ${C}:\n either the index type is abstract, the chain starts " +
	                  "with abstract Chain (rather than @~) and the index is non-negative, ${N} >= length, " +
	                  "${N} < -length, the item type can't be unified with ${X}, or we ran out of known integers.")
	abstract class ChainGet[-C <: Chain, N <: Numeral, +X] private {
		def apply(chain :C) :X
	}

	object ChainGet {
		private[this] val last :ChainGet[Chain ~ Any, 0, Any] = new ChainGet[Chain ~ Any, 0, Any] {
			override def apply(chain :Chain ~ Any) = chain.last
		}

		private class GetPrev[-C <: Chain, N <: Numeral, +X](prev :ChainGet[C, _, X]) extends ChainGet[C ~ Any, N, X] {
			override def apply(chain :C ~ Any) = prev(chain.init)
		}


		implicit def getLastPositive[C <: Chain, N <: Numeral, X](implicit length :ChainLength[C, N]) :ChainGet[C ~ X, N, X] =
			last.asInstanceOf[ChainGet[C ~ X, N, X]]

		implicit def getEarlierPositive[C <: Chain, N <: Numeral, X]
		                               (implicit prev :ChainGet[C, N, X], positive :Positive[N]) :ChainGet[C ~ Any, N, X] =
			new GetPrev(prev)

		implicit def getLastNegative[X] :ChainGet[Chain ~ X, -1, X] = last.asInstanceOf[ChainGet[Chain ~ X, -1, X]]

		implicit def getEarlierNegative[C <: Chain, M <: Numeral, N <: Numeral, X]
		                               (implicit prev :ChainGet[C, N, X], negative :NegativeInc[M, N]) :ChainGet[C ~ Any, M, X] =
			new GetPrev(prev)
	}






	@implicitNotFound("Can't set the ${N}-th element of chain ${C} to ${X}: either the index type is abstract, " +
	                  "the chain starts with an abstract Chain (rather than @~) and the index is non-negative, " +
	                  "${N} >= length, ${N} < -length, the result can't be unified with ${R}, or we ran out of integers.")
	sealed abstract class ChainSet[-C <: Chain, N <: Numeral, X, +R <: Chain] private {
		def apply(chain :C, elem :X) :R
	}

	object ChainSet {
		private[this] val last :ChainSet[Chain ~ Any, -1, Any, Chain ~ Any] =
			new ChainSet[Chain ~ Any, -1, Any, Chain ~ Any] {
				override def apply(chain :Chain ~ Any, elem :Any) = chain.init ~ elem
			}

		private class SetPrev[-C <: Chain, T, N <: Numeral, X, +R <: Chain](prev :ChainSet[C, _, X, R])
			extends ChainSet[C ~ T, N, X, R ~ T]
		{
			override def apply(chain :C ~ T, elem :X) = prev(chain.init, elem) ~ chain.last
		}

		implicit def setLastPositive[C <: Chain, N <: Numeral, X]
		                            (implicit length :ChainLength[C, N]) :ChainSet[C ~ Any, N, X, C ~ X] =
			last.asInstanceOf[ChainSet[C ~ Any, N, X, C ~ X]]

		implicit def setEarlierPositive[C <: Chain, N <: Numeral, X, T, R <: Chain]
		                               (implicit prev :ChainSet[C, N, X, R], positive :Positive[N])
				:ChainSet[C ~ T, N, X, R ~ T] =
			new SetPrev(prev)

		implicit def setLastNegative[C <: Chain, X] :ChainSet[C ~ Any, -1, X, C ~ X] =
			last.asInstanceOf[ChainSet[C ~ Any, -1, X, C ~ X]]

		implicit def setEarlierNegative[C <: Chain, M <: Numeral, N <: Numeral, X, T, R <: Chain]
		                               (implicit prev :ChainSet[C, N, X, R], negative :NegativeInc[M, N])
				:ChainSet[C ~ T, M, X, R ~ T] =
			new SetPrev(prev)

	}






	@implicitNotFound("Can't insert ${X] at the ${N}-th position into chain ${C}: either the index type is abstract, " +
	                  "the chain starts with an abstract Chain (rather than @~) and the index is non-negative, " +
	                  "${N} > length, ${N} < -length - 1, the result can't be unified with ${R}, or we ran out of integers.")
	sealed abstract class ChainInsert[-C <: Chain, N <: Numeral, X, +R <: Chain] private {
		def apply(chain :C, elem :X) :R
	}

	object ChainInsert {
		private[this] val last :ChainInsert[Chain, -1, Any, Chain ~ Any] =
			new ChainInsert[Chain, -1, Any, Chain ~ Any] {
				override def apply(chain :Chain, elem :Any) = chain ~ elem
			}

		private class InsertPrev[-C <: Chain, T, N <: Numeral, X, +R <: Chain](prev :ChainInsert[C, _, X, R])
			extends ChainInsert[C ~ T, N, X, R ~ T]
		{
			override def apply(chain :C ~ T, elem :X) = prev(chain.init, elem) ~ chain.last
		}

		implicit def insertLastPositive[C <: Chain, N <: Numeral, X]
		                               (implicit length :ChainLength[C, N]) :ChainInsert[C, N, X, C ~ X] =
			last.asInstanceOf[ChainInsert[C, N, X, C ~ X]]

		implicit def insertEarlierPositive[C <: Chain, N <: Numeral, X, T, R <: Chain]
		                                  (implicit prev :ChainInsert[C, N, X, R], positive :Positive[N])
				:ChainInsert[C ~ T, N, X, R ~ T] =
			new InsertPrev(prev)

		implicit def insertLastNegative[C <: Chain, X] :ChainInsert[C, -1, X, C ~ X] =
			last.asInstanceOf[ChainInsert[C, -1, X, C ~ X]]

		implicit def insertEarlierNegative[C <: Chain, M <: Numeral, N <: Numeral, X, T, R <: Chain]
		                                  (implicit prev :ChainInsert[C, N, X, R], negative :NegativeInc[M, N])
				:ChainInsert[C ~ T, M, X, R ~ T] =
			new InsertPrev(prev)

	}



	@implicitNotFound("Can't delete ${N}-th element from chain ${C}: either the index type is abstract, the chain " +
	                  "starts with an abstract Chain (rather than @~) and the index is non-negative, ${N} >= length, " +
	                  "${N} < -length, the result chain can't be unified with ${R}, or we ran out of known integers.")
	sealed abstract class ChainDelete[-C <: Chain, N <: Numeral, +R <: Chain] private {
		def apply(chain :C) :R
	}

	object ChainDelete {
		private[this] val last :ChainDelete[Chain ~ Any, 0, Chain] =
			new ChainDelete[Chain ~ Any, 0, Chain] {
				override def apply(chain :Chain ~ Any) = chain
			}

		private class DeletePrev[-C <: Chain, T, N <: Numeral, +R <: Chain](prev :ChainDelete[C, _, R])
			extends ChainDelete[C ~ T, N, R ~ T]
		{
			override def apply(chain :C ~ T) = prev(chain.init) ~ chain.last
		}

		implicit def deleteLastPositive[C <: Chain, N <: Numeral](implicit length :ChainLength[C, N])
				:ChainDelete[C ~ Any, N, C] = last.asInstanceOf[ChainDelete[C ~ Any, N, C]]

		implicit def deleteEarlierPositive[C <: Chain, X, N <: Numeral, R <: Chain]
		                                  (implicit prev :ChainDelete[C, N, R], positive :Positive[N])
				:ChainDelete[C ~ X, N, R ~ X] =
			new DeletePrev(prev)

		implicit def deleteLastNegative[C <: Chain] :ChainDelete[C ~ Any, -1, C] =
			last.asInstanceOf[ChainDelete[C ~ Any, -1, C]]

		implicit def deleteEarlierNegative[C <: Chain, X, M <: Numeral, N <: Numeral, R <: Chain]
		                                  (implicit prev :ChainDelete[C, N, R], negative :NegativeInc[M, N])
				:ChainDelete[C ~ X, M, R ~ X] =
			new DeletePrev(prev)

	}






	@implicitNotFound("Can't perform a natural transformation of the chain ${X} (of applied ${XF}) to a chain of ${YF}" +
	                  "applied to all type arguments of elements of the mapped chain.")
	sealed abstract class MapChain[XF[T], X <: Chain, YF[T], Y <: Chain] private {
		def apply(f :GenericFun[XF, YF])(x :X) :Y
	}

	object MapChain {
		@inline implicit def mapEmptyChain[XF[T], YF[T]] :MapChain[XF, @~, YF, @~] = new MapChain[XF, @~, YF, @~] {
			override def apply(f :GenericFun[XF, YF])(x: @~): @~ = x
		}

		@inline implicit def mapChainHead[XF[T], XT <: Chain, YF[T], YT <: Chain, H]
		                         (implicit mapTail :MapChain[XF, XT, YF, YT]) :MapChain[XF, XT~XF[H], YF, YT~YF[H]] =
			new MapChain[XF, XT~XF[H], YF, YT~YF[H]] {
				override def apply(f :GenericFun[XF, YF])(x :XT ~ XF[H]): YT ~ YF[H] =
					mapTail(f)(x.init) ~ f(x.last)
			}
	}






	@implicitNotFound("Can't apply object ${F} to the chain ${X} with any known conversion.")
	abstract class ChainApplication[-X <: Chain, -F, +Y] extends ((F, X) => Y)

	/** Forces SAM conversion of a function literal `(F, X) => Y` into a `ChainApplication`. */
	@inline private[collection] def ChainApplication[X <: Chain, F, Y]
	                                                (apply :ChainApplication[X, F, Y]) :ChainApplication[X, F, Y] =
		apply



	implicit def applyChain[C <: Chain, Y] :ChainApplication[C, C => Y, Y] =
		ChainApplication { (f :C => Y, xs :C) => f(xs) }



	implicit def applyFunction1[A, Y] :ChainApplication[@~ ~ A, A => Y, Y] =
		ChainApplication { (f :A => Y, xs: @~ ~ A) => f(xs.last) }

	implicit def applyFunction2[A, B, Y] :ChainApplication[@~ ~ A ~ B, (A, B) => Y, Y] =
		ChainApplication { (f :(A, B) => Y, xs: @~ ~A~B) => f(xs.init.last, xs.last) }

	implicit def applyFunction3[A, B, C, Y] :ChainApplication[@~ ~ A ~ B ~ C, (A, B, C) => Y, Y] =
		ChainApplication {
			(f :(A, B, C) => Y, xs: @~ ~A~B~C) => val xs1 = xs.init; f(xs1.init.last, xs1.last, xs.last)
		}

	implicit def applyFunction4[A, B, C, D, Y] :ChainApplication[@~ ~ A ~ B ~ C ~ D, (A, B, C, D) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D) => Y, xs: @~ ~A~B~C~D) =>
				val xs1 = xs.init; val xs2 = xs1.init; f(xs2.init.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction5[A, B, C, D, E, Y] :ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E, (A, B, C, D, E) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E) => Y, xs: @~ ~A~B~C~D~E) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init
				f(xs3.init.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction6[A, B, C, D, E, F, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F, (A, B, C, D, E, F) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F) => Y, xs: @~ ~A~B~C~D~E~F) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init
				f(xs4.init.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction7[A, B, C, D, E, F, G, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G, (A, B, C, D, E, F, G) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G) => Y, xs: @~ ~A~B~C~D~E~F~G) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				f(xs5.init.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction8[A, B, C, D, E, F, G, H, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H, (A, B, C, D, E, F, G, H) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H) => Y, xs: @~ ~A~B~C~D~E~F~G~H) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init
				f(xs6.init.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction9[A, B, C, D, E, F, G, H, I, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I, (A, B, C, D, E, F, G, H, I) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init
				f(xs7.init.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction10[A, B, C, D, E, F, G, H, I, J, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J,
			                  (A, B, C, D, E, F, G, H, I, J) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init
				f(xs8.init.last, xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction11[A, B, C, D, E, F, G, H, I, J, K, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K,
			                  (A, B, C, D, E, F, G, H, I, J, K) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init
				f(xs9.init.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction12[A, B, C, D, E, F, G, H, I, J, K, L, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L,
			                  (A, B, C, D, E, F, G, H, I, J, K, L) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				f(xs10.init.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction13[A, B, C, D, E, F, G, H, I, J, K, L, M, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M,
			                  (A, B, C, D, E, F, G, H, I, J, K, L, M) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init
				f(xs11.init.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init
				f(xs12.init.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O,
			                  (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init
				f(xs13.init.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				f(xs14.init.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init
			f(xs15.init.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
				xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init
				f(xs16.init.last, xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R ~ S,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R~S) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init
				f(xs17.init.last, xs17.last,
					xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R ~ S ~ T,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R~S~T) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				f(xs18.init.last, xs18.last, xs17.last,
					xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R ~ S ~ T ~ U,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R~S~T~U) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				val xs19 = xs18.init
				f(xs19.init.last, xs19.last, xs18.last, xs17.last,
					xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R ~ S ~ T ~ U ~ V,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R~S~T~U~V) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				val xs19 = xs18.init; val xs20 = xs19.init
				f(xs20.init.last, xs20.last, xs19.last, xs18.last, xs17.last,
					xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}






	implicit def applyTuple1[A, Y] :ChainApplication[@~ ~ A, Tuple1[A] => Y, Y] =
		ChainApplication { (f :Tuple1[A] => Y, xs: @~ ~ A) => f(Tuple1(xs.last)) }

	implicit def applyTuple2[A, B, Y] :ChainApplication[@~ ~ A ~ B, ((A, B)) => Y, Y] =
		ChainApplication { (f :((A, B)) => Y, xs: @~ ~A~B) => f((xs.init.last, xs.last)) }

	implicit def applyTuple3[A, B, C, Y] :ChainApplication[@~ ~ A ~ B ~ C, ((A, B, C)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C)) => Y, xs: @~ ~A~B~C) => val xs1 = xs.init; f((xs1.init.last, xs1.last, xs.last))
		}

	implicit def applyTuple4[A, B, C, D, Y] :ChainApplication[@~ ~ A ~ B ~ C ~ D, ((A, B, C, D)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D)) => Y, xs: @~ ~A~B~C~D) =>
				val xs1 = xs.init; val xs2 = xs1.init; f((xs2.init.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple5[A, B, C, D, E, Y] :ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E, ((A, B, C, D, E)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E)) => Y, xs: @~ ~A~B~C~D~E) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init
				f((xs3.init.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple6[A, B, C, D, E, F, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F, ((A, B, C, D, E, F)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F)) => Y, xs: @~ ~A~B~C~D~E~F) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init
				f((xs4.init.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple7[A, B, C, D, E, F, G, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G, ((A, B, C, D, E, F, G)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G)) => Y, xs: @~ ~A~B~C~D~E~F~G) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				f((xs5.init.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple8[A, B, C, D, E, F, G, H, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H, ((A, B, C, D, E, F, G, H)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H)) => Y, xs: @~ ~A~B~C~D~E~F~G~H) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init
				f((xs6.init.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple9[A, B, C, D, E, F, G, H, I, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I, ((A, B, C, D, E, F, G, H, I)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I)) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init
				f((xs7.init.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple10[A, B, C, D, E, F, G, H, I, J, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J,
			                  ((A, B, C, D, E, F, G, H, I, J)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J)) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init
				f((xs8.init.last, xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple11[A, B, C, D, E, F, G, H, I, J, K, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K,
			                  ((A, B, C, D, E, F, G, H, I, J, K)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K)) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init
				f((xs9.init.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple12[A, B, C, D, E, F, G, H, I, J, K, L, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L,
			                  ((A, B, C, D, E, F, G, H, I, J, K, L)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L)) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				f((xs10.init.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple13[A, B, C, D, E, F, G, H, I, J, K, L, M, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M,
			                  ((A, B, C, D, E, F, G, H, I, J, K, L, M)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M)) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init
				f((xs11.init.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N,
		                      ((A, B, C, D, E, F, G, H, I, J, K, L, M, N)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N)) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init
				f((xs12.init.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O,
			                  ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init
				f((xs13.init.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P,
		                      ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				f((xs14.init.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q,
		                      ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init
			f((xs15.init.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
				xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R,
		                      ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init
				f((xs16.init.last, xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R ~ S,
		                      ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R~S) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init
				f((xs17.init.last, xs17.last,
					xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R ~ S ~ T,
		                      ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R~S~T) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				f((xs18.init.last, xs18.last, xs17.last,
					xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R ~ S ~ T ~ U,
		                      ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R~S~T~U) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				val xs19 = xs18.init
				f((xs19.init.last, xs19.last, xs18.last, xs17.last,
					xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}

	implicit def applyTuple22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R ~ S ~ T ~ U ~ V,
		                      ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R~S~T~U~V) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				val xs19 = xs18.init; val xs20 = xs19.init
				f((xs20.init.last, xs20.last, xs19.last, xs18.last, xs17.last,
					xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last))
		}


}






/** Base class for the companion object of the `Listing` implementation of `Chain` as well as companion objects
  * of its subclasses. Contains implicit witnesses which form the basis of implementation of most operations shared
  * by all classes in the `Listing` type hierarchy and which need access to the factory method for the particular
  * implementation, preventing their declaration in a static context. A `Listing` is a `Chain` consisting
  * of pairs, which first element is always a singleton type (with the intention of it being a literal type),
  * forming a specialized but limited ''shapeless'' `HMap` variant.
  * @see [[net.noresttherein.oldsql.collection.Listing]]
  */
sealed abstract class ListingFactory extends ChainFactory {
	type Type >: @~ <: Chain
	type Link[+I <: Type, +L <: Item] <: (I ~ L) with Type

	/** The upper bound type of the first member of each tuple in the index. */
	type Key

	/** The upper bound for the second member of each tuple in the index */
	type Value
	type Item = Entry[Key, Value]
	type Entry[+K <: Key, +V <: Value]


	protected def get[K <: Key, V <: Value](index :Type Link Entry[K, V]) :V

	protected def get[K <: Key, V <: Value](index :Type Link Entry[K, V], key :K) :V

	protected def set[I <: Type, K <: Key, V <: Value](i :I Link Entry[K, V], key :K, v :V) :I Link Entry[K, V]

	protected def append[I <: Type, K <: Key, V <: Value](index :I, key :K, value :V) :I Link Entry[K, V]



	@implicitNotFound("Type ${K} is not a key in index ${I} (or is not mapped to type ${V}).")
	sealed abstract class IndexGet[-I <: Type, K <: Key, +V <: Value] extends ((I, K) => V)

	object IndexGet {

		implicit def getLast[K <: Key, V <: Value] :IndexGet[Type Link Entry[K, V], K, V] =
			new IndexGet[Type Link Entry[K, V], K, V] {
				override def apply(index :Type Link Entry[K, V], key :K) = get(index, key)
			}

		@inline implicit def getPrev[I <: Type, K <: Key, V <: Value]
		                    (implicit get :IndexGet[I, K, V]) :IndexGet[I Link Item, K, V] =
			new IndexGet[I Link Item, K, V] {
				override def apply(i :I Link Item, key :K) = get(i.init, key)
			}
	}




	@implicitNotFound("Can't put ${V} under ${K} in index ${I}: either the type is abstract or the result " +
	                  "does not conform to ${O}.")
	sealed abstract class IndexPut[-I <: Type, K <: Key, -V <: Value, +O <: Type] extends ((I, K, V) => O)

	@implicitNotFound("Can't set ${K} to ${V} in index ${I}: either the key is not present, the type is abstract, " +
	                  "or the result does not conform to ${O}.")
	sealed abstract class IndexSet[-I <: Type, K <: Key, -V <: Value, +O <: Type] extends IndexPut[I, K, V, O]

	@implicitNotFound("Can't add ${V} under ${K} in index ${I}: either the key already exists, the type is abstract, " +
	                  "or the result does not conform to ${O}.")
	sealed abstract class IndexAdd[-I <: Type, K <: Key, -V <: Value, +O <: Type] extends IndexPut[I, K, V, O]


	sealed abstract class AddWhenMissing {
		private[this] val add = new IndexAdd[Type, Key, Value, Type Link Item] {
			override def apply(tail :Type, key :Key, v :Value) =
				append(tail, key, v)
		}

		@inline implicit def addEntry[I <: Type, K <: Key, V <: Value]
		                     (implicit unique :UniqueKey[I, K]) :IndexAdd[I, K, V, I Link Entry[K, V]] =
			add.asInstanceOf[IndexAdd[I, K, V, I Link Entry[K, V]]]
	}

	object IndexPut extends AddWhenMissing {
		private[this] val last = new IndexSet[Type Link Item, Key, Value, Type Link Item] {

			override def apply(i :Type Link Item, key :Key, v :Value) =
				set[Type, Key, Value](i, key, v)

		}

		@inline implicit def setLast[I <: Type, K <: Key, V <: Value]
				:IndexSet[I Link Entry[K, Value], K, V, I Link Entry[K, V]] =
			last.asInstanceOf[IndexSet[I Link Entry[K, Value], K, V, I Link Entry[K, V]]]

		@inline implicit def setPrev[I <: Type, K <: Key, V <: Value, T <: Type, E <: Item]
		                    (implicit set :IndexSet[I, K, V, T]) :IndexSet[I Link E, K, V, T Link E] =
			new IndexSet[I Link E, K, V, T Link E] {
				override def apply(r :I Link E, k :K, v :V) = link(set(r.init, k, v), r.last)
			}
	}




	@implicitNotFound("Key type ${K} is present in index ${I} or the index is abstract.")
	final class UniqueKey[-I <: Type, K <: Key] private ()

	object UniqueKey {
		private[this] final val instance = new UniqueKey[@~, Key]

		implicit def uniqueInEmpty[K <: Key] :UniqueKey[@~ , K] = instance.asInstanceOf[UniqueKey[@~, K]]

		@inline implicit def uniqueIn[I <: Type, E <: Item, K <: Key](implicit unique :UniqueKey[I, K]) :UniqueKey[I Link E, K] =
			unique.asInstanceOf[UniqueKey[I Link E, K]]

		implicit def conflictWhenPresent[K <: Key] :UniqueKey[Type Link Entry[K, Value], K] =
			instance.asInstanceOf[UniqueKey[Type Link Entry[K, Value], K]]
	}





	@implicitNotFound("Type (${K}, ${V}) is not an upper bound of elements in chain ${C}")
	/** A specialized `UpperBound` implementation existing to force ''scalac'' to infer singleton types when needed. */
	final class UpperIndexBound[C <: Type, +K, +V] private[ListingFactory]() extends UpperBound[C, (K, V)]

	implicit final val EmptyIndexBound = new UpperIndexBound[@~, Nothing, Nothing]

	@inline implicit def upperIndexBound[T <: Type, HK <: Key, HV <: Value, K, V]
	                                    (implicit tail :UpperIndexBound[T, K, V], k :HK <:< K, v :HV <:< V)
			:UpperIndexBound[T Link Entry[HK, HV], K, V] =
		tail.asInstanceOf[UpperIndexBound[T Link Entry[HK, HV], K, V]]




	sealed abstract class ToValueChain[-I <: Type, +O <: Chain] {
		def apply(index :I) :O
	}

	implicit final val EmptyValueChain :ToValueChain[@~, @~] = new ToValueChain[@~, @~] {
		override def apply(index: @~) = index
	}

	@inline implicit final def ToValueChain[I <: Type, K <: Key, V <: Value, O <: Chain]
	                                       (implicit init :ToValueChain[I, O]) :ToValueChain[I Link Entry[K, V], O ~ V] =
		new ToValueChain[I Link Entry[K, V], O ~ V] {
			override def apply(index :Link[I, Entry[K, V]]) = init(index.init) ~ get(index)
		}





	//the following multitude of ChainApplication implicits apply on the values of the index, ignoring the keys
	implicit def applyValueChain[C <: Type, V <: Chain, Y](implicit vals :ToValueChain[C, V])
			:ChainApplication[C, V => Y, Y] =
		ChainApplication { (f :V => Y, xs :C) => f(vals(xs)) }



	implicit def applyValuesFunction1[A <: Value, Y] :ChainApplication[@~ Link Entry[Key, A], A => Y, Y] =
		ChainApplication { (f :A => Y, xs: @~ Link Entry[Key, A]) => f(get(xs)) }

	implicit def applyValuesFunction2[A <: Value, B <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B], (A, B) => Y, Y] =
		ChainApplication { (f :(A, B) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B]) => f(get(xs.init), get(xs)) }

	implicit def applyValuesFunction3[A <: Value, B <: Value, C <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C], (A, B, C) => Y, Y] =
		ChainApplication {
			(f :(A, B, C) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C]) =>
				val xs1 = xs.init; f(get(xs1.init), get(xs1), get(xs))
		}

	implicit def applyValuesFunction4[A <: Value, B <: Value, C <: Value, D <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D], (A, B, C, D) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D]) =>
				val xs1 = xs.init; val xs2 = xs1.init;
				f(get(xs2.init), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction5[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E], (A, B, C, D, E) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init
				f(get(xs3.init), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction6[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F], (A, B, C, D, E, F) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init
				f(get(xs4.init), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction7[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G], (A, B, C, D, E, F, G) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				f(get(xs5.init), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction8[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H], (A, B, C, D, E, F, G, H) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init
				f(get(xs6.init), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction9[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I], (A, B, C, D, E, F, G, H, I) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init
				f(get(xs7.init), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction10[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J],
				(A, B, C, D, E, F, G, H, I, J) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init
				f(get(xs8.init), get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction11[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K],
				(A, B, C, D, E, F, G, H, I, J, K) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init
				f(get(xs9.init), get(xs9),
				  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction12[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L],
				(A, B, C, D, E, F, G, H, I, J, K, L) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				f(get(xs10.init), get(xs10), get(xs9),
				  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction13[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M],
				(A, B, C, D, E, F, G, H, I, J, K, L, M) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init
				f(get(xs11.init), get(xs11), get(xs10), get(xs9),
				  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction14[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N],
				(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init
				f(get(xs12.init), get(xs12), get(xs11), get(xs10), get(xs9),
				  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction15[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O],
				(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init
				f(get(xs13.init), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
				  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction16[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P],
				(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				f(get(xs14.init), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
				  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction17[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Q <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q],
				(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init
				f(get(xs15.init), get(xs15), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
				  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction18[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Q <: Value, R <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R],
				(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Y,
			 xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init
				f(get(xs16.init), get(xs16), get(xs15), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
				  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction19[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Q <: Value, R <: Value, S <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S],
				(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Y,
			 xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init
				f(get(xs17.init), get(xs17),
				  get(xs16), get(xs15), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
				  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction20[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Q <: Value, R <: Value, S <: Value, T <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S] Link Entry[Key, T],
				(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Y,
			 xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S] Link Entry[Key, T]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				f(get(xs18.init), get(xs18), get(xs17),
				  get(xs16), get(xs15), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
				  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction21[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Q <: Value, R <: Value, S <: Value, T <: Value, U <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S] Link Entry[Key, T] Link Entry[Key, U],
				(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Y,
			 xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S] Link Entry[Key, T] Link Entry[Key, U]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				val xs19 = xs18.init
				f(get(xs19.init), get(xs19), get(xs18), get(xs17),
				  get(xs16), get(xs15), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
				  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}

	implicit def applyValuesFunction22[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Q <: Value, R <: Value, S <: Value, T <: Value, U <: Value, V <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S] Link Entry[Key, T] Link Entry[Key, U] Link Entry[Key, V],
				(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Y,
			 xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S] Link Entry[Key, T] Link Entry[Key, U] Link Entry[Key, V]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				val xs19 = xs18.init; val xs20 = xs19.init
				f(get(xs20.init), get(xs20), get(xs19), get(xs18), get(xs17),
				  get(xs16), get(xs15), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
				  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs))
		}






	implicit def applyValuesTuple1[A <: Value, Y] :ChainApplication[@~ Link Entry[Key, A], Tuple1[A] => Y, Y] =
		ChainApplication { (f :Tuple1[A] => Y, xs: @~ Link Entry[Key, A]) => f(Tuple1(get(xs))) }

	implicit def applyValuesTuple2[A <: Value, B <: Value, Y] :ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B], ((A, B)) => Y, Y] =
		ChainApplication { (f :((A, B)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B]) => f((get(xs.init), get(xs))) }

	implicit def applyValuesTuple3[A <: Value, B <: Value, C <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C], ((A, B, C)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C]) => val xs1 = xs.init; f((get(xs1.init), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple4[A <: Value, B <: Value, C <: Value, D <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D], ((A, B, C, D)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D]) =>
				val xs1 = xs.init; val xs2 = xs1.init; f((get(xs2.init), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple5[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E], ((A, B, C, D, E)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init
				f((get(xs3.init), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple6[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F], ((A, B, C, D, E, F)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init
				f((get(xs4.init), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple7[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G], ((A, B, C, D, E, F, G)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				f((get(xs5.init), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple8[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H], ((A, B, C, D, E, F, G, H)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init
				f((get(xs6.init), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple9[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I], ((A, B, C, D, E, F, G, H, I)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init
				f((get(xs7.init), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple10[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J],
				((A, B, C, D, E, F, G, H, I, J)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init
				f((get(xs8.init), get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple11[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K],
				((A, B, C, D, E, F, G, H, I, J, K)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init
				f((get(xs9.init), get(xs9),
					  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple12[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L],
				((A, B, C, D, E, F, G, H, I, J, K, L)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				f((get(xs10.init), get(xs10), get(xs9),
					  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple13[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M],
				((A, B, C, D, E, F, G, H, I, J, K, L, M)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init
				f((get(xs11.init), get(xs11), get(xs10), get(xs9),
					  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple14[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N],
				((A, B, C, D, E, F, G, H, I, J, K, L, M, N)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init
				f((get(xs12.init), get(xs12), get(xs11), get(xs10), get(xs9),
					  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple15[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O],
				((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init
				f((get(xs13.init), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
					  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple16[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P],
				((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				f((get(xs14.init), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
					  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple17[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Q <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q],
				((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)) => Y, xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init
				f((get(xs15.init), get(xs15), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
					  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple18[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Q <: Value, R <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R],
				((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)) => Y,
			 xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init
				f((get(xs16.init), get(xs16), get(xs15), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
					  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple19[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Q <: Value, R <: Value, S <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S],
				((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)) => Y,
			 xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init
				f((get(xs17.init), get(xs17),
					  get(xs16), get(xs15), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
					  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple20[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Q <: Value, R <: Value, S <: Value, T <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S] Link Entry[Key, T],
				((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)) => Y,
			 xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S] Link Entry[Key, T]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				f((get(xs18.init), get(xs18), get(xs17),
					  get(xs16), get(xs15), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
					  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple21[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Q <: Value, R <: Value, S <: Value, T <: Value, U <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S] Link Entry[Key, T] Link Entry[Key, U],
				((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)) => Y,
			 xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S] Link Entry[Key, T] Link Entry[Key, U]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				val xs19 = xs18.init
				f((get(xs19.init), get(xs19), get(xs18), get(xs17),
					  get(xs16), get(xs15), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
					  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}

	implicit def applyValuesTuple22[A <: Value, B <: Value, C <: Value, D <: Value, E <: Value, F <: Value, G <: Value, H <: Value, I <: Value, J <: Value, K <: Value, L <: Value, M <: Value, N <: Value, O <: Value, P <: Value, Q <: Value, R <: Value, S <: Value, T <: Value, U <: Value, V <: Value, Y]
			:ChainApplication[@~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S] Link Entry[Key, T] Link Entry[Key, U] Link Entry[Key, V],
				((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)) => Y, Y] =
		ChainApplication {
			(f :((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)) => Y,
			 xs: @~ Link Entry[Key, A] Link Entry[Key, B] Link Entry[Key, C] Link Entry[Key, D] Link Entry[Key, E] Link Entry[Key, F] Link Entry[Key, G] Link Entry[Key, H] Link Entry[Key, I] Link Entry[Key, J] Link Entry[Key, K] Link Entry[Key, L] Link Entry[Key, M] Link Entry[Key, N] Link Entry[Key, O] Link Entry[Key, P] Link Entry[Key, Q] Link Entry[Key, R] Link Entry[Key, S] Link Entry[Key, T] Link Entry[Key, U] Link Entry[Key, V]) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				val xs19 = xs18.init; val xs20 = xs19.init
				f((get(xs20.init), get(xs20), get(xs19), get(xs18), get(xs17),
					  get(xs16), get(xs15), get(xs14), get(xs13), get(xs12), get(xs11), get(xs10), get(xs9),
					  get(xs8), get(xs7), get(xs6), get(xs5), get(xs4), get(xs3), get(xs2), get(xs1), get(xs)))
		}


}






/** A variant of `Chain` where all elements are pairs `(K, Any)`, with `K` is an arbitrary type used as a key,
  * erased at runtime. It exists to decrease the reliance on implicit witnesses for all operations
  * and putting a static upper bound on such chains, simplifying generic operations, especially when the chain
  * is abstract. The keys are not stored in the chain, only the values. This means that in order to extract
  * the keys, their type must be known to leverage the implicit `ValueOf[K]`.
  * Note that, like `Chain`, but unlike `List`, it is left-associative, thus being built
  * 'from left to right', with the easy access to the last element rather than the first.
  * An empty `Listing` is simply the empty chain [[net.noresttherein.oldsql.collection.Chain.@~$ @~]].
  * @see [[net.noresttherein.oldsql.collection.Listing.|~]]
  * @see [[net.noresttherein.oldsql.collection.LabeledChain]]
  */
sealed trait Listing extends Chain {
	type IndexedCat[+P <: Listing] <: Listing
	protected[collection] def indexedCat[P <: Listing](prefix :P) :IndexedCat[P]
}





object Listing extends ListingFactory {

	/** The upper bound type of the first member of each tuple in the index. */
	override type Key = Any
	/** The upper bound for the second member of each tuple in the index */
	override type Value = Any
	override type Entry[+K <: Key, +V] = K :~ V
	override type Type = Listing
	override type Link[+I <: Type, +L <: Item] = I |~ L

	/** Non-singleton lowest upper bound of `Item`, or `Item` if it is not a singleton type nor does it contain one. */
	override type NonSingleton = Any :~ Value

	/** Factory method for non-empty chains of type `Type`.  */
	protected override def link[I <: Type, L <: Item](init :I, last :L) = new link[I, L](init, last)

	protected override def get[K <: Key, V <: Value](index :Listing |~ (K :~ V)) :V = index.last.value

	protected override def get[K <: Key, V <: Value](index: Listing |~ (K :~ V), key :K) :V = index.last.value

	protected override def set[I <: Listing, K <: Key, V <: Value](i :I |~ (K :~ V), key :K, v :V) :I |~ (K :~ V) =
		new link(i.init, new :~[K, V](v))

	protected override def append[I <: Listing, K <: Key, V <: Any](index :I, key :K, value :V) :I |~ (K :~ V) =
		new link(index, new :~[K, V](value))



	/** An entry of `Listing`, indexed solely on the type level by the singleton type `K` and storing only
	  * the value `V`.
	  */
	class :~[+K, +V](val value :V) extends AnyVal {
		type Key <: K
		type Value <: V

		def key[U >: K <: Singleton](implicit k :ValueOf[U]): U = k.value

		def get :V = value

		def isEmpty = false

		override def toString :String = ":~ " + value
	}



	/** An extractor for pairs being elements of a `Listing`. Aside from introducing an infix format for the tuple,
	  * it declares also an `unapply` method accepting a `Listing` itself, matching it ''iff'' it contains
	  * exactly one element. This allows to write extractors without the initial `@~`:
	  * {{{
	  *     val record = "silver" :~ "monsters |# "steel" :~ humans
	  *     val (sword1 :~ victim1 |~ sword2 :~ victim2) = record
	  * }}}
	  */
	object :~ {

		/** Returns a factory with an `apply[V](value :V)` method which creates a `K :~ V` instance.
		  * This split allows specifying explicitly only the key type, with the value type being inferred by the compiler.
		  */
		@inline def apply[K <: Key] :constructor_:~[K] = new constructor_:~[K] {}

		/** Creates an entry `K :~ V` of a `Listing`. Only the value is stored in the entry, with the key
		  * existing solely in the type signature.
		  */
		@inline def apply[K <: Key, V](key :K, value :V) :K :~ V = new :~(value)

		@inline def unapply[K <: Key :ValueOf, V](entry :K :~ V) :Option[(K, V)] =
			Some(valueOf[K] -> entry.value)


		def unapply[K <: Key :ValueOf, V](index: Listing |~ (K :~ V)) :Option[(K, V)] =
			if (index.init eq @~) Some(valueOf[K] -> index.last.value) else None

		def unapply(index :Listing) :Option[Value] = index match {
			case @~ |~ value => Some(value)
			case _ => None
		}


		/** A factory creating entries of a `Listing` with the type parameter as their key */
		trait constructor_:~[K <: Key] extends Any {
			@inline def apply[V](value :V): K :~ V = new :~(value)
		}
	}

	class method_:~[K](private val key :K) extends AnyVal {
		@inline def :~[V](value :V) :K :~ V = new :~(value)
	}



	/*** A non-empty `Listing`, consisting of another (possibly empty) `Listing` `init`, followed by
	  * the entry `last`. The entry is a value type `K :~ V` wrapping only its the value `V` - neither the key
	  * nor the wrapper itself is really stored in this instance, only the value.
	  * @tparam I the type of the chain with all elements but the last element of this type.
	  * @tparam L the type of the last element in the chain.
	  */
	sealed trait |~[+I <: Type, +L <: Item] extends (I ~ L) with Listing {

		override type IndexedCat[+P <: Listing] <: init.IndexedCat[P] |~ L

		@inline final def values[O <: Chain](implicit chain :ToValueChain[I |~ L, O]) :O =
			this.asInstanceOf[O] //last is erased to last.value, so it will work in the bytecode!

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[|~[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other: |~[_, _] if other canEqual this => other.last == last && other.init == init
			case _ => false
		}

		protected override def symbol = "|~"

	}

	protected[collection] class link[I <: Type, L <: Item](override val init :I, override val last :L)
		extends Chain.link[I, L](init, last) with |~[I, L]
	{
		override type IndexedCat[+P <: Listing] = init.IndexedCat[P] |~ L

		protected[collection] override def indexedCat[P <: Listing](prefix :P) :IndexedCat[P] =
			init.indexedCat(prefix) |~ last
	}



	/** A factory and extractor of non-empty `Listing` instances.
	  * The extractor part is designed to be used in the infix notation, following the same order as the type `|~`
	  * appears in the index's definition:
	  * {{{
	  *     "silver" :~ "monsters" |~ "steel" :~ "humans" match {
	  *         case key1 :~ value1 |~ key2 :~ value2
	  *     }
	  * }}}
	  * Note that in the above example we also use the feature of the `:~` entry extractor which matches also
	  * any singleton `Listing` (that is, any value of type `@~ |~ E`).
	  * Importing this symbol imports also implicit conversion extending all types `K` with a method `:~` for creating
	  * `K :~ V` entries, as well as a conversion extending any `K :~ V` with a `|~` method for creating two-element
	  * chains. Both of these features together allow omitting of `@~` when creating records and starting
	  * with the first element instead, as also shown in the above example.
	  */
	object |~ {
		@inline def apply[K <: Key] :constructor_|~[K] = new constructor_|~[K] {}

		@inline def apply[I <: Listing, L <: Singleton :~ Any](tail :I, head :L) :I |~ L = new link(tail, head)


		trait constructor_|~[K <: Key] extends Any {
			@inline def apply[I <: Listing, V](init :I, value :V) :I |~ (K :~ V) =
				new link[I, K :~ V](init, new :~(value))
		}



		@inline def unapply[I <: Listing, L <: Item](index :I |~ L) :I |~ L = index

		@inline def unapply(index :Listing) :Option[(Listing, Any)] = index match {
			case nonEmpty: |~[_, _] => Some(nonEmpty.init -> nonEmpty.last.value)
			case _ => None
		}
	}



	@inline implicit def |~[K <: Key, V](entry :K :~ V) :ListingOps[@~ |~ (K :~ V)] = @~ |~ entry

	@inline implicit def |~[K <: Key](key :K) :method_:~[K] = new method_:~(key)



	/** Operations on the index `I` */
	implicit class ListingOps[I <: Listing](private val self :I) extends AnyVal {

		/** Extends the index with another entry `N`, becoming the new last entry. */
		@inline def |~[N <: Key :~ Value](next :N) :I |~ N = new link(self, next)

		/** Retrieves the value of associated with the given key. The key comparison
		  * is made based on the static types, rather than values, which are not stored. For this reason only entries
		  * in the non-abstract part of type `I` (i.e., following the abstract `Listing`, if present) can
		  * be accessed this way.
		  * @tparam K the type of the key (often a literal type).
		  * @tparam V the type of the value associated with the key `K`.
		  */
		@inline def apply[K <: Key, V](key :K)(implicit get :IndexGet[I, K, V]) :V = get(self, key)

		/** Puts the given `(key, value)` pair in this index. If the key `K` is part of this index's type definition as
		  * seen in the caller's context, this will create a new index, where the entry with that key is replaced
		  * with the entry `K :~ V`, storing `value`. If the key is not present, and the index is fully instantiated
		  * (it starts with `@~` rather than `Listing`), it returns a new index with the new pair at the end.
		  */
		@inline def update[K <: Key, V, R <: Listing](key :K, value :V)(implicit put :IndexPut[I, K, V, R]) :R =
			put(self, key, value)

		/** Appends the given index to the end of this chain. */ //fixme: ambiguity with ChainOps.++
		@inline def ++[S <: Listing](suffix :S) :suffix.IndexedCat[I] = suffix.indexedCat(self)

		@inline def toMap[K, V](implicit convert :ToMap[I, K, V]) :Map[K, V] = convert(self).toMap

	}






	sealed abstract class ToMap[I <: Listing, K, +V] {
		def apply(index :I) :List[(K, V)]
	}

	implicit def emptyMap[K <: Key] :ToMap[@~, K, Nothing] = new ToMap[@~, K, Nothing] {
		def apply(monkey: @~) = Nil
	}

	implicit def toMap[I <: Listing, K <: U, L <: U :ValueOf, U <: Key, V]
	                  (bounds :ToMap[I, K, V]) :ToMap[I |~ (L :~ V), U, V]  =
		new ToMap[I |~ (L :~ V), U, V] {
			def apply(index :I |~ (L :~ V)) = (valueOf[L], index.last.value)::bounds(index.init)
		}

}







/** A variant of `Chain` where all elements are pairs `(L, Any)`, with `L` being a singleton `String` type,
  * (intended to by literal types). It exists to decrease the reliance on implicit witnesses for all operations
  * and putting a static upper bound on such chains, simplifying generic operations, especially when the chain
  * is abstract. The keys are not stored in the chain, only the values. This means that in order to extract
  * the keys, their type must be known to leverage the implicit `ValueOf[K]`.
  * Note that, like `Chain`, but unlike `List`, it is left-associative, thus being built
  * 'from left to right', with the easy access to the last element rather than the first.
  * An empty `LabeledChain` is simply the empty chain [[net.noresttherein.oldsql.collection.Chain.@~$ @~]].
  * @see [[net.noresttherein.oldsql.collection.LabeledChain.>~]]
  */
sealed trait LabeledChain extends Listing {
	type LabeledCat[+P <: LabeledChain] <: LabeledChain
	protected[collection] def labeledCat[P <: LabeledChain](prefix :P) :LabeledCat[P]
}





object LabeledChain extends ListingFactory {

	/** The upper bound type of the first member of each tuple in the index. */
	override type Key = String with Singleton
	/** The upper bound for the second member of each tuple in the index */
	override type Value = Any
	override type Entry[+K <: Key, +V] = K :~ V
	override type Type = LabeledChain
	override type Link[+I <: Type, +L <: Item] = I >~ L

	/** Non-singleton lowest upper bound of `Item`, or `Item` if it is not a singleton type nor does it contain one. */
	override type NonSingleton = String :~ Any

	/** Factory method for non-empty chains of type `Type`.  */
	protected override def link[I <: Type, L <: Item](init :I, last :L) = new link[I, L](init, last)

	protected override def get[K <: Key, V <: Value](index :LabeledChain >~ (K :~ V)) :V = index.last.value

	protected override def get[K <: Key, V <: Value](index: LabeledChain >~ (K :~ V), key :K) :V = index.last.value

	protected override def set[I <: LabeledChain, K <: Key, V <: Value](i :I >~ (K :~ V), key :K, v :V) :I >~ (K :~ V) =
		new link(i.init, new :~[K, V](v))

	protected override def append[I <: LabeledChain, K <: Key, V <: Any](index :I, key :K, value :V) :I >~ (K :~ V) =
		new link(index, new :~[K, V](value))



	/*** A non-empty `LabeledChain`, consisting of another (possibly empty) `LabeledChain` `init`, followed by
	  * the entry `last`. The entry is a value type `K :~ V` wrapping only its the value `V` - neither the key
	  * nor the wrapper itself is really stored in this instance, only the value.
	  * @tparam I the type of the chain with all elements but the last element of this type.
	  * @tparam L the type of the last element in the chain.
	  */
	trait >~[+I <: Type, +L <: Item] extends |~[I, L] with LabeledChain {
		override type LabeledCat[+P <: LabeledChain] <: init.LabeledCat[P] >~ L

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[>~[_, _]]

		protected override def symbol = ">~"
	}

	protected[collection] class link[I <: Type, L <: Item](override val init :I, override val last :L)
		extends Listing.link[I, L](init, last) with >~[I, L]
	{
		override type LabeledCat[+P <: LabeledChain] = init.LabeledCat[P] >~ L

		protected[collection] override def labeledCat[P <: LabeledChain](prefix :P) :LabeledCat[P] =
			init.labeledCat(prefix) >~ last
	}



	/** A factory and extractor of non-empty `LabeledChain` instances.
	  * The extractor part is designed to be used in the infix notation, following the same order as the type `>~`
	  * appears in the index's definition:
	  * {{{
	  *     "silver" :~ "monsters" >~ "steel" :~ "humans" match {
	  *         case key1 :~ value1 >~ key2 :~ value2
	  *     }
	  * }}}
	  * Note that in the above example we also use the feature of the `:~` entry extractor which matches also
	  * any singleton `LabeledChain` (that is, any value of type `@~ >~ E`).
	  * Importing this symbol imports also implicit conversion extending all string literal types with a method `:~`
	  * for creating index entries, as well as a conversion extending any `K :~ V` with a `>~` method for creating
	  * two-element chains. Both of these features together allow omitting of `@~` when creating records and starting
	  * with the first element instead, as also shown in the above example.
	  */
	object >~ {
		@inline def apply[K <: Key] :constructor_>~[K] = new constructor_>~[K] {}

		@inline def apply[I <: LabeledChain, L <: Key :~ Any](tail :I, head :L) :I >~ L = new link(tail, head)


		trait constructor_>~[K <: Key] extends Any {
			@inline def apply[I <: LabeledChain, V](init :I, value :V) :I >~ (K :~ V) =
				new link[I, K :~ V](init, new :~(value))
		}



		@inline def unapply[I <: LabeledChain, L <: Item](index :I >~ L) :I >~ L = index

		@inline def unapply(index :LabeledChain) :Option[(LabeledChain, Any)] = index match {
			case nonEmpty: >~[_, _] => Some(nonEmpty.init -> nonEmpty.last.value)
			case _ => None
		}
	}


	@inline implicit def >~[K <: Key, V](entry :K :~ V) :LabeledChainOps[@~ >~ (K :~ V)] = @~ >~ entry
	@inline implicit def >~[K <: Key](key :K) :method_:~[K] = new method_:~[K](key)



	/** Operations on the index `I` */
	implicit class LabeledChainOps[I <: LabeledChain](private val self :I) extends AnyVal {

		/** Extends the index with another entry `N`, becoming the new last entry. */
		@inline def >~[N <: Key :~ Value](next :N) :I >~ N = new link(self, next)

		/** Retrieves the value of associated with the given key. This assumes that the keys in this index are
		  * literal types (or at least, they where in the context in which it was created) and the key comparison
		  * is made based on the types, rather than values, which are not stored. For this reason only entries
		  * in the non-abstract part of type `I` (i.e., following the abstract `LabeledChain`, if present) can
		  * be accessed this way.
		  * @tparam K a label for the key (a string literal type in non-abstract contexts).
		  * @tparam V the type of the value associated with the key `K`.
		  */
		@inline def apply[K <: Key, V](key :K)(implicit get :IndexGet[I, K, V]) :V = get(self, key)

		/** Puts the given `(key, value)` pair in this index. If key `K` is part of this index's type definition as
		  * seen in the caller's context, this will create a new index, where the entry with that key is replaced
		  * with the entry `K :~ V`, storing `value`. If the key is not present, and the index is fully instantiated
		  * (it starts with `@~` rather than `LabeledChain`), it returns a new index with the new pair at the end.
		  * Note that, as `LabeledChain` is covariant regarding both of its type arguments, it is possible to break
		  * this method by upcasting a key type simply to `String with Singleton`,
		  * which would then match any other `K =:= String with Singleton` provided here.
		  */
		@inline def update[K <: Key, V, R <: LabeledChain](key :K, value :V)(implicit put :IndexPut[I, K, V, R]) :R =
			put(self, key, value)

		/** Appends the given index to the end of this chain. */ //fixme: ambiguity with ChainOps.++
		@inline def ++[S <: LabeledChain](suffix :S) :suffix.LabeledCat[I] = suffix.labeledCat(self)

	}

}






sealed trait ChainMapFactory extends ListingFactory {
	override type Type >: @~ <: ChainMap
	override type Key <: Singleton
	override type Entry[+K <: Key, +V <: Value] = (K, V)
	override type Link[+I <: Type, +L <: Item] <: (I &~ L) with Type


	protected override def get[K <: Key, V <: Value](index :Type Link (K, V)) :V = index.last._2
	
	protected override def get[K <: Key, V <: Value](index :Type Link (K, V), key :K) :V = {
		val entry = index.last
		if (entry._1 == key)
			entry._2
		else
			throw new IllegalArgumentException(s"Key $key matches the type, but not the value of the first key in index $entry")
	}

	override protected def set[I <: Type, K <: Key, V <: Value](index :I Link (K, V), key :K, value :V) :I Link (K, V) = {
		val entry = index.last
		if (entry._1 == key) link(index.init, (key, value))
		else
			throw new IllegalArgumentException(s"Key $key matches the type, but not the value of the key in entry $entry")
	}

	override protected def append[I <: Type, K <: Key, V <: Value](index :I, key :K, value :V) :I Link (K, V) =
		link(index, (key, value))

}






sealed trait ChainMap extends Chain {
	type MapCat[+P <: ChainMap] <: ChainMap
	protected[collection] def mapCat[P <: ChainMap](prefix :P) :MapCat[P]
}



object ChainMap extends ChainMapFactory {
	override type Type = ChainMap
	override type Link[+T <: Type, +H <: Item] = T &~ H
	override type Key = Singleton
	override type Value = Any
	override type NonSingleton = (Any, Any)
	override type Entry[+K <: Key, +V <: Value] = (K, V)

	protected override def link[T <: ChainMap, H <: (Key, Value)](tail :T, head :H) :T &~ H = new link(tail, head)


	/*** A non-empty `ChainMap`, consisting of another (possibly empty) `ChainMap` `init`, followed by
	  * the entry `last`.
	  * @tparam I the type of the chain with all elements but the last element of this type.
	  * @tparam L the type of the last element in the chain.
	  */
	sealed trait &~[+I <: ChainMap, +L <: (Key, Any)] extends ~[I, L] with ChainMap {

		@inline final def values[O <: Chain](implicit chain :ToValueChain[I &~ L, O]) :O = chain(this)

		override type MapCat[+P <: ChainMap] <: init.MapCat[P] &~ L


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[&~[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other: &~[_, _] if other canEqual this => other.last == last && other.init == init
			case _ => false
		}

		override def toString :String = {
			def entry(sb :StringBuilder, e :(Key, Any)) :StringBuilder =
				if (e._1.isInstanceOf[Chain] || e._2.isInstanceOf[Chain])
					sb append "(" append e._1 append "->" append e._2 append ")"
				else
                    sb append e._1 append "->" append e._2

			def rec(t :ChainMap, h: (Key, Any)) :StringBuilder = t match {
				case r &~ e => entry(rec(r, e) append " &~ ", h)
				case _ => entry(new StringBuilder(), h)
			}
			rec(init, last).toString
		}

	}

	protected[collection] class link[I <: Type, L <: Item](override val init :I, override val last :L)
		extends Chain.link[I, L](init, last) with &~[I, L]
	{
		override type MapCat[+P <: ChainMap] = init.MapCat[P] &~ L

		protected[collection] override def mapCat[P <: ChainMap](prefix :P) :MapCat[P] = init.mapCat(prefix) &~ last
	}



	/** A constructor and extractor of non-empty `ChainMap` implementations.
	  * The extractor part is designed to be used in the infix notation, following the same order as the type `&~`
	  * appears in the index's definition:
	  * {{{
	  *     index match {
	  *         case @~ &~ key1 -> value1 &~ key2 - value2
	  *     }
	  * }}}
	  */
	object &~ {
		@inline def apply[T <: ChainMap, H <: (Key, Any)](tail :T, head :H) :T &~ H = new link(tail, head)

		@inline def unapply[T<: ChainMap, H <: (Key, Any)](index :T &~ H) :T &~ H = index

		@inline def unapply(index :ChainMap) :Option[(ChainMap, (Key, Any))] = index match {
			case nonEmpty: &~[_, _] => Some(nonEmpty.init -> nonEmpty.last)
			case _ => None
		}
	}



	/** Implicitly extends an index of type `T` with methods requiring its static type. As this method uses the same
	  * name as the non-empty index class [[net.noresttherein.oldsql.collection.ChainMap.&~ &~]], this implicit
	  * conversion is imported automatically automatically alongside it.
	  */
	@inline implicit def &~[T <: ChainMap](index :T) :ChainMapOps[T] = new ChainMapOps(index)



	/** Operations on the index `I` */
	class ChainMapOps[I <: ChainMap](private val self :I) extends AnyVal {

		/** Extends the index with another entry `N`, becoming the new last entry. */
		@inline def &~[N <: (Key, Value)](next :N) :I &~ N = new link(self, next)

		/** Retrieves the value of associated with the given key. This assumes that the keys in this index are
		  * literal types (or at least, they where in the context in which it was created) and the key comparison
		  * is made based on the types, rather than values. As `ChainMap` is covariant regarding both
		  * of its type parameters, it is possible to break the implicit entry resolution mechanism by upcasting
		  * an entry to `Singleton` (or some `I with Singleton`), in which case `K =:= Singleton` (alternatively,
		  * the same `I with Singleton`) would resolve the first of such entries. If the key of the found entry
		  * does not equal the argument, an `IllegalArgumentException` will be thrown. Note that this might happen
		  * even the key is actually present in the index and in the fully instantiated part of this index's type
		  * definition, but following a bogus widened key.
		  * This method can't be called if the key `K` is not a part of the (known) type definition of the index `I`.
		  * @tparam K a singleton type of the key (a literal type in non-abstract contexts).
		  * @tparam V the type of the value associated with the key `K`.
		  */
		@inline def apply[K <: Key, V](key :K)(implicit get :IndexGet[I, K, V]) :V = get(self, key)

		/** Puts the given `(key, value)` pair in this index. If key `K` is part of this index's type definition as
		  * seen in the caller's context, this will create a new index, where the entry with that key is replaced
		  * with the entry `(key, value) :(K, V)`. If the key is not present, and the index is fully instantiated
		  * (it starts with `@~` rather than `ChainMap`), it returns a new index with the new pair at the end.
		  * Note that, as `ChainMap` is covariant regarding both of its type arguments, it is possible to break
		  * this method by upcasting a key type simply to `Singleton` (or `I with Singleton` for some type `I`),
		  * which would then match any other `K =:= I with Singleton` provided here. If the actual key of the entry
		  * returned by the `IndexPut` implicit parameter does not equal the key `key` provided here, an
		  * `IllegalArgumentException` will be thrown.
		  */
		@inline def update[K <: Key, V, R <: ChainMap](key :K, value :V)(implicit put :IndexPut[I, K, V, R]) :R =
			put(self, key, value)

		/** Appends the given chain to the end of this chain. */ //fixme: ambiguity with ChainOps.++
		@inline def ++[S <: ChainMap](suffix :S) :suffix.MapCat[I] = suffix.mapCat(self)

	}



}






/** A `Record` is, simply put, just a `ChainMap` where the key types are string literals (singleton types).
  *
  * @see [[net.noresttherein.oldsql.collection.Record.|#]]
  * @see [[net.noresttherein.oldsql.collection.Record.#>]]
  */
sealed trait Record extends ChainMap {
	type RecordCat[+P <: Record] <: Record
	protected[collection] def recordCat[P <: Record](prefix :P) :RecordCat[P]
}



object Record extends ChainMapFactory {
	override type Type = Record
	override type Link[+T <: Record, +H <: Item] = T |# H
	type Key = String with Singleton
	override type Value = Any
	override type NonSingleton = (String, Any)

	protected override def link[T <: Record, H <: (Key, Any)](tail :T, head :H) :T |# H = new link(tail, head)






	/** A non-empty record, consisting of the given record as its first elements, followed by the element `next`.
	  * Importing this class also imports implicit conversions which add a `#>` method to any `String`,
	  */
	sealed trait |#[+I <: Record, +E <: (Key, Any)] extends &~[I, E] with Record {

		override type RecordCat[+P <: Record] <: init.RecordCat[P] |# E


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[|#[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case other: |#[_, _] => (other eq this) || other.last == last && other.init == init
			case _ => false
		}

		override def toString :String = {
			def entry(sb :StringBuilder, e :(Key, Any)) :StringBuilder = e._2 match {
				case _ :Record => sb append e._1 append ": (" append e._2 append ")"
				case _ => sb append e._1 append ": " append e._2
			}
			def rec(t :Record, h: (Key, Any)) :StringBuilder = t match {
				case r |# e => entry(rec(r, e) append " |# ", h)
				case _ => entry(new StringBuilder(), h)
			}
			rec(init, last).toString
		}

	}

	protected[collection] class link[I <: Type, L <: Item](override val init :I, override val last :L)
		extends ChainMap.link[I, L](init, last) with |#[I, L]
	{
		override type RecordCat[+P <: Record] = init.RecordCat[P] |# L

		protected[collection] override def recordCat[P <: Record](prefix :P) :RecordCat[P] =
			init.recordCat(prefix) |# last
	}



	/** A factory and extractor of non-empty `Record` instances.
	  * The extractor part is designed to be used in the infix notation, following the same order as the type `|#`
	  * appears in the index's definition:
	  * {{{
	  *     "silver" #> "monsters" |# "steel" #> "humans" match {
	  *         case key1 #> value1 |# key2 #> value2
	  *     }
	  * }}}
	  * Note that in the above example we also use the feature of the `#>` tuple extractor which matches also
	  * any singleton `Record` (that is, any value of type `@~ |# E`).
	  * Importing this symbol imports also implicit conversion which from any valid entry
	  * (a pair `(String with Singleton, V)`) to a singleton record, and a conversion extending all string singletons
	  * with a method `#>` for creating tuples with a singleton type as the key. Both of these features together
	  * allow omitting of `@~` when creating records and starting with the first element instead, as also shown
	  * in the above example.
	  */
	object |# {

		@inline def apply[T <: Record, K <: Key, V](tail :T, head :(K, V)) :T |# (K, V) =
			new link(tail, head)

		@inline def unapply[T <: Record, E <: (Key, Any)](record :T |# E) :T |# E = record

		@inline def unapply(record :Record) :Option[(Record, (Key, Any))] = record match {
			case rec: |#[_, _] => Some(rec.init -> rec.last)
			case _ => None
		}
	}



	@inline implicit def |#[K <: Key, V](entry :(K, V)) :RecordOps[@~ |# (K, V)] = @~ |# entry

	@inline implicit def |#[K <: Key](key :K) :method_#>[K] = new method_#>(key)

	class method_#>[K <: Key](private val key :K) extends AnyVal {
		@inline def #>[V](value :V) :(K, V) = (key, value)
	}



	/** A type alias for a tuple where the first element is a string singleton type. */
	type #>[+K <: Key, V] = (K, V) //todo: rename to @>

	/** An extractor for pairs being elements of a `Record`. Aside from introducing an infix format for the tuple,
	  * it declares also an `unapply` method accepting a `Record` itself, matching it ''iff'' it contains
	  * exactly one element. This allows to write extractors without the initial `@~`:
	  * {{{
	  *     val record = "silver" #> "monsters |# "steel" #> humans
	  *     val (sword1 #> victim1 |# sword2# #> victim2) = record
	  * }}}
	  */
	object #> {

		def unapply[K <: Key, V](entry :(K, V)) :Some[(K, V)] = Some(entry)

		def unapply[K <: Key, V](record :Record |# (K, V)) :Option[(K, V)] =
			if (record.init eq @~) Some(record.last) else None

		def unapply(record :Record) :Option[(Key, Any)] = record match {
			case @~ |# entry => Some(entry)
			case _ => None
		}

	}





	implicit class RecordOps[I <: Record](private val self :I) extends AnyVal {

		/** Appends a next entry to the end of the record. */
		@inline def |#[E <: (Key, Any)](entry :E) :I |# E = new link(self, entry)

		/** Retrieves the value of associated with the given key. This assumes that the keys in this index are
		  * literal types (or at least, they where in the context in which it was created) and the key comparison
		  * is made based on the types, rather than values. As `Listing` is covariant regarding both
		  * of its type parameters, it is possible to break the implicit entry resolution mechanism by upcasting
		  * an entry to `Singleton` (or some `I with Singleton`), in which case `K =:= Singleton` (alternatively,
		  * the same `I with Singleton`) would resolve the first of such entries. If the key of the found entry
		  * does not equal the argument, an `IllegalArgumentException` will be thrown. Note that this might happen
		  * even the key is actually present in the index and in the fully instantiated part of this index's type
		  * definition, but following a bogus widened key.
		  * This method can't be called if the key `K` is not a part of the (known) type definition of the index `I`.
		  * @tparam K a singleton type of the key (a literal type in non-abstract contexts).
		  * @tparam V the type of the value associated with the key `K`.
		  */
		@inline def apply[K <: Key, V](key :K)(implicit get :IndexGet[I, K, V]) :V = get(self, key)

		/** Puts the given `(key, value)` pair in this index. If key `K` is part of this index's type definition as
		  * seen in the caller's context, this will create a new index, where the entry with that key is replaced
		  * with the entry `(key, value) :(K, V)`. If the key is not present, and the index is fully instantiated
		  * (it starts with `@~` rather than `Listing`), it returns a new index with the new pair at the end.
		  * Note that, as `Listing` is covariant regarding both of its type arguments, it is possible to break
		  * this method by upcasting a key type simply to `Singleton` (or `I with Singleton` for some type `I`),
		  * which would then match any other `K =:= I with Singleton` provided here. If the actual key of the entry
		  * returned by the `IndexPut` implicit parameter does not equal the key `key` provided here, an
		  * `IllegalArgumentException` will be thrown.
		  */
		@inline def update[K <: Key, V, R <: Record](key :K, value :V)(implicit put :IndexPut[I, K, V, R]) :R =
			put(self, key, value)

		/** Appends the given record to the end of this record. */ //fixme: ambiguity with ChainOps.++
		@inline def ++[S <: Record](suffix :S) :suffix.RecordCat[I] = suffix.recordCat(self)

	}

}
