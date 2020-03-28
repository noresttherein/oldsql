package net.noresttherein.oldsql.collection

import net.noresttherein
import net.noresttherein.oldsql
import net.noresttherein.oldsql.collection
import net.noresttherein.oldsql.collection.Chain.{@~, ~, UpperBound}
import net.noresttherein.oldsql.collection.TypeIndex.{&~, IndexGet, IndexSet}
import net.noresttherein.oldsql.morsels.generic.{Const, GenericFun, Self}
import net.noresttherein.oldsql.morsels.LUB

import scala.annotation.{implicitNotFound, tailrec}


/**
  * @author Marcin Mościcki
  */
sealed trait Chain extends Serializable {
	def isEmpty :Boolean = true
}



object Chain extends ChainFactory {
	override type Type = Chain
	override type Item = Any
	override type NonSingleton = Any
	override type Link[+T <: Chain, +H] = T ~ H

	override def link[T <: Chain, H <: Any](tail :T, head :H) :T ~ H = new ~(tail, head)



	sealed case class ~[+T <: Chain, +H](tail :T, head :H) extends Chain {
		def get :(T, H) = tail -> head

		override def isEmpty = false

		override def toString :String = {
			def entry(sb :StringBuilder, e :Any) :StringBuilder = e match {
				case _ : Chain => sb append "(" append e append ")"
				case _ => sb append e
			}
			def rec(chain :Chain) :StringBuilder = chain match {
				case t ~ h => entry(rec(t) append "~", h)
				case _ => new StringBuilder append chain
			}
			rec(this).toString
		}
	}

	sealed class @~ private[Chain] extends Record

	case object @~ extends @~



	@inline implicit class ChainOps[C <: Chain](private val self :C) extends AnyVal {
		@inline def ~[H](head :H) :C ~ H = new ~(self, head)

		@inline def map[XF[T], YF[T], Y <: Chain](f :GenericFun[XF, YF])(implicit result :MapChain[XF, C, YF, Y]) :Y =
			result(f)(self)

		def foreach(f :GenericFun[Self, Const[Unit]#T]) :Unit = {
			def rec(chain :Chain) :Unit = chain match {
				case t ~ h => rec(t); f(h)
				case _ => ()
			}
			rec(self)
		}


		@inline def toSeq[U](implicit ub :UpperBound[C, U]) :Seq[U] = toList

		def toList[U](implicit ub :UpperBound[C, U]) :List[U] = {
			@tailrec def rec(chain :Chain, res :List[U] = Nil) :List[U] = chain match {
				case t ~ h => rec(t, h.asInstanceOf[U]::res)
				case _ => res
			}
			rec(self)
		}
	}












	sealed abstract class MapChain[XF[T], X <: Chain, YF[T], Y <: Chain] {
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
					mapTail(f)(x.tail) ~ f(x.head)
			}
	}



	@implicitNotFound("Type ${U} is not an upper bound of elements in chain ${C}")
	sealed class UpperBound[C <: Chain, +U] private[collection] ()

	implicit final val EmptyChainBound = new UpperBound[@~, Nothing]

	@inline implicit def upperBound[T <: Chain, H, U, X](implicit tail :UpperBound[T, U], lub :LUB[H, U, X]) :UpperBound[T ~ H, X] =
		tail.asInstanceOf[UpperBound[T ~ H, X]]


}






sealed trait ChainFactoryBase {
	type Type >: @~ <: Chain
	type Item
	type NonSingleton >: Item
	type Link[+T <: Type, +H <: Item] <: (T ~ H) with Type



	def link[T <: Type, H <: Item](tail :T, head :H) :T Link H

	private[this] final val noBound = new UpperBound[Type, Item]
	implicit def noUpperBound[C <: Type, I >: Item] :UpperBound[C, I] = noBound.asInstanceOf[UpperBound[C, I]]
//	implicit final val noUpperBound = new UpperBound[Type, Item]
}



trait ChainFactory extends ChainFactoryBase {
	private[this] final val noBound = new UpperBound[Type, NonSingleton]
	implicit def nonSingletonUpperBound[C <: Type] :UpperBound[C, NonSingleton] = noBound.asInstanceOf[UpperBound[C, NonSingleton]]
//	implicit final val nonSingletonUpperBound = new UpperBound[Type, NonSingleton]
}






sealed abstract class TypeIndexFactory extends ChainFactory {
	type Type >: @~ <: TypeIndex
	type Link[+T <: Type, +H <: Item] <: (T &~ H) with Type
	type Key <: Singleton
	type Value
	type Item = (Key, Value)



	@implicitNotFound("Type ${K} is not a key in index ${I} (or is not mapped to type ${V}).")
	sealed abstract class IndexGet[-I <: Type, K <: Key, +V <: Value] extends (I => V)

	object IndexGet {
		private[this] val last = new IndexGet[Type Link Item, Key, Value] {
			override def apply(index :Type Link Item) = index.head._2
		}

		implicit def getLast[K <: Key, V <: Value] :IndexGet[Type Link (K, V), K, V] =
			last.asInstanceOf[IndexGet[Type Link (K, V), K, V]]

		@inline implicit def getPrev[I <: Type, K <: Key, V <: Value]
		                    (implicit get :IndexGet[I, K, V]) :IndexGet[I Link Item, K, V] =
			new IndexGet[I Link Item, K, V] {
				override def apply(i :I Link Item) = get(i.tail)
			}
	}






	@implicitNotFound("Can't put ${V} under ${K} in index ${I}: either the type is abstract or the result does not conform to ${O}")
	sealed abstract class IndexPut[-I <: Type, K <: Key, -V <: Value, +O <: Type] extends ((I, V) => O)

	@implicitNotFound("Can't set ${K} to ${V} in index ${I}: either the key is not present, the type is abstract, or the result does not conform to ${O}")
	sealed abstract class IndexSet[-I <: Type, K <: Key, -V <: Value, +O <: Type] extends IndexPut[I, K, V, O]

	@implicitNotFound("Can't add ${V} under ${K} in index ${I}: either the key already exists, the type is abstract, or the result does not conform to ${O}")
	sealed abstract class IndexAdd[-I <: Type, K <: Key, -V <: Value, +O <: Type] extends IndexPut[I, K, V, O]

	sealed abstract class AddWhenMissing {
		@inline implicit def addEntry[I <: Type, K <: Key :ValueOf, V <: Value]
		                     (implicit unique :UniqueKey[I, K]) :IndexAdd[I, K, V, I Link (K, V)] =
			new IndexAdd[I, K, V, I Link (K, V)] {
				override def apply(tail :I, v :V) = link(tail, (valueOf[K], v))
			}
	}

	object IndexPut extends AddWhenMissing {
		@inline implicit def setLast[I <: Type, K <: Key :ValueOf, V <: Value] :IndexSet[I Link (K, Value), K, V, I Link (K, V)] =
			new IndexSet[I Link (K, Value), K, V, I Link (K, V)] {
				override def apply(i :I Link (K, Value), v :V) = link(i.tail, (valueOf[K], v))
			}

		@inline implicit def setPrev[I <: Type, K <: Key, V <: Value, T <: Type, E <: Item]
		                    (implicit set :IndexSet[I, K, V, T]) :IndexSet[I Link E, K, V, T Link E] =
			new IndexSet[I Link E, K, V, T Link E] {
				override def apply(r :I Link E, v :V) = link(set(r.tail, v), r.head)
			}
	}






	@implicitNotFound("Key type ${K} is present in index ${I} or the index is abstract.")
	final class UniqueKey[-I <: Type, K <: Key] private ()

	object UniqueKey {
		private[this] final val instance = new UniqueKey[@~, Key]

		implicit def uniqueInEmpty[K <: Key] :UniqueKey[@~ , K] = instance.asInstanceOf[UniqueKey[@~, K]]

		@inline implicit def uniqueIn[I <: Type, E <: Item, K <: Key](implicit unique :UniqueKey[I, K]) :UniqueKey[I Link E, K] =
			unique.asInstanceOf[UniqueKey[I Link E, K]]

		implicit def conflictWhenPresent[K <: Key] :UniqueKey[Type Link (K, Value), K] =
			instance.asInstanceOf[UniqueKey[Type Link (K, Value), K]]
	}





	@implicitNotFound("Type (${K}, ${V}) is not an upper bound of elements in chain ${C}")
	final class UpperIndexBound[C <: Type, +K, +V] private[TypeIndexFactory] () extends UpperBound[C, (K, V)]

	implicit final val EmptyIndexBound = new UpperIndexBound[@~, Nothing, Nothing]

	@inline implicit def upperIndexBound[T <: Type, HK <: Key, HV <: Value, K, V]
	                                    (implicit tail :UpperIndexBound[T, K, V], k :HK <:< K, v :HV <:< V)
			:UpperIndexBound[T Link (HK, HV), K, V] =
		tail.asInstanceOf[UpperIndexBound[T Link (HK, HV), K, V]]

}






sealed trait TypeIndex extends Chain



object TypeIndex extends TypeIndexFactory {
	override type Type = TypeIndex
	override type Link[+T <: Type, +H <: Item] = T &~ H
	override type Key = Singleton
	override type Value = Any
	override type NonSingleton = (Any, Any)

	override def link[T <: TypeIndex, H <: (Key, Value)](tail :T, head :H) :T &~ H = new &~(tail, head)






	class &~[+T <: TypeIndex, +H <: (Singleton, Any)](tail :T, head :H) extends ~[T, H](tail, head) with TypeIndex {

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[&~[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other: &~[_, _] if other canEqual this => other.head == head && other.tail == tail
			case _ => false
		}

		override def toString :String = {
			def entry(sb :StringBuilder, e :(Singleton, Any)) :StringBuilder =
				if (e._1.isInstanceOf[Chain] || e._2.isInstanceOf[Chain])
					sb append "(" append e._1 append "->" append e._2 append ")"
				else
                    sb append e._1 append "->" append e._2

			def rec(t :TypeIndex, h: (Singleton, Any)) :StringBuilder = t match {
				case r &~ e => entry(rec(r, e) append " &~ ", h)
				case _ => entry(new StringBuilder(), h)
			}
			rec(tail, head).toString
		}

	}



	object &~ {
		@inline def apply[T <: TypeIndex, H <: (Singleton, Any)](tail :T, head :H) :T &~ H = new &~(tail, head)

		@inline def unapply[T<: TypeIndex, H <: (Singleton, Any)](index :T &~ H) :T &~ H = index

		@inline def unapply(index :TypeIndex) :Option[(TypeIndex, (Singleton, Any))] = index match {
			case nonEmpty: &~[_, _] => Some(nonEmpty.tail -> nonEmpty.head)
			case _ => None
		}
	}



	@inline implicit def &~[T <: TypeIndex](index :T) :TypeIndexOps[T] = new TypeIndexOps(index)



	class TypeIndexOps[T <: TypeIndex](private val self :T) extends AnyVal {
		@inline def &~[H <: (Singleton, Any)](hd :H) :T &~ H = new &~(self, hd)

		@inline def apply[K <: Singleton, V](key :K)(implicit get :IndexGet[T, K, V]) :V = get(self)

		@inline def update[K <: Singleton, V, I <: TypeIndex](key :K, value :V)(implicit put :IndexPut[T, K, V, I]) :I =
			put(self, value)

		@inline def toMap[K, V](implicit ub :UpperBound[T, (K, V)]) :Map[K, V] =
			self.toSeq[(K, V)].toMap

	}



}






sealed trait Record extends TypeIndex



object Record extends TypeIndexFactory {
	override type Type = Record
	override type Link[+T <: Record, +H <: Item] = T |# H
	type Key = String with Singleton
	override type Value = Any
	override type NonSingleton = (String, Any)

	override def link[T <: Record, H <: (Key, Any)](tail :T, head :H) :T |# H = new |#(tail, head)






	final class |#[+T <: Record, +E <: (Key, Any)](tail :T, head :E) extends &~[T, E](tail, head) with Record {

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[|#[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case other: |#[_, _] => (other eq this) || other.head == head && other.tail == tail
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
			rec(tail, head).toString
		}

	}



	object |# {

		@inline def apply[T <: Record, K <: Key, V](tail :T, head :(K, V)) :T |# (K, V) =
			new |#(tail, head)

		@inline def unapply[T <: Record, E <: (Key, Any)](record :T |# E) :T |# E = record

		@inline def unapply(record :Record) :Option[(Record, (Key, Any))] = record match {
			case rec: |#[_, _] => Some(rec.tail -> rec.head)
			case _ => None
		}
	}



	@inline implicit def |#[K <: Key, V](entry :(K, V)) :RecordOps[@~ |# (K, V)] = @~ |# entry

	@inline implicit def |#[K <: Key](key :K) :method_#>[K] = new method_#>(key)

	class method_#>[K <: Key](private val key :K) extends AnyVal {
		@inline def #>[V](value :V) :(K, V) = (key, value)
	}



	type #>[+K <: Key, V] = (K, V)

	object #> {

		def unapply[K <: Key, V](entry :(K, V)) :Some[(K, V)] = Some(entry)

		def unapply[T <: Record, K <: Key, V](record :T |# (K, V)) :Option[(K, V)] =
			if (record.tail eq @~) Some(record.head) else None

		def unapply(record :Record) :Option[(Key, Any)] = record match {
			case @~ |# entry => Some(entry)
			case _ => None
		}

	}





	implicit class RecordOps[T <: Record](private val self :T) extends AnyVal {
		@inline def |#[E <: (Key, Any)](entry :E) :T |# E = new |#(self, entry)

		@inline def apply[K <: Key, X](key :K)(implicit get :IndexGet[T, K, X]) :X = get(self)

		@inline def update[K <: Key, X, R <: Record](key :K, value :X)(implicit put :IndexPut[T, K, X, R]) :R =
			put(self, value)

		@inline def toMap[K, V](implicit ub :UpperBound[T, (K, V)]) :Map[K, V] =
			self.toSeq[(K, V)].toMap

	}

/*
	final class :->[+K <: Key, +V](val key :K, val value :V) {
		def isEmpty = false
		def get :(K, V) = key -> value

//		def |~[S <: Key, X](other :S :-> X): @~ |~ (K :-> V) |~ (S :-> X) = @~ |~ this |~ other
//		def |~

		override def equals(that :Any) :Boolean = that match {
			case other: :->[_, _] => (other eq this) || other.key == key && other.value == value
			case _ => false
		}

		override def hashCode :Int = key.hashCode * 31 + value.hashCode

		override def toString :String = value match {
			case _ :Record => key.toString + " :-> (" + value + ")"
			case _ => key.toString + " :-> " + value
		}
	}




	object :-> {
		@inline def apply[K <: Key, V](key :K, value :V) :K :-> V = new :->(key, value)

		@inline def unapply[K <: Key, V](entry :K :-> V) :K :-> V = entry

		@inline implicit def fromTuple2[K <: Key, V](entry :(K, V)) :K :-> V =
			new :->(entry._1, entry._2)

		@inline implicit def toTuple2[K <: Key, V](entry :K :-> V)  :(K, V) =
			entry.key -> entry.value

	}

	@inline implicit def :->[K <: Key](key :K) :method_:->[K] = new method_:->(key)

	class method_:->[K <: Key](private val key :K) extends AnyVal {
		def :->[V](value :V) :K :-> V = new :->(key, value)
	}
*/

}
