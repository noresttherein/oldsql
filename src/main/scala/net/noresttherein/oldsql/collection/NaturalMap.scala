package net.noresttherein.oldsql.collection

//import scala.annotation.nowarn
import scala.annotation.nowarn
import scala.collection.{immutable, AbstractIterable, AbstractSet, ArrayOps, Factory, IterableOps}
import scala.collection.mutable.Builder
import scala.util.hashing.MurmurHash3

import net.noresttherein.oldsql.collection.NaturalMap.{Assoc, BaseNaturalMap, WhenNoKey}
import net.noresttherein.oldsql.collection.NaturalMap.WhenNoKey.Throw
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.morsels.generic.=>:
import net.noresttherein.oldsql.morsels.ComparableFactory
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}






/** A generic `Map`-like interface which implements a natural transformation between type functors `K` and `V`.
  * Each key is of type `K[X] forSome { type X }` and is mapped to a value of type `V[X]` for the same type argument `X`.
  * @author Marcin Mościcki
  */ //todo: rename to NatMap
//consider: making it invariant. Type inference sucks for this trait and we don't really take advantage of covariance
// much as we need casting on the key type anyway. The problem is that TypedColumn overrides extracts with a narrowed
// down value type, but ColumnMappingTemplate could without much trouble retain the signature of MappingTemplate
trait NaturalMap[K[X], +V[X]]
	extends Iterable[NaturalMap.Assoc[K, V, _]] with (K =>: V) with Equals with Serializable
{ outer =>
	def get[X](key :K[X]) :Option[V[X]]
	//todo: make getOrElse default instead of get to avoid creating an option
	def getOrElse[U[T] >: V[T], X](key :K[X], default: => U[X]) :U[X] = get(key) getOrElse default

	def apply[X](key :K[X]) :V[X]

	def contains(key :K[_]) :Boolean

	def removed(key :K[_]) :NaturalMap[K, V]

	def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NaturalMap[K, U]

	def +[U[T] >: V[T], X](key :K[X], value : U[X]) :NaturalMap[K, U] = updated(key, value)

	def +[U[T] >: V[T], X](entry :NaturalMap.Assoc[K, U, X]) :NaturalMap[K, U] = updated(entry._1, entry._2)

	def ++[U[T] >: V[T]](entries :IterableOnce[NaturalMap.Assoc[K, U, _]]) :NaturalMap[K, U]



	def withDefault[U[T] >: V[T]](whenNoKey :K =>: U) :NaturalMap[K, U] = new BaseNaturalMap[K, U] {
		override def get[X](key :K[X]) = outer.get(key)

		override def contains(key :K[_]) = outer.contains(key)

		override def removed(key :K[_]) = outer.removed(key).withDefault(whenNoKey)

		override def updated[W[T] >: U[T], X](key :K[X], value :W[X]) =
			outer.updated(key, value).withDefault(whenNoKey)

		override def ++[W[T] >: U[T]](entries :IterableOnce[Assoc[K, W, _]]) :NaturalMap[K, W] =
			(outer ++ entries).withDefault(whenNoKey)

		override def iterator = outer.iterator
		override def keySet = outer.keySet
		override def values = outer.values

		override def withDefault[S[T] >: U[T]](whenNoKey :K =>: S) =
			outer.withDefault(whenNoKey)

		protected override def default[X](key :K[X]) = whenNoKey(key)

		override def defaults :WhenNoKey[K, U] = WhenNoKey(whenNoKey)
	}


	override def iterator :Iterator[NaturalMap.Assoc[K, V, _]]

	def keySet :collection.Set[K[_]]

	def values :Iterable[V[_]]

	//todo: it's fishy that map and flatMap have the same erased signature as inherited methods from Iterable, but different in reality
	def map[A[_], B[_]](f :Assoc[K, V, _] => Assoc[A, B, _]) :NaturalMap[A, B] = {
		val res = NaturalMap.newBuilder[A, B]
		val iter = iterator
		while (iter.hasNext)
			res += f(iter.next())
		res.result()
	}

	def flatMap[A[_], B[_]](f :Assoc[K, V, _] => IterableOnce[Assoc[A, B, _]]) :NaturalMap[A, B] = {
		val res = NaturalMap.newBuilder[A, B]
		val iter = iterator
		while (iter.hasNext)
			res ++= f(iter.next())
		res.result()
	}

	override def filter(p :Assoc[K, V, _] => Boolean) :NaturalMap[K, V] = {
		val res = NaturalMap.newBuilder[K, V]
		val iter = iterator
		while (iter.hasNext) {
			val entry = iter.next()
			if (p(entry))
				res += entry
		}
		res.result()
	}

	override def partition(p :Assoc[K, V, _] => Boolean) :(NaturalMap[K, V], NaturalMap[K, V]) = {
		val yes = NaturalMap.newBuilder[K, V]
		val no  = NaturalMap.newBuilder[K, V]
		val iter = iterator
		while (iter.hasNext) {
			val entry = iter.next()
			if (p(entry)) yes += entry
			else no += entry
		}
		(yes.result(), no.result())
	}

	private[collection] implicit def defaults :WhenNoKey[K, V]

	protected[this] override def newSpecificBuilder :Builder[NaturalMap.Assoc[K, V, _], NaturalMap[K, V]] =
		NaturalMap.newBuilder[K, V]

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[NaturalMap[K @unchecked, V @unchecked]]

	override def className :String = "NaturalMap"

}






/** Brings into the implicit scope a conversion from the [[net.noresttherein.oldsql.collection.NaturalMap$ NaturalMap]]
  * object to [[scala.collection.Factory Factory]]`[`[[net.noresttherein.oldsql.collection.NaturalMap.Assoc Assoc]]`[K[?], V[?], ?], `[[net.noresttherein.oldsql.collection.NaturalMap NaturalMap]]`[K, V]]`.
  */
sealed abstract class ImplicitNaturalMapFactory

object ImplicitNaturalMapFactory {
	/** Converts `NaturalMap` companion object to a standard `Factory` from the scala collection framework, allowing
	  * its use as an argument to `to` method of any collection.
	  */
	implicit def toNaturalMapFactory[K[_], V[_]](companion :NaturalMap.type)
	                                            (implicit default :WhenNoKey[K, V] = Throw.aNoSuchElementException[K])
			:Factory[Assoc[K, V, _], NaturalMap[K, V]] =
		companion.factory
}



object NaturalMap extends ImplicitNaturalMapFactory {

	/** A single entry of a [[net.noresttherein.oldsql.collection.NaturalMap NaturalMap]],
	  * associating a value of `V[X]` with a key of `K[X]`.
	  */
	sealed trait Assoc[+K[_], +V[_], X] extends Product2[K[X], V[X]] {
		private[NaturalMap] def toMap[U[A] >: K[A]] :NaturalMap[U, V]
		private[NaturalMap] def keyHashCode :Int
	}

	object Assoc {
		def apply[K[_], V[_], X](key :K[X], value :V[X]) :Assoc[K, V, X] =
			new Singleton(key, value)

		@inline def unapply[K[_], V[_], X](assoc :Assoc[K, V, X]) :Got[(K[X], V[X])] = Got((assoc._1, assoc._2))
//		@inline def unapply[K[_], V[_], X](assoc :Assoc[K, V, X]) :Some[(K[X], V[X])] = Some((assoc._1, assoc._2))
	}

	implicit class ->:[K[_], X](private val key :K[X]) extends AnyVal {
		@inline def ->:[V[_]](value :V[X]) :Assoc[K, V, X] = Assoc(key, value)
	}

	implicit class NaturalMapExtension[K[_], V[_]](private val self :NaturalMap[K, V]) extends AnyVal {
		@inline def +[X](key :K[X], value :V[X]) :NaturalMap[K, V] = self.updated(key, value)
		@inline def +[X](entry :(K[X], V[X])) :NaturalMap[K, V] = self.updated(entry._1, entry._2)
		@inline def +[X](entry :Assoc[K, V, X]) :NaturalMap[K, V] = self.updated(entry._1, entry._2)
	}



	def apply[K[_], V[_]](entries :Assoc[K, V, _]*)
	                     (implicit default :WhenNoKey[K, V] = Throw.aNoSuchElementException[K]) :NaturalMap[K, V] =
		from(entries)

	def from[K[_], V[_]](entries :IterableOnce[Assoc[K, V, _]])
	                    (implicit default :WhenNoKey[K, V] = Throw.aNoSuchElementException[K]) :NaturalMap[K, V] =
		withDefault(entries)(default)

	def single[K[_], V[_], X](key :K[X], value :V[X])
	                         (implicit default :WhenNoKey[K, V] = Throw.aNoSuchElementException[K]) :NaturalMap[K, V] =
		withDefault(key, value)(default)

	def empty[K[_], V[_]](implicit default :WhenNoKey[K, V] = Throw.aNoSuchElementException[K]) :NaturalMap[K, V] =
		withDefault(default)


	def withDefault[K[_], V[_]](entries :Assoc[K, V, _]*)(default :WhenNoKey[K, V]) :NaturalMap[K, V] =
		withDefault(entries :IterableOnce[Assoc[K, V, _]])(default)

	def withDefault[K[_], V[_]](entries :IterableOnce[Assoc[K, V, _]])(default :WhenNoKey[K, V]) :NaturalMap[K, V] =
		entries match {
			case map :NaturalMap[K @unchecked, V @unchecked] if map.defaults == default => map
			case it :Iterable[Assoc[K, V, _]] if it.isEmpty => withDefault[K, V](default)
			case it :Iterable[Assoc[K, V, _]] if it.sizeIs == 1 =>
				val res = it.head.toMap
				if (res.defaults == default) res
				else {
					def entry[T](pair :Assoc[K, V, T]) = withDefault(pair._1, pair._2)(default)
					entry(it.head)
				}
			case it :Iterable[Assoc[K, V, _]] if it.sizeIs <= SmallNaturalMapCap =>
				new SmallNaturalMap[K, V](it.toArray)(default)

			case it :Iterator[Assoc[K, V, _]] if it.isEmpty => withDefault[K, V](default)
			case _ => (newBuilder[K, V](default) ++= entries).result()
		}

	def withDefault[K[_], V[_], X](key :K[X], value :V[X])(default :WhenNoKey[K, V]) :NaturalMap[K, V] =
		new Singleton(key, value, key.hashCode)(default)

	def withDefault[K[_], V[_]](default :WhenNoKey[K, V]) :NaturalMap[K, V] =
		if (default == Throw.aNoSuchElementException[K])
			instance.asInstanceOf[NaturalMap[K, V]]
		else
			new EmptyMap[K, V]()(default)


	private[this] final val instance = new EmptyMap[Seq, Seq]


	def newBuilder[K[_], V[_]](implicit default :WhenNoKey[K, V] = Throw.aNoSuchElementException[K])
			:Builder[Assoc[K, V, _], NaturalMap[K, V]] =
		new NaturalMapBuilder

	def factory[K[_], V[_]](implicit default :WhenNoKey[K, V] = Throw.aNoSuchElementException[K])
			:Factory[Assoc[K, V, _], NaturalMap[K, V]] =
		new ComparableFactory[Assoc[K, V, _], NaturalMap[K, V]] {
			override def factory = NaturalMap
			override def fromSpecific(it :IterableOnce[Assoc[K, V, _]]) = NaturalMap.from(it)
			override def newBuilder = NaturalMap.newBuilder
		}


	def Lazy[K[_], V[_]](entries: => IterableOnce[Assoc[K, V, _]]) :NaturalMap[K, V] =
		new LazyNaturalMap(NaturalMap(entries.iterator.toSeq :_*))

	def delayed[K[_], V[_]](map: => NaturalMap[K, V]) :NaturalMap[K, V] = new LazyNaturalMap(map)



	/** An implicit opportunistic type class used by some `NaturalMap` implementations to handle the situation
	  * of a missing key. This allows 'plugging in' behaviour of throwing a desired, more informational exception,
	  * without a need of guarding every map access against thrown default `NoSuchElementException` exceptions.
	  * This class has no implicit values in the implicit scope and they must be imported explicitly,
	  * but client code typically makes providing it optional.
	  * See the companion object for common predefined implementations.
	  */
	trait WhenNoKey[K[_], +V[_]] extends Serializable {
		def apply[X](key :K[X]) :V[X]
	}

	object WhenNoKey {
		def apply[K[_], V[_]](f :K =>: V) :WhenNoKey[K, V] = new Wrapper[K, V](f)

		private class Wrapper[K[_], V[_]](f :K =>: V) extends WhenNoKey[K, V] {
			override def apply[X](key :K[X]) = f(key)
			def defaults = f

			override def equals(that :Any) :Boolean = that match {
				case other :Wrapper[K @unchecked, V @unchecked] => (this eq other) || f == other.defaults
				case _ => false
			}
			override def hashCode = f.hashCode
		}

		type Throw[X] = Nothing

		object Throw {
			def aNoSuchElementException[K[_]] :WhenNoKey[K, Throw] =
				noSuch.asInstanceOf[WhenNoKey[K, Throw]]

			private object noSuch extends WhenNoKey[({ type K[_] = Any })#K, Throw] {
				override def apply[X](key :Any) = throw new NoSuchElementException(key.toString)
			}


			def aNoSuchComponentException[C[X] <: TypedMapping[X, O], O]
			                             (implicit mapping :MappingAt[O]) :WhenNoKey[C, Throw] =
				new ThrowNoSuchComponentException[C, O]

			private class ThrowNoSuchComponentException[C[X] <: TypedMapping[X, O], O](implicit mapping :MappingAt[O])
				extends WhenNoKey[C, Throw]
			{
				override def apply[X](key :C[X]) =
					throw new NoSuchComponentException("Mapping " + key + " is not a component of " + mapping + ".")

				def root = mapping

				override def equals(that :Any) :Boolean = that match {
					case other :ThrowNoSuchComponentException[C @unchecked, _] =>
						(this eq other) || mapping == other.root
					case _ => false
				}
				override def hashCode :Int = mapping.hashCode
			}
		}
	}



	private trait BaseNaturalMap[K[_], +V[_]] extends NaturalMap[K, V] { outer =>

		override def get[X](key :K[X]) :Option[V[X]]

		override def apply[X](key :K[X]) :V[X] = get(key) match {
			case Some(value) => value
			case _ => default(key)
		}

		override def contains(key :K[_]) :Boolean = get(key).isDefined

		override def removed(key :K[_]) :NaturalMap[K, V] =
			if (contains(key)) {
				val res = NaturalMap.newBuilder[K, V]
				for (entry <- this if entry._1 != key)
					res += entry
				res.result()
			} else
				  this

		override def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NaturalMap[K, U] =
			get(key) match {
				case Some(x) if x == value => this
				case _ => (NaturalMap.newBuilder[K, U] ++= this += Assoc(key, value)).result()
			}

		override def ++[U[T] >: V[T]](entries :IterableOnce[Assoc[K, U, _]]) :NaturalMap[K, U] =
			if (entries.iterator.isEmpty) this
			else (NaturalMap.newBuilder[K, U] ++= this ++= entries).result()

		override def keySet :collection.Set[K[_]] = new AbstractSet[K[_]] {
			override def knownSize = BaseNaturalMap.this.knownSize
			override def iterator = BaseNaturalMap.this.iterator.map(_._1)
			override def contains(elem :K[_]) = BaseNaturalMap.this.contains(elem)
			override def diff(that :collection.Set[K[_]]) =
				((Set.newBuilder[K[_]] /: BaseNaturalMap.this) { (res, entry) =>
					if (!that(entry._1)) res += entry._1
					else res
				}).result()
		}

		override def values :Iterable[V[_]] = new AbstractIterable[V[_]] {
			override def knownSize = BaseNaturalMap.this.knownSize
			override def iterator  = BaseNaturalMap.this.iterator.map(_._2)
			override def foreach[U](f :V[_] => U) :Unit = BaseNaturalMap.this.foreach(entry => f(entry._2))
		}

		protected def default[X](key :K[X]) :V[X] = defaults(key)

		implicit override def defaults :WhenNoKey[K, V] = Throw.aNoSuchElementException
	}




	private class EmptyMap[K[_], +V[_]](implicit override val defaults :WhenNoKey[K, V] = Throw.aNoSuchElementException[K])
		extends BaseNaturalMap[K, V]
	{
		override def knownSize = 0
		override def size = 0
		override def isEmpty = true

		override def head = throw new NoSuchElementException("NaturalMap().head")
		override def tail = throw new UnsupportedOperationException("NaturalMap().tail")

		override def apply[X](key :K[X]) = defaults(key)
		override def get[X](key :K[X]) :Option[V[X]] = None
		override def getOrElse[U[T] >: V[T], X](key :K[X], default : => U[X]) :U[X] = default

		override def contains(key :K[_]) :Boolean = false
		override def removed(key :K[_]) :NaturalMap[K, V] = this
		override def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NaturalMap[K, U] = new Singleton(key, value)

		override def filter(p :Assoc[K, V, _] => Boolean) :NaturalMap[K, V] = this
		override def map[A[_], B[_]](f :Assoc[K, V, _] => Assoc[A, B, _]) :NaturalMap[A, B] = NaturalMap.empty
		override def flatMap[A[_], B[_]](f :Assoc[K, V, _] => IterableOnce[Assoc[A, B, _]]) :NaturalMap[A, B] = NaturalMap.empty

		override val iterator = Iterator.empty
		override def keySet = Set.empty
		override def values = Set.empty

		override def canEqual(that :Any) :Boolean = equals(that)
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case map :NaturalMap[K @unchecked, V @unchecked] => map.isEmpty
			case _ => false
		}
		override val hashCode = MurmurHash3.mapHash(Map.empty)

		override def toString = "NaturalMap()"
	}




	/** A singleton `NaturalMap` and a map entry in one*/
	private class Singleton[K[_], +V[_], T](override val _1 :K[T], override val _2 :V[T], override val keyHashCode :Int)
	                                       (implicit override val defaults :WhenNoKey[K, V] = Throw.aNoSuchElementException[K])
		extends Assoc[K, V, T] with BaseNaturalMap[K, V]
	{
		def this(_1 :K[T], _2 :V[T]) = this(_1, _2, _1.hashCode)

		override def head :Assoc[K, V, T] = this
		override def tail :NaturalMap[K, V] = NaturalMap.empty

		override def knownSize = 1
		override def size = 1
		override def isEmpty = false

		override def apply[X](key :K[X]) :V[X] =
			if (key == _1) _2.asInstanceOf[V[X]]
			else default(key)

		override def get[X](key :K[X]) :Option[V[X]] =
			if (key == _1) Some(_2.asInstanceOf[V[X]]) else None

		override def getOrElse[U[A] >: V[A], X](key :K[X], default : => U[X]) :U[X] =
			if (key == _1) _2.asInstanceOf[V[X]] else default


		override def contains(key :K[_]) :Boolean = key == _1

		override def removed(key :K[_]) :NaturalMap[K, V] =
			if (key == _1) NaturalMap.empty[K, V] else this

		override def updated[U[A] >: V[A], X](key :K[X], value :U[X]) :NaturalMap[K, U] =
			if (key == _1)
				if (value == _2) this
				else new Singleton(key, value)
			else
				new SmallNaturalMap[K, U](Array(this, Assoc(key, value)))

		override def filter(p :Assoc[K, V, _] => Boolean) :NaturalMap[K, V] =
			if (p(this)) this else NaturalMap.empty

		override def map[A[_], B[_]](f :Assoc[K, V, _] => Assoc[A, B, _]) :NaturalMap[A, B] =
			f(this).toMap

		override def flatMap[A[_], B[_]](f :Assoc[K, V, _] => IterableOnce[Assoc[A, B, _]]) :NaturalMap[A, B] =
			NaturalMap.from(f(this))

		override def iterator :Iterator[Assoc[K, V, _]] = Iterator.single(this)

		override def keySet :Set[K[_]] = Set(_1)

		override def values :Iterable[V[_]] = Unique.single(_2)

		override def toMap[U[A] >: K[A]] :NaturalMap[U, V] = this.asInstanceOf[NaturalMap[U, V]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case pair :Assoc[K @unchecked, V @unchecked, _] =>
				_1 == pair._1 && _2 == pair._2
			case map :NaturalMap[K @unchecked, V @unchecked] if map canEqual this =>
				map.sizeIs == 1 && map.head == this
			case _ => false
		}
		override def hashCode = MurmurHash3.mapHash(Map(_1 -> _2))

		override def toString = "(" + _1 + ", " + _2 + ")"
	}




	/** Maximum size of the array-backed `SmallNaturalMap`. */
	private final val SmallNaturalMapCap = 16

	/** A `NaturalMap` backed by an array, with a size limit of `SmallNaturalMapCap`. */
	private class SmallNaturalMap[K[_], +V[_]]
	                             (private[this] val entries :Array[Assoc[K, V, _]])
	                             (implicit override val defaults :WhenNoKey[K, V] = Throw.aNoSuchElementException[K])
		extends BaseNaturalMap[K, V]
	{
		override def knownSize = entries.length

		private def indexOf(key :K[_]) :Int = indexOf(key, key.hashCode)

		private def indexOf(key :K[_], hash :Int) :Int = {
			var i = entries.length - 1
			while (i >= 0 && entries(i).keyHashCode != hash && entries(i)._1 != key)
				i -= 1
			i
		}

		override def apply[X](key :K[X]) :V[X] = indexOf(key) match {
			case -1 => defaults(key)
			case n => entries(n)._2.asInstanceOf[V[X]]
		}

		override def get[X](key :K[X]) :Option[V[X]] = indexOf(key) match {
			case -1 => None
			case n => Some(entries(n)._2.asInstanceOf[V[X]])
		}

		override def getOrElse[U[T] >: V[T], X](key :K[X], default : => U[X]) :U[X] = indexOf(key) match {
			case -1 => default
			case n => entries(n)._2.asInstanceOf[V[X]]
		}

		override def contains(key :K[_]) :Boolean = indexOf(key) >= 0

		override def removed(key :K[_]) :NaturalMap[K, V] = {
			val i = indexOf(key)
			if (i < 0)
				this
			else entries.length match {
				case 1 => NaturalMap.empty
				case 2 => (if (i == 0) entries(1) else entries(0)).toMap
				case n =>
					val res = new Array[Assoc[K, V, _]](n - 1)
					var j = 0
					while (j < i) {
						res(j) = entries(j); j += 1
					}
					j += 1
					while (j < n) {
						res(j - 1) = entries(j); j += 1
					}
					new SmallNaturalMap(res)
			}
		}

		override def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NaturalMap[K, U] =
			updated(key, value, null)

		override def +[U[T] >: V[T], X](entry :Assoc[K, U, X]) :NaturalMap[K, U] =
			updated(entry._1, entry._2, entry)

		private def updated[U[T] >: V[T], X](key :K[X], value :U[X], entry :Assoc[K, U, X]) :NaturalMap[K, U] = {
			val size = entries.length
			val hash = if (entry == null) key.hashCode else entry.keyHashCode
			val i = indexOf(key, hash)
			if (i >= 0)
				if (entries(i)._2.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef])
					this
				else {
					val res = new Array[Assoc[K, U, _]](entries.length)
					var j = 0
					while (j < i) {
						res(j) = entries(j); j += 1
					}
					res(i) = if (entry != null) entry else new Singleton[K, U, X](key, value, hash)
					j += 1
					while (j < size) {
						res(j) = entries(j); j += 1
					}
					new SmallNaturalMap[K, U](res)
				}
			else if (entries.length < SmallNaturalMapCap) {
				val res = new Array[Assoc[K, U, _]](entries.length + 1)
				System.arraycopy(entries, 0, res, 0, size)
				res(size) = if (entry != null) entry else new Singleton[K, U, X](key, value, hash)
				new SmallNaturalMap[K, U](res)
			} else {
				var res = Map.empty[K[_], U[_]]
				var i = 0
				while (i < size) {
					val e = entries(i)
					res = res.updated(e._1, e._2)
					i += 1
				}
   				new NaturalizedMap[K, U](res.updated(key, value))
			}
		}

		override def iterator = new ArrayOps(entries).iterator

		override def foreach[U](f :Assoc[K, V, _] => U) :Unit = new ArrayOps(entries).foreach(f)

		override def map[A[_], B[_]](f :Assoc[K, V, _] => Assoc[A, B, _]) :NaturalMap[A, B] =
			new SmallNaturalMap[A, B](new ArrayOps(entries).map(f))
	}




	/** Default `NaturalMap` implementation backed by a regular `Map[K[_], V[_]]`. */
	private class NaturalizedMap[K[_], +V[_]]
	                            (private val entries :Map[K[_], V[_]] = Map.empty[K[_], V[_]])
	                            (implicit override val defaults :WhenNoKey[K, V] = Throw.aNoSuchElementException[K])
		extends NaturalMap[K, V]
	{
		override def size :Int = entries.size
		override def knownSize :Int = entries.knownSize

		override def contains(key :K[_]) :Boolean = entries.contains(key)

		override def apply[X](key :K[X]) :V[X] = {
			val res = entries.getOrElse(key, null.asInstanceOf[V[_]]).asInstanceOf[V[X]]
			if (res == null) defaults(key) else res
		}

		override def get[X](key :K[X]) :Option[V[X]] = entries.get(key).asInstanceOf[Option[V[X]]]

		override def getOrElse[U[T] >: V[T], X](key :K[X], default: => U[X]) :U[X] =
			entries.getOrElse(key, default).asInstanceOf[U[X]]


		override def removed(key :K[_]) :NaturalMap[K, V] = {
			val res = entries.removed(key)
			if (res eq entries) this else new NaturalizedMap[K, V](res)
		}

		override def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NaturalMap[K, U] = {
			val res = entries.updated[U[_]](key, value)
			if (res eq entries) this else new NaturalizedMap[K, U](res)
		}

		override def ++[U[T] >: V[T]](entries :IterableOnce[NaturalMap.Assoc[K, U, _]]) :NaturalMap[K, U] =
			entries match {
				case _ if entries.iterator.isEmpty => this
				case map :NaturalizedMap[K @unchecked, U @unchecked] =>
					val res = this.entries ++ map.entries
					if (res eq this.entries) this
					else if (res eq map.entries) map
					else new NaturalizedMap[K, U](res)
				case _ =>
					val res = ((this.entries :Map[K[_], U[_]]) /: entries) {
						(acc, entry) => acc.updated(entry._1, entry._2)
					}
					if (res eq this.entries) this
					else new NaturalizedMap[K, U](res)
			}

		override def iterator :Iterator[NaturalMap.Assoc[K, V, _]] = entries.iterator.map {
			e => Assoc(e._1.asInstanceOf[K[Any]], e._2.asInstanceOf[V[Any]])
		}

		override def keySet :Set[K[_]] = entries.keySet

		override def values :Iterable[V[_]] = entries.values


		override def withDefault[U[T] >: V[T]](default :K =>: U) :NaturalMap[K, U] =
			new NaturalizedMap[K, U](entries)(WhenNoKey(default))

		override def equals(that :Any) :Boolean = that match {
			case pair :Assoc[K, V, _] @unchecked => pair == this : @nowarn
			case other :NaturalizedMap[K @unchecked, V @unchecked] => entries == other.entries
			case other :NaturalMap[K @unchecked, V @unchecked] if other canEqual this =>
				size == other.size && keySet == other.keySet && keySet.forall { k => this(k) == other(k) }
			case _ => false
		}
		override def hashCode :Int = entries.hashCode
	}




	private class LazyNaturalMap[K[_], +V[_]](construct: => NaturalMap[K, V]) extends BaseNaturalMap[K, V] {
		@volatile private[this] var initializer = () => construct
		@volatile private[this] var backing :NaturalMap[K, V] = _
		private[this] var cache :NaturalMap[K, V] = _ //fixme: replace with lazy val

		private[this] def target :NaturalMap[K, V] = {
			var res = cache
			if (res == null) {
				res = backing
				if (res == null) synchronized {
					res = backing
					if (res == null) {
						val init = initializer
						if (init == null)
							throw new NullPointerException(
								"NaturalMap.later: null initializer or null returned from the initializer.")
						res = init()
						backing = res
						initializer = null
					}
				}
			}
			cache = res
			res
		}

		override def size :Int = target.size

		override def get[X](key :K[X]) :Option[V[X]] = target.get(key)
		override def apply[X](key :K[X]) :V[X] = target(key)
		override def contains(key :K[_]) :Boolean = target.contains(key)

		override def removed(key :K[_]) :NaturalMap[K, V] =
			if (cache != null)
				cache.removed(key)
			else {
				val map = backing
				if (map != null) {
					cache = map
					map.removed(key)
				} else
					new LazyNaturalMap(target.removed(key))
			}

		override def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NaturalMap[K, U] =
			if (cache != null)
				cache.updated(key, value)
			else {
				val map = backing
				if (map != null) {
					cache = map
					map.updated(key, value)
				} else
					new LazyNaturalMap(target.updated(key, value))
			}

		override def ++[U[T] >: V[T]](entries :IterableOnce[Assoc[K, U, _]]) :NaturalMap[K, U] =
			if (cache != null)
				cache ++ entries
			else {
				val map = backing
				if (map != null) {
					cache = map
					map ++ entries
				} else
					new LazyNaturalMap(target ++ entries)
			}


		override def map[A[_], B[_]](f :Assoc[K, V, _] => Assoc[A, B, _]) :NaturalMap[A, B] =
			if (cache != null)
				cache.map(f)
			else {
				val map = backing
				if (map != null) {
					cache = map
					map.map(f)
				} else
                    new LazyNaturalMap(target.map(f))
			}

		override def flatMap[A[_], B[_]](f :Assoc[K, V, _] => IterableOnce[Assoc[A, B, _]]) :NaturalMap[A, B] =
			if (cache != null)
				cache.flatMap(f)
			else {
				val map = backing
				if (map != null) {
					cache = map
					map.flatMap(f)
				} else
					new LazyNaturalMap(target flatMap f)
			}


		override def filter(p :Assoc[K, V, _] => Boolean) :NaturalMap[K, V] =
			if (cache != null)
				cache filter p
			else {
				val map = backing
				if (map != null) {
					cache = map
					map filter p
				} else
					new LazyNaturalMap(target filter p)
			}

		override def partition(p :Assoc[K, V, _] => Boolean) :(NaturalMap[K, V], NaturalMap[K, V]) =
			if (cache != null)
				cache partition p
			else {
				val map = backing
				if (map != null) {
					cache = map
					map partition p
				} else {
					lazy val (yes, no) = target partition p
					(new LazyNaturalMap(yes), new LazyNaturalMap(no))
				}
			}


		override def iterator :Iterator[Assoc[K, V, _]] = target.iterator
		override def keySet :collection.Set[K[_]] = target.keySet
		override def values :Iterable[V[_]] = target.values

		override def defaults = target.defaults
	}




	/** Default builder for `NaturalMap`. Starts with filling a small array for use with a `SmallNaturalMap`,
	  * but if the number of elements exceeds `SmallNaturalCap`, switches to building a regular `Map[K[_], V[_]]`
	  * for wrapping in a `NaturalizedMap`.
	  */
	private class NaturalMapBuilder[K[_], V[_]](implicit default :WhenNoKey[K, V] = Throw.aNoSuchElementException)
		extends Builder[Assoc[K, V, _], NaturalMap[K, V]]
	{
		private[this] var small :Array[Assoc[K, V, _]] = _ //new Array[Assoc[K, V, _]](SmallNaturalMapCap)
		private[this] var large :Map[K[_], V[_]] = _
		private[this] var size = 0 //-1: large != null

		override def sizeHint(size :Int) :Unit =
			if (this.size == 0 & size > 0) {
				if (this.size <= SmallNaturalMapCap)
					small = new Array[Assoc[K, V, _]](size)
				else {
					large = Map.empty
					this.size = -1
				}
			}

		override def addOne(elem :Assoc[K, V, _]) :this.type = size match {
			case -1 =>
				large = large.updated(elem._1, elem._2)
				this
			case 0 =>
				if (small == null)
					small = new Array[Assoc[K, V, _]](SmallNaturalMapCap)
				small(0) = elem
				size = 1
				this
			case _ =>
				var i = size - 1
				while (i >= 0 && { val e = small(i); e.keyHashCode != elem.keyHashCode && e._1 != elem._1 })
					i -= 1
				if (i >= 0)
					small(i) = elem
				else if (size < small.length) {
					small(size) = elem
					size += 1
				} else {
					large = Map.empty
					i = 0
					while (i < size) {
						val e = small(i)
						large = large.updated(e._1, e._2)
						i += 1
					}
					large = large.updated(elem._1, elem._2)
					size = -1
					small = null
				}
				this
		}


		override def result() :NaturalMap[K, V] = {
			val res =
				if (large != null)
					new NaturalizedMap[K, V](large)
				else if (small != null)
					if (size == small.length)
						new SmallNaturalMap[K, V](small)
					else
						new SmallNaturalMap[K, V](Array.copyOf(small, size))
				else empty[K, V]
			clear()
			res
		}

		override def clear() :Unit = { small = null; large = null; size = 0 }
	}



/*
	private class NaturalizedMapBuilder[K[_], V[_],
		                                G[X, +Y] <: Map[X, Y] with immutable.MapOps[X, Y, G, G[X, Y]],
		                                M <: G[K[_], V[_]] with immutable.MapOps[K[_], V[_], G, M]]
	                                   (private[this] var entries :G[K[_], V[_]])
	                                   (implicit default :WhenNoKey[K, V] = Throw.aNoSuchElementException[K])
		extends Builder[Assoc[K, V, _], NaturalMap[K, V]]
	{
		override def clear() :Unit = entries = (entries :IterableOps[(K[_], V[_]), Iterable, G[K[_], V[_]]]).empty

		override def result() :NaturalMap[K, V] = entries.size match {
			case 0 => empty[K, V]
			case 1 => val elem = entries.head; single(elem._1.asInstanceOf[K[Any]], elem._2.asInstanceOf[V[Any]])
			new NaturalizedMap[K, V](entries)
		}

		override def addOne(elem :Assoc[K, V, _]) :this.type = { entries = entries.updated(elem._1, elem._2); this }
	}
*/

}





