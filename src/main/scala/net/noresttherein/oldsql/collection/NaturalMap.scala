package net.noresttherein.oldsql.collection
import net.noresttherein.oldsql.collection.NaturalMap.{Assoc, BaseNaturalMap}
import net.noresttherein.oldsql.morsels.generic.{=#>, GenericFun}

import scala.collection.mutable.Builder
import scala.collection.{immutable, IterableOps, MapOps}



/** A generic `Map`-like interface which implements a natural transformation between type functors `K` and `V`.
  * Each key is of type `K[X] forSome { type X }` and is mapped to a value of type `V[X]` for the same type argument `X`.
  * @author Marcin Mościcki
  */
trait NaturalMap[K[X], +V[X]] extends Iterable[NaturalMap.Assoc[K, V, _]] with (K =#> V) { outer =>
	def get[X](key :K[X]) :Option[V[X]]

	def getOrElse[U[T] >: V[T], X](key :K[X], default: => U[X]) :U[X] = get(key) getOrElse default

	def apply[X](key :K[X]) :V[X]

	def contains(key :K[_]) :Boolean

	def removed(key :K[_]) :NaturalMap[K, V]

	def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NaturalMap[K, U]

	def +[U[T] >: V[T], X](key :K[X], value : U[X]) :NaturalMap[K, U] = updated(key, value)

	def +[U[T] >: V[T], X](entry :NaturalMap.Assoc[K, U, X]) :NaturalMap[K, U] = updated(entry._1, entry._2)

	def ++[U[T] >: V[T]](entries :IterableOnce[NaturalMap.Assoc[K, U, _]]) :NaturalMap[K, U]



	def withDefault[U[T] >: V[T]](whenNoKey :K =#> U) :NaturalMap[K, U] = new BaseNaturalMap[K, U] {
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

		override def withDefault[S[T] >: U[T]](whenNoKey :K =#> S) =
			outer.withDefault(whenNoKey)

		protected override def default[X](key :K[X]) = whenNoKey(key)

	}

//	protected def default[X](key :K[X]) :V[X] =
//		throw new NoSuchElementException("No value for key " + key)

	override def iterator :Iterator[NaturalMap.Assoc[K, V, _]]

	def keySet :collection.Set[K[_]]

	def values :Iterable[V[_]]

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

	override def filter(f :Assoc[K, V, _] => Boolean) :NaturalMap[K, V] = {
		val res = NaturalMap.newBuilder[K, V]
		val iter = iterator
		while (iter.hasNext) {
			val entry = iter.next()
			if (f(entry))
				res += entry
		}
		res.result()
	}


	protected[this] override def newSpecificBuilder :Builder[NaturalMap.Assoc[K, V, _], NaturalMap[K, V]] =
		NaturalMap.newBuilder[K, V]

	override def className :String = "NaturalMap"

}






object NaturalMap {

	case class Assoc[+K[_], +V[_], X](_1 :K[X], _2 :V[X]) extends Product2[K[X], V[X]] {
		@inline def key :K[X] = _1
		@inline def value :V[X] = _2

		override def toString :String = String.valueOf(_1) + "->" + _2
	}


	implicit class -#>[K[_], X](private val key :K[X]) extends AnyVal {
		@inline def -#>[V[_]](value :V[X]) :Assoc[K, V, X] = new Assoc(key, value)
	}


	implicit class NaturalMapMethods[K[_], V[_]](private val self :NaturalMap[K, V]) extends AnyVal {
		@inline def +[X](key :K[X], value :V[X]) :NaturalMap[K, V] = self.updated(key, value)
		@inline def +[X](entry :(K[X], V[X])) :NaturalMap[K, V] = self.updated(entry._1, entry._2)
		@inline def +[X](entry :Assoc[K, V, X]) :NaturalMap[K, V] = self.updated(entry._1, entry._2)
	}




	def apply[K[_], V[_]](entries :Assoc[K, V, _]*) :NaturalMap[K, V] = (newBuilder[K, V] ++= entries).result()

	def single[K[_], V[_], X](key :K[X], value :V[X]) :NaturalMap[K, V] = new Singleton(key, value)

	def empty[K[_], V[_]] :NaturalMap[K, V] = instance.asInstanceOf[NaturalMap[K, V]]

	private[this] final val instance = new NaturalizedMap[Seq, Seq](Map.empty[Seq[_], Seq[_]])


	def newBuilder[K[_], V[_]] :Builder[Assoc[K, V, _], NaturalMap[K, V]] =
		new NaturalMapBuilder[K, V, Map, Map[K[_], V[_]]](Map[K[_], V[_]]())



	def Lazy[K[_], V[_]](entries: => IterableOnce[Assoc[K, V, _]]) :NaturalMap[K, V] =
		new LazyNaturalMap(NaturalMap(entries.iterator.toSeq :_*))

	def delayed[K[_], V[_]](map: => NaturalMap[K, V]) :NaturalMap[K, V] = new LazyNaturalMap(map)






	trait BaseNaturalMap[K[_], +V[_]] extends NaturalMap[K, V] { outer =>

		override def get[X](key :K[X]) :Option[V[X]]

		override def apply[X](key :K[X]) :V[X] = get(key) match {
			case Some(value) => value
			case _ => default(key)
		}

		override def contains(key :K[_]) :Boolean = get(key).isDefined



		override def removed(key :K[_]) :NaturalMap[K, V] =
			if (contains(key)) {
				val res = NaturalMap.newBuilder[K, V]
				for (entry <- this if entry.key != key)
					res += entry
				res.result()
			} else
				  this

		override def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NaturalMap[K, U] =
			(NaturalMap.newBuilder[K, U] ++= this += Assoc(key, value)).result()

		override def ++[U[T] >: V[T]](entries :IterableOnce[Assoc[K, U, _]]) :NaturalMap[K, U] =
			if (entries.iterator.isEmpty) this
			else (NaturalMap.newBuilder[K, U] ++= this ++= entries).result()



		protected def default[X](key :K[X]) :V[X] =
			throw new NoSuchElementException(key.toString)
	}






	private class LazyNaturalMap[K[_], +V[_]](construct: => NaturalMap[K, V]) extends BaseNaturalMap[K, V] {
		@volatile private[this] var initializer = () => construct
		@volatile private[this] var backing :NaturalMap[K, V] = _
		private[this] var cache :NaturalMap[K, V] = _

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


		override def filter(f :Assoc[K, V, _] => Boolean) :NaturalMap[K, V] =
			if (cache != null)
				cache.filter(f)
			else {
				val map = backing
				if (map != null) {
					cache = map
					map filter f
				} else
					new LazyNaturalMap(target filter f)
			}


		override def iterator :Iterator[Assoc[K, V, _]] = target.iterator

		override def keySet :collection.Set[K[_]] = target.keySet

		override def values :Iterable[V[_]] = target.values
	}






	private class Singleton[K[_], +V[_], T](key :K[T], value :V[T])
		extends Assoc[K, V, T](key, value) with BaseNaturalMap[K, V]
	{
		override def head :Assoc[K, V, T] = this
		override def tail :NaturalMap[K, V] = NaturalMap.empty

		override def size = 1

		override def apply[X](key :K[X]) :V[X] =
			if (key == _1) value.asInstanceOf[V[X]]
			else default(key)

		override def get[X](key :K[X]) :Option[V[X]] =
			if (key == _1) Some(_2.asInstanceOf[V[X]]) else None

		override def contains(key :K[_]) :Boolean = key == _1

		override def removed(key :K[_]) :NaturalMap[K, V] =
			if (key == _1) NaturalMap.empty[K, V] else this

		override def updated[U[A] >: V[A], X](key :K[X], value :U[X]) :NaturalMap[K, U] =
			if (key == _1) new Singleton(key, value)
			else (NaturalMap.newBuilder[K, U] += this += Assoc(key, value)).result()

		override def iterator :Iterator[Assoc[K, V, _]] = Iterator.single(this)

		override def keySet :Set[K[_]] = Set(key)

		override def values :Iterable[V[_]] = Unique.single(value)
	}






	private class NaturalizedMap[K[_], +V[_]] private[NaturalMap] (private val entries :Map[K[_], V[_]] = Map.empty[K[_], V[_]])
		extends NaturalMap[K, V]
	{
		override def size :Int = entries.size

		override def knownSize :Int = entries.knownSize


		override def contains(key :K[_]) :Boolean = entries.contains(key)

		override def apply[X](key :K[X]) :V[X] = entries(key).asInstanceOf[V[X]]

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
			case key -> value => new Assoc(key.asInstanceOf[K[Any]], value.asInstanceOf[V[Any]])
		}

		override def keySet :Set[K[_]] = entries.keySet

		override def values :Iterable[V[_]] = entries.values


		override def withDefault[U[T] >: V[T]](default :K =#> U) :NaturalMap[K, U] =
			new NaturalizedMap[K, U](entries.withDefault(default.existential))

	}



	private class NaturalMapBuilder[K[_], V[_],
		                            G[X, +Y] <: Map[X, Y] with immutable.MapOps[X, Y, G, G[X, Y]],
		                            M <: G[K[_], V[_]] with immutable.MapOps[K[_], V[_], G, M]]
	                               (private[this] var entries :G[K[_], V[_]])
		extends Builder[Assoc[K, V, _], NaturalMap[K, V]]
	{
		override def clear() :Unit = entries = (entries :IterableOps[(K[_], V[_]), Iterable, G[K[_], V[_]]]).empty

		override def result() :NaturalMap[K, V] = new NaturalizedMap[K, V](entries)

		override def addOne(elem :Assoc[K, V, _]) :this.type = { entries = entries.updated(elem._1, elem._2); this }

	}
}





