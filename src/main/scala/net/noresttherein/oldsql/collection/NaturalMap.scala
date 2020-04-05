package net.noresttherein.oldsql.collection
import net.noresttherein.oldsql.morsels.generic.{=#>, GenericFun}

import scala.collection.mutable.Builder
import scala.collection.{immutable, IterableOps, MapOps}



/** A generic `Map`-like interface which implements a natural transformation between type functors `K` and `V`.
  * Each key is of type `K[X] forSome { type X }` and is mapped to a value of type `V[X]` for the same type argument `X`.
  * @author Marcin Mościcki
  */
trait NaturalMap[K[X], +V[X]] extends Iterable[NaturalMap.Entry[K, V, _]] {
//	type Entry[X] = NaturalMap.Entry[K, V, X]

	def get[X](key :K[X]) :Option[V[X]]

	def getOrElse[U[T] >: V[T], X](key :K[X], default: => U[X]) :U[X] = get(key) getOrElse default

	def apply[X](key :K[X]) :V[X]

	def contains(key :K[_]) :Boolean

	def removed(key :K[_]) :NaturalMap[K, V]

	def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NaturalMap[K, U]

	def +[U[T] >: V[T], X](key :K[X], value : U[X]) :NaturalMap[K, U] = updated(key, value)

	def +[U[T] >: V[T], X](entry :NaturalMap.Entry[K, U, X]) :NaturalMap[K, U] = updated(entry._1, entry._2)

	def ++[U[T] >: V[T]](entries :IterableOnce[NaturalMap.Entry[K, U, _]]) :NaturalMap[K, U]


	def withDefault[U[T] >: V[T]](default :K =#> U) :NaturalMap[K, U]
//	protected def default[X](key :K[X]) :V[X] =
//		throw new NoSuchElementException("No value for key " + key)

	def iterator :Iterator[NaturalMap.Entry[K, V, _]]
}





object NaturalMap {

	case class Entry[+K[_], +V[_], X](_1 :K[X], _2 :V[X]) extends Product2[K[X], V[X]] {
		@inline def key :K[X] = _1
		@inline def value :V[X] = _2
		override def toString :String = String.valueOf(_1) + "->" + _2
	}

	implicit class -#>[K[_], X](private val key :K[X]) extends AnyVal {
		@inline def -#>[V[_]](value :V[X]) :Entry[K, V, X] = new Entry(key, value)
	}



	def apply[K[_], V[_]](entries :Entry[K, V, _]*) :NaturalMap[K, V] = (newBuilder[K, V] ++= entries).result

	def empty[K[_], V[_]] :NaturalMap[K, V] = instance.asInstanceOf[NaturalMap[K, V]]

	private[this] final val instance = new NaturalizedMap[Seq, Seq](Map[Seq[_], Seq[_]]())


	def newBuilder[K[_], V[_]] :Builder[Entry[K, V, _], NaturalMap[K, V]] =
		new NaturalMapBuilder[K, V, Map, Map[K[_], V[_]]](Map[K[_], V[_]]())



	private class NaturalizedMap[K[_], +V[_]] private[NaturalMap] (private val entries :Map[K[_], V[_]])
		extends NaturalMap[K, V]
	{
		protected[this] override def newSpecificBuilder :Builder[NaturalMap.Entry[K, V, _], NaturalMap[K, V]] =
			NaturalMap.newBuilder[K, V]


		override def contains(key :K[_]) :Boolean = entries.contains(key)

		override def apply[X](key :K[X]) :V[X] = entries(key).asInstanceOf[V[X]]

		override def get[X](key :K[X]) :Option[V[X]] = entries.get(key).asInstanceOf[Option[V[X]]]

		override def getOrElse[U[T] >: V[T], X](key :K[X], default: => U[X]) :U[X] =
			entries.getOrElse(key, default).asInstanceOf[U[X]]


		override def removed(key :K[_]) :NaturalMap[K, V] = new NaturalizedMap[K, V](entries.removed(key))

		override def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NaturalMap[K, U] =
			new NaturalizedMap[K, U](entries.updated[U[_]](key, value))

		override def ++[U[T] >: V[T]](entries :IterableOnce[NaturalMap.Entry[K, U, _]]) :NaturalMap[K, U] =
			if (entries.iterator.isEmpty) this
			else (newBuilder[K, U] ++= this ++= entries).result()

		override def iterator :Iterator[NaturalMap.Entry[K, V, _]] = entries.iterator.map {
			case key -> value => new Entry(key.asInstanceOf[K[Any]], value.asInstanceOf[V[Any]])
		}


		override def withDefault[U[T] >: V[T]](default :K =#> U) :NaturalMap[K, U] =
			new NaturalizedMap[K, U](entries.withDefault(default.existential))
	}



	private class NaturalMapBuilder[K[_], V[_],
		                            G[X, +Y] <: Map[X, Y] with immutable.MapOps[X, Y, G, G[X, Y]],
		                            M <: G[K[_], V[_]] with immutable.MapOps[K[_], V[_], G, M]]
	                               (private[this] var entries :G[K[_], V[_]])
		extends Builder[Entry[K, V, _], NaturalMap[K, V]]
	{
		override def clear() :Unit = entries = (entries :IterableOps[(K[_], V[_]), Iterable, G[K[_], V[_]]]).empty

		override def result() :NaturalMap[K, V] = new NaturalizedMap[K, V](entries)

		override def addOne(elem :Entry[K, V, _]) :this.type = { entries = entries.updated(elem._1, elem._2); this }

	}
}
