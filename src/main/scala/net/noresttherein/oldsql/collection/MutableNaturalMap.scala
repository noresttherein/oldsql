package net.noresttherein.oldsql.collection

import scala.collection.mutable
import scala.collection.mutable.Builder

import net.noresttherein.oldsql
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.generic.=#>


/**
  * @author Marcin Mościcki
  */
private[oldsql] trait MutableNaturalMap[K[_], V[_]]
	extends NaturalMap[K, V] with Builder[Assoc[K, V, _], MutableNaturalMap[K, V]]
{
	type Item[X] = Assoc[K, V, X]

	override def knownSize :Int = super.knownSize

	@inline final def +=[X](entry :(K[X], V[X])) :this.type = { put(entry._1, entry._2); this }


	def +=[X](key :K[X], value :V[X]) :this.type = { put(key, value); this }

	def put[X](key :K[X], value :V[X]) :Option[V[X]]

	def mapValuesInPlace(f :Item =#> V) :this.type
}






private[oldsql] object MutableNaturalMap {

	def apply[K[_], V[_]](entries :Assoc[K, V, _]*) :MutableNaturalMap[K, V] =
		new NaturalizedMutableMap[K, V] ++= entries

	def empty[K[_], V[_]] :MutableNaturalMap[K, V] = new NaturalizedMutableMap[K, V]

	def freezable[K[_], V[_]] :FreezableMap[K, V] = new NaturalizedMutableMap[K, V] with FreezableMap[K, V]



	trait FreezableMap[K[_], V[_]] extends MutableNaturalMap[K, V] {
		private[this] var frozen = false

		def freeze() :NaturalMap[K, V] = {
			frozen = true
			oldsql.publishMutable()
			this
		}

		private def guard() :Unit =
			if (frozen)
				throw new IllegalArgumentException("Can't modify a frozen Map")

		abstract override def addAll(xs :IterableOnce[Assoc[K, V, _]]) :this.type = {
			guard(); super.addAll(xs)
		}

		abstract override def addOne(elem :Assoc[K, V, _]) :this.type = {
		    guard(); super.addOne(elem)
		}

		abstract override def put[X](key :K[X], value :V[X]) :Option[V[X]] = {
			guard(); super.put(key, value)
		}

		override def clear() :Unit = {
			guard(); clear()
		}

		override def mapValuesInPlace(f :Item =#> V) :this.type = {
			guard(); mapValuesInPlace(f)
		}

	}






	private class NaturalizedMutableMap[K[_], V[_]](entries :mutable.Map[K[_], V[_]])
		extends MutableNaturalMap[K, V]
	{
		def this() = this(mutable.Map.empty[K[_], V[_]])

		override def size :Int = entries.size

		override def knownSize :Int = entries.knownSize


		protected[this] override def newSpecificBuilder :Builder[Assoc[K, V, _], MutableNaturalMap[K, V]] =
			new NaturalizedMutableMap[K, V]

		override def getOrElse[U[T] >: V[T], X](key :K[X], default : => U[X]) :U[X] =
			entries.getOrElse(key, default).asInstanceOf[V[X]]

		override def get[X](key :K[X]) :Option[V[X]] = entries.get(key).asInstanceOf[Option[V[X]]]

		override def apply[X](key :K[X]) :V[X] = entries(key).asInstanceOf[V[X]]

		override def contains(key :K[_]) :Boolean = entries.contains(key)



		override def iterator :Iterator[Assoc[K, V, _]] =
			entries.iterator.map { case (k, v) => Assoc(k.asInstanceOf[K[Any]], v.asInstanceOf[V[Any]]) }

		override def keySet = entries.keySet
		override def values = entries.values

		override def removed(key :K[_]) :NaturalMap[K, V] = {
			val res = NaturalMap.newBuilder[K, V]
			for ((key, value) <- entries)
				res += Assoc(key.asInstanceOf[K[Any]], value.asInstanceOf[V[Any]])
			res.result()
		}

		override def updated[U[T] >: V[T], X](key :K[X], value :U[X]) :NaturalMap[K, U] = {
			val res = NaturalMap.newBuilder[K, U]
			for ((k, v) <- entries)
				if (k == key) res += Assoc(key, value)
				else res += Assoc(k.asInstanceOf[K[Any]], v.asInstanceOf[V[Any]])
			res.result()
		}


		override def ++[U[T] >: V[T]](entries :IterableOnce[Assoc[K, U, _]]) :NaturalMap[K, U] =
			NaturalMap.empty[K, U] ++ this ++ entries



		override def put[X](key :K[X], value :V[X]) :Option[V[X]] =
			entries.put(key, value).asInstanceOf[Option[V[X]]]

		override def clear() :Unit = entries.clear()

		override def result() :MutableNaturalMap[K, V] = this

		override def addOne(elem :Assoc[K, V, _]) :this.type = {
			entries.addOne (elem._1, elem._2); this
		}



		override def mapValuesInPlace(f :Item =#> V) :this.type = {
			entries.mapValuesInPlace {
				(k, v) => f(Assoc[K, V, Any](k.asInstanceOf[K[Any]], v.asInstanceOf[V[Any]]))
			}
			this
		}
	}

}