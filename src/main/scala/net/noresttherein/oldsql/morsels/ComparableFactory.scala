package net.noresttherein.oldsql.morsels

import scala.collection.{EvidenceIterableFactory, Factory, IterableFactory, MapFactory, SortedMapFactory}

import net.noresttherein.oldsql.slang.classNameMethods






/** Implementations of the standard Scala collection `Factory` interface which implement `equals` in terms
  * of the underlying [[scala.collection.IterableFactory IterableFactory]] (or its equivalent).
  * It is also `Serializable`, which additionally enhances its declarative use apart from the operative application.
  */
trait ComparableFactory[-E, +C] extends Factory[E, C] with Serializable {
	def factory :Any

	def canEqual(that :Any) :Boolean = that.isInstanceOf[ComparableFactory[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case other :ComparableFactory[_, _] => (other eq this) || other.factory == factory
		case _ => false
	}
	override def hashCode :Int = factory.hashCode

	override def toString :String = factory.innerClassName
}



object ComparableFactory {
	implicit def apply[E, C[_]](iterable :IterableFactory[C]) :ComparableFactory[E, C[E]] =
		new ComparableFactory[E, C[E]] {
			override val factory = iterable
			override def fromSpecific(it :IterableOnce[E]) = factory.from(it)
			override def newBuilder = factory.newBuilder
		}

	implicit def apply[E, C[_], Ev[_]](iterable :EvidenceIterableFactory[C, Ev])(implicit ev :Ev[E])
			:ComparableFactory[E, C[E]] =
		new EvidenceFactory[E, C, Ev](iterable)

	implicit def apply[K, V, M[_, _]](map :MapFactory[M]) :ComparableFactory[(K, V), M[K, V]] =
		new ComparableFactory[(K, V), M[K, V]] {
			override val factory = map
			override def fromSpecific(it :IterableOnce[(K, V)]) = map.from(it)
			override def newBuilder = map.newBuilder
		}

	implicit def apply[K :Ordering, V, M[_, _]](map :SortedMapFactory[M]) :ComparableFactory[(K, V), M[K, V]] =
		new EvidenceMapFactory[K, V, M](map)



	private class EvidenceFactory[E, C[_], Ev[_]](override val factory :EvidenceIterableFactory[C, Ev])
	                                             (implicit val evidence :Ev[E])
		extends ComparableFactory[E, C[E]]
	{
		override def fromSpecific(it :IterableOnce[E]) = factory.from(it)
		override def newBuilder = factory.newBuilder

		override def equals(that :Any) :Boolean = that match {
			case other :EvidenceFactory[_, _, _] =>
				(other eq this) || other.factory == factory && other.evidence == evidence
			case _ => false
		}
		override def hashCode :Int = factory.hashCode * 31 + evidence.hashCode
	}

	private class EvidenceMapFactory[K, V, M[_, _]](override val factory :SortedMapFactory[M])
	                                               (implicit val evidence :Ordering[K])
		extends ComparableFactory[(K, V), M[K, V]]
	{
		override def fromSpecific(it :IterableOnce[(K, V)]) = factory.from(it)
		override def newBuilder = factory.newBuilder

		override def equals(that :Any) :Boolean = that match {
			case other :EvidenceMapFactory[_, _, _] =>
				(other eq this) || other.factory == factory && other.evidence == evidence
			case _ => false
		}
		override def hashCode :Int = factory.hashCode() * 31 + evidence.hashCode
	}
}




