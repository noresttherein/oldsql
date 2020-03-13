package net.noresttherein.oldsql.collection


//import net.noresttherein.oldsql.collection.InverseIndexSeq.Indexed
//
//import scala.collection.{AbstractSeq, AbstractSet, GenTraversableOnce}
//import scala.collection.generic.CanBuildFrom
//import scala.collection.mutable.Builder
//import scala.reflect.ClassTag



/** A sequence providing O(1) indexOf(T), lastIndexOf(T) and contains(T) implementation.
  * The indexed property is sticky only in regards to the `newBuilder` method used for homomorphic sequences.
  * This means that while `filter`, `take` and similar will result an `InverseIndexSeq[T]`,
  * `map`, `flatMap`, and other calls will result in some default sequence type, which currently is an `IndexedSeq`.
  * This class is used for the column lists exported by mappings in order to quickly find the column mapping for
  * a given column result in a `ResultSet`.
  *
  * @tparam T element type.
  */
/*
trait InverseIndexSeq[+T] extends Seq[T] with Serializable {
	protected[this] override def newBuilder :Builder[T, Seq[T]] =
		List.newBuilder[T].mapResult(new Indexed(_))

	override def genericBuilder[B] :Builder[B, Seq[B]] = IndexedSeq.newBuilder[B]

}



/** Companion object serving as a factory for InverseIndexSeq - sequences with fast indexOf operations. */
object InverseIndexSeq {

	/** A sequence providing O(1) indexOf(T) and lastIndexOf(T) implementation. Backed by a passed on seq argument and an index map mapping its elements to their indices
	  * @param elems any sequence, but returned indexed sequence delegates its apply(Int) calls directly to the corresponding method on the passed argument,
	  *              so it should be an IndexedSeq if you intend to use random access on the returned object
	  * @tparam T element type
	  * @return elems if its dynamic type is already InverseIndexSeq, a new InverseIndexSeq instance backed by elemes otherwise.
	  */
	def apply[T](elems :Seq[T]) :InverseIndexSeq[T] = elems match {
		case _:InverseIndexSeq[_] => elems.asInstanceOf[InverseIndexSeq[T]]
		case _ => new Indexed[T](elems)
	}


	def apply[T](elems :Iterable[T]) :InverseIndexSeq[T] = elems match {
		case _:InverseIndexSeq[_] => elems.asInstanceOf[InverseIndexSeq[T]]
		case _ => elems.toSeq.indexed
	}


	def mapped[T, X :ClassTag](elems :Seq[T])(map :T=>X)(unmap :X=>T) :InverseIndexSeq[X] =
		new MappedInverseIndexSeq[X, T](elems, map, unmap)

	implicit class implicitIndexing[T](private val elems :Seq[T]) extends AnyVal {
		/** A shorthand method for converting any Seq to an InverseIndexSeq.
		  * Implementation always returns this, but there is an implicit conversion available from Seq to InverseIndexSeq,
		  * so instead of writing InverseIndexSeq(aSeq) you can write aSeq.indexed.
		  * If aSeq's dynamic type was already an InverseIndexSeq, it returns itself cast to InverseIndexSeq.
		  * Of course, if aSeq static type already was an InverseIndexSeq, it is a simple static noOp call.
		  */
		final def indexed :InverseIndexSeq[T] = apply(elems)
	}


	/** A sequence providing O(1) indexOf(T), lastIndexOf(T) and contains(T) implementation.
	  * Backed by a passed on seq argument and an index map mapping its elements to their indices.
	  * The indexed property is not sticky relative to collection operations, i.e. any map, flatMap, filter, etc. calls will not return an InverseIndexSeq.
	  * @param backing contents of this collection
	  * @tparam T element type
	  */
	private class Indexed[+T] private[InverseIndexSeq] (backing :Seq[T]) extends AbstractSeq[T] with InverseIndexSeq[T] {
		private[this] val index :Map[Any, Int] = backing.view.zipWithIndex.force.reverse.toMap
		private[this] lazy val lastIndex :Map[Any, Int] = backing.view.zipWithIndex.toMap

		override val length: Int = backing.length

		override def apply(idx: Int): T = backing(idx)

		override def foreach[U](f :T=>U) :Unit = backing.foreach(f)

		override def iterator: Iterator[T] = backing.iterator

		override def indexOf[B >: T](elem: B): Int = index.getOrElse(elem, -1)

		override def lastIndexOf[B >: T](elem: B): Int = lastIndex.getOrElse(elem, -1)


		override def contains[A1 >: T](elem: A1): Boolean = index.contains(elem)

		override def toSet[B >: T]: Set[B] = index.keySet.asInstanceOf[Set[B]]



/*
		override def +:[B >: T, That](elem :B)(implicit bf :CanBuildFrom[Seq[T], B, That]) :That =
			if (bf == Seq.canBuildFrom && contains(elem))
				this.asInstanceOf[That]
			else if (bf == Seq.canBuildFrom && backing.isInstanceOf[List[_]]) {
				new Indexed(elem :: backing.toList).asInstanceOf[That]
			} else {
				val builder = bf(this)
				if (!contains(elem))
					builder += elem
				builder ++= this
				builder.result()
			}

		override def :+[B >: T, That](elem :B)(implicit bf :CanBuildFrom[Seq[T], B, That]) :That =
			if (bf == Seq.canBuildFrom && contains(elem))
				this.asInstanceOf[That]
			else {
				val builder = bf(this)
				builder ++= this
				if (!contains(elem))
					builder += elem
				builder.result()
			}

		override def ++[B >: T, That](that :GenTraversableOnce[B])(implicit bf :CanBuildFrom[Seq[T], B, That]) :That = {
			val builder = bf(this)
			builder ++= this
			that foreach { b => if (!contains(b)) builder += b }
			builder.result()
		}

		override def ++:[B >: T, That](that :TraversableOnce[B])(implicit bf :CanBuildFrom[Seq[T], B, That]) :That = {
			val builder = bf(this)
			that foreach { b => if (!contains(b)) builder += b }
			builder ++= this
			builder.result()
		}

		override def ++:[B >: T, That](that :Traversable[B])(implicit bf :CanBuildFrom[Seq[T], B, That]) :That =
			if (bf == Seq.canBuildFrom && that.forall(contains))
				this.asInstanceOf[That]
			else if (bf == Seq.canBuildFrom && backing.isInstanceOf[List[_]]) {
				new Indexed(that.toList ::: backing.toList).asInstanceOf[That]
			} else {
				val builder = bf(this)
				that foreach { b => if (!contains(b)) builder += b }
				builder ++= this
				builder.result()
			}
*/



	}





	private class MappedInverseIndexSeq[A :ClassTag, B] (backing :Seq[B], map :B=>A, unmap :A=>B)
		extends AbstractSeq[A] with InverseIndexSeq[A]
	{ adapter =>
		private[this] val A = implicitly[ClassTag[A]]
		override val length: Int = backing.length

		override def apply(idx: Int): A = map(backing(idx))

		override def foreach[U](f :A => U) :Unit = backing foreach (map andThen f)

		override def iterator: Iterator[A] = backing.iterator.map(map)

		override def indexOf[T >: A](elem: T): Int = elem match {
			case A(a) => backing.indexOf(unmap(a))
			case _ => super.indexOf(elem)
		}

		override def lastIndexOf[T >: A](elem: T): Int = elem match {
			case A(a) => backing.lastIndexOf(unmap(a))
			case _ => super.lastIndexOf(elem)
		}


		override def contains[A1 >: A](elem: A1): Boolean = elem match {
			case A(a) => backing.contains(unmap(a))
			case _ => super.contains(elem)
		}

		override def toSet[A1 >: A]: Set[A1] = new AbstractSet[A1] with Set[A1] {
			override def contains(elem: A1): Boolean = adapter.contains(elem)
			override def +(elem: A1): Set[A1] = Set(elem +: adapter :_*)
			override def -(elem: A1): Set[A1] = Set(adapter.filterNot(elem==_):_*)
			override def iterator: Iterator[A1] = adapter.iterator
		}

	}
}
*/
