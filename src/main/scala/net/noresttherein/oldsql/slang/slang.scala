package net.noresttherein.oldsql

import java.lang.{StringBuilder => JStringBuilder}

import scala.annotation.tailrec
import scala.collection.{IterableOnce, IterableOps, LinearSeq}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.Builder
import scala.reflect.{classTag, ClassTag}
import scala.util.Try

import net.noresttherein.oldsql.collection.{Opt, PassedArray, ReversedList}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.raise






package object slang {

	private[oldsql] object && {
		def unapply[T](value :T) :Opt[(T, T)] = Got((value, value))
	}

	/** Extends any non-value object with a `neq` method being the opposite of the built-in method `eq`. */
	private[oldsql] implicit class neq(private val self :AnyRef) extends AnyVal {
		/** `!(this eq other)` */
		@inline def neq(other :AnyRef) :Boolean = !(self eq other)
	}


	/** An implicit conversion extending Int with a method 'repeat' which executes a block the given number of times. */
	private[oldsql] implicit final class timesMethods(private val count :Int) extends AnyVal {
		/** Execute the given block the number of times specified by 'this' argument. */
		@inline def times(block : => Unit): Unit =
			for (_ <- 0 until count) block

		/** Apply `f` recursively to its own result `this` number of times, starting with value `start`. */
		@tailrec def times[T](f :T => T)(start :T) :T =
			if (count <= 0) start
			else (count - 1).times(f)(f(start))

		/** Apply `f` recursively to its own result `this` number of times, starting with value `start`. */
		@inline def timesFrom[T](start :T)(f :T => T) :T = count.times(f)(start)

		/** Apply `f` to its own result `this` number of times, starting with value `start`.
		  * Equivalent to `this.timesFrom(start)(f)` but helps with type inference.
		  * The name is in analogy to equivalent fold left over a range:
		  * `def pow(x :Int, n:Int) = (1 /: (0 until n)){ (acc, _) => acc * x }` (in pseudo code)
		  * @param start start value for the recursion.
		  * @param f function to recursively apply to itself.
		  * @return `start` if `this<=0` or `f(f(...f(start)))` (composed `this` number of times).
		  * @usecase `(new StringBuilder /: n)(_ += ":)"`
		  */
		@inline def /:[T](start :T)(f :T=>T) :T = times(f)(start)

		/** Executes `f` `this` number of times, passing the ordinal number of the `execution` in the sequence
		  * as the argument.
		  * @return a sequence of length `this`, containing the results of each execution of `f`.
		  */
		def enumerate[T](f :Int => T) :Seq[T] = {
			@tailrec def rec(i :Int, acc :List[T]) :List[T] =
				if (i < 0) acc
				else rec(i, f(i) :: acc)
			rec(count - 1, Nil)
		}

	}


	/** A syntactic wrapper for collections, injecting methods implementing
	  * 'breakable' `foldLeft`, that is folding only some prefix of the collection
	  * based on some predicate or other termination condition.
	  *
	  * @param items any collection to fold.
	  * @tparam T element type of this collection.
	  */
	private[oldsql] implicit class foldingMethods[T](private val items :IterableOnce[T]) extends AnyVal {
		/** Applies the given folding function `op` to the elements of this collection starting with the given
		  * initial value `start` while the predicate `until` is false for the most recently computed value.
		  * Note that this is not equivalent to `foldWhile(start)(!until(_))(op)` as the recursion goes
		  * one step further, returning the first element which satisfies the given condition,
		  * where a negated `foldWhile` would return the result of the last iteration which did not.
		  * Another difference is that `until` is applied starting with `op(start, this.head)`
		  * (assuming this collection is not empty), rather than `op(start)` as `foldWhile` would.
		  * @param start an initial value.
		  * @param until a predicate which needs to be satisfied for the folding to stop.
		  * @param op    a function generating subsequent values based on the previously computed value
		  *              and the next element of the collection.
		  * @tparam A the type of generated and tested values.
		  * @return `start` if `this.isEmpty`, the first generated value which satisfies predicate `until`,
		  *         or the result of folding the whole collection if no such element was computed.
		  */
		def foldUntil[A](start :A)(op :(A, T) => A)(until :A => Boolean) :A = {
			var last = start; val i = items.iterator
			while (i.hasNext && { last = op(last, i.next()); !until(last) })
				{}
			last
		}


		/** Applies the given folding function `op` to the elements of this collection starting with the given
		  * initial value `start` while the predicate `pred` is true for the most recently computed value.
		  * The predicate is applied for the first time to `op(start, head)` (assuming this collection is not empty),
		  * not to `start` itself. If this collection is empty, then `start` is returned immediately,
		  * regardless of whether it satisfies the condition. If all generated values satisfy `pred`,
		  * then this method is equivalent to `foldLeft(start)(op)`.
		  * Note that this is not equivalent to `foldUntil(start)(!pred(_))(op)`, as the latter would apply `op`
		  * one more time unless the end of collection is reached without falsifying the predicate.
		  * @param start an initial value.
		  * @param pred  a predicate which needs to be satisfied for folding to continue.
		  * @param op    a function generating subsequent values based on the previously computed value
		  *              and the next element of the collection.
		  * @tparam A    the type of generated and tested values.
		  * @return      `start` if this collection is empty or `op(start, this.head)` is false,
		  *              or the result of the last application of `op` which still satisfied `pred`.
		  */
		def foldWhile[A](start :A)(pred :A => Boolean)(op :(A, T) => A) :A = {
			var last = start; var next = start
			val i = items.iterator
			while (i.hasNext && pred({ next = op(last, i.next()); next }))
				last = next
			last
//			var acc = start; val it = items.iterator
//			while (pred(acc) && it.hasNext)
//				acc = op(acc, it.next())
//			acc
		}

		/** Applies the given folding function `op` to the elements of this collection starting with the given initial
		  * value `start` for as long as `op` is defined for the previously computed value.
		  * @param start accumulator given as the first argument to first invocation of `op`.
		  * @param op a partial function combining the accumulator with the elements of the collection working
		  *           as the breaking condition when non-defined.
		  * @tparam A type of generated values.
		  * @return result of `this.foldLeft(a)(op)` or first value `a :A` such that `op` is not defined
		  *         for `(a, e)` where `e` is the first non-folded element of this collection.
		  */
		def partialFold[A](start :A)(op :PartialFunction[(A, T), A]) :A = {
			val lift = op.lift
			foldSome(start) { (acc, elem) => lift((acc, elem)) }
		}

		/** Applies the given folding function to the elements of this collection and current accumulator value
		  * for as long as it returns non-empty results.
		  * @param start initial accumulator value passed to the first call of `op` together with the first element
		  *              of this collection.
		  * @param op a function generating consecutive values of `A` from the previous value and subsequent element
		  *           of this collection, yielding `None` to signal the break condition for folding.
		  * @tparam A type of generated values.
		  * @return the result of the last execution of `op` which returned `Some`,
		  *         or `start` if this collection is empty or `op(start, this.head) == None`.
		  */
		def foldSome[A](start :A)(op :(A, T) => Option[A]) :A = items match {
			case it :Iterable[_] if it.isEmpty => start
			case it :Iterator[_] if it.isEmpty => start
			case _ =>
				val it = items.iterator
				var last = start
				while (op(last, it.next()) match {
					case Some(next) => last = next; true
					case _ => false
				}) {}
				last
		}
	}


	/** Additional extension methods for collections of the standard library framework. */
	private[oldsql] implicit class mappingMethods[C[X] <: Iterable[X], E](private val self :IterableOps[E, C, C[E]])
		extends AnyVal
	{
		/** Maps this collection from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the second elements
		  * of the tuples returned by the given function) are returned in a collection of the same dynamic type
		  * as this collection.
		  */
		def mapWith[A, O](z :A)(f :(E, A) => (O, A)) :C[O] =
			self.view.scanLeft((null.asInstanceOf[O], z)) { //safe because null is never passed to f and we are in an erased context
				(acc, e) => f(e, acc._2)
			}.tail.map(_._1).to(self.iterableFactory)

		/** Flat maps this collection from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the collections returned by
		  * by the given function) are returned in a collection of the same dynamic type as this collection.
		  */
		def flatMapWith[A, O](z :A)(f :(E, A) => (IterableOnce[O], A)) :C[O] =
			self.view.scanLeft((Nil :IterableOnce[O], z)) {
				(acc, e) => f(e, acc._2)
			}.flatMap(_._1).to(self.iterableFactory)

		/** Maps this collection in order consistent with `foreach`, passing as the first argument the index
		  * of the mapped element.
		  */
		def mapWithIndex[O](f :(E, Int) => O) :C[O] = {
			var i = 0
			val b = self.iterableFactory.newBuilder[O]
			b sizeHint self
			self foreach { e => b += f(e, i); i += 1 }
			b.result()
		}

		/** Flat maps this collection in order consistent with `foreach`, passing as the first argument the index
		  * of the mapped element in this collection (that is, the number of elements processed before it).
		  */
		def flatMapWithIndex[O](f :(E, Int) => IterableOnce[O]) :C[O] = {
			var i = 0
			val b = self.iterableFactory.newBuilder[O]
			self foreach { e => b ++= f(e, i); i += 1 }
			b.result()
		}

		/** Iterates over the collection, passing the index of the current element to the given function.
		  * Note that in collections with an undefined order, this index applies only to this particular iteration,
		  * rather than some absolute position.
		  */
		def foreachWithIndex[O, U](f :(E, Int) => U) :Unit = {
			var i = 0
			self foreach { e => f(e, i); i += 1 }
		}


		/** Zips this collection with another one and maps the result in one step.
		  * No intermediate collection is created, and the mapping function accepts two arguments rather than a tuple,
		  * making it more convenient to use with placeholder parameters.
		  */
		def zipMap[X, O](that :IterableOnce[X])(f :(E, X) => O) :C[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			res sizeHint self
			while (l.hasNext && r.hasNext)
				res += f(l.next(), r.next())
			res.result()
		}

		/** Equivalent to [[net.noresttherein.sugar.collection.MappingMethods.zipMap zipMap]],
		  * but throws a [[NoSuchElementException]] if the collections are of different sizes.
		  */
		def zipMapAll[X, O](that :IterableOnce[X])(f :(E, X) => O) :C[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			res sizeHint self
			while (l.hasNext && r.hasNext)
				res += f(l.next(), r.next())
			if (l.hasNext)
				throw new NoSuchElementException("Cannot zipMapAll: left collection has more elements than the right one.")
			else if (r.hasNext)
				throw new NoSuchElementException("Cannot zipMapAll: right collection has more elements than the left one.")
			res.result()
		}

		/** Equivalent to `this.zipAll(that, thisElem, thatElem).map(f)`, but happens in one step and the argument function
		  * takes two arguments instead of a pair, which makes it possible to use with lambda placeholder parameters.
		  */
		def zipMapAll[X, O](that :IterableOnce[X], thisElem :E, thatElem :X)(f :(E, X) => O) :C[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			val thisSize = self.knownSize
			val thatSize = that.knownSize
			if (thisSize >= 0)
				if (thatSize >= 0)
					res sizeHint (thisSize max thatSize)
				else
					res sizeHint thisSize
			else if (thatSize >= 0)
				res sizeHint thatSize
			while (l.hasNext && r.hasNext)
				res += f(l.next(), r.next())
			while (l.hasNext)
				res += f(l.next(), thatElem)
			while (r.hasNext)
				res += f(thisElem, r.next())
			res.result()
		}

		/** Equivalent to `this.zip(rights).map`, but takes a two argument function instead of a function of a pair,
		  * which makes it possible to use with placeholder lambda parameters.
		  */
		def zipFlatMap[X, O](that :IterableOnce[X])(f :(E, X) => IterableOnce[O]) :C[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			while (l.hasNext && r.hasNext)
				res ++= f(l.next(), r.next())
			res.result()
		}

		/** Equivalent to [[net.noresttherein.sugar.collection.MappingMethods.zipFlatMap zipFlatMap]],
		  * but throws a [[NoSuchElementException]] if the collections are of different sizes.
		  */
		def zipFlatMapAll[X, O](that :IterableOnce[X])(f :(E, X) => IterableOnce[O]) :C[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			while (l.hasNext && r.hasNext)
				res ++= f(l.next(), r.next())
			if (l.hasNext)
				throw new NoSuchElementException("Cannot zipFlatMapAll: left collection has more elements than the right one.")
			else if (r.hasNext)
				throw new NoSuchElementException("Cannot zipFlatMapAll: right collection has more elements than the left one.")
			res.result()
		}

		/** Equivalent to `this.zipAll(that, thisElem, thatElem).map(f)`, but happens in one step and the argument function
		  * takes two arguments instead of a pair, which makes it possible to use with lambda placeholder parameters.
		  */
		def zipFlatMapAll[X, O](that :IterableOnce[X], thisElem :E, thatElem :X)(f :(E, X) => IterableOnce[O]) :C[O] = {
			val l = self.iterator
			val r = that.iterator
			val res = self.iterableFactory.newBuilder[O]
			while (l.hasNext && r.hasNext)
				res ++= f(l.next(), r.next())
			while (l.hasNext)
				res ++= f(l.next(), thatElem)
			while (r.hasNext)
				res ++= f(thisElem, r.next())
			res.result()
		}

		/** Equivalent to `this.zip(that).forall { case (a, b) => p(a, b) }`, but does not build an intermediate
		  * collection of tuples and accepts a two argument function rather than a function of tuple, which makes
		  * it more convenient to use with the lambda parameter placeholder syntax. Additionally, it requires
		  * that both collections are of the same size, returning `false` if it is not true.
		  */
		def zipForAll[X](that :IterableOnce[X])(p :(E, X) => Boolean) :Boolean = {
			val l = self.iterator
			val r = that.iterator
			var wasTrue = true
			while (l.hasNext && r.hasNext && { wasTrue = p(l.next(), r.next()); wasTrue })
				{}
			wasTrue && !l.hasNext && !r.hasNext
		}


		/** Maps the elements of the collection and reverses their order. The order in which the mapping function
		  * will be applied to the elements is undefined and depends on the runtime type of this collection.
		  * Note that if this collection is unordered, the order of the elements in the mapped collection
		  * is likewise undefined and depends on the implementation of this collection's builder.
		  * This operation is faster than `this.map(f).reverse`.
		  */
		def mapReverse[O](f :E => O) :C[O] = self match {
			case _ if self.isEmpty =>
				self.iterableFactory.empty
			case list :List[E] =>
				@tailrec def mapList(unmapped :List[E], mapped :List[O]) :List[O] = unmapped match {
					case h::t => mapList(t, f(h)::mapped)
					case _ => mapped
				}
				mapList(list, Nil).asInstanceOf[C[O]]
			case list :LinearSeq[E] =>
				@tailrec def mapLinear(unmapped :LinearSeq[E], mapped :LinearSeq[O]) :LinearSeq[O] =
					if (unmapped.isEmpty) mapped
					else mapLinear(unmapped.tail, f(unmapped.head) +: mapped)
				mapLinear(list, list.iterableFactory.empty).asInstanceOf[C[O]]
			case seq :scala.collection.IndexedSeq[E] =>
				def mapIndexed() = {
					val b = self.iterableFactory.newBuilder[O]
					var i = seq.length
					while (i > 0) {
						i -= 1
						b += f(seq(i))
					}
					b.result()
				}
				mapIndexed()
			case seq :scala.collection.Seq[E] =>
				def mapSeq() = {
					val i = seq.reverseIterator
					val b = self.iterableFactory.newBuilder[O]
					b sizeHint self
					while (i.hasNext)
						b += f(i.next())
					b.result()
				}
				mapSeq()
			case _ =>
				def mapIterable() = {
					val mapped = (List.empty[O] /: self){ (acc, e) => f(e)::acc }
					val b = self.iterableFactory.newBuilder[O]
					b sizeHint self
					b ++= mapped
					b.result()
				}
				mapIterable()
		}
	}




	/** Extension methods of mutable and immutable sequences (and arrays through a wrapper):
	  *   1. alternative, safer implementations of [[scala.collection.SeqOps.indexOf indexOf]],
	  *      which do not return a negative index when the element is not found;
	  *   1. methods related to subsequences: sequences containing selected elements from another sequence,
	  *      in the same order.
	  */
	private[oldsql] implicit class SeqExtension[X](private val self :scala.collection.Seq[X]) extends AnyVal {
		@inline def length :Int = self.length

		/** Checks if the elements in this sequence follow the implicit ordering.
		  * @return [[net.noresttherein.oldsql.slang.SeqExtension.isSortedWith isSortedWith]]`(implicitly[Ordering[U]].compare(_, _) <= 0)`
		  */
		def isSorted[U >: X :Ordering] :Boolean = {
			val order = implicitly[Ordering[U]]
			isSortedWith(order.compare(_, _) <= 0)
		}

		/** Checks if the elements in this sequence are sorted by the given ordered property.
		  * @return [[net.noresttherein.oldsql.slang.SeqExtension.isSortedWith isSortedWith]]`(implicitly[Ordering[U]].compare(by(_), by(_)) <= 0)`
		  */
		def isSortedBy[U :Ordering](by :X => U) :Boolean = {
			val order = implicitly[Ordering[U]]
			isSortedWith((x, y) => order.compare(by(x), by(y)) <= 0)
		}

		/** Checks if the given predicate holds for all consecutive element pairs in this sequence. */
		def isSortedWith[U >: X](lte :(U, U) => Boolean) :Boolean =
			self.sizeIs <= 1 || {
				self match {
					case _ :LinearSeq[X] =>
						lte(self.head, self.tail.head) &&
							self.view.zip(self.tail).forall { case (left, right) => lte(left, right) }
					case _ :IndexedSeq[X] =>
						var i = self.length - 1
						var second = self(i);
						i -= 1
						var first = self(i)
						while (i >= 1 && lte(first, second)) {
							second = first
							i -= 1
							first = self(i)
						}
						i == 0
					case _ => (self to ArraySeq.untagged).isSortedWith(lte)
				}
			}

		def isReverselySorted[U >: X :Ordering] :Boolean = {
			val order = implicitly[Ordering[U]]
			isSortedWith(order.compare(_, _) >= 0)
		}

		def isIncreasing[U >: X :Ordering] :Boolean = {
			val order = implicitly[Ordering[U]]
			isSortedWith(order.compare(_, _) < 0)
		}

		def isDecreasing[U >: X :Ordering] :Boolean = {
			val order = implicitly[Ordering[U]]
			isSortedWith(order.compare(_, _) > 0)
		}

		/** Finds the location of the given element in this sequence, returning its index as an option.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def getIndexOf(x :X, from :Int = 0) :Opt[Int] = self.indexOf(x, from) match {
			case n if n >= 0 => Got(n)
			case _ => Lack
		}
		/** Finds the last location of the given element in this sequence, returning its index as an option.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		@inline def getLastIndexOf(x :X, end :Int = length - 1) :Opt[Int] = self.lastIndexOf(x, end) match {
			case n if n >= 0 => Got(n)
			case _ => Lack
		}
		/** Finds an element of this sequence which satisfies the predicate, returning its index as an option.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def getIndexWhere(p :X => Boolean, from :Int = 0) :Opt[Int] = self.indexOf(p, from) match {
			case n if n >= 0 => Got(n)
			case _ => Lack
		}
		/** Finds the last element of this sequence which satisfies the predicate, returning its index as an option.
		  * @param p   a function applied consecutively to all elements with indices lesser or equal `end`,
		  *            in a decreasing order.
		  * @param end the upper, inclusive bound on the returned index; elements after this position will not be checked.
		  */
		@inline def getLastIndexWhere(p :X => Boolean, end :Int = length - 1) :Opt[Int] =
			self.lastIndexWhere(p, end) match {
				case n if n >= 0 => Got(n)
				case _ => Lack
			}

		/** Finds the location of the given element in this sequence, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def sureIndexOf(x :X, from :Int = 0) :Int = self.indexOf(x, from) match {
			case n if n >= 0 => n
			case _ => throw new NoSuchElementException(indexOfErrorMessage(x, from))
		}
		/** Finds the last location of the given element in this sequence, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		@inline def sureLastIndexOf(x :X, end :Int = length - 1) :Int = self.lastIndexOf(x, end) match {
			case n if n >= 0 => n
			case _ => throw new NoSuchElementException(lastIndexOfErrorMessage(x, end))
		}
		/** Finds an element of this sequence which satisfies the predicate, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`,
		  *             until satisfied.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def sureIndexWhere(p :X => Boolean, from :Int = 0) :Int = self.indexWhere(p, from) match {
			case n if n >= 0 => n
			case _ => throw new NoSuchElementException(indexWhereErrorMessage(from))
		}
		/** Finds the last element of this sequence which satisfies the predicate, throwing a [[NoSuchElementException]]
		  * if it does not exist.
		  * @param p   a function applied consecutively to all elements, starting with the one at position `end`,
		  *            until satisfied.
		  * @param end the upper, inclusive bound on the returned index; elements after this index are not checked.
		  */
		@inline def sureLastIndexWhere(p :X => Boolean, end :Int = length - 1) :Int =
			self.lastIndexWhere(p, end) match {
				case n if n >= 0 => n
				case _ => throw new NoSuchElementException(lastIndexWhereErrorMessage(end))
			}

		/** Finds the location of the given element in this sequence, throwing an exception `E`,
		  * given as the type parameter, if it does not exist.
		  * @tparam E   the exception to be thrown; it must contain either `(String, Throwable)` or `(String)`
		  *             constructor.
		  * @param x    the element, whose index is to be determined.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def indexOfOrThrow[E <: Exception :ClassTag](x :X, from :Int = 0) :Int =
			self.indexOf(x, from) match {
				case n if n >= 0 => n
				case _ => raise[E](indexOfErrorMessage(x, from))
			}
		/** Finds the last location of the given element in this sequence, throwing an exception `E`,
		  * given as the type parameter, if it does not exist.
		  * @tparam E  the exception to be thrown; it must contain either `(String, Throwable)` or `(String)`
		  *            constructor.
		  * @param x   the element, whose index is to be determined.
		  * @param end the upper, inclusive bound on the returned index.
		  */
		@inline def lastIndexOfOrThrow[E <: Exception :ClassTag](x :X, end :Int = length - 1) :Int =
			self.lastIndexOf(x, end) match {
				case n if n >= 0 => n
				case _ => raise[E](lastIndexOfErrorMessage(x, end))
			}
		/** Finds an element of this sequence which satisfies the predicate, throwing an exception `E`,
		  * given as the type parameter, if it does not exist.
		  * @tparam E   the exception to be thrown; it must contain either `(String, Throwable)` or `(String)`
		  *             constructor.
		  * @param p    a function applied consecutively to all elements with indices greater or equal `from`,
		  *             until satisfied.
		  * @param from the lowest index which will be checked; preceding sequence prefix is skipped entirely.
		  */
		@inline def indexWhereOrThrow[E <: Exception :ClassTag](p :X => Boolean, from :Int = 0) :Int =
			self.indexWhere(p, from) match {
				case n if n >= 0 => n
				case _ => raise[E](indexWhereErrorMessage(from))
			}
		/** Finds the last element of this sequence which satisfies the predicate, throwing an exception `E`,
		  * given as the type parameter, if it does not exist.
		  * @tparam E  the exception to be thrown; it must contain either `(String, Throwable)` or `(String)`
		  *            constructor.
		  * @param p   a function applied consecutively to all elements, starting with the one at position `end`,
		  *            until satisfied.
		  * @param end the upper, inclusive bound on the returned index; elements after this index are not checked.
		  */
		@inline def lastIndexWhereOrThrow[E <: Exception :ClassTag](p :X => Boolean, end :Int = length - 1) :Int =
			self.lastIndexWhere(p, end) match {
				case n if n >= 0 => n
				case _ => raise[E](lastIndexWhereErrorMessage(end))
			}

		/** Returns `this.indexOf(x)`, adding an assertion that the result is not negative (the element has been found).
		  * @param x    an element of this collection.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexOf(x :X, msg: => String) :Int = assertPresent(self.indexOf(x), msg)
		/** Returns `this.indexOf(x, from)`, adding an assertion that the result is not negative
		  * (the element has been found).
		  * @param x    an element of this collection.
		  * @param from an inclusive lower bound on the index of the searched element.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexOf(x :X, from :Int, msg: => String) :Int = assertPresent(self.indexOf(x, from), msg)
		/** Returns `this.lastIndexOf(x)`, adding an assertion that the result is not negative
		  * (the element has been found).
		  * @param x    an element of this collection.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexOf(x :X, msg: => String) :Int = assertPresent(self.lastIndexOf(x), msg)
		/** Returns `this.lastIndexOf(x, end)`, adding an assertion that the result is not negative
		  * (the element has been found).
		  * @param x    an element of this collection.
		  * @param end  an inclusive upper bound on the index of the searched element.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexOf(x :X, end :Int, msg: => String) :Int = assertPresent(self.lastIndexOf(x, end), msg)
		/** Returns `this.indexWhere(p)`, adding an assertion that the result is not negative (a satisfying element
		  * has been found).
		  * @param p   a function applied consecutively to all elements, in the increasing order of indices.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexWhere(p :X => Boolean, msg: => String) :Int = assertPresent(self.indexWhere(p), msg)
		/** Returns `this.indexWhere(p, from)`, adding an assertion that the result is not negative (a satisfying element
		  * has been found).
		  * @param p    a function applied consecutively to all elements, starting with index `from`,
		  *             until satisfied.
		  * @param from an index from which to start checking; elements if lower indices are not considered.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertIndexWhere(p :X => Boolean, from :Int, msg: => String) :Int =
			assertPresent(self.indexWhere(p, from), msg)
		/** Returns `this.lastIndexWhere(p)`, adding an assertion that the result is not negative (a satisfying element
		  * has been found).
		  * @param p   a function applied consecutively to all elements in a decreasing order of indices,
		  *            starting with the one at position `end`, until satisfied.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexWhere(p :X => Boolean, msg: => String) :Int =
			assertPresent(self.lastIndexWhere(p), msg)
		/** Returns `this.lastIndexWhere(p, end)`, adding an assertion that the result is not negative
		  * (a satisfying element has been found).
		  * @param p   a function applied consecutively to all elements in a decreasing order of indices,
		  *            starting with the one at position `end`, until satisfied.
		  * @param end the upper, inclusive bound on the returned index; elements after this index are not checked.
		  * @param msg  an error message given to `assert`.
		  */
		@inline def assertLastIndexWhere(p :X => Boolean, end :Int, msg: => String) :Int =
			assertPresent(self.lastIndexWhere(p, end), msg)

		@inline private[slang] def assertPresent(i :Int, msg: => String) :Int = {
			assert(i >= 0, msg)
			i
		}

		private[slang] def indexOfErrorMessage(x :X, from :Int) :String =
			"No " + x + " in " + self + (if (from == 0) "." else " at or after index " + from + ".")

		private[slang] def lastIndexOfErrorMessage(x :X, end :Int) :String =
			"No " + x + " in " + self + (if (end == length - 1) "." else " at or before index " + end + ".")

		private[slang] def indexWhereErrorMessage(from :Int) :String =
			"No element satisfying the predicate in " + self +
				(if (from == 0) "." else " at or after index " + from + ".")

		private[slang] def lastIndexWhereErrorMessage(end :Int) :String =
			"No element satisfying the predicate in " + self +
				(if (end == length - 1) "." else " at or before index " + end + ".")


		/** Checks if this sequence is a subsequence of `other`, that is there is a function `f :Int => Boolean` such that
		  * `this == other.zipWithIndex.collect { case (e, i) if f(i) => e }`.
		  */
		def subseqOf(other :scala.collection.Seq[X]) :Boolean = {
			val thisLength = self.knownSize
			val thatLength = other.knownSize
			@tailrec def sublistOf(left :scala.collection.Seq[X], right :scala.collection.Seq[X]) :Boolean =
				left.isEmpty || right.nonEmpty && (
					if (left.head == right.head) sublistOf(left.tail, right.tail)
					else sublistOf(left, right.tail)
				)
			@tailrec def indexedSubseqOf(left :scala.collection.Seq[X], leftIdx :Int,
			                             right :scala.collection.Seq[X], rightIdx :Int) :Boolean =
				leftIdx < 0 || rightIdx >= 0 && (
					if (left(leftIdx) == right(rightIdx)) indexedSubseqOf(left, leftIdx - 1, right, rightIdx - 1)
					else indexedSubseqOf(left, leftIdx, right, rightIdx - 1)
				)
			(thatLength < 0 || thisLength <= thatLength && thisLength >= 0) &&
				((self, other) match {
					case (_ :scala.collection.IndexedSeq[_], _ :scala.collection.IndexedSeq[_]) =>
						indexedSubseqOf(self, thisLength - 1, other, thatLength - 1)
					case (_:LinearSeq[_] | _:Vector[_] | _:PassedArray[_], _:LinearSeq[_] | _:Vector[_] | _:PassedArray[_]) =>
						sublistOf(self, other)
					case (_ :LinearSeq[_], _) =>
						sublistOf(self, other.toList)
					case (_, _ :LinearSeq[_]) =>
						sublistOf(self.toList, other)
					case (_, _) =>
						indexedSubseqOf(
							self to scala.collection.IndexedSeq, thisLength - 1,
							other to scala.collection.IndexedSeq, thatLength - 1
						)
			})
		}

		/** Checks if this sequence is a subsequence of `other`, that is there is a function `f :Int => Boolean` such that
		  * `this == other.zipWithIndex.collect { case (e, i) if f(i) => e }`.
		  */
		def subseqOf[U >: X](other :Array[U]) :Boolean =
			new SeqExtension[U](self).subseqOf(ArraySeq.unsafeWrapArray(other))
	}






	private[oldsql] implicit class classMethods(private val self :Class[_]) extends AnyVal {
		/** True for Java classes which serve as wrappers for Java primitive types (`Integer`, `Character`, etc.). */
		def isBox :Boolean = Unwrapped.contains(self)

		/** True if the argument is a class for a built in value type represented by a Java primitive,
		  * and this class is the Java class used to box it when lifting the argument to a reference type. */
		def isBoxOf(valueClass :Class[_]) :Boolean = Wrappers.get(self).contains(valueClass)

		/** If this class represents a built in value type (a Java primitive type), return the Java class to which
		  * it is auto boxed when a reference type is needed. */
		@throws[UnsupportedOperationException]("if this class is not a built in value type.")
		def boxed   :Class[_] = Wrappers(self)

		/** If this is a Java class to which a Java primitive type is auto boxed, return the class for the primitive type. */
		@throws[UnsupportedOperationException]("if this class is not a box for a value type.")
		def unboxed :Class[_] = Unwrapped(self)

		def innerName  :String  = innerNameOf(self)
		def localName  :String  = localNameOf(self)
		def abbrevName :String  = abbrevNameOf(self)
		def name       :String  = fullNameOf(self)
	}

	private val Wrappers :Map[Class[_], Class[_]] = Map[Class[_], Class[_]](
		classOf[Char]    -> classOf[java.lang.Character],
		classOf[Byte]    -> classOf[java.lang.Byte],
		classOf[Short]   -> classOf[java.lang.Short],
		classOf[Int]     -> classOf[java.lang.Integer],
		classOf[Long]    -> classOf[java.lang.Long],
		classOf[Float]   -> classOf[java.lang.Float],
		classOf[Double]  -> classOf[java.lang.Double],
		classOf[Boolean] -> classOf[java.lang.Boolean]
	).withDefault(c => throw new UnsupportedOperationException("Class " +c.getName + " is not a built in value class."))

	private val Unwrapped :Map[Class[_], Class[_]] =
		Wrappers.map { case (k, v) => (v, k) }.withDefault(
			c => throw new UnsupportedOperationException(
				"Class " + c.getName + " is not a wrapper for a Java primitive type."
			)
		)



	/** Implicit conversion patching any object with methods providing prettified/shortened class names. */
	private[oldsql] implicit class classNameMethods[T](private val self :T) extends AnyVal {

		/** An approximation of the imported type symbol of the class of this object, as it would be referenced
		  * in code. First, the whole package prefix and all trailing '$' characters are dropped.
		  * Then, all escape sequences for special characters which are legal for use in identifiers are unescaped
		  * to their original forms. Then, dropped is the prefix up until and including to the last '$'. If the class
		  * is specialized, its mangled type parameters are resolved and composed in a type parameter list
		  * in Scala's syntax. If the class is anonymous, a simple '.anon' replaces its whole anonymous name section,
		  * and prepended to it is the directly preceding/enclosing class name, that is the inner-most class name
		  * from the non-anonymous prefix. Primitive types are capitalized to their Scala names and arrays are formatted
		  * recursively as 'Array['`innerClassNameOf(element)`']'. This algorithm is a heuristic and can only be used
		  * for informational, debugging purposes, and not for identifiers or in any sort of reflection operations,
		  * as it can fail to produce the correct and unique type representation for a number of reasons. Most notably,
		  * any kind of generic, non-specialized classes will not have any type arguments listed,
		  * and only `@specialized` type parameters of specialized classes will be shown. Use of '$' in a demangled name
		  * will throw it off, as will identifiers quoted in backticks. Finally, for the obvious reason, the name
		  * of the anonymous class is synthetic and the same for all anonymous inner classes of the same enclosing
		  * class/object.
		  */
		@inline def innerClassName: String = innerNameOf(self.getClass)

		/** An approximation of the type name of the class of the given object, as it would appear in code.
		  * It doesn't include the package prefix, but includes the demangled names of all enclosing classes/objects.
		  * The demangling proceeds as follows: first, all trailing '$' characters are dropped.
		  * Then, all escape sequences for special characters which are legal for use in identifiers are unescaped
		  * to their original forms. All individual '$' signs (used in name mangling of inner classes as the separators)
		  * are replaced with a '.', and so is the double '$$' in '$$anon' sequence for anonymous classes. If the class
		  * is specialized, its mangled type parameters are resolved and composed in a type parameter list
		  * in Scala's syntax. Primitive types are capitalized to their Scala names and arrays are formatted recursively
		  * as 'Array['`localClassNameOf(element)`']'. This algorithm is a heuristic and can only be used
		  * for informational, debugging purposes, and not for identifiers or in any sort of reflection operations,
		  * as it can fail to produce the correct and unique type representation for a number of reasons. Most notably,
		  * any kind of generic, non-specialized classes will not have any type arguments listed, and only `@specialized`
		  * type parameters of specialized classes will be shown. Use of '$' in a demangled name will throw it off,
		  * as will identifiers quoted in backticks. Finally, for the obvious reason, the name of the anonymous class
		  * is synthetic.
		  */
		@inline def localClassName :String = localNameOf(self.getClass)

		/** An abbreviated qualified name of the class of this object, demangled to an approximation of how it would
		  * appear in code. All package names are replaced with their first letters, while the class name is demangled
		  * as follows: first, all trailing '$' are dropped and escape sequences
		  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
		  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
		  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
		  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
		  * to their Scala names and arrays are formatted recursively as 'Array['`classNameOf(element)`']'.
		  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
		  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique
		  * type representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
		  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
		  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
		  * Finally, anonymous classes receive synthetic names for the obvious reason.
		  */
		@inline def abbrevClassName :String = abbrevNameOf(self.getClass)

		/** An approximation of the full, qualified and demangled name of the class of this object, as it would
		  * appear in code. Demangling proceeds as follows: first, all trailing '$' are dropped and escape sequences
		  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
		  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
		  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
		  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
		  * to their Scala names and arrays are formatted recursively as 'Array['`classNameOf(element)`']'.
		  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
		  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique
		  * type representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
		  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
		  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
		  * Finally, anonymous classes receive synthetic names for the obvious reason.
		  */
		@inline def className :String = fullNameOf(self.getClass)

		/** Same as the default `Object.toString`, but uses 
		  * `this.`[[net.noresttherein.oldsql.slang.classNameMethods.innerClassName innerClassName]]
		  * instead of `this.getClass.getName`.
		  */
		@inline def toInnerClassString :String =
			if (self == null) "null" else innerClassName + "@" + self.hashCode.toHexString

		/** Same as the default `Object.toString`, but uses 
		  * `this.`[[net.noresttherein.oldsql.slang.classNameMethods.localClassName localClassName]]
		  * instead of `this.getClass.getName`.
		  */
		@inline def toLocalClassString :String =
			if (self == null) "null" else localClassName + "@" + self.hashCode.toHexString

		/** Same as the default `Object.toString`, but uses 
		  * `this.`[[net.noresttherein.oldsql.slang.classNameMethods.abbrevClassName abbrevClassName]]
		  * instead of `this.getClass.getName`.
		  */
		@inline def toAbbrevClassString :String =
			if (self == null) "null" else abbrevClassName + "@" + self.hashCode.toHexString

		/** Same as the default `Object.toString`, but uses 
		  * (printable) `this.`[[net.noresttherein.oldsql.slang.classNameMethods.className className]]
		  * instead of `this.getClass.getName`.
		  */
		@inline def toFullNameString :String =
			if (self == null) "null" else className + "@" + self.hashCode.toHexString

		/** Same as the default `Object.toString`, but uses 
		  * `this.`[[net.noresttherein.oldsql.slang.classNameMethods.innerClassName innerClassName]]
		  * instead of `this.getClass.getName` and the hashCode is compacted to two bytes.
		  */
		@inline def toShortInnerClassString :String =
			if (self == null) "null" else innerClassName + "@" + shortHashString

		/** Same as the default `Object.toString`, but uses 
		  * `this.`[[net.noresttherein.oldsql.slang.classNameMethods.localClassName localClassName]]
		  * instead of `this.getClass.getName` and the hashCode is compacted to two bytes.
		  */
		@inline def toShortLocalClassString :String =
			if (self == null) "null" else localClassName + "@" + shortHashString

		/** Same as the default `Object.toString`, but uses 
		  * `this.`[[net.noresttherein.oldsql.slang.classNameMethods.abbrevClassName abbrevClassName]]
		  * instead of `this.getClass.getName` and the hashCode is compacted to two bytes.
		  */
		@inline def toShortAbbrevClassString :String =
			if (self == null) "null" else abbrevClassName + "@" + shortHashString

		/** Same as the default `Object.toString`, but uses 
		  * (printable) `this.`[[net.noresttherein.oldsql.slang.classNameMethods.className className]]
		  * instead of `this.getClass.getName` and the hashCode is compacted to two bytes.
		  */
		@inline def toShortFullNameString :String =
			if (self == null) "null" else className + "@" + shortHashString

		/** Formats `this.hashCode` compacted to two bytes as a hexadecimal string. */
		def shortHashString :String = {
			val hash = self.hashCode
			(hash ^ (hash >> 16) & 0xffff).toHexString
		}

	}



	/** An approximation of the imported type symbol of the class of the given object, as it would be referenced
	  * in code. First, the whole package prefix and all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * Then, dropped is the prefix up until and including to the last '$'. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * If the class is anonymous, a simple '.anon' replaces its whole anonymous name section, and prepended to it
	  * is the directly preceding/enclosing class name, that is the inner-most class name from the non-anonymous prefix.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`innerClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic and the same
	  * for all anonymous inner classes of the same enclosing class/object.
	  */
	@inline private[oldsql] def innerClassNameOf(o :Any) :String = innerNameOf(o.getClass)

	/** An approximation of the imported type symbol of the given class, as it would be referenced
	  * in code. First, the whole package prefix and all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * Then, dropped is the prefix up until and including to the last '$'. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * If the class is anonymous, a simple '.anon' replaces its whole anonymous name section, and prepended to it
	  * is the directly preceding/enclosing class name, that is the inner-most class name from the non-anonymous prefix.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`innerClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic and the same
	  * for all anonymous inner classes of the same enclosing class/object.
	  */
	@inline private[oldsql] def innerNameOf[C :ClassTag] :String = innerNameOf(classTag[C].runtimeClass)

	/** An approximation of the imported type symbol of the given class, as it would be referenced
	  * in code. First, the whole package prefix and all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * Then, dropped is the prefix up until and including to the last '$'. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * If the class is anonymous, a simple '.anon' replaces its whole anonymous name section, and prepended to it
	  * is the directly preceding/enclosing class name, that is the inner-most class name from the non-anonymous prefix.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`innerClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic and the same
	  * for all anonymous inner classes of the same enclosing class/object.
	  */
	private[oldsql] def innerNameOf(clazz :Class[_]) :String = clazz.getComponentType match {
		case _ if clazz == java.lang.Void.TYPE => "Unit"

		case null =>
			val qualified = clazz.getName
			val len = qualified.length
			val start = qualified.lastIndexOf('.') + 1
			val anon = qualified.indexOf("$$anon", start)
			val end =
				if (anon >= 0) anon
				else if (start == len) len
				     else trimTrailingDollars(qualified)
			var i = start
			val res = new JStringBuilder(end - i + 5) //5 for anon
			while (i < end) qualified.charAt(i) match {
				case '$' =>
					i += 1
					if (qualified.startsWith(specializationPrefix, i))
						i = demangleSpecialization(qualified, i, end, res)
					else {
						val jump = unescape(qualified, i, res)
						if (jump == i) //no escape sequence, individual '$' treated as a class name separator
							res.delete(0, res.length)
						i = jump
					}
				case c => res append c; i += 1
			}
			if (anon >= 0)
				res append ".anon"
			res.toString

		case elem if elem.isPrimitive => "Array[" + elem.getName.capitalize + "]"
		case elem => "Array[" + innerNameOf(elem) + "]"
	}



	/** An approximation of the type name of the class of the given object, as it would appear in code.
	  * It doesn't include the package prefix, but includes the demangled names of all enclosing classes/objects.
	  * The demangling proceeds as follows: first, all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * All individual '$' signs (used in name mangling of inner classes as the separators) are replaced with a '.',
	  * and so is the double '$$' in '$$anon' sequence for anonymous classes. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`localClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic.
	  */
	@inline private[oldsql] def localClassNameOf(obj :Any): String = localNameOf(obj.getClass)

	/** An approximation of the type name of the given class, as it would appear in code.
	  * It doesn't include the package prefix, but includes the demangled names of all enclosing classes/objects.
	  * The demangling proceeds as follows: first, all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * All individual '$' signs (used in name mangling of inner classes as the separators) are replaced with a '.',
	  * and so is the double '$$' in '$$anon' sequence for anonymous classes. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`localClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic.
	  */
	@inline private[oldsql] def localNameOf[C :ClassTag] :String = localNameOf(classTag[C].runtimeClass)

	/** An approximation of the type name of the given class, as it would appear in code.
	  * It doesn't include the package prefix, but includes the demangled names of all enclosing classes/objects.
	  * The demangling proceeds as follows: first, all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * All individual '$' signs (used in name mangling of inner classes as the separators) are replaced with a '.',
	  * and so is the double '$$' in '$$anon' sequence for anonymous classes. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`localClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic.
	  */
	private[oldsql] def localNameOf(clazz :Class[_]) :String = clazz.getComponentType match {
		case _ if clazz == java.lang.Void.TYPE => "Unit"

		case null =>
			val qualified = clazz.getName
			val end = trimTrailingDollars(qualified)
			val start = qualified.lastIndexOf('.') + 1
			val res = new JStringBuilder(end - start)
			demangleClassName(qualified, start, end, res)
			res.toString

		case elem if elem.isPrimitive => "Array[" + elem.getName.capitalize + "]"
		case elem => "Array[" + localNameOf(elem) + "]"
	}



	/** An abbreviated qualified name of the class of the given object, demangled to an approximation of how it would
	  * appear in code. All package names are replaced with their first letters, while the class name is demangled
	  * as follows: first, all trailing '$' are dropped and escape sequences
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
	  * to their Scala names and arrays are formatted recursively as 'Array['`abbrevNameOf(element)`']'.
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	@inline private[oldsql] def abbrevClassNameOf(obj :Any) :String = abbrevNameOf(obj.getClass)

	/** An abbreviated qualified name of the given class, demangled to an approximation of how it would
	  * appear in code. All package names are replaced with their first letters, while the class name is demangled
	  * as follows: first, all trailing '$' are dropped and escape sequences
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
	  * to their Scala names and arrays are formatted recursively as 'Array['`abbrevNameOf(element)`']'.
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	@inline private[oldsql] def abbrevNameOf[C :ClassTag] :String = abbrevNameOf(classTag[C].runtimeClass)

	/** An abbreviated qualified name of the given class, demangled to an approximation of how it would
	  * appear in code. All package names are replaced with their first letters, while the class name is demangled
	  * as follows: first, all trailing '$' are dropped and escape sequences
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
	  * to their Scala names and arrays are formatted recursively as 'Array['`abbrevNameOf(element)`']'.
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	private[oldsql] def abbrevNameOf(clazz :Class[_]) :String = clazz.getComponentType match {
		case _ if clazz == java.lang.Void.TYPE => "Unit"

		case null =>
			val qname = clazz.getName
			val end = trimTrailingDollars(qname)
			val sb = new JStringBuilder(end)
			val start = clazz.getPackage match {
				case null => 0
				case p =>
					val pname = p.getName; val plen = pname.length
					var i = 0
					if (plen > 0) {
						sb append pname.charAt(0)
						i += 1
					}
					while (i < plen) pname.charAt(i) match {
						case '.' => sb append '.' append pname.charAt(i + 1); i += 2
						case _ => i += 1
					}
					if (i > 0) {
						sb append '.'
						i += 1 //skip the '.' separating the package and class name
					}
					i
			}
			demangleClassName(qname, start, end, sb)
			sb.toString

		case c if c.isPrimitive => "Array[" + abbrevNameOf(c).capitalize + "]"
		case c => "Array[" + abbrevNameOf(c) + "]"
	}



	/** An approximation of the full, qualified and demangled name of the class of the given object, as it would appear
	  * in code. Demangling proceeds as follows: first, all trailing '$' are dropped and escape sequences for characters
	  * which are legal for use in identifiers are unescaped. Encoding of type arguments for `@specialized` classes
	  * is resolved and replaced with a parameter list, as it would occur in the code. Finally, all individual '$'
	  * (used in particular for separating names of nested classes) are replaced with '.', as is the double '$$'
	  * of '$$anon' marking an anonymous class. Primitive types are capitalized to their Scala names and arrays
	  * are formatted recursively as 'Array['`fullNameOf(element)`']'. This algorithm is a heuristic and can
	  * only be used for informational, debugging purposes, and not for identifiers or in any sort of reflection
	  * operations, as it can fail to produce the correct and unique type representation for a number of reasons.
	  * Most notably, any kind of generic, non-specialized classes will not have any type arguments listed,
	  * and only `@specialized` type parameters of specialized classes will be shown. Use of '$' in a demangled name
	  * will throw it off, as will identifiers quoted in backticks. Finally, anonymous classes receive synthetic names
	  * for the obvious reason.
	  */
	@inline private[oldsql] def classNameOf(obj :Any) :String = fullNameOf(obj.getClass)

	/** An approximation of the full, qualified and demangled name of the given class, as it would
	  * appear in code. Demangling proceeds as follows: first, all trailing '$' are dropped and escape sequences
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
	  * to their Scala names and arrays are formatted recursively as 'Array['`fullNameOf(element)`']'.
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	@inline private[oldsql] def fullNameOf[T :ClassTag] :String = fullNameOf(classTag[T].runtimeClass)

	/** An approximation of the full, qualified and demangled name of the given class, as it would
	  * appear in code. Demangling proceeds as follows: first, all trailing '$' are dropped and escape sequences
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
	  * to their Scala names and arrays are formatted recursively as 'Array['`fullNameOf(element)`']'.
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	private[oldsql] def fullNameOf(clazz :Class[_]) :String = clazz.getComponentType match {
		case _ if clazz == java.lang.Void.TYPE => "Unit"

		case null =>
			val qname = clazz.getName
			val start = qname.lastIndexOf('.') + 1
			val end = trimTrailingDollars(qname)
			val res = new JStringBuilder(qname.length)
			var i = 0
			while (i < start) {
				res append qname.charAt(i); i += 1
			}
			demangleClassName(qname, start, end, res)
			res.toString

		case elem if elem.isPrimitive => "Array[" + elem.getName.capitalize + "]"
		case elem => "Array[" + fullNameOf(elem) + "]"
	}




	private def trimTrailingDollars(input :String) :Int = {
		var i = input.length - 1
		while (i >= 0 && input.charAt(i) == '$')
			i -= 1
		i + 1
	}


	private def demangleClassName(input :String, offset :Int, end :Int, result :JStringBuilder) :Unit = {
		var i  = offset
		while (i < end) input.charAt(i) match {
			case '$' =>
				i += 1
				if (input.startsWith(specializationPrefix, i))
					i = demangleSpecialization(input, i, end, result)
				else if (input.startsWith("anon", i)) {
					result append "anon"; i += 4
				} else
					i = unescape(input, i, result)

			case c => result append c; i += 1
		}
	}


	private def unescape(input :String, offset :Int, result :JStringBuilder) :Int = {
		var s = escapes.length - 1
		var symbol = ""
		while ({ symbol = escapes(s); !input.startsWith(symbol, offset)}) //escapes has "" at 0 as a guard
			s -= 1
		val name = symbols(s)
		result append name
		offset + symbol.length
	}


	private def demangleSpecialization(input :String, offset :Int, end :Int, result :JStringBuilder) :Int = {
		//inputStartsWith("mc", offset)
		val rollbackPoint = result.length
		result append '['

		def rollback() = {
			result.delete(rollbackPoint, result.length)
			result append '.' //for the '$' starting '$mc'.
			offset //no infinite loop as input doesn't starti with a '$' and "mc" will get normal char treatment
		}
		@tailrec def demangle(pos :Int, params :List[String] = Nil) :Int =
			if (pos == end)  //end of input encoutered before the closing sequence of specialized parameters encoding
				rollback()
			else {
				val param = input.charAt(pos)
				var t = typeParamCodes.length - 1
				while (t >= 0 && typeParamCodes(t) != param)
					t -= 1
				if (t >= 0)
					demangle(pos + 1, typeParamNames(t)::params)
				else if (input.startsWith("$sp", pos) && params.nonEmpty) {
					params foreach { p => result append p append ',' }
					result.delete(result.length - 1, result.length) //last ','
					result append ']'
					pos + 3
				} else  //illegal specialization mangling, revert to standard behaviour
					rollback()
			}
		demangle(offset + 2)
	}



	private[this] val escapes = Array(//empty string at start as an old fashioned guard which maps to '.' at the same time
		"", "tilde", "bang", "at", "hash", "percent", "up", "amp", "times", "minus", "plus", "eq", "less", "greater",
		"qmark", "div", "bar", "bslash", "colon"
	)
	private[this] val symbols = Array(
		".", "~", "!", "@", "#", "%", "^", "&", "*", "-", "+", "=", "<", ">", "?", "/", "|", "\\", ":"
	)

	private[this] val typeParamCodes = Array('S', 'V', 'Z', 'C', 'F', 'B', 'D', 'J', 'I')
	private[this] val typeParamNames = Array("Short", "Unit", "Bool", "Char", "Float", "Byte", "Double", "Long", "Int")

	private[this] final val specializationPrefix = "mc"






	/** Implicit conversion to a lazy value of type T which can be lifted to an Option[T] by one of its methods. */
	private[oldsql] implicit class ProvidingAndUnless[T](expr: =>T) {

		/** Returns Some(this) if passed condition is true, None otherwise;
		  * `this` is passed by name and evaluated only if condition is true!
		  */
		@inline def providing(condition :Boolean) :Option[T] =
			if (condition) Some(expr) else None

		/** Returns Some(this) if passed condition is true for this, None otherwise;
		  * `this` is evaluated once, before passing its value to the condition!
		  */
		@inline def providing(condition :T => Boolean) :Option[T] = {
			val x = expr
			if (condition(x)) Some(x) else None
		}


		/** Returns Some(this) if passed condition is false, None otherwise;
		  * `this` is passed by name and evaluated only if condition is false!
		  */
		@inline def unless(condition :Boolean) :Option[T] =
			if (!condition) Some(expr) else None


		/** Returns Some(this) if passed condition is false for this, None otherwise;
		  * `this` is evaluated once, before passing its value to the condition!
		  */
		@inline def unless(condition :T => Boolean) :Option[T] = {
			val x = expr
			if (!condition(x)) Some(x) else None
		}

	}



	private[oldsql] implicit class IfTrueAndIfFalse(private val condition :Boolean) extends AnyVal {
		@inline def ifTrue[T](expr: => T) :Option[T] = if (condition) Some(expr) else None

		@inline def ifFalse[T](expr: => T) :Option[T] = if (!condition) Some(expr) else None

		@inline def thenMaybe[T](expr : => Option[T]) :Option[T] = if (condition) expr else None

		@inline def otherwiseMaybe[T](expr : => Option[T]) :Option[T] = if (!condition) expr else None
	}



	private[oldsql] implicit class OptionGuardExtension[T](opt : => Option[T]) {
		@inline def orNoneIf(expr :Boolean) :Option[T] =
			if (expr) None else opt

		@inline def orNoneUnless(expr :Boolean) :Option[T] =
			if (expr) opt else None

		@inline def mapOrElse[X](expr : T => X, default : => X) :X = opt match {
			case Some(t) => expr(t)
			case _ => default
		}
	}






	/** Extension casting methods for any type `X` putting constraints on the target type in relation to `X`,
	  * intended to prevent devious bugs introduced by refactors changing the type of the cast expression
	  * (which would not produce any compiler warning with `asInstanceOf`).
	  */
	implicit class saferCasting[X](private val self :X) extends AnyVal {
		/** Promotes any value to its runtime class representation. */
		@inline def asAnyRef :AnyRef = self.asInstanceOf[AnyRef]

		/** Casts this expression to its subtype `T`. */
		@inline def downcastTo[T <: X] :T = self.asInstanceOf[T]

		/** A safer casting expression which, in addition to the target type, accepts also the type of the cast
		  * expression itself (`this`).
		  * Providing both is a defence against inadvertent casting from a wrongly presumed source type and,
		  * more importantly, against an expression changing type silently due to a refactor.
		  * @tparam U the (super)type of `this` expression.
		  * @tparam Y the target type of the expression after casting.
		  */
		@inline def castFrom[U >: X, Y] :Y = self.asInstanceOf[Y]

//		/** A safer casting expression which, in addition to the target type, accepts also the type of the cast
//		  * expression itself (`this`). Both types are given as a single argument function `X => Y`,
//		  * with `X` being the type to which `this` must conform, and `Y` the desired target type.
//		  * Providing both is a defence against inadvertent casting from a wrongly presumed source type and,
//		  * more importantly, against an expression changing type silently due to a refactor.
//		  */
//		@inline def castWith[F <: X => Any](implicit function :ReturnTypeOf[F]) :function.Return =
//			self.asInstanceOf[function.Return]

		/** A safer casting expression intended for cases where the cast is an optimisation meant to
		  * eliminate the re-creation of a composite object solely to change its type signature
		  * (such as with phantom type parameters), while the implementation is reusable for the target type.
		  * It accepts an (unused) function re-creating the object `Y` in a type safe manner, which both
		  * serves as an illustration and documentation of why the cast is safe, and specifies the target type.
		  */
		@inline def castAsWith[Y](like : => X => Y) :Y = self.asInstanceOf[Y]

		/** Applies the given function to `this` if `this.isInstanceOf[T]`, returning the result in an [[Option]].
		  * {{{
		  *     animal.ifInstanceOf[Cat](_.meow)
		  * }}}
		  * Note that as with `isInstanceOf`, this method is implemented in terms of the runtime ''class'',
		  * and the conformance of erased type parameters is not checked.
		  * @return an instance of a SAM type accepting a function `[X] T=>X`, and returning an `Option[X]`.
		  */
		@inline def ifInstanceOf[T] = new IfInstanceOf[T](self)

		/** Returns `this` as a [[net.noresttherein.oldsql.collection.Opt.Got Got]]`(this)` if `this.isInstanceOf[T]`,
		  * or [[net.noresttherein.oldsql.collection.Opt.Lack Lack]] otherwise.
		  */
		@inline def asInstanceOpt[T](implicit tag :ClassTag[T]) :Option[T] = tag.unapply(self)
	}

	class IfInstanceOf[T](private val self :Any) extends AnyVal {
		@inline def apply[X](f :T => X)(implicit tag :ClassTag[T]) :Option[X] =
			if (tag.runtimeClass.isInstance(self)) Some(f(self.asInstanceOf[T]))
			else None
	}




	/** Extension casting methods for the single type parameter of a higher type,
	  * preserving the original type constructor.
	  */
	implicit class castTypeParam[T[A], X](private val self :T[X]) extends AnyVal {
		/** Casts the type parameter of this expression's type, preserving its type constructor. */
		@inline def castParam[A] :T[A] = self.asInstanceOf[T[A]]
	}

	/** Extension downcasting methods for the single type parameter of a higher type,
	  * preserving the original type constructor.
	  */
	implicit class downcastTypeParam[T[A <: X], X](private val self :T[X]) extends AnyVal {
		/** Casts down the type parameter of this expression's type. */
		@inline def downcastParam[A <: X] :T[A] = self.asInstanceOf[T[A]]
	}

	/** Extension casting methods for the type parameters of a higher type,
	  * preserving the original, binary type constructor.
	  */
	implicit class cast2TypeParams[T[_, _], X, Y](private val self :T[X, Y]) extends AnyVal {
		/** Casts both type parameters of this expression's type, preserving its type constructor. */
		@inline def castParams[A, B] :T[A, B] = self.asInstanceOf[T[A, B]]

		/** Casts the first type parameter of this expression's type,
		  * preserving its type constructor and the second parameter.
		  */
		@inline def castParam1[A] :T[A, Y] = self.asInstanceOf[T[A, Y]]

		/** Casts the second type parameter of this expression's type,
		  * preserving its type constructor and the first parameter.
		  */
		@inline def castParam2[B] :T[X, B] = self.asInstanceOf[T[X, B]]
	}

	/** Extension downcasting methods for the type parameters of a higher type,
	  * preserving the original, binary type constructor.
	  */
	implicit class downcast2TypeParams[T[_1 <: X, _2 <: Y], X, Y](private val self :T[X, Y]) extends AnyVal {
		/** Casts down both type parameters of this expression's type. */
		@inline def downcastParams[A <: X, B <: Y] :T[A, B] = self.asInstanceOf[T[A, B]]

		/** Casts down the first type parameter of this expression's type. */
		@inline def downcastParam1[A <: X] :T[A, Y] = self.asInstanceOf[T[A, Y]]

		/** Casts down the second type parameter of this expression's type. */
		@inline def downcastParam2[B <: Y] :T[X, B] = self.asInstanceOf[T[X, B]]
	}

	/** Extension casting methods for the type parameters of a higher type,
	  * preserving the original, ternary type constructor.
	  */
	implicit class cast3TypeParams[T[_, _, _], X, Y, Z](private val self :T[X, Y, Z]) extends AnyVal {
		/** Casts the type parameters of this expression's type, preserving its type constructor. */
		@inline def castParams[A, B, C] :T[A, B, C] = self.asInstanceOf[T[A, B, C]]

		/** Casts the first type parameter of this expression's type,
		  * preserving the type constructor and the rest of the parameters unchanged.
		  */
		@inline def castParam1[A] :T[A, Y, Z] = self.asInstanceOf[T[A, Y, Z]]

		/** Casts the second type parameter of this expression's type,
		  * preserving the type constructor and the rest of the parameters unchanged.
		  */
		@inline def castParam2[B] :T[X, B, Z] = self.asInstanceOf[T[X, B, Z]]

		/** Casts the third type parameter of this expression's type,
		  * preserving the type constructor and the rest of the parameters unchanged.
		  */
		@inline def castParam3[C] :T[X, Y, C] = self.asInstanceOf[T[X, Y, C]]
	}

	/** Extension casting methods for the type parameters of a higher type,
	  * preserving the original, ternary type constructor.
	  */
	implicit class downcast3TypeParams[T[_1 <: X, _2 <: Y, _3 <: Z], X, Y, Z]
	                                  (private val self :T[X, Y, Z]) extends AnyVal
	{
		/** Casts own all type parameters of this expression's type. */
		@inline def downcastParams[A <: X, B <: Y, C <: Z] :T[A, B, C] = self.asInstanceOf[T[A, B, C]]

		/** Casts down the first type parameter of this expression's type,
		  * preserving the type constructor and the rest of the parameters unchanged.
		  */
		@inline def downcastParam1[A <: X] :T[A, Y, Z] = self.asInstanceOf[T[A, Y, Z]]

		/** Casts down the second type parameter of this expression's type,
		  * preserving the type constructor and the rest of the parameters unchanged.
		  */
		@inline def downcastParam2[B <: Y] :T[X, B, Z] = self.asInstanceOf[T[X, B, Z]]

		/** Casts down the third type parameter of this expression's type,
		  * preserving the type constructor and the rest of the parameters unchanged.
		  */
		@inline def downcastParam3[C <: Z] :T[X, Y, C] = self.asInstanceOf[T[X, Y, C]]
	}



}
