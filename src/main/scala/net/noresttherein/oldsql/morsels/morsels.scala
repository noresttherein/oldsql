package net.noresttherein.oldsql

import net.noresttherein.oldsql.collection.{PassedArray, Unique}
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq






/**
  * @author Marcin Mościcki
  */
package object morsels {

	final class LUB[-X, -Y, +U] private ()

	object LUB {
		implicit def lub[X] :LUB[X, X, X] = new LUB[X, X, X]
	}


	/** Mixin trait implementing equality as equality of classes of the compared objects.
	  * Extended by singleton-like classes which prefer to avoid the additional overhead of `object` definitions.
	  * The main difference lies in serialization, as even if only one instance is created directly by the application,
	  * serialization and deserialization can introduce others.
	  */
	private[oldsql] trait Stateless {
		override def equals(that :Any) = that.getClass == getClass
		override def hashCode :Int = getClass.hashCode
	}



	@throws[IllegalArgumentException]("in case precedes relation has a cycle and failOnCycle is true.")
	private[oldsql] def topologicalSort[T](elems :Seq[T], failOnCycle :Boolean = true)
	                                      (precedes :(T, T) => Boolean) :Seq[T] =
		if (elems.sizeIs <= 1)
			elems
		else {
			var sorted = PassedArray.empty[T]
			val indexed = elems match {
				case ok :IndexedSeq[T] => ok
				case _ => elems to PassedArray
			}
			val count = indexed.length
			val visited = new Array[Int](count)
			def dfs(curr :Int, tail :PassedArray[T]) :PassedArray[T] =
				visited(curr) match {
					case 0 =>
						var res = tail
						visited(curr) = 1
						var j = count
						while (j > 0) {
							j -= 1
							if (curr != j && precedes(indexed(curr),  indexed(j)))
								res = dfs(j, res)
						}
						visited(curr) = 2
						indexed(curr) +: res
					case 1 if failOnCycle =>
						throw new IllegalArgumentException(
							"A cycle involving " + indexed(curr) + " detected among " + elems + "."
						)
					case _ => tail
				}
			var i = count
			while (i > 0) {
				i -= 1
				sorted = dfs(i, sorted)
			}
			sorted
		}

	/** Performs a weighted topological sort of `elems`. Consider a complete graph with edge weight
	  * defined by `precedence` function. Let the weight of a path in that graph be defined as the minimum edge weight
	  * on the path. This method sorts `elems` in such a way, that if `a` appears before `b`, then the maximum weight
	  * of all paths from `a` to `b` is not lesser than the maximum weight of all paths from `b` to `a`.
	  * Intuitively, weight of a path describes the difficulty of breaking it, and
	  * 'effective precedence' of `a` over `b` is the difficulty of completely disconnecting `a` from `b`.
	  *
	  * More formally,
	  *
	  * '''Contract'''
	  *
	  * let
	  *   1. `maxPrecedence == max(precedence(a, b))` for `a, b` in `elems`,
	  *   1. `powerSet` be the infinite (if `elems.nonEmpty`) set of all finite sequences `p` with elements from `elems`,
	  *   1. `weight(path :Seq[T])` be `min(precedence(path(i), path(j)) for 0 <= i <= j < path.length`,
	  *   1. `weight(a :T, b :T)` be `max(weight(a +: (p :+ b))) for p in powerSet`,
	  * Then the result is a permutation `p` of `elems` such that `weight(p(i), p(j)) >= weight(p(j), p(i))`
	  * for all `i <= j`.
	  *
	  * The implementation leverages the fact that regular topological sort is stable, i.e. doesn't change the order
	  * of any two elements if there is no path between them in either direction. It recursively performs topological
	  * sort of the input, specifying that an edge between `a` and `b` exists if `precedence(a, b) >= threshold`,
	  * for threshold taking increasing values from `1` to `maxThreshold`.
	  *
	  * Proof:
	  *
	  * '''Lemma''' For any `n >= 1`, let `p_n` be the permutation of `elems` after sorting with increasing
	  * thresholds `1..n`, and let `max(preceding(a, b)) <= n`. Then for all `0 <= i <= j < elems.length`,
	  * `weight(p_n(i), p_n(j)) >= weight(p_n(j), p_n(i))`.
	  *
	  * If the lemma is true, and `maxPrecedence` == `max(precedence(a, b))`, then the required post condition
	  * immediately follows.
	  *   1. When `n = 1`, the problem is reduced to an unweighted topological sort, and lemma is proved
	  *      by the correctness of DFS-based standard topological sort algorithm:
	  *      `weight(a, b) == 1` ''iff'' there is a path from `a` to `b`, so if `a` is sorted before `b`,
	  *      then existence of a path from `b` to `a` (`weight(b, a) == 1`) implies the existence of a path from
	  *      `a` to `b` (`weight(a, b) == 1`), proving the inequality.
	  *   1. Let us assume that the lemma holds for some `n >= 1` and that `max(precedence(a, b)) <= n + 1`.
	  *      Let `0 <= i <= j < elems.length, a == p_n+1(i), b == p_n+1(j)`. From the properties of topological sort,
	  *      existence of a path from `b` to `a`, consisting only of links of `precedence == n + 1`, implies
	  *      the existence of such a path from `a` to `b`, hence if `weight(b, a) == n + 1` then
	  *      `weight(a, b) >= weight(b, a)`. Let us then assume then that `weight(b, a) <= n`. Then, from the stability
	  *      of topological sort, `a` must appear before `b` in `p_n`, and we have `weight(a, b) >= weight(b, a)`
	  *      from ''lemma(n)''.
	  *
	  * @param elems         a sequence to sort.
	  * @param maxPrecedence the maximum value returned by `precedence` for any element pair from `elems`.
	  * @param precedence    a weight function defining the precedence of first argument over the second argument,
	  *                      taking values from `[0..maxPrecedence]` range.
	  */
	private[oldsql] def weightedTopologicalSort[T](elems :Seq[T], maxPrecedence :Int)(precedence :(T, T) => Int) :Seq[T] =
		if (elems.sizeIs <= 1)
			elems
		else {
			@tailrec def sortByDecreasingPriority(elems :Seq[T], threshold :Int) :Seq[T] =
				if (threshold > maxPrecedence)
					elems
				else {
					val sortedToThreshold = topologicalSort(elems, false)(precedence(_, _) >= threshold)
					sortByDecreasingPriority(sortedToThreshold, threshold - 1)
				}
			sortByDecreasingPriority(elems, 1)
		}


	/** Performs a weighted topological sort of `elems`. Consider a complete graph with edge weight
	  * defined by `precedence` function. Let the weight of a path in that graph be defined as the minimum edge weight
	  * on the path. This method sorts `elems` in such a way, that if `a` appears before `b`, then the maximum weight
	  * of all paths from `a` to `b` is not greater than the maximum weight of all paths from `b` to `a`.
	  * Intuitively, it is a topological sort of a graph in which an edge `a->b` exists if `precedence(a, b) >= 0`,
	  * which resolves cycle conflicts in favour of higher `precedence` values.
	  *
	  * This method determines the maximum weight present between any two elements and calls
	  * the overloaded `weightedTopologicalSort` method, passing the former as an argument.
	  * @param elems         a sequence to sort.
	  * @param precedence    a weight function defining the precedence of first argument over the second argument,
	  *                      taking values from `[0..maxPrecedence]` range.
	  */
	private[oldsql] def weightedTopologicalSort[T](elems :Seq[T])(precedence :(T, T) => Int) :Seq[T] =
		if (elems.isEmpty)
			elems
		else {
			val maxPrecedence = elems.view.flatMap { a => elems.map(precedence(a, _)) }.max
			weightedTopologicalSort(elems, maxPrecedence)(precedence)
		}




	private[oldsql] def permutation[T](input :IndexedSeq[T])(precedes :(T, T) => Boolean) :IndexedSeq[Int] = {
		val sorted = topologicalSort(input)(precedes) to Unique
		input.map(sorted.sureIndexOf)
	}

	/** Creates a permutation of indices `0..all.size-1` such that applying it to `all` yields
	  * a sequence `reordered` in which `selected` form a (non consecutive) subsequence.
	  * More precisely,
	  * {{{
	  *     permutation.map(all(_)).filter(selected.contains) == selected)
	  * }}}
	  * All elements in `selected` must be also present in `all`.
	  */
	private[oldsql] def superPermutation[T](all :Unique[T], selected :Unique[T]) :IndexedSeq[Int] =
		if (all.size == selected.size)
			selected.toIndexedSeq.map(all.sureIndexOf)
		else {
			val indices = selected.toIndexedSeq.map(all.sureIndexOf).sorted
			IndexedSeq.tabulate(all.size) { n =>
				selected.indexOf(all(n)) match {
					case -1 => n
					case i => indices(i)
				}
			}
	}

	private[oldsql] def validatePermutation(permutation :IndexedSeq[Int], expect :Int) :Unit =
		if (permutation.length != expect)
			throw new IllegalArgumentException(
				"Length of permutation " + permutation + " (" + permutation.length +
				") does not match the expected number of items " + expect + "."
			)
		else if (permutation.sorted != permutation.indices)
			throw new IllegalArgumentException(
				permutation.toString + " is not a valid permutation of " + expect + " values."
			)

	private[oldsql] def inversePermutation(permutation :IndexedSeq[Int]) :IndexedSeq[Int] =
		if (permutation.isEmpty)
			permutation
		else try {
			val inverse = new Array[Int](permutation.length)
			var i = inverse.length
			while (i > 0) {
				i -= 1
				inverse(permutation(i)) = i
			}
			ArraySeq.unsafeWrapArray(inverse)
		} catch {
			case e :IndexOutOfBoundsException =>
				throw new IllegalArgumentException(permutation.toString + " is not a valid permutation", e)
		}

}






package morsels {
	/** A factory of $Res, acting like a function `((`$X`*) => `$Y`) => `$Res, where `(`$X`*)` denotes the same argument
	  *  repeated any number of times between 1 and 20. The function passed to any of `apply` methods of this object
	  * is applied to the same $X instance given to this applicator on creation. All these methods are thus
	  * equivalent, but this redundancy allows to use the shorthand syntax for lambda functions,
	  * simply using `_` for the argument as many times as required in the expression.
	  * @define Res `Res`
	  * @define X `X`
	  * @define Y `Y`
	  */
	class MultiApplicator[+X, -Y, +Res] private (arg :X, result :Y => Res) extends ((X => Y) => Res) {
		def apply(f :X => Y) :Res = result(f(arg))
		def apply(f :(X, X) => Y) :Res = result(f(arg, arg))
		def apply(f :(X, X, X) => Y) :Res = result(f(arg, arg, arg))
		def apply(f :(X, X, X, X) => Y) :Res = result(f(arg, arg, arg, arg))
		def apply(f :(X, X, X, X, X) => Y) :Res = result(f(arg, arg, arg, arg, arg))
		def apply(f :(X, X, X, X, X, X) => Y) :Res = result(f(arg, arg, arg, arg, arg, arg))
		def apply(f :(X, X, X, X, X, X, X) => Y) :Res = result(f(arg, arg, arg, arg, arg, arg, arg))
		def apply(f :(X, X, X, X, X, X, X, X) => Y) :Res = result(f(arg, arg, arg, arg, arg, arg, arg, arg))
		def apply(f :(X, X, X, X, X, X, X, X, X) => Y) :Res = result(f(arg, arg, arg, arg, arg, arg, arg, arg, arg))
		def apply(f :(X, X, X, X, X, X, X, X, X, X) => Y) :Res =
			result(f(arg, arg, arg, arg, arg, arg, arg, arg, arg, arg))

		def apply(f :(X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
			result(f(arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg))

		def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
			result(f(arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg))

		def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
			result(f(arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg))

		def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
			result(f(arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg))

		def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
			result(f(arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg))

		def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
			result(f(arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg))

		def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
			result(f(arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg))

		def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
			result(f(arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg))

		def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
			result(f(arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg))

		def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
			result(f(arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg, arg))
	}


	private[oldsql] object MultiApplicator {
		def apply[X, Y, Res](arg :X)(result :Y => Res) :MultiApplicator[X, Y, Res] = new MultiApplicator(arg, result)
	}
}