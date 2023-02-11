package net.noresttherein.oldsql

import net.noresttherein.oldsql.collection.PassedArray






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
							if (curr != j && precedes(elems(curr),  elems(j)))
								res = dfs(j, res)
						}
						visited(curr) = 2
						indexed(curr) +: res
					case 1 if failOnCycle =>
						throw new IllegalArgumentException(
							"A cycle involving " + elems(curr) + " detected among " + elems + "."
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
	  * of all paths from `a` to `b` is not greater than the maximum weight of all paths from `b` to `a`.
	  * Intuitively, it is a topological sort of a graph in which an edge `a->b` exists if `precedence(a, b) >= 0`,
	  * which resolves cycle conflicts in favour of higher `precedence` values.
	  *
	  * More formally, let
	  *   - `powerSet` be the infinite (if `elems.nonEmpty`) set of all finite sequences with elements from `elems`;
	  *   - `weight(path :Seq[T])` be `min(precedence(path(i), path(j)) for 0 <= i <= j < path.length`;
	  *   - `weight(a :T, b :T)` be `max(weight(a +: (p :+ b))) for p in powerSet`
	  * Then the result is a permutation `p` of `elems` such that `weight(p(i), p(j)) <= weight(p(j), p(i))` for `i <= j`.
	  *
	  * The implementation leverages the fact that regular topological sort is stable, i.e. doesn't change the order
	  * of any two elements if there is no path between them in either direction. It recursively performs topological
	  * sort of the input, specifying that an edge between `a` and `b` exists if `precedence(a, b) >= threshold`,
	  * for threshold taking decreasing values from `maxPrecedence` to `1`.
	  * @param elems         a sequence to sort.
	  * @param maxPrecedence the maximum value returned by `precedence` for any element pair from `elems`.
	  * @param precedence    a weight function defining the precedence of first argument over the second argument,
	  *                      taking values from `[0..maxPrecedence]` range.
	  */
	private[oldsql] def weightedTopologicalSort[T](elems :Seq[T], maxPrecedence :Int)(precedence :(T, T) => Int) :Seq[T] =
		if (elems.sizeIs <= 1)
			elems
		else {
			def sortByDecreasingPriority(elems :Seq[T], threshold :Int) :Seq[T] =
				if (threshold <= 0)
					elems
				else {
					val sortedToThreshold = topologicalSort(elems, false)(precedence(_, _) >= threshold)
					sortByDecreasingPriority(sortedToThreshold, threshold - 1)
				}
			sortByDecreasingPriority(elems, maxPrecedence)
		}

	private[oldsql] def weightedTopologicalSort[T](elems :Seq[T])(precedence :(T, T) => Int) :Seq[T] = {
		val maxPrecedence = elems.view.flatMap { a => elems.map(precedence(a, _)) }.max
		weightedTopologicalSort(elems, maxPrecedence)(precedence)
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