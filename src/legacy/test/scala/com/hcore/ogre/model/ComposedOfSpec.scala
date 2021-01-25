package com.hcore.ogre.model

import com.hcore.ogre.model.ComposedOf.{ComposableFrom, DecomposableTo}
import org.scalatest.{FlatSpec, Matchers}

class ComposedOfSpec extends FlatSpec with Matchers {

	"ComposedOf" should "have implicit value for T ComposedOf T" in {
		val composition = implicitly[ComposableFrom[Seq[Int], Seq[Int]]]
		val decomposition = implicitly[DecomposableTo[Seq[Int], Seq[Int]]]
		val composedOf = implicitly[Seq[Int] ComposedOf Seq[Int]]

		composedOf.composition should equal(composition)
		composedOf.decomposition should equal(decomposition)

		composedOf.composition.compatibleWith(composition) should equal(true)
		composedOf.decomposition.compatibleWith(decomposition) should equal(true)
	}

	"ComposedOf" should "have implicit value for Option[T] ComposedOf T" in {
		val composition = implicitly[ComposableFrom[Option[Int], Int]]
		val decomposition = implicitly[DecomposableTo[Option[Int], Int]]
		val composedOf = implicitly[Option[Int] ComposedOf Int]

		composedOf.composition should equal(composition)
		composedOf.decomposition should equal(decomposition)
		composedOf.composition.compatibleWith(composition) should equal(true)
		composedOf.decomposition.compatibleWith(decomposition) should equal(true)
	}


	"ComposedOf" should "have implicit value for Iterable[T] ComposedOf T" in {
		val composition = implicitly[ComposableFrom[Iterable[Int], Int]]
		val decomposition = implicitly[DecomposableTo[Iterable[Int], Int]]
		val composedOf = implicitly[Iterable[Int] ComposedOf Int]
		val second = implicitly[Iterable[Int] ComposedOf Int]

		composedOf should equal(second)
		composedOf.compatibleWith(second) should equal(true)
//		composedOf.composition should equal(composition)
		composedOf.decomposition should equal(decomposition)
		composedOf.composition.compatibleWith(composition) should equal(true)
		composedOf.decomposition.compatibleWith(decomposition) should equal(true)
	}

	"ComposedOf" should "have implicit value for Seq[T] ComposedOf T" in {
		val composition = implicitly[ComposableFrom[Seq[Int], Int]]
		val decomposition = implicitly[DecomposableTo[Seq[Int], Int]]
		val composedOf = implicitly[Seq[Int] ComposedOf Int]

		composedOf.decomposition should equal(decomposition)

		composedOf.composition.compatibleWith(composition) should equal(true)
		composedOf.decomposition.compatibleWith(decomposition) should equal(true)
	}


	"ComposedOf" should "have compatible implicit values for different iterables" in {
		val seq = implicitly[Seq[Int] ComposedOf Int]
		val set = implicitly[Set[Int] ComposedOf Int]
		val sec = implicitly[Seq[Int] ComposedOf Int]


		seq.composition.compatibleWith(set.composition) should equal(true)
		seq.decomposition.compatibleWith(set.decomposition) should equal(true)
		seq.composition.compatibleWith(sec.composition) should equal(true)
		seq.decomposition.compatibleWith(sec.decomposition) should equal(true)
		seq.compatibleWith(sec) should equal(true)
		seq.decomposition should equal(set.decomposition)
	}

	"ComposedOf" should "have non-equal implicit values for different iterables" in {
		val seq = implicitly[Seq[Int] ComposedOf Int]
		val set = implicitly[Set[Int] ComposedOf Int]

		(seq == set) should equal(false)
		(seq.composition == set.composition) should equal(false)
	}


}
