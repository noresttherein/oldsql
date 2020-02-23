package com.hcore.clientapi

import com.hcore.ogre.model.ComposedOf.{ComposableFrom, DecomposableTo}
import com.hcore.ogre.model.{ComposedOf, Reference}
import com.hcore.ogre.morsels.implicits.Not


object Playground extends App {

	class Owner {
		case class Comp(value :Int)

		val _1 = new Comp(1)
		val _2 = new Comp(2)
	}

	val owner = new Owner
	def other  :owner.type = owner

	def takesComp[O<:Owner](comp :O#Comp) :Int = comp.value

	takesComp[owner.type](other._1)

//	val res = ReferenceMapping.Test(ComponentValues(Reference((1, 2)), ReferenceMapping.Test :ReferenceMapping.Test.type))
//	System.err.println(implicitlyres)


	//	object Test extends StaticMapping[Reference[(Int, Int)]] with ReferenceMapping[(Int, Int), Reference[(Int, Int)]] {
	//		val first = column("first", _.get._1)
	//		val second = column("second", _.get._2)
	//
	//		override def assemble(implicit res: Values): Reference[(Int, Int)] = {
	//			System.err.println(implicitly"Test.assemble")
	//			Reference((first: Int, second: Int))
	//		}
	//	}
	//	def unapply[]

	implicit class FetchExtension[T](ref :Reference[T]) {
		def fetch[E](implicit composition :T ComposedOf E) :E = composition(ref.get).head
//		def fetch :E = composition.items(ref.get).head
	}

//	implicit def valueOfRef[T, E](ref :Reference[T])(implicit compose :T ConsistsOf E) :E =
//		ref.fetch
	implicitly[Not[Seq[Int]=:=Iterable[Int]]]
	System.err.println(implicitly[DecomposableTo[Iterable[Int], Int]])
	System.err.println(implicitly[ComposableFrom[Iterable[Int], Int]])
	System.err.println(implicitly[ComposedOf[Iterable[Int], Int]])
	System.err.println(implicitly[DecomposableTo[Seq[Int], Int]])
	System.err.println(implicitly[ComposableFrom[Seq[Int], Int]])
	System.err.println(implicitly[ComposedOf[Seq[Int], Int]])
	System.err.println(implicitly[DecomposableTo[Option[Int], Int]])
	System.err.println(implicitly[ComposableFrom[Option[Int], Int]])
	System.err.println(implicitly[ComposedOf[Option[Int], Int]])
	System.err.println(implicitly[DecomposableTo[Int, Int]])
	System.err.println(implicitly[ComposableFrom[Int, Int]])
	System.err.println(implicitly[ComposedOf[Int, Int]])

	System.err.println(implicitly[ComposedOf[Int, _]])
	System.err.println(implicitly[ComposedOf[Seq[Int], _]])
	System.err.println(implicitly[ComposedOf[Option[Int], _]])

	Some(10).forall(_>0)

	implicit def valueOfIterable[T](ref :Reference[Iterable[T]]) :T = ref.fetch[T]
	
	implicit def valueOfRef[T](ref :Reference[T]) :T = ref.fetch[T]
	
	implicit def valueOfOpt[T](ref :Reference[Option[T]]) :T = ref.fetch[T]

	class A
	object A {
		implicit def toB(a :A) :B = new B
	}

	class B {
		def hello = "hello"
	}

	val a = new A

	a.hello

//	trait G[+X]
//	class H[+X, Y] extends G[X]
//	class F extends G[Int]
//	def f[A<:G[X], X<:Int](g :A) = g
//
//	val h: H[Int, String] = ???
//	val g :F = ???
//	f(g)
//	f(h)

//	implicit def fetching[T, E](ref :Reference[T])(implicit composition :T ConsistsOf E) = new FetchExtension(ref)

//	val ref = Reference(1)
//
//	val x = ref.fetch
//	System.err.println(implicitlyx)
//
//	val seqRef = Reference(Seq(2, 1))
//
//	val sx :Int = seqRef.fetch[Int]
//	System.err.println(implicitlysx)

/*
	case class Target(prop :Int)

	class Subtarget(prop :Int) extends Target(prop) {
		def subprop = "sub"+prop
	}

	val seqRef = Reference(Seq(new Target(2)))
	System.err.println(implicitlyseqRef.prop +", "+ seqRef.fetch)


	val optRef = Reference(Some(Target(3)))
	System.err.println(implicitlyoptRef.prop +", "+optRef.fetch)

	val ref = Reference(Target(3))
	System.err.println(implicitlyref.prop +", "+ ref.fetch)

//	val subref = Reference(new Subtarget(4) :Target)
//	System.err.println(implicitlyref.fetch[Subtarget].subprop)

	case class Path(path :String) {
		def +=(node :String) = new Path(path+"/"+node)
	}
	class RootPath(path :String) extends Path("/"+path)
	
	trait Observer[T] {
		def apply(value :T) :Unit
	}
	object RootObserver extends Observer[RootPath] {
		override def apply(value: RootPath): Unit = System.err.println(implicitly"observed:"+value)
	}
	
	trait Resource[+T<:Path] {
		def path :T
	}

	case class ObservedResource[T<:Path](path :T, observer :Observer[T]) extends Resource[T]

	val root = ObservedResource(new RootPath(""), RootObserver)
	
	def create(under :Resource[Path], name :String) :Path = under match {
		case ObservedResource(path, observer) =>
			val created = path += name; observer(created); created
		case _ => under.path += name
	}

	create(root, "fucked")
*/

	
//	class TwoC extends

/*
	trait Reference[+E]

	class A {
		def sayHello = "hello"
	}
	class B

	object repo {
		class Store[T] {
			def get[X](ref :Reference[X])(implicit cons :X ConstrainedBy T) :X= ???
			def apply[X](ref :Reference[X])(implicit cons :X ConstrainedBy T) :X = ???
		}
		implicit val as = new Store[A]
		implicit val bs = new Store[B]
		def apply[E :Store] = System.err.println(implicitly[Store[E]]

//		def apply[X](ref :Reference[X])(implicit repo :Repo[X]) = ???
		def apply[X, E](ref :Reference[X])(implicit cons :X ConstrainedBy E, repo :Store[X]) =
			repo.get(ref)
	}

	class ConstrainedBy[T, E]

	object ConstrainedBy {
		implicit def itself[T] :T ConstrainedBy T = new ConstrainedBy[T, T]
		implicit def byOptElem[T] :Option[T] ConstrainedBy T = new ConstrainedBy[Option[T], T]
		implicit def bySeqElem[T] :Seq[T] ConstrainedBy T = new ConstrainedBy[Seq[T], T]
	}

	case class One[T](value :T) extends Reference[T]
	case class Opt[T](value :Option[T]) extends Reference[Option[T]]
	case class Many[T](values :Seq[T]) extends Reference[Seq[T]]

	import repo._
	object LazyEval {
		val a = repo[A].get(One(new A))
		a.sayHello
		repo[A].get(Opt(Some(new A)))
		repo[A].get(Many(Seq(new A)))

		repo.as(One(new A))
		repo.as(Opt(Some(new A)))
		repo.as(Many(Seq(new A)))
	}
*/



}
