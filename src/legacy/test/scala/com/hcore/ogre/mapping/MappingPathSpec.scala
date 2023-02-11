package com.hcore.ogre.mapping

import com.hcore.ogre.mapping.ComponentPath._
import com.hcore.ogre.mapping.MappingMorphism.{ComponentMorphism, ValueMorphism}
import com.hcore.ogre.mapping.Mapping.{MappingExtension}
import com.hcore.ogre.mapping.MappingPath.{ConcatPath, \~\}
import com.hcore.ogre.mapping.support.BaseMappings.FlatMapping
import com.hcore.ogre.mapping.support.StaticMapping
import org.scalatest.{FlatSpec, Matchers}


class MappingPathSpec extends FlatSpec with Matchers {

	class Recur(rec :Recur)

	class Recurs(i :Int=0) extends FlatMapping[Recur] {
		lazy val rec = new Recurs(i+1)
		override def modifiers: Seq[MappingExtension[Recur]] = Seq()

		override def assemble(values: Values): Option[Recur] = ???

		override def columns: Seq[Component[_]] = Seq()

		override implicit def \\[T](component: Component[T]): TypedComponentPath[this.type, component.type, T] =
			DirectComponent(this)(component)(ValueMorphism.none, ComponentMorphism.empty[Component])

		override def sqlName=Some("R"+i)
	}

	val root = new Recurs
	val selfRoot = SelfPath(root :root.type)



	def appendComponents(left :Int)(last :Recurs)(path :root.type\**\last.type) :root.type \**\ (_<:Recurs) =
		if (left<=0) path
		else appendComponents(left-1)(last.rec)(path :\ last.rec)
	
	def appendDirects(left :Int)(last :Recurs)(path :root.type\**\last.type) :root.type \**\ (_<:Recurs) =
		if (left<=0) path
		else appendDirects(left-1)(last.rec)(path ++ (last \\ last.rec))
	
	def appendToNext(left :Int)(last :Recurs)(path :root.type\**\last.type) :root.type \**\ (_<:Recurs) =
		if (left<=0) path  
		else appendToNext(left-1)(last.rec)(path ++: (last \\ last.rec))

	def backComps(left :Int)(last :Recurs) :last.type \**\ (_<:Recurs) =
		if (left<=0) SelfPath(last :last.type)
		else (last :last.type) \: (backComps(left-1)(last.rec) :last.rec.type \**\ (_<:Recurs))

	def backDirects(left :Int)(last :Recurs) :last.type \**\ (_<:Recurs) =
		if (left<=0) SelfPath(last)
		else (last \\ last.rec) ++ backDirects(left-1)(last.rec)

	def backToNext(left :Int)(last :Recurs) :last.type \**\ (_<:Recurs) =
		if (left<=0) SelfPath(last)
		else (last \\ last.rec) ++: backDirects(left-1)(last.rec)



	def concat[X<:Recurs, Y<:Recurs](left :X\**\(_<:Recurs), right :(_<:Recurs)\**\Y) :X\**\Y =
		left.asInstanceOf[X\**\Recurs] ++ right.asInstanceOf[Recurs\**\Y]

	def append[X<:Recurs, Y<:Recurs](path :X\**\Y, count :Int, fun : Int=>Recurs=> (_<:Recurs)\**\(_<:Recurs)) = {
		val end = path.end
		path.asInstanceOf[X\**\end.type] ++ fun(count)(end).asInstanceOf[end.type\**\Recurs]
	}


	val count = 5
	val concatenations = Seq(
		appendComponents(count)(root)(selfRoot),
		appendDirects(count)(root)(selfRoot),
		appendToNext(count)(root)(selfRoot),
		backComps(count)(root),
		backDirects(count)(root),
		backToNext(count)(root)
	)



	"MappingPath" should "concatenate correctly with self paths" in {
		val m1 = new Recurs
		val m2 = new Recurs

		val self = SelfPath(m1)
		val second = SelfPath(m2)
		val direct = m1 \\ m2

		(self ++ self) should equal(self)
		(self ++: self) should equal(self)
		(self ++ direct) should equal(direct)
		(self ++: direct) should equal(direct)

		(direct ++ second) should equal(direct)
		(direct ++: second) should equal(direct)
	}

	"MappingPath" should "produce equal concatenations regardless of order" in {

//		concatenations.zipWithIndex.foreach { case (path :ConcatPath[_, _, _], i) =>
//			System.err.println(s"path $i: "+formatPath(path))
//			System.err.println(s"asc   : "+formatPath(path.ascending))
//			System.err.println(s"desc  : "+formatPath(path.descending))
//			System.err.println
//		}

		for ((p1, i1)<-concatenations.zipWithIndex; (p2, i2)<-concatenations.zipWithIndex) {
//			System.err.println(s"comparing $i1 with $i2")
			p1 should equal(p2)
		}


		val funs = Seq(
			(x:Int)=>(y:Recurs)=>backComps(x)(y).asInstanceOf[Recurs\**\Recurs],
			(x:Int)=>(y:Recurs)=>backDirects(x)(y).asInstanceOf[Recurs\**\Recurs],
			(x:Int)=>(y:Recurs)=>backToNext(x)(y).asInstanceOf[Recurs\**\Recurs]
		)

		val complex =
			for (p<-concatenations; f<-funs)
				yield (append(p, count, f))

		for (p1<-complex; p2<-complex) {
			p1 should equal(p2)
		}
	}

	"MappingPath" should "slice first element correctly of a concatenated path" in {
		def test(n :Int)(last :Recurs)(left :last.type\**\(_<:Recurs)) :Unit =
			if (n==1) {
				left should equal(last \\ last.rec)
			} else {
				left match {
					case NestedComponent(head, tail) =>
						head should equal(last \\ last.rec)
						test(n-1)(last.rec)(tail.asInstanceOf[last.rec.type \**\ (_<:Recurs)])
					case _ => fail(s"Couldn't get last from $left")
				}

			}

		concatenations.foreach { path =>
			test(count)(root)(path)
		}
	}

	"MappingPath" should "split itself into prefix and suffix" in {
		val prefixes = (1 until count).map(backComps(_)(root))

		for (path<-concatenations; prefix<-prefixes) {
			path.startsWith(prefix) should equal(true)
			val suffix = path.drop(prefix)
			val concat = suffix.map(prefix.asInstanceOf[root.type\~\Mapping] ++ _.asInstanceOf[Mapping\~\Mapping])
			concat should equal(Some(path))
		}
	}



	case class Parent(child :Child)
	case class Child(name :String)

	object Parents extends StaticMapping[Parent] {
		val child = new StaticComponent(_.child) {
			val name = column("name", _.name)

			override protected def construct(implicit res: Values): Child = Child(name)
			override def toString = "child"
		}

		override protected def construct(implicit res: Values): Parent = Parent(child)
	}

	"MappingPath" should "map its target component preserving structure" in {
		val mappedPath = Parents(_.child).map(_.name)(new Child(_))
		val extendedPath = mappedPath(_.adaptee.name)
		extendedPath(new Parent(new Child("Rosemary's"))) shouldEqual Some("Rosemary's")
	}



	private def formatPath(path :_ \~\ _) :String = path match {
		case c:ConcatPath[_, _, _] =>
			s"(${formatPath(c.prefix)}, ${formatPath(c.suffix)})"
		case _ => path.toString
	}
}
