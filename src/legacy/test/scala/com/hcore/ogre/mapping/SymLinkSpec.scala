package com.hcore.ogre.mapping

import com.hcore.ogre.mapping.support.StaticMapping
import org.scalatest.{Matchers, FlatSpec}


class SymLinkSpec extends FlatSpec with Matchers {

	case class Box[T](value :T)

	class Boxes[T](val mapping :Mapping[T]) extends StaticMapping[Box[T]] {
		override def sqlName = Some(s"Boxes[$mapping]")
		val value = embed(_.value, mapping)

		override protected def construct(implicit res: Values): Box[T] = Box(value)
	}

	case class Drawer(letters :Box[String], socks :Box[String], otherSocks :Box[String], letter :String, ties :Box[Box[String]], moreTies :Box[Box[String]])


	object Drawers extends StaticMapping[Drawer] {
		val letter = column("letter", _.letter)
		val letters = new Boxes[String](symLink(letter)) with Component[Box[String]] {
			override def sqlName = Some("letters")
			override val pick = (d :Drawer) => Some(d.letters)
			override val surepick = Some((_:Drawer).letters)
		}

		val socks = new StaticComponent(_.socks) {
			override def sqlName = Some("socks")
			val list = column("list", _.value)
			override protected def construct(implicit res: Values): Box[String] = Box(list)
		}

		val otherSocks = embed(_.otherSocks, new Boxes[String](symLink(this \\ socks :\ socks.list)))

		val ties = embed(_.ties, new Boxes[Box[String]](new Boxes[String](TypedColumn("ties"))))

		val moreTies = embed(_.moreTies, new Boxes[Box[String]](symLink(this \\ ties :\ ties.adaptee.value)))

		override protected def construct(implicit res: Values): Drawer =
			Drawer(letters, socks, otherSocks, letter, ties, moreTies)
	}


	"StaticMapping" should "remove symlinks from its components list" in {
//		System.err.println(Drawers.components.map(c => s"$c(${c.modifiers.mkString(",")})"))
//		System.err.println(Drawers.columns.map(c => s"$c(${c.modifiers.mkString(",")} symlink? ${Drawers.isSymLink(c)})"))
//		System.err.println(Drawers.subcomponents.map(c => s"$c(${c.modifiers.mkString(",")})"))

		Drawers.columns.size should equal(3)
		Drawers.subcomponents.size should equal(9)
		Drawers.components.size should equal(6)
	}

	"StaticMapping" should "contain components for symlinks if their target is not in scope" in {
		Drawers.letters.columns.size should equal(1)
		Drawers.letters.components.size should equal(1)
		Drawers.letters.subcomponents.size should equal(1)

		Drawers.otherSocks.columns.size should equal(1)
		Drawers.otherSocks.components.size should equal(1)
//		System.err.println(Drawers.otherSocks.subcomponents)
		Drawers.otherSocks.subcomponents.size should equal(1)

		Drawers.moreTies.columns.size should equal(1)
		Drawers.moreTies.components.size should equal(1)
		Drawers.moreTies.subcomponents.size should equal(2)

	}
}
