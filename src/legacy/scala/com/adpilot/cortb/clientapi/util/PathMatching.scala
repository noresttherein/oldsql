package com.adpilot.cortb.clientapi.util

import Matching._
import com.adpilot.cortb.clientapi.util.Matching.Unapply.TryUnapply

object PathMatching {
//	import java.net.URL
	import scala.util.matching.Regex


	implicit class PathInterpolation(sc :StringContext) {
		private[this] val pattern = sc.parts.map(Regex.quote).mkString("(.*)") //"([^/]*)"
		private[this] val regexp = new Regex(pattern, (1 to sc.parts.size).map(_=>"x"):_*)

		object path {
			def unapplySeq(string :String) = regexp.unapplySeq(string)
		}
	}

//	type StringParser[T] = TryUnapply[String, T]
	case class StringParser[T](parse :String => T) extends TryUnapply[String, T](parse)
	case class OptParam[X](parse :String=>X)

	val IntParam = StringParser(_.toInt)
	val IntOpt = IntParam.optional
	val LongParam = StringParser(_.toLong)
	val LongOpt = IntParam.optional
	val StringParam = StringParser(identity)




	def test(args :Array[String]): Unit = {
		"ala/ma/5/kotow" match {
			case path"ala/ma/${s @ IntParam(x)}/kotow" => x; System.err.println(s"ala ma $x ($s) kotow; x :${x.getClass()}")
		}
		"ala/ma/dupa/kotow" match {
			case path"ala/ma/${s @ IntOpt(x)}/kotow" => x; System.err.println(s"ala ma $x ($s) kotow; x :${x.getClass()}")
		}
	}
}
