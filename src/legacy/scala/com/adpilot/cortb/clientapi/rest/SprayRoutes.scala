package com.adpilot.cortb.clientapi.rest

import com.adpilot.cortb.clientapi.rest.SprayRoutes.RequestPathMatcher.{MethodPathMatcher, FilteringPathMatcher}
import com.adpilot.cortb.clientapi.util.Matching.{UnapplySeq, Unapply}
import com.adpilot.cortb.clientapi.util.Matching.Unapply.TryUnapply
import com.adpilot.cortb.clientapi.util.{Matching, OptionOps}
import spray.http.{StatusCodes, StatusCode, HttpMethod, HttpMethods}
import spray.httpx.marshalling.ToResponseMarshallable
import spray.routing.RequestContext


object SprayRoutes {
	import scala.util.matching.Regex
	import Matching.RegexpInterpolation


	type RequestMatcher[T] = Unapply[RequestContext, T]

	class RequestPathMatcher(regexp :Regex) extends UnapplySeq[RequestContext, String] {

		def unapplySeq(path :String) :Option[Seq[String]] =
			regexp.unapplySeq(path)

		def unapplySeq(rq :RequestContext) :Option[Seq[String]] =
			regexp.unapplySeq(rq.unmatchedPath.toString())
		
		def providing(accept :RequestContext=>Boolean) :RequestPathMatcher = new FilteringPathMatcher(regexp, accept)

		def unless(reject :RequestContext=>Boolean) :RequestPathMatcher = providing(!reject(_))
	}

	object RequestPathMatcher {
		
		def apply(regexp :Regex) :RequestPathMatcher = new RequestPathMatcher(regexp)
		
		def apply(regexp :Regex, accept :RequestContext=>Boolean) :RequestPathMatcher = new FilteringPathMatcher(regexp, accept)


		class FilteringPathMatcher(regexp :Regex, accept :RequestContext=>Boolean) extends RequestPathMatcher(regexp) {
			override def unapplySeq(rq: RequestContext): Option[Seq[String]] =
				if (accept(rq)) None
				else super.unapplySeq(rq)

			override def providing(expr: (RequestContext) => Boolean): RequestPathMatcher =
				super.providing(rq => expr(rq) && accept(rq))
		}

		class MethodPathMatcher(regexp :Regex, method :HttpMethod) extends FilteringPathMatcher(regexp, _.request.method==HttpMethods.GET)
	}
	
	
	

	implicit class RequestContextInterpolation(sc :StringContext) {
		private[this] val pattern = sc.parts.map(Regex.quote).mkString("(.*)") //"([^/]*)"
		private[this] val regexp = new Regex(pattern, (1 to sc.parts.size).map(_=>"x"):_*)

		object PATH extends RequestPathMatcher(regexp)

		object GET extends MethodPathMatcher(regexp, HttpMethods.GET)
		object POST extends MethodPathMatcher(regexp, HttpMethods.POST)
		object PUT extends MethodPathMatcher(regexp, HttpMethods.PUT)
		object DELETE extends MethodPathMatcher(regexp, HttpMethods.DELETE)

	}

	//	type StringParser[T] = TryUnapply[String, T]
	case class StringParser[T](parse :String => T) extends TryUnapply[String, T](parse)

//	case class SegmentParser[T](parse :String =>T) extends TryUnapply[String, T](
//		segment =>
//			if (segment.endsWith("/")) parse(segment.substring(0, segment.length))
//			else parse(segment)
//	)
	case class SegmentParser[T](parse :String=>T) extends Unapply[String, T] {
		private[this] val pattern = Unapply.Try(parse)
		def unapply(s :String) :Option[T] = s match {
			case rx"${pattern(x)}/" => Some(x)
			case _ => None
		}
	}

	val int = SegmentParser(_.toInt)
	val IntOpt = int.optional
	val long = SegmentParser(_.toLong)
	val LongOpt = long.optional
	val string = SegmentParser(identity)

	val int_? = int.optional


	def Routes(routes :PartialFunction[RequestContext, Unit]) :RequestContext => Unit =
		rq => if (routes.isDefinedAt(rq)) routes(rq) else ()

//	def Accept(successCode :StatusCode)(routes :PartialFunction[RequestContext, ToResponseMarshallable]) :RequestContext => Unit =
//		rq => if (routes.isDefinedAt(rq)) { rq.complete(routes(rq)) } else ()

	def Accept(routes :PartialFunction[RequestContext, ToResponseMarshallable]) :RequestContext => Unit =
//		Accept(StatusCodes.OK)(routes)
		rq => if (routes.isDefinedAt(rq)) { rq.complete(routes(rq)) } else rq.reject() //todo: rejection?


	def test(args :Array[String]): Unit = {
		"ala/ma/5/kotow" match {
			case GET"ala/ma/${int(x)}/kotow" => x
			case PATH"ala/ma/${s @ int(x)}/kotow" => x; System.err.println(s"ala ma $x ($s) kotow; x :${x.getClass()}")

		}

		"ala/ma/dupa/kotow" match {
			case PATH"ala/ma/${s @ IntOpt(x)}/kotow" => x; System.err.println(s"ala ma $x ($s) kotow; x :${x.getClass()}")
		}
	}

}
