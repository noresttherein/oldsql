package com.adpilot.cortb.clientapi.util

import scala.util.{Failure, Success, Try}


object Time {
	def apply[T](operation :String)(expr : =>T) :T = {
		val start = System.currentTimeMillis()
		Try(expr) match {
			case Success(res) =>
				log(operation, start)
				res
			case Failure(exc) =>
				log(operation, start, Some(exc.toString))
				throw exc
		}
	}

	private def log(operation :String, startTime :Long, res :Option[String]=None) :Unit = {
		val time = System.currentTimeMillis() - startTime
		System.err.println(s"took $time milis for $operation " + (res.map("("+_+")") getOrElse ""))
	}
}
