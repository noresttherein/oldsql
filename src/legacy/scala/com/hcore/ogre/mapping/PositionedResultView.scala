package com.hcore.ogre.mapping

import java.sql.ResultSet

import com.hcore.ogre.mapping.PositionedResultView.ExtendedPositionedResult

import scala.slick.jdbc.PositionedResult


object PositionedResultView {
	trait ExtendedPositionedResult { this : PositionedResult =>
		def offset(idx :Int) :PositionedResult
	}
	
	
	def apply(res :PositionedResult, offset :Int=0) :PositionedResultView = res match {
		case view :ExtendedPositionedResult => view.offset(offset)
		case _ => new PositionedResultView(res, res.currentPos + offset)
	}

	def apply(rs :ResultSet, offset :Int) :PositionedResult = new PositionedResult(rs) {
		override def close(): Unit = rs.close()
		pos += offset
	}



	implicit def implicitView(res :PositionedResult) :PositionedResultView = res match {
		case view :PositionedResultView => view
		case _ => new PositionedResultView(res)
	}

}

class PositionedResultView protected (protected val result :PositionedResult, position :Int)
	extends PositionedResult(result.rs) with ExtendedPositionedResult
{
	def this(res :PositionedResult) = this(res, res.currentPos)

	def close() = ()

	pos = position

	def offset(idx :Int) = new PositionedResultView(result, pos + idx)

	def view = offset(0)
}

