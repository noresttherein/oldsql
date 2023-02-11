package net.noresttherein.oldsql.pixies

import java.sql.PreparedStatement

import net.noresttherein.oldsql.sql.jdbc.PreparedStatementProxy






/**
  * @author Marcin Mościcki
  */
private[oldsql] class CachedUpdateCountStatement(protected override val statement: PreparedStatement)
	extends PreparedStatementProxy
{
	private[this] var count = Long.MinValue

	override def getMoreResults :Boolean = { count = Long.MinValue; statement.getMoreResults }

	override def getUpdateCount :Int = {
		if (count == Long.MinValue) try {
			count = statement.getLargeUpdateCount
		} catch {
			case _ :UnsupportedOperationException =>
				count = statement.getUpdateCount
		}
		if (count > Int.MaxValue) Int.MaxValue else count.toInt
	}

	override def getLargeUpdateCount :Long = {
		if (count == Long.MinValue)
			count = statement.getLargeUpdateCount
		count
	}
}
