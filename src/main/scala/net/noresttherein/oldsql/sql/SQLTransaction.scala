package net.noresttherein.oldsql.sql

import java.sql.Connection






/**
  * @author Marcin Mościcki
  */
trait SQLTransaction {
	def apply[T](run :Connection => T) :T
	def rollback() :Unit
	def commit() :Unit
	def close() :Unit
}



