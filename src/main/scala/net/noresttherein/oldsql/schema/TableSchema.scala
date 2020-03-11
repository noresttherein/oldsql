package net.noresttherein.oldsql.schema


import scala.reflect.runtime.universe.TypeTag

/**
  * @author Marcin Mościcki
  */
abstract class TableSchema[S](val tableName :String) extends RowRootSchema[S]

abstract class ReflectedTable[S](tableName :String)(implicit val subjectType :TypeTag[S])
	extends TableSchema[S](tableName) with ReflectedSchema[S]
