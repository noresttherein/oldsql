package net.noresttherein.oldsql.schema


import scala.reflect.runtime.universe.TypeTag

/**
  * @author Marcin Mościcki
  */
abstract class AbstractTableSchema[O, S](val tableName :String) extends RowRootSchema[O, S] {
	override val sqlName = Some(tableName)
}

abstract class ReflectedTableSchema[O, S](tableName :String)(implicit val subjectType :TypeTag[S])
	extends AbstractTableSchema[O, S](tableName) with ReflectedSchema[O, S]

abstract class TableSchema[O <: String with Singleton :ValueOf, S :TypeTag]
	extends AbstractTableSchema[O, S](valueOf[O])