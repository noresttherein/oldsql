package net.noresttherein.oldsql.schema


import scala.reflect.runtime.universe.TypeTag

/**
  * @author Marcin Mościcki
  */
abstract class AbstractTableMapping[O, S](val tableName :String) extends RootMappingSupport[O, S] {
	override val sqlName = Some(tableName)
}

abstract class ReflectedTableMapping[O, S](tableName :String)(implicit val subjectType :TypeTag[S])
	extends AbstractTableMapping[O, S](tableName) with ReflectedMapping[O, S]

abstract class TableMapping[O <: String with Singleton :ValueOf, S :TypeTag]
	extends AbstractTableMapping[O, S](valueOf[O])