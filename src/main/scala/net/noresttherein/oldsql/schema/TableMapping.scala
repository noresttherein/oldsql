package net.noresttherein.oldsql.schema


import scala.reflect.runtime.universe.TypeTag

/**
  * @author Marcin Mościcki
  */
abstract class AbstractTableMapping[S, O](val tableName :String) extends RootMappingSupport[S, O] {
	override val sqlName = Some(tableName)
}

abstract class ReflectedTableMapping[S, O](tableName :String)(implicit val subjectType :TypeTag[S])
	extends AbstractTableMapping[S, O](tableName) with ReflectedMapping[S, O]

abstract class TableMapping[S :TypeTag, O <: String with Singleton :ValueOf]
	extends AbstractTableMapping[S, O](valueOf[O])