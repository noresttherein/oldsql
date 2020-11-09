package net.noresttherein.oldsql.schema


import net.noresttherein.oldsql.schema.support.{ReflectedMapping, RootMappingFrame}

import scala.reflect.runtime.universe.TypeTag

/**
  * @author Marcin Mościcki
  */
abstract class AbstractTableMapping[S, O](val tableName :String) extends RootMappingFrame[S, O]

abstract class ReflectedTableMapping[S, O](tableName :String)(implicit val subjectType :TypeTag[S])
	extends AbstractTableMapping[S, O](tableName) with ReflectedMapping[S, O]

abstract class TableMapping[S :TypeTag, O <: String with Singleton :ValueOf]
	extends AbstractTableMapping[S, O](valueOf[O])