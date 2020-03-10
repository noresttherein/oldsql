package net.noresttherein.oldsql

import net.noresttherein.oldsql.slang._

/**
  * @author Marcin Mościcki
  */
package object schema {

	private[schema] def prefixOption(prefix :String) :Option[String] = prefix.length > 0 ifTrue prefix

}
