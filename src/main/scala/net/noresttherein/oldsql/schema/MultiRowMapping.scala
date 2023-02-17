package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.Mapping.TypedMapping






/**
  * @author Marcin Mościcki
  */
trait MultiRowMapping[S, O] {
	type Item
	type Grouping
	val grouping :TypedMapping[Grouping, O]
	val mapping :TypedMapping[Item, O]
}
