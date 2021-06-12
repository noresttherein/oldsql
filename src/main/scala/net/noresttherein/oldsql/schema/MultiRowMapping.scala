package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.Mapping.RefinedMapping






/**
  * @author Marcin Mościcki
  */
trait MultiRowMapping[S, O] {
	type Item
	type Grouping
	val grouping :RefinedMapping[Grouping, O]
	val mapping :RefinedMapping[Item, O]
}
