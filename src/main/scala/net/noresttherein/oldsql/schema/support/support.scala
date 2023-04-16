package net.noresttherein.oldsql.schema

/** [[net.noresttherein.oldsql.schema.Mapping Mapping]] [[net.noresttherein.oldsql.schema.support.MappingAdapter adapters]]
  * infrastructure containing both interfaces and skeleton base classes, as well as implementations supporting
  * features provided by standard `Mapping` methods.
  *
  * These are public interfaces, but providing functionality used primarily when extending the framework
  * with custom, generic features. They are not expected to be used directly by the majority of applications.
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter]]
  * @see [[net.noresttherein.oldsql.schema.support.DelegateMapping]]
  * @author Marcin Mościcki
  */
//consider: moving everything that is not an adapter/delegate to new package adapters
// and moving here from bases everything not expected to be extended by client code
package object support
