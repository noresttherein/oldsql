package net.noresttherein.oldsql.schema


/** Skeleton base classes for application's concrete [[net.noresttherein.oldsql.schema.Mapping Mapping]]
  * implementations. They do not in general add any new 'external' features - that is, they are seen
  * the same as the most generic `Mapping`s by the framework, but simply implement the existing methods
  * based on certain assumptions about the properties of implementing classes. Additionally, more specific traits
  * provide an 'internal' interface for defining a concrete component structure: declaring the components and columns
  * of the mapping.
  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame]]
  * @see [[net.noresttherein.oldsql.schema.bases.SimpleMapping]]
  */
package object bases