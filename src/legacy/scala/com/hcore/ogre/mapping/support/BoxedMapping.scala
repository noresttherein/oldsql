package com.hcore.ogre.mapping.support

import com.hcore.ogre.mapping.Mapping
import com.hcore.ogre.mapping.support.MappingAdapter.MappingMorphismAdapter


/*
abstract class BoxedMapping[V, T, M<:Mapping[T]] extends MappingMorphismAdapter[V, T, M] {
	type Component[X] = M

	override def components: Seq[Component[_]] = Seq(adaptee)
	override def subcomponents: Seq[Component[_]] = adaptee +: adaptee.s

	override def columns: Seq[Component[_]] = ???

	/** All columns which can be part of an sql filter (don't have NoQuery flag set) */
	override def queryable: Seq[Component[_]] = ???

	/** All columns which can be listed in the select clause of an sql */
	override def selectable: Seq[Component[_]] = ???

	/** All columns which can be updated on existing database records */
	override def updatable: Seq[Component[_]] = ???

	/** All columns which can occur in an insert statement */
	override def insertable: Seq[Component[_]] = ???

	/** Columns autogenerated by the database; this implies being non-insertable. */
	override def generated: Seq[Component[_]] = ???
}
*/
