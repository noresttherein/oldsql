package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.support.EmptyMapping
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm, SQLWriteForm}






/** A `Mapping` implementation with no components or columns, always returning the predefined value.
  * Any value present in `ComponentValues` is always ignored.
  * @author Marcin Mościcki
  */
class ConstantMapping[S, O](subject :S) extends EmptyMapping[S, O] {
	private[this] val result = Some(subject)

	override def assemble(values :Pieces) :Option[S] = result

	override def optionally(values :Pieces) :Option[S] = result

	override def apply(values :Pieces) :S = subject

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		//fixme: not selectable, but default select list
		SQLReadForm.const(subject, (0 /: components) { _ + _.selectable.size })

	override val selectForm :SQLReadForm[S] = SQLReadForm.const(subject)

	override def toString :String = "Const(" + subject + ")"
}






/** A `Mapping` implementation returning  the result of evaluating the given expression at each call to
  * the `apply` and `optionally` methods. This value overrides any values preset for this mapping in the `Pieces`.
  */
class GeneratorMapping[S, O](generator: => S) extends EmptyMapping[S, O] {

	override def assemble(values :Pieces) :Option[S] = Some(generator)

	override def optionally(values :Pieces) :Option[S] = Some(generator)

	override def apply(values :Pieces) :S = generator

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		//fixme: proper semantics of including optional components
		SQLReadForm.eval(generator, (0 /: components) { _ + _.selectable.size })

	override val selectForm :SQLReadForm[S] = SQLReadForm.eval(generator)

	override def toString :String = "Generator#" + System.identityHashCode(this)
}

