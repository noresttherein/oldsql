package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.schema.SQLReadForm
import net.noresttherein.oldsql.schema.support.EmptyMapping

//implicits
import net.noresttherein.oldsql.slang._





/** A `Mapping` implementation with no components or columns, always returning the predefined value.
  * Any value present in `ComponentValues` is always ignored.
  * @author Marcin Mościcki
  */
class ConstantMapping[S, O](subject :S, val columnCount :Int = 0) extends EmptyMapping[S, O] {
	private[this] val result = Got(subject)

	override def assemble(values :Pieces) :Opt[S] = result

	override def optionally(values :Pieces) :Opt[S] = result

	override def apply(values :Pieces) :S = subject

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		SQLReadForm.const((0 /: components) { _ + _.selectedByDefault.size })(subject)

	override val selectForm :SQLReadForm[S] = SQLReadForm.const(columnCount)(subject)

	override def mappingName :String = "Const[" + subject.localClassName + "]"
	override def toString :String = "Const(" + subject + ")"
}






/** A `Mapping` implementation returning  the result of evaluating the given expression at each call to
  * the `apply` and `optionally` methods. This value overrides any values preset for this mapping in the `Pieces`.
  */
class GeneratorMapping[S, O](generator: => S) extends EmptyMapping[S, O] {

	override def assemble(values :Pieces) :Opt[S] = Got(generator)

	override def optionally(values :Pieces) :Opt[S] = Got(generator)

	override def apply(values :Pieces) :S = generator

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		SQLReadForm.eval((0 /: components) { _ + _.selectedByDefault.size })(generator)

	override val selectForm :SQLReadForm[S] = SQLReadForm.eval(0)(generator)

	override def mappingName :String = "Generator#" + System.identityHashCode(this)
}

