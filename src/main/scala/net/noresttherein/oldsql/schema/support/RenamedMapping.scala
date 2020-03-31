package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.schema.Mapping.Component
import net.noresttherein.oldsql.schema.support.ComponentProxy.{EagerDeepProxy, ShallowProxy}
import net.noresttherein.oldsql.schema.support.MappingAdapter.Adapted
import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.slang.InferTypeParams.IsBoth


class RenamedMapping[M <: Component[O, S] , O, S](name :String, override val egg :M)
	extends ShallowProxy[O, S] with MappingAdapter[M, O, S]
{
	override val sqlName = Some(name)

	override def equals(that :Any) :Boolean = that match {
		case other :RenamedMapping[_, _, _] =>
			(other eq this) || (other canEqual this) && other.sqlName == sqlName && other.egg == egg
		case _ => false
	}

	override def hashCode :Int = egg.hashCode * 31 + name.hashCode

	override def toString :String = name + "->" + egg
}



object RenamedMapping {
	def apply[O, S](name :String, mapping :Component[O, S]) :Component[O, S] =
		if (mapping.columns.size == 1)
			new DeeplyRenamedMapping[O, S](name, mapping)
		else
			new RenamedMapping[Component[O, S], O, S](name, mapping)


	def column[O, S](name :String, mapping :Component[O, S]) :Component[O, S] =
		new DeeplyRenamedMapping(name, mapping)

	def deep[O, S](name :String, mapping :Component[O, S]) :Adapted[mapping.type] =
		new RenamedMapping[mapping.type, O, S](name, mapping)

	def shallow[O, S](name :String, mapping :Component[O, S]) :Adapted[mapping.type] =
		new RenamedMapping[mapping.type, O, S](name, mapping)

	def generic[M <: Mapping, C <: Component[O, S], O, S]
	           (name :String, mapping :M)(implicit types :IsBoth[M, C, Component[O, S]]) :Adapted[C] =
		new RenamedMapping[C, O, S](name, mapping)



	class DeeplyRenamedMapping[O, S](name :String, column :Component[O, S])
		extends EagerDeepProxy[Component[O, S], O, S](column)
	{
		override val sqlName = Some(name)

		protected override def adapt[T](component :egg.Component[T]) :Component[T] = component.renamed(name)

		override def equals(that :Any) :Boolean = that match {
			case other :DeeplyRenamedMapping[_, _] =>
				(other eq this) || (other canEqual this) && other.sqlName == sqlName && other.egg == egg
			case _ => false
		}
		override def hashCode :Int = egg.hashCode * 31 + name.hashCode

		override def toString :String = name + "->>" + egg
	}
}