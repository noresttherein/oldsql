package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.support.ComponentProxy.{EagerDeepProxy, ShallowProxy}
import net.noresttherein.oldsql.schema.support.MappingAdapter.Adapted
import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms


class RenamedMapping[M <: TypedMapping[S, O], S, O](name :String, override val egg :M)
	extends ShallowProxy[S, O] with MappingAdapter[M, S, O]
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
	def apply[S, O](name :String, mapping :TypedMapping[S, O]) :TypedMapping[S, O] =
		if (mapping.columns.size == 1)
			new DeeplyRenamedMapping[S, O](name, mapping)
		else
			new RenamedMapping[TypedMapping[S, O], S, O](name, mapping)


	def column[S, O](name :String, mapping :TypedMapping[S, O]) :TypedMapping[S, O] =
		new DeeplyRenamedMapping(name, mapping)

	def deep[S, O](name :String, mapping :TypedMapping[S, O]) :Adapted[mapping.type] =
		new RenamedMapping[mapping.type, S, O](name, mapping)

	def shallow[S, O](name :String, mapping :TypedMapping[S, O]) :Adapted[mapping.type] =
		new RenamedMapping[mapping.type, S, O](name, mapping)

	def generic[M <: Mapping, C <: TypedMapping[S, O], S, O]
	           (name :String, mapping :M)(implicit types :Conforms[M, C, TypedMapping[S, O]]) :Adapted[C] =
		new RenamedMapping[C, S, O](name, mapping)



	class DeeplyRenamedMapping[S, O](name :String, column :TypedMapping[S, O])
		extends EagerDeepProxy[TypedMapping[S, O], S, O](column)
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