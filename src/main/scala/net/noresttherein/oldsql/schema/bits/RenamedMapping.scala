package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.support.MappingProxy.{EagerDeepProxy, DirectProxy}
import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.bits.MappingAdapter.{Adapted, ComposedAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.support.DelegateMapping
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms





class RenamedMapping[M <: RefinedMapping[S, O], S, O](name :String, protected override val backer :M)
	extends DirectProxy[S, O] with DelegateMapping[M, S, O]
{
	override val sqlName = Some(name)


	override def renamed(name :String) :Component[S] = RenamedMapping[M, S, O](name, backer)


	override def equals(that :Any) :Boolean = that match {
		case other :RenamedMapping[_, _, _] =>
			(other eq this) || (other canEqual this) && other.sqlName == sqlName && other.backer == backer
		case _ => false
	}

	override def hashCode :Int = backer.hashCode * 31 + name.hashCode

	override def toString :String = "\"" + name + "\":" + backer
}



object RenamedMapping {

	def apply[M <: RefinedMapping[S, O], S, O](name :String, mapping :M) :Adapted[M] =
		new RenamedMapping[M, S, O](name, mapping) with DelegateAdapter[M, S, O] {
			override def renamed(name :String) :Adapted[M] = RenamedMapping[M, S, O](name, mapping)
		}

//	def apply[M <: Mapping, C <: RefinedMapping[S, O], S, O]
//	           (name :String, mapping :M)(implicit types :Conforms[M, C, RefinedMapping[S, O]]) :Adapted[C] =
//		new RenamedMapping[C, S, O](name, mapping)

	def apply[M <: MappingAt[O], S, O](name :String, mapping :MappingAdapter[M, S, O]) :MappingAdapter[M, S, O] =
		new RenamedMapping[MappingAdapter[M, S, O], S, O](name, mapping) with ComposedAdapter[M, S, S, O] {
			override def renamed(name :String) :MappingAdapter[M, S, O] = RenamedMapping[M, S, O](name, backer)
		}

}