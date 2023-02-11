package com.hcore.ogre.mapping.support

import com.hcore.ogre.mapping.ComponentPath.TypedComponentPath
import com.hcore.ogre.mapping.support.MappingAdapter.MappingImpostor
import com.hcore.ogre.mapping.{Mapping, AnyMapping}


class LiftedMapping[M<:AnyMapping, C<:Mapping[V], V](val adaptee :C, val adapteePath :TypedComponentPath[M, C, V])
	extends MappingImpostor[V, C]
{
	override def toString = s"^$adaptee"
}
