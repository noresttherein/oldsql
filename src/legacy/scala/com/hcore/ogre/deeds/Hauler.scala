package com.hcore.ogre.deeds

import com.hcore.ogre.deeds.Haul.JoinWithJoinedSubselects
import com.hcore.ogre.mapping.{AnyMapping, MappingPath}
import com.hcore.ogre.sql.From


trait Hauler {
	def apply[T<:AnyMapping](rootTable :T) :Haul[T]

	def apply[T<:AnyMapping](paths :Seq[MappingPath[T, _<:AnyMapping]]) :Haul[T] =
		if (paths.isEmpty)
			throw new IllegalArgumentException(s"Can't create a haul for empty path list")
		else
			(apply(paths.head.start) /: paths)(_ fetch _)
}



object Hauler {

	class JoinToOneSelectToMany extends Hauler {
		def apply[T<:AnyMapping](rootTable :T) :Haul[T] = {
			val from = From(rootTable)
			new JoinWithJoinedSubselects[From[T], T](from, Haul.TablesIndex(from))
		}
	}

}