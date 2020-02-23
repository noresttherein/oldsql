package com.hcore.ogre.mapping.bits

import com.hcore.ogre.mapping.Mapping
import com.hcore.ogre.mapping.support.BaseMappings.{AbstractEmptyMapping, EmptyMapping}
import com.hcore.ogre.mapping.support.MappingAdapter.MappingImpostor
import com.hcore.ogre.slang.SaferCasts

//implicits
import SaferCasts._


trait ImportedMapping[T] extends AbstractEmptyMapping[T] {
	def mapping :Mapping[T]

	override def modifiers = mapping.modifiers

}

object ImportedMapping {

	def apply[T](backing :Mapping[T]) :TypedImportedMapping[T, backing.type] =
		new MappingImpostor[T, backing.type] with TypedImportedMapping[T, backing.type] {
			override def mapping :backing.type = backing
			override val adaptee :backing.type = backing
		}

//	def empty[T](backing :Mapping[T]) :TypedImportedMapping[T, backing.type] =
//		new EmptyMapping[T] with TypedImportedMapping[T, backing.type] {
//			override val mapping :backing.type = backing
//		}



	def unapply[T](mapping :Mapping[T]) :Option[Mapping[T]] = mapping.ifSubclass[ImportedMapping[T]] { _.mapping }


	trait TypedImportedMapping[T, M<:Mapping[T]] extends ImportedMapping[T] {
		def mapping :M
	}
}
