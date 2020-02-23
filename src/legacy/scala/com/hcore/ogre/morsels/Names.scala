package com.hcore.ogre.morsels

import com.hcore.ogre.slang.options.extensions
import extensions._
import com.hcore.ogre.slang.options.extensions

object Names {

	def unqualifiedClassName(x :Any) :String = x.unqualifiedClassName

	implicit class PrintableObject[T](val value :T) extends AnyVal {
		def unqualifiedClassName = {
			val qualified = value.getClass.getName
			val i = qualified.lastIndexOf('.')
			qualified.substring(i+1, qualified.length.providingOrElse(l => qualified(l - 1)!='$', qualified.length-1))
		}
	}
}
