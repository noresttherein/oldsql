package com.hcore.ogre.sql



trait SQLTypes {
	def get[X](value :X) :Option[SQLForm[X]]

}
