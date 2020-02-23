package com.hcore.ogre.mapping

import com.hcore.ogre.mapping.Mapping.SetParameters

import scala.slick.jdbc.SetParameter


trait MappingWithPK[E, PK] extends Mapping[E] {
	val PK :Component[PK]

	def pk(entity :E) :PK = (this \\ PK).pick(entity).get

	lazy val WherePKParameter :SetParameter[PK] = SetParameters(PK)(_.QueryParameters, PK.querable)

//	override def isDefined(values: Values): Boolean =
//		PK.isDefined((values \ PK))
}
