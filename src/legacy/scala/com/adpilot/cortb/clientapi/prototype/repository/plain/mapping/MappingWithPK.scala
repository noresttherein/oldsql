package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping

import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.Mapping.SetParameters

import scala.slick.jdbc.SetParameter


trait MappingWithPK[E, PK] extends Mapping[E] {
	val PK :Component[PK]

	def pk(entity :E) :PK = PK.value(entity)

	lazy val WherePKParameter :SetParameter[PK] = SetParameters[PK](PK.querable:_*)



}
