package com.adpilot.cortb.clientapi.prototype.repository.entities

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{Id, HasId}


case class TagKey(name :String, description :Option[String]) extends HasId {
	val id :Option[Id] = None
}
