package com.adpilot.cortb.clientapi.prototype.repository.entities

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.HasId


case class StatusType(name :String, description :Option[String]) extends HasId {
	val id = None
}
