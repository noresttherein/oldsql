package com.adpilot.cortb.clientapi.prototype.repository.entities

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{HasId, Id}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.One


case class Tag(id :Option[Id], owner :One[Nothing], key :String, value :String) extends HasId
