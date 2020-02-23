package com.adpilot.cortb.clientapi.prototype.repository.entities

import com.adpilot.cortb.clientapi.prototype.repository.entities.AppnexusCreativeData.Category
import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{HasId, Id}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.One


case class AppnexusCreativeData(id: Option[Id], creative: One[Creative], categories: Seq[Category]) extends HasId


object AppnexusCreativeData {
  case class Category(id: Long)
}
