package com.adpilot.cortb.clientapi.rest.items

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.Id


case class TagItem(id :Option[Id], owner :IdItem[Any], key :String, value :String)

//case class Tag(id :Option[Id], owner :One[Nothing], key :String, value :String) extends HasId