package com.hcore.clientapi.rest.items

import com.hcore.clientapi.entities.Model.Id


case class TagItem(id :Option[Id], owner :IdItem[Any], key :String, value :String)

//case class Tag(id :Option[Id], owner :One[Nothing], key :String, value :String) extends HasId