package com.hcore.clientapi.entities

import com.hcore.clientapi.entities.Model.{HasId, Id, One}


case class Tag(id :Option[Id], owner :One[Nothing], key :String, value :String) extends HasId
