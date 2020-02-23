package com.hcore.clientapi.rest.items


trait ItemUpdate[I]

case class FullItemUpdate[I](value :I) extends ItemUpdate[I]
