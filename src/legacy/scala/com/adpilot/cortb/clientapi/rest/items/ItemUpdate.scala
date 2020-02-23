package com.adpilot.cortb.clientapi.rest.items


trait ItemUpdate[I]

case class FullItemUpdate[I](value :I) extends ItemUpdate[I]

object ItemUpdate {
	implicit def fullUpdate[I](value :I) :ItemUpdate[I] = new FullItemUpdate(value)
	implicit def fullUpdates[I](values :Seq[I]) :Seq[ItemUpdate[I]] = values.view.map(new FullItemUpdate(_))
}
