package com.adpilot.cortb.clientapi.golem


class Schema(val name :String) {
	override def toString = name

	protected implicit val schema = this
}
