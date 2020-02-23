package com.adpilot.cortb.clientapi.util

import scala.reflect.ClassTag


/*


class MutableProperty[O, T] private (val designator :PropertyDesignator[O], get :O=>T, set :(O, T)=>O) {

	def apply(entity :O) :T = get(entity)
	def update(entity :O, value :T) = set(entity, value)
}


object MutableProperty {
	def apply[O :ClassTag, T](get :O=>T, set :(O, T) => O) :MutableProperty[O, T] =
		new MutableProperty(PropertyDesignator(get), get, set)
}
*/
