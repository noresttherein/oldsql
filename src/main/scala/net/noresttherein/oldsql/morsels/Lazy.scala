package net.noresttherein.oldsql.morsels

/**
  * @author Marcin Mościcki
  */
trait Lazy[+T] {
	def get :T

	def isInitialized :Boolean

	override def toString :String = if (isInitialized) get.toString else "Lazy(?)"
}



object Lazy {

	def apply[T](init: => T) :Lazy[T] = new Lazy[T] {
		@volatile private[this] var f = () => init
		@volatile private[this] var value :T = _
		private[this] var cache :T = _

		override def isInitialized :Boolean = f == null

		override def get :T = {
			if (cache == null) {
				var init = f
				if (init == null)
					cache = value
				else synchronized {
					init = f
					if (init != null) {
						cache = init()
						value = cache
						f = null
					}
				}
			}
			cache
		}
	}



	implicit def lazyUnwrap[T](lzy :Lazy[T]) :T = lzy.get

}