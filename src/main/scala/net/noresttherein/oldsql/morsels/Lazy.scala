package net.noresttherein.oldsql.morsels

import net.noresttherein.oldsql






/**
  * @author Marcin Mościcki
  */ //this is private[oldsql] to seal countless private[schema] fields of LazyMapping, because I'm too lazy to wrap them in Sealed
private[oldsql] trait Lazy[+T] extends Serializable {
	def get :T

	def isInitialized :Boolean

	private def writeReplace = Lazy.eager(get)

	override def toString :String = if (isInitialized) get.toString else "Lazy(?)"
}



private[oldsql] object Lazy {

	def apply[T](init: => T) :Lazy[T] = new Lazy[T] {
		@volatile @transient private[this] var f = () => init
		@volatile private[this] var value :T = _
		private[this] var cache :T = _

		override def isInitialized :Boolean = f == null
		//fixme: this almost certainly doesn't work - cache!=null doesn't imply all changes from init() are visible
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
						oldsql.publishMutable()
					}
				}
			}
			cache
		}
	}



	def eager[T](value :T) :Lazy[T] = new Lazy[T] {
		override def get = value
		override def isInitialized = true
	}



	implicit def lazyUnwrap[T](lzy :Lazy[T]) :T = lzy.get

}