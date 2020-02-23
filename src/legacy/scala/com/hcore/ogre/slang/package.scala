package com.hcore.ogre


package object slang {
	/** An implicit conversion extending Int with a method 'repeat' which executes a block the given number of times. */
	implicit class repeat(val count :Int) extends AnyVal {
		/** Execute the given block the number of times specified by 'this' argument. */
		def times(block : =>Unit): Unit =
			for (i<- 0 until count) block

	}

}
