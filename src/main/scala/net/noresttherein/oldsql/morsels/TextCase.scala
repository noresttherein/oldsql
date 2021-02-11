package net.noresttherein.oldsql.morsels



/** Enum-like type for string capitalization strategies. Includes `LowerCase`, `UpperCase` and `CamelCase`. */
trait TextCase extends Serializable {
	def apply(text :String) :String
}


/** Constants of `TextCase` applying specific capitalizations to strings. */
object TextCase {
	val LowerCase :TextCase = _.toLowerCase
	val UpperCase :TextCase = _.toUpperCase
	val CamelCase :TextCase = { text =>
		val res = new java.lang.StringBuilder(text.length)
		var i = 0; var wordStart = 0
		@inline
		def toCamel() =
			if (wordStart < i) {
				res append text.charAt(wordStart).toUpper
				while ({wordStart += 1; wordStart < i })
					res append text.charAt(wordStart).toLower
			}

		while (i < text.length) {
			val char = text.charAt(i)
			if (!char.isLetter) {
				toCamel()
				wordStart += 1
			}
			i += 1
		}
		toCamel()
		res.toString
	}
}
