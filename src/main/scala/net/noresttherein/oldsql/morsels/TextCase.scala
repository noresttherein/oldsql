package net.noresttherein.oldsql.morsels

import java.util.Locale






/** Enum-like type for string capitalization strategies. Includes `LowerCase`, `UpperCase` and `CamelCase`. */
trait TextCase extends Serializable {
	def apply(text :String) :String

	/** A conversion function from text in this capitalization scheme to a new capitalization. */
	def to(result :TextCase) :String => String = result(_)
}


//consider: a version using a dictionary to camel case individual words
/** Constants of `TextCase` applying specific capitalization strategies to strings. */
object TextCase {
	case class LowerCase(locale :Locale) extends TextCase {
		override def apply(text :String) :String = text.toLowerCase(locale)
	}
	object LowerCase extends LowerCase(Locale.getDefault()) {
		override def toString = "LowerCase"
	}
	case class UpperCase(locale :Locale) extends TextCase {
		override def apply(text :String) :String = text.toUpperCase(locale)
	}
	object UpperCase extends UpperCase(Locale.getDefault) {
		override def toString :String = "UpperCase"
	}
	case object OriginalCase extends TextCase {
		override def apply(text :String) :String = text
	}
	case class CamelCase(locale :Locale) extends TextCase {
		override def apply(text :String) :String = {
			val res = new java.lang.StringBuilder(text.length)
			var i = 0; var wordStart = 0
			@inline
			def toCamel() =
				if (wordStart < i) {
					res append text.charAt(wordStart).toUpper
					while ({ wordStart += 1; wordStart < i })
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
	object CamelCase extends CamelCase(Locale.getDefault) {
		override def toString :String = "CamelCase"
	}
}
