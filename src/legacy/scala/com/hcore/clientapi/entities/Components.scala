package com.hcore.clientapi.entities


object Components {

	abstract class Box[Contents](protected[Components] val contents :Contents) {
		override def toString = s"[$contents]"


		def canEqual(other: Any): Boolean = other.getClass == getClass

		override def equals(other: Any): Boolean = other match {
			case that: Box[_] => (that canEqual this) && contents == that.contents
			case _ => false
		}

		override def hashCode(): Int = contents.hashCode
	}

	class Boxing[Contents, Boxed<:Box[Contents]](val box :Contents=>Boxed) extends ((Contents) => Boxed) {
		def apply(s :Contents) :Boxed = box(s)
		def unapply(t :Boxed) :Some[Contents] = Some(unbox(t))

//		def box(s :Contents) :Boxed
		def unbox(t :Boxed) :Contents = t.contents
	}
	type StringBoxing[B<:Box[String]] = Boxing[String, B]



	class Country(val code :String) extends Box(code)
	object Country extends StringBoxing(new Country(_))

	class Geo(val location :String) extends Box(location)
	object Geo extends StringBoxing(new Geo(_))

	case class Hour(hour :Int) {
		def this(hour :String) = this(Integer.parseInt(hour))

		override def toString = hour.toString
	}


	case class Language(code :String)


	case class Network(name :String)


	case class IPRange(range :String)


	case class Bucket(name :String)

	case class BucketSSP(buckets :String)


	case class BiddingStrategy(name :String)

	case class URL(name :String)

//	type Pixel = Nothing
//	type Client = Nothing
}
