package com.adpilot.cortb.clientapi.prototype.repository.entities

import com.adpilot.utils.web.DateHelper
import org.joda.time.LocalTime

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

	case class Hour(from :LocalTime, to: LocalTime)

	object Hour {

		private val pattern = "(\\d{2}:\\d{2})-(\\d{2}:\\d{2})".r

		def fromString(s: String) : Hour = {
			DateHelper.convertDateRangeToPair(s) match {
				case Some((minutesFrom, minutesTo)) => {
					val times = List(minutesFrom, minutesTo)
						.map((l: Long) => {
						val hourOfDay: Int = l.toInt / 60
						val minuteOfHour: Int = l.toInt % 60
						new LocalTime(hourOfDay, minuteOfHour)
					})
					new Hour(times.head, times.last)
				}
				case _ => throw new IllegalArgumentException()
			}
		}
		
		def format(hour: Hour) : String = {
			List(hour.from, hour.to)
				.map((t: LocalTime) => "%02d:%02d".format(t.getHourOfDay, t.getMinuteOfHour))
				.mkString("-")
		}
	}


	case class Language(code :String)


	case class Network(name :String)


	case class IPRange(range :String)


	case class Bucket(name :String)

	case class BucketSSP(buckets :String)


	case class BiddingStrategy(name :String) {
		if (!BiddingStrategy.strategies(name))
			throw new IllegalArgumentException(s"Illegal bidding strategy $name")
	}

	object BiddingStrategy {
		val CPC = "CPC"
		val CPA = "CPA"
		val CPM = "CPM"
		val strategies = Set(CPC, CPA, CPM)
	}

	case class URL(name :String)

//	type Pixel = Nothing
//	type Client = Nothing
}
