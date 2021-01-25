package com.adpilot.cortb.clientapi.prototype.repository.entities

import com.adpilot.cortb.clientapi.prototype.repository.entities.Components.Hour
import org.joda.time.LocalTime
import org.scalatest.{FunSuite, Matchers}

class HoursCompanionObjectTests extends FunSuite with Matchers {

  val hour = new Hour(new LocalTime(4, 45), new LocalTime(14, 5))
  val stringRepresentation = "04:45-14:05"

  test("testFromString") {
    Hour.fromString(stringRepresentation) should equal(hour)
  }

  test("testFormat") {

    Hour.format(hour) should equal(stringRepresentation)
  }

}
