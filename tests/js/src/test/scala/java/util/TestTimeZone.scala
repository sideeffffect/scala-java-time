package java.util

import org.scalatest.funsuite.AnyFunSuite
import org.threeten.bp.ZoneId

import scala.scalajs.js

class TestTimeZone extends AnyFunSuite {

  val sampleTimeZones    = Seq("UTC", "GMT", "Europe/Madrid", "Australia/Sydney")
  val sampleOffsets      = Seq(0, 0, 3600000, 36000000)
  val sampleDisplayNames = Seq(
    "Coordinated Universal Time",
    "Greenwich Mean Time",
    "Central European Time",
    "Australian Eastern Standard Time (New South Wales)"
  )

  test("default") {
    TimeZone.getDefault.getID === "GMT"
  }

  // On a JS host the default comes from the host, not from the UTC fallback that
  // is used on WebAssembly without a JS host
  test("default is detected from the JS host") {
    val hostId     = js.Dynamic.global.Intl.DateTimeFormat().resolvedOptions().timeZone
    val hostOffset = new js.Date().getTimezoneOffset().toInt * 60 * 1000
    val default    = TimeZone.getDefault
    if (!js.isUndefined(hostId))
      assert(default.getID === hostId.asInstanceOf[String])
    assert(default.getRawOffset === hostOffset)
  }

  test("ZoneId.systemDefault agrees with TimeZone.getDefault") {
    assert(ZoneId.systemDefault.getId === ZoneId.of(TimeZone.getDefault.getID).getId)
  }

  test("getTimeZone") {
    val zonesWithOffsets = sampleTimeZones.zip(sampleOffsets)

    for ((tzId, offset) <- zonesWithOffsets) {
      ZoneId.of(tzId)
      val tz = TimeZone.getTimeZone(tzId)

      assert(tz.getID === tzId)
      assert(tz.getRawOffset === offset)
      // assert(tz.toZoneId === zoneId)
    }
  }

  // The raw offset is computed when it is first read, not when the time zone is
  // created, so the result must be the same as an eager computation
  test("getTimeZone raw offset is the current standard offset") {
    for (tzId <- sampleTimeZones :+ "America/New_York") {
      val tz       = TimeZone.getTimeZone(tzId)
      val expected =
        ZoneId.of(tzId).getRules.getStandardOffset(org.threeten.bp.Instant.now).getTotalSeconds * 1000
      assert(tz.isInstanceOf[SimpleTimeZone])
      assert(tz.getID === tzId)
      assert(tz.getRawOffset === expected)
      assert(tz.getRawOffset === expected)
      val cloned = tz.clone.asInstanceOf[TimeZone]
      assert(cloned.getID === tzId)
      assert(cloned.getRawOffset === expected)
    }
  }

  test("getTimeZone fails on an unknown zone") {
    assertThrows[Exception](TimeZone.getTimeZone("Not/AZone"))
  }

  test("availableIDs") {
    val availableIds = TimeZone.getAvailableIDs
    for (tzId <- sampleTimeZones)
      assert(availableIds.contains(tzId))
  }

  test("availableIDs by offset") {
    val zonesByOffsets = sampleOffsets.zip(sampleTimeZones).groupBy(_._1).mapValues(_.map(_._2))
    for ((offset, ids) <- zonesByOffsets) {
      val tzs = TimeZone.getAvailableIDs(offset)
      assert(ids.forall(tzs.contains))
    }
  }

  ignore("getDisplayName") {
    val zonesWithNames = sampleTimeZones.zip(sampleDisplayNames)
    for ((id, name) <- zonesWithNames) {
      val tz = TimeZone.getTimeZone(id)
      assert(tz.getDisplayName(Locale.ENGLISH) === name)
    }
  }

}
