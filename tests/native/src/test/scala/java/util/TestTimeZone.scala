package java.util

import org.scalatest.funsuite.AnyFunSuite
import org.threeten.bp.ZoneId

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
    val tz = TimeZone.getDefault
    assert(ZoneId.of(tz.getID).getId === tz.getID)
    assert(ZoneId.systemDefault.getId === tz.getID)
  }

  test("zoneIdFromPath") {
    assert(
      SystemTimeZone.zoneIdFromPath("/usr/share/zoneinfo/Europe/Prague") === Some("Europe/Prague")
    )
    assert(SystemTimeZone.zoneIdFromPath("../usr/share/zoneinfo/Etc/UTC") === Some("Etc/UTC"))
    assert(
      SystemTimeZone.zoneIdFromPath("/var/db/timezone/zoneinfo/America/New_York") === Some(
        "America/New_York"
      )
    )
    assert(
      SystemTimeZone.zoneIdFromPath("/usr/share/zoneinfo/posix/Asia/Tokyo") === Some("Asia/Tokyo")
    )
    assert(SystemTimeZone.zoneIdFromPath("/usr/share/zoneinfo/") === None)
    assert(SystemTimeZone.zoneIdFromPath("/etc/localtime") === None)
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
