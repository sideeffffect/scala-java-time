package java.util

import org.scalatest.funsuite.AnyFunSuite
import org.threeten.bp.ZoneId
import scala.scalanative.meta.LinktimeInfo
import scala.scalanative.unsafe._
import scala.collection.immutable.{ Set => ScalaSet }

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

  private val noSources = SystemTimeZone.Sources(_ => None, _ => None, _ => None, () => None)

  private def sources(
    tz:        Option[String] = None,
    localtime: Option[String] = None,
    timezone:  Option[String] = None,
    windows:   Option[String] = None
  ) =
    SystemTimeZone.Sources(
      env = name => if (name == "TZ") tz else None,
      readSymbolicLink = path => if (path == "/etc/localtime") localtime else None,
      readFile = path => if (path == "/etc/timezone") timezone else None,
      windowsTimeZoneKeyName = () => windows
    )

  private def candidates(s: SystemTimeZone.Sources) = SystemTimeZone.candidateIds(s).toSeq

  test("candidateIds: none") {
    assert(candidates(noSources) === Seq.empty)
  }

  test("candidateIds: TZ") {
    assert(candidates(sources(tz = Some("Europe/Prague"))) === Seq("Europe/Prague"))
    assert(candidates(sources(tz = Some(":Europe/Prague"))) === Seq("Europe/Prague"))
    assert(candidates(sources(tz = Some("posix/Europe/Prague"))) === Seq("Europe/Prague"))
    assert(
      candidates(sources(tz = Some(":/usr/share/zoneinfo/Europe/Prague"))) === Seq("Europe/Prague")
    )
    assert(candidates(sources(tz = Some("/etc/localtime"))) === Seq("/etc/localtime"))
    assert(candidates(sources(tz = Some(" "))) === Seq.empty)
    assert(candidates(sources(tz = Some(":"))) === Seq.empty)
  }

  test("candidateIds: /etc/localtime") {
    assert(
      candidates(sources(localtime = Some("/usr/share/zoneinfo/Asia/Tokyo"))) === Seq("Asia/Tokyo")
    )
    assert(candidates(sources(localtime = Some("/some/other/file"))) === Seq.empty)
  }

  test("candidateIds: /etc/timezone") {
    assert(
      candidates(sources(timezone = Some("\n  Asia/Tokyo  \nignored\n"))) === Seq("Asia/Tokyo")
    )
    assert(candidates(sources(timezone = Some(" \n"))) === Seq.empty)
  }

  test("candidateIds: Windows") {
    assert(
      candidates(sources(windows = Some("Central Europe Standard Time"))) === Seq(
        "Europe/Budapest"
      )
    )
    assert(candidates(sources(windows = Some("Tokyo Standard Time"))) === Seq("Asia/Tokyo"))
    assert(candidates(sources(windows = Some("UTC"))) === Seq("Etc/UTC"))
    assert(candidates(sources(windows = Some("No Such Standard Time"))) === Seq.empty)
  }

  test("candidateIds: precedence") {
    val all = sources(
      tz = Some("America/New_York"),
      localtime = Some("/usr/share/zoneinfo/Europe/Prague"),
      timezone = Some("Asia/Tokyo"),
      windows = Some("AUS Eastern Standard Time")
    )
    assert(
      candidates(all) === Seq("America/New_York", "Europe/Prague", "Asia/Tokyo", "Australia/Sydney")
    )
  }

  test("candidateIds: later sources are read only when needed") {
    var reads = Seq.empty[String]
    val s     = SystemTimeZone.Sources(
      env = _ => { reads :+= "env"; Some("Europe/Prague") },
      readSymbolicLink = _ => { reads :+= "link"; None },
      readFile = _ => { reads :+= "file"; None },
      windowsTimeZoneKeyName = () => { reads :+= "windows"; None }
    )
    assert(SystemTimeZone.zoneId(s, _ => true) === Some("Europe/Prague"))
    assert(reads === Seq("env"))
  }

  test("zoneId: skips unknown candidates") {
    val s     = sources(
      tz = Some("Bogus/Zone"),
      localtime = Some("/usr/share/zoneinfo/Europe/Prague"),
      timezone = Some("Asia/Tokyo")
    )
    val known = ScalaSet("Asia/Tokyo", "Europe/Prague")
    assert(SystemTimeZone.zoneId(s, known) === Some("Europe/Prague"))
    assert(
      SystemTimeZone.zoneId(s, ScalaSet("Asia/Tokyo")) === Some("Asia/Tokyo")
    )
    assert(SystemTimeZone.zoneId(s, ScalaSet.empty[String]) === None)
    assert(SystemTimeZone.zoneId(noSources, _ => true) === None)
  }

  test("default on Windows comes from the Windows time zone") {
    if (LinktimeInfo.isWindows) {
      val keyName = WindowsTimeZoneApi.timeZoneKeyName
      assert(keyName.isDefined)
      val zoneId  = keyName.flatMap(WindowsTimeZones.toZoneId.get)
      assert(zoneId.isDefined, s"no zone ID for Windows time zone $keyName")
      if (Option(System.getenv("TZ")).isEmpty)
        assert(TimeZone.getDefault.getID === zoneId.get)
    }
  }

  test("DYNAMIC_TIME_ZONE_INFORMATION layout matches the Windows ABI") {
    import WindowsTimeZoneExtern.DynamicTimeZoneInformation
    val info                    = stackalloc[DynamicTimeZoneInformation]()
    def offset(p: Ptr[_]): Long = p.toLong - info.toLong
    assert(sizeOf[DynamicTimeZoneInformation] === 432)
    assert(offset(info.at2) === 4)   // StandardName
    assert(offset(info.at3) === 68)  // StandardDate
    assert(offset(info.at8) === 172) // TimeZoneKeyName
    assert(offset(info.at9) === 428) // DynamicDaylightTimeDisabled
  }

  test("Windows mapping targets are known zone IDs") {
    val available = TimeZone.getAvailableIDs.toSet
    val unknown   = WindowsTimeZones.toZoneId.values.filterNot(available).toSeq.sorted
    assert(unknown === Seq.empty)
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
