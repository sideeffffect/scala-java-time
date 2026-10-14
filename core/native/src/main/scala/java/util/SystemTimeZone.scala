package java.util

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths }
import scala.scalanative.meta.LinktimeInfo
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.util.Try

/**
 * Finds the operating system time zone the same way the JVM does.
 */
private[util] object SystemTimeZone {

  /**
   * The operating system facilities the time zone is looked up from. Replaceable in tests.
   *
   * @param env
   *   reads an environment variable
   * @param readSymbolicLink
   *   reads the target of a symbolic link
   * @param readFile
   *   reads the contents of a text file
   * @param windowsTimeZoneKeyName
   *   reads the Windows time zone key name, e.g. `Central Europe Standard Time`
   */
  final case class Sources(
    env:                    String => Option[String],
    readSymbolicLink:       String => Option[String],
    readFile:               String => Option[String],
    windowsTimeZoneKeyName: () => Option[String]
  )

  val systemSources: Sources = Sources(
    env = name => Option(System.getenv(name)),
    readSymbolicLink = path => Try(Files.readSymbolicLink(Paths.get(path)).toString).toOption,
    readFile =
      path => Try(new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8)).toOption,
    windowsTimeZoneKeyName =
      () => if (LinktimeInfo.isWindows) WindowsTimeZoneApi.timeZoneKeyName else None
  )

  /**
   * The first candidate zone ID for which `isKnown` is `true`.
   */
  def zoneId(sources: Sources, isKnown: String => Boolean): Option[String] =
    candidateIds(sources).find(isKnown)

  /**
   * Candidate zone IDs, in order of precedence:
   *   1. the `TZ` environment variable, either a zone ID or a path into a tzdata directory
   *   1. the target of the `/etc/localtime` symbolic link (Linux, macOS, BSDs)
   *   1. the contents of `/etc/timezone` (Debian and derivatives)
   *   1. the Windows time zone, mapped to a zone ID
   *
   * Each candidate is computed only when the previous ones are not usable.
   */
  def candidateIds(sources: Sources): scala.collection.Iterator[String] = {
    def fromTzEnv: Option[String] =
      sources.env("TZ").map(_.stripPrefix(":").trim).filter(_.nonEmpty).map { tz =>
        if (tz.startsWith("/")) zoneIdFromPath(tz).getOrElse(tz) else stripPosix(tz)
      }

    def fromLocaltime: Option[String] =
      sources.readSymbolicLink("/etc/localtime").flatMap(zoneIdFromPath)

    def fromTimezoneFile: Option[String] =
      sources.readFile("/etc/timezone").flatMap(_.split('\n').map(_.trim).find(_.nonEmpty))

    def fromWindows: Option[String] =
      sources.windowsTimeZoneKeyName().flatMap(WindowsTimeZones.toZoneId.get)

    scala.collection
      .Iterator[() => Option[String]](
        () => fromTzEnv,
        () => fromLocaltime,
        () => fromTimezoneFile,
        () => fromWindows
      )
      .flatMap(_())
  }

  /**
   * Extracts a zone ID from a path into a tzdata directory, e.g. `Europe/Prague` from
   * `/usr/share/zoneinfo/Europe/Prague` or `../usr/share/zoneinfo/posix/Europe/Prague`.
   */
  def zoneIdFromPath(path: String): Option[String] = {
    val marker = "zoneinfo/"
    val idx    = path.indexOf(marker)
    if (idx < 0) None
    else Some(stripPosix(path.substring(idx + marker.length))).filter(_.nonEmpty)
  }

  private def stripPosix(id: String): String = id.stripPrefix("posix/")
}

@extern
private[util] object WindowsTimeZoneExtern {
  def GetDynamicTimeZoneInformation(info: Ptr[Byte]): UInt = extern
}

private[util] object WindowsTimeZoneApi {
  // Layout of DYNAMIC_TIME_ZONE_INFORMATION: Bias (4 bytes), StandardName (32 WCHARs),
  // StandardDate (16), StandardBias (4), DaylightName (32 WCHARs), DaylightDate (16),
  // DaylightBias (4), TimeZoneKeyName (128 WCHARs), DynamicDaylightTimeDisabled (1), padding.
  private final val KeyNameOffset     = 4 + 64 + 16 + 4 + 64 + 16 + 4
  private final val KeyNameMaxChars   = 128
  private final val StructSize        = 432
  private final val TimeZoneIdInvalid = 0xffffffff

  def timeZoneKeyName: Option[String] = {
    val info = stackalloc[Byte](StructSize)
    if (WindowsTimeZoneExtern.GetDynamicTimeZoneInformation(info).toInt == TimeZoneIdInvalid) None
    else {
      val sb = new java.lang.StringBuilder
      var i  = 0
      var ch = readWChar(info, KeyNameOffset)
      while (i < KeyNameMaxChars && ch != 0) {
        sb.append(ch)
        i += 1
        ch = readWChar(info, KeyNameOffset + 2 * i)
      }
      Some(sb.toString).filter(_.nonEmpty)
    }
  }

  // WCHAR is UTF-16 little-endian on Windows
  private def readWChar(p: Ptr[Byte], offset: Int): Char =
    ((p(offset + 1) & 0xff) << 8 | (p(offset) & 0xff)).toChar
}
