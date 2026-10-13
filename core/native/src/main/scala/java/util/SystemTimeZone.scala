package java.util

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths }
import scala.util.Try

/**
 * Finds the operating system time zone the same way the JVM does on Unix-like systems.
 */
private[util] object SystemTimeZone {

  /**
   * Candidate zone IDs, in order of precedence: the `TZ` environment variable, the target of the
   * `/etc/localtime` symbolic link and the contents of `/etc/timezone`. Each candidate is computed
   * only when the previous ones are not usable.
   */
  def candidateIds: scala.collection.Iterator[String] =
    scala.collection
      .Iterator[() => Option[String]](() => fromTzEnv, () => fromLocaltime, () => fromTimezoneFile)
      .flatMap(_())

  private def fromTzEnv: Option[String] =
    Option(System.getenv("TZ")).map(_.stripPrefix(":").trim).filter(_.nonEmpty).map { tz =>
      if (tz.startsWith("/")) zoneIdFromPath(tz).getOrElse(tz) else tz
    }

  private def fromLocaltime: Option[String] =
    Try(Files.readSymbolicLink(Paths.get("/etc/localtime")).toString).toOption
      .flatMap(zoneIdFromPath)

  private def fromTimezoneFile: Option[String] =
    Try(new String(Files.readAllBytes(Paths.get("/etc/timezone")), StandardCharsets.UTF_8)).toOption
      .flatMap(_.split('\n').map(_.trim).find(_.nonEmpty))

  /**
   * Extracts a zone ID from a path into a tzdata directory, e.g. `Europe/Prague` from
   * `/usr/share/zoneinfo/Europe/Prague` or `../usr/share/zoneinfo/posix/Europe/Prague`.
   */
  def zoneIdFromPath(path: String): Option[String] = {
    val marker = "zoneinfo/"
    val idx    = path.indexOf(marker)
    if (idx < 0) None
    else Some(path.substring(idx + marker.length).stripPrefix("posix/")).filter(_.nonEmpty)
  }
}
