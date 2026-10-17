package java.util

import java.text.DateFormatSymbols

import java.time.{ Instant, ZoneId }
import java.time.zone.ZoneRulesProvider

import scala.collection.JavaConverters._
import scala.util.Try
import scala.scalajs.LinkingInfo
import scala.scalajs.js
import js.annotation._

@js.native
trait DateTimeFormatOptions extends js.Object {
  val timeZone: js.UndefOr[String]
}

@js.native
@JSGlobal("Intl.DateTimeFormat")
class DateTimeFormat() extends js.Object {
  def resolvedOptions(): DateTimeFormatOptions = js.native
}

object TimeZone {
  final val SHORT = 0
  final val LONG  = 1

  private var default: TimeZone =
    // On a WebAssembly target without a JavaScript host (the scala-wasm fork's
    // `ModuleKind.MinimalWasmModule` = 4 and `ModuleKind.WasmComponent` = 5),
    // `js.Date` and `Intl` do not exist, and any reference to them fails at link
    // time. `linkTimeIf` removes the unused branch at link time, so on such
    // targets the default time zone is UTC and no JavaScript is linked. All other
    // targets (JS, Wasm with a JS host) keep the detection below.
    // The values are literals because mainline Scala.js does not define these
    // two `LinkingInfo.ModuleKind` constants. Mainline uses only 1 to 3.
    LinkingInfo.linkTimeIf[TimeZone](
      LinkingInfo.isWebAssembly && (LinkingInfo.moduleKind == 4 || LinkingInfo.moduleKind == 5)
    ) {
      new SimpleTimeZone(0, "UTC")
    } {
      hostDefault()
    }

  private def hostDefault(): TimeZone = {
    // This is supported since EcmaScript 1
    def offsetInMillis: Int = {
      val browserDate = new scalajs.js.Date()
      browserDate.getTimezoneOffset().toInt * 60 * 1000
    }

    def timeZone: String = {
      def browserTZ: Try[String] =
        Try {
          val browserDate = new scalajs.js.Date()
          browserDate.toTimeString().split(' ')(1).takeWhile(e => e != ' ')
        }

      Try {
        // First try with the intl API
        new DateTimeFormat().resolvedOptions().timeZone.getOrElse(browserTZ.getOrElse("UTC"))
      }.orElse {
        // If it fails try to parse it from the date string
        browserTZ
      }.getOrElse("UTC") // Fallback to UTC
    }

    new SimpleTimeZone(offsetInMillis, timeZone)
  }

  def getDefault: TimeZone                 = default
  def setDefault(timeZone: TimeZone): Unit = default = timeZone

  def getTimeZone(timeZone: String): TimeZone = getTimeZone(ZoneId.of(timeZone))
  def getTimeZone(zoneId: ZoneId): TimeZone   = new ZoneIdTimeZone(zoneId)

  // The raw offset is the standard offset at the current instant, so it needs the
  // wall clock. It is computed on the first call of `getRawOffset`, not when the
  // time zone is created. Code that only needs the ID or the display names (for
  // example the zone text printer-parser of `DateTimeFormatter`, used by every
  // `DateTimeFormatter.ofPattern`) then does not read the clock, and the linker can
  // drop `Instant.now`. This matters on WebAssembly without a JavaScript host, where
  // the clock is not available and any reference to it fails at link time.
  // The rules are still looked up eagerly, so an unknown zone fails here, as before.
  private final class ZoneIdTimeZone(zoneId: ZoneId) extends SimpleTimeZone(0, zoneId.getId) {
    private val rules = zoneId.getRules

    private lazy val currentStandardOffset: Int =
      rules.getStandardOffset(Instant.now).getTotalSeconds * 1000

    override def getRawOffset: Int = currentStandardOffset
  }

  def getAvailableIDs: Array[String]                    = ZoneRulesProvider.getAvailableZoneIds.asScala.toArray
  def getAvailableIDs(offsetMillis: Int): Array[String] =
    getAvailableIDs.filter(getTimeZone(_).getRawOffset == offsetMillis)

}

@SerialVersionUID(3581463369166924961L)
abstract class TimeZone extends Serializable with Cloneable {
  /* values */
  private var ID: String = null

  /* abstract methods */
  // def getOffset(era: Int, year: Int, month: Int, day: Int, dayOfWeek: Int, milliseconds: Int): Int
  def getRawOffset: Int
  // def inDaylightTime(date: Date): Boolean
  // def setRawOffset(offsetMillis: Int): Unit
  // def useDaylightTime: Boolean

  /* concrete methods */
  def getID: String           = ID
  def setID(id: String): Unit = ID = id

  def getDisplayName(daylight: Boolean, style: Int, locale: Locale): String = {
    if (style != TimeZone.SHORT && style != TimeZone.LONG)
      throw new IllegalArgumentException(s"Illegal timezone style: $style")

    // Safely looks up given index in the array
    def atIndex(strs: Array[String], idx: Int): Option[String] =
      if (idx >= 0 && idx < strs.length) Option(strs(idx))
      else None

    val id                                             = getID
    def currentIdStrings(strs: Array[String]): Boolean =
      atIndex(strs, 0).contains(id)

    val zoneStrings = DateFormatSymbols.getInstance(locale).getZoneStrings
    val zoneName    = zoneStrings.find(currentIdStrings).flatMap { strs =>
      (daylight, style) match {
        case (false, TimeZone.LONG)  => atIndex(strs, 1)
        case (false, TimeZone.SHORT) => atIndex(strs, 2)
        case (true, TimeZone.LONG)   => atIndex(strs, 3)
        case (true, TimeZone.SHORT)  => atIndex(strs, 4)
        case _                       => None
      }
    }

    zoneName.orElse {
      if (id.startsWith("GMT+") || id.startsWith("GMT-")) Some(id)
      else None
    }.orNull
  }

  def getDisplayName(daylight: Boolean, style: Int): String =
    getDisplayName(daylight, style, Locale.getDefault(Locale.Category.DISPLAY))

  def getDisplayName(locale: Locale): String =
    getDisplayName(daylight = false, TimeZone.LONG, locale)

  def getDisplayName: String =
    getDisplayName(daylight = false, TimeZone.LONG, Locale.getDefault(Locale.Category.DISPLAY))

  def toZoneId: ZoneId = ZoneId.of(getID)

  override def clone: AnyRef = {
    val cloned = super.clone.asInstanceOf[TimeZone]
    cloned.ID = this.ID
    cloned
  }
}
