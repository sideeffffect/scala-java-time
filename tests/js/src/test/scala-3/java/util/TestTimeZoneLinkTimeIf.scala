package java.util

import org.scalatest.funsuite.AnyFunSuite

import scala.scalajs.LinkingInfo

// The default time zone selects its implementation with `LinkingInfo.linkTimeIf`.
// Scala 3 compiles `linkTimeIf` natively only since 3.8.0; with older Scala 3
// compilers the call is a stub that throws at run time. This checks that the
// Scala 3 build uses a compiler that supports it, and that a JS host target takes
// the JS host branch.
class TestTimeZoneLinkTimeIf extends AnyFunSuite {

  test("linkTimeIf selects the JS host branch on a JS host") {
    val noJSHost = LinkingInfo.linkTimeIf(
      LinkingInfo.isWebAssembly && (LinkingInfo.moduleKind == 4 || LinkingInfo.moduleKind == 5)
    )(true)(false)
    assert(!noJSHost)
  }

  test("TimeZone.getDefault initializes") {
    assert(TimeZone.getDefault.getID.nonEmpty)
  }
}
