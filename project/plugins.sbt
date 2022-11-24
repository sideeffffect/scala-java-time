val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.11.0")

addSbtPlugin("org.portable-scala" % "sbt-crossproject" % "1.2.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")

addSbtPlugin("io.github.cquiroz" % "sbt-tzdb" % "4.1.0")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.0")

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.4.1")

addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.11")

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.5.5")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.2.0")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.9")

libraryDependencies += "org.snakeyaml" % "snakeyaml-engine" % "2.5"
libraryDependencySchemes ++= Seq(
  "org.scala-native" % "sbt-scala-native" % VersionScheme.Always
)
