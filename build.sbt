organization := "com.example"

name := "nettyplayin"

val unusedWarnings = (
  "-Ywarn-unused" ::
  "-Ywarn-unused-import" ::
  Nil
)

scalacOptions ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)){
  case Some((2, v)) if v >= 11 => unusedWarnings
}.toList.flatten

Seq(Compile, Test).flatMap(c =>
  scalacOptions in (c, console) --= unusedWarnings
)

scalacOptions ++= "-deprecation" :: "unchecked" :: "-feature" :: Nil

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.11"

val unfilteredVersion = "0.9.1"

libraryDependencies ++= Seq(
  "ws.unfiltered" %% "unfiltered-netty-server" % unfilteredVersion,
  "net.databinder.dispatch" %% "dispatch-core" % "0.13.1",
  "ws.unfiltered" %% "unfiltered-specs2" % unfilteredVersion % "test",
  "org.prevayler" % "prevayler-core" % "2.6",
  "io.spray" %%  "spray-json" % "1.3.3",
  "commons-io" % "commons-io" % "2.5",
  "joda-time" % "joda-time" % "2.9.9"
)


assemblyMergeStrategy in assembly := {
  case x if Assembly.isConfigFile(x) =>
    MergeStrategy.concat
  case PathList(ps @ _*) if Assembly.isReadme(ps.last) || Assembly.isLicenseFile(ps.last) =>
    MergeStrategy.rename
  case PathList("META-INF", xs @ _*) =>
    (xs map {_.toLowerCase}) match {
      case ("manifest.mf" :: Nil) | ("index.list" :: Nil) | ("dependencies" :: Nil) =>
        MergeStrategy.discard
      case ps @ (x :: xs) if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") =>
        MergeStrategy.discard
      case "plexus" :: xs =>
        MergeStrategy.discard
      case "services" :: xs =>
        MergeStrategy.filterDistinctLines
      case ("spring.schemas" :: Nil) | ("spring.handlers" :: Nil) =>
        MergeStrategy.filterDistinctLines
      case "io.netty.versions.properties" :: Nil => MergeStrategy.filterDistinctLines
      case _ => MergeStrategy.deduplicate
    }
  case PathList(ps @ _*) => ps match {
    case "io" :: "netty" :: tail  => MergeStrategy.first
    case _ => MergeStrategy.deduplicate
  }
  case _ => MergeStrategy.deduplicate
}
