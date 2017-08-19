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
  "commons-io" % "commons-io" % "2.5"
)
