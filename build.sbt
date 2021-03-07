import sbtcrossproject.{crossProject, CrossType}

enablePlugins(ScalaNativePlugin)

val sharedSettings = Seq(
  name := "advent-of-code",
  organization := "org.tritsch",
  version := "0.2.0",

  scalaVersion in ThisBuild:= "2.13.4"
)

val jvmSettings = Seq(
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.5",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test",

  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
)

val nativeSettings = Seq(
  nativeMode := "debug",
  nativeGC := "immix"
)

lazy val aoc = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(sharedSettings)
  .jvmSettings(jvmSettings)
  .nativeSettings(nativeSettings)

lazy val aocJVM = aoc.jvm
lazy val aocNative = aoc.native
