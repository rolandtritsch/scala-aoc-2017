import sbtcrossproject.{crossProject, CrossType}

val sharedSettings = Seq(
  name := "advent-of-code",
  organization := "org.tritsch",
  version := "0.1.0-SNAPSHOT",

  scalaVersion := "2.11.11"
)

val jvmSettings = Seq(
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.9" % "test",

  testFrameworks += new TestFramework(
    "org.scalameter.ScalaMeterFramework"
  ),

  scalacOptions ++= Seq("-unchecked", "-deprecation")
)

val nativeSettings = Seq(
  nativeMode := "debug",
  nativeGC := "boehm"
)

lazy val aoc = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(sharedSettings)
  .jvmSettings(jvmSettings)
  .nativeSettings(nativeSettings)

lazy val aocJVM = aoc.jvm
lazy val aocNative = aoc.native
