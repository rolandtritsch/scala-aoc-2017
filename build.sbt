import sbtcrossproject.{crossProject, CrossType}

val sharedSettings = Seq(
  name := "advent-of-code",
  organization := "org.tritsch",
  version := "0.2.0",

  scalaVersion in ThisBuild:= "2.13.4"
)

val jvmSettings = Seq(
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.5",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test",
  libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.3" % "test",
  libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.20" % "test",

  testFrameworks += new TestFramework(
    "org.scalameter.ScalaMeterFramework"
  ),

  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
)

val nativeSettings = Seq(
  nativeMode := "debug",
  nativeGC := "boehm"
)

lazy val aoc = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(sharedSettings)
  .jvmSettings(jvmSettings)
  .nativeSettings(nativeSettings)

lazy val aocJVM = aoc.jvm
lazy val aocNative = aoc.native
