enablePlugins(ScalaNativePlugin)

scalaVersion := "2.11.11"

nativeMode := "debug"
nativeGC := "boehm" // Note: Setting this to none, will make the run fail with a core dump
