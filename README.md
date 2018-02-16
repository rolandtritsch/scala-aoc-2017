![Build Status](https://travis-ci.org/rolandtritsch/scala-aoc-2017.svg?branch=master) [![GitHub issues](https://img.shields.io/github/issues/rolandtritsch/scala-aoc-2017.svg)](https://github.com/rolandtritsch/scala-aoc-2017/issues)

# Doing the [Advent of Code](https://adventofcode.com) - 2017

This is a JVM/Native crossproject.

To make this work you need to ...

* install git (`brew install git`)
* install sbt (`brew install sbt`)
* clone the repo (`git clone ...`)
* run the tests (`sbt aocJVM/test`)
  * this will run all tests (ScalaTest, ScalaCheck, ScalaMeter) and this might take a while
  * you can run specific tests with `sbt "aocJVM/testOnly aoc.Day01Spec"`
  * you can run `sbt "aocJVM/testOnly aoc.Day*Spec -- -l aoc.SlowTest"` to only run the *fast* tests (exclude the slow tests)
  * you can run `sbt "aocJVM/testOnly aoc.Day*Spec -- -n aoc.SolutionTest"` to only run the tests that will test for the correct solutions
* run the JVM main (`sbt aocJVM/run`)
* run the Native main (`sbt aocNative/run`)
* generate the doc (`sbt doc`) and look at it (`open target/scala-2.10/api/index.html`)

Note: To [make Scala Native work](http://www.scala-native.org/en/latest/user/setup.html) you probably need to install a couple more packages.

Have fun!!!

# Refactoring checklist

For every day ...

* review [other solutions](https://github.com/topics/advent-of-code-2017?l=scala) and adjust accordingly
* add require/ensuring
* run/review/adjust (all) tests - sbt aocJVM/testOnly aoc.Day??[Spec|Check|Meter]
* run the fast tests - ???
* run the solution tests - ???
* add a/the documentation on the chosen approach/solution/algorithm
* merge