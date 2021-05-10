# [Advent of Code](https://adventofcode.com) - 2017 (Scala3 Edition)

This is a port/migration of the original scala2 implementation to scala3. The main purpose is to run a quick benchmark between scala2 and scala3.

Note: (As of May 2021) ScalaNative is not available for scala3 yet. Means this port is jvm only.

To make this work you need to ...

* install git (`brew install git`)
* install sbt (`brew install sbt`)
* clone the repo (`git clone ...`)
* run the tests (`sbt test`)
  * this will run all tests and this might take a while
  * you can run specific tests with `sbt "testOnly aoc.Day01Spec"`
  * you can run `sbt "testOnly aoc.Day*Spec -- -l aoc.SlowTest"` to only run the *fast* tests (exclude the slow tests)
  * you can run `sbt "testOnly aoc.Day*Spec -- -n aoc.SolutionTest"` to only run the tests that will test for the correct solutions
* run the JVM main (`sbt run`)
