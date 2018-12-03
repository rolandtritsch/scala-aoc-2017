![Build Status](https://travis-ci.org/rolandtritsch/scala-aoc-2017.svg?branch=master) [![GitHub issues](https://img.shields.io/github/issues/rolandtritsch/scala-aoc-2017.svg)](https://github.com/rolandtritsch/scala-aoc-2017/issues)

# [Advent of Code](https://adventofcode.com) - 2017 (Scala Edition)

Note: There are implementations of this in [Scala](https://github.com/rolandtritsch/scala-aoc-2017) (JVM and native), [Kotlin](https://github.com/rolandtritsch/kotlin-aoc-2017), [Haskell](https://github.com/rolandtritsch/haskell-aoc-2017) and [Eta](https://github.com/rolandtritsch/eta-aoc-2017). If you like any of these ... star it :).

This is a JVM/Native crossproject. To make this work you need to ...

* install git (`brew install git`)
* install sbt (`brew install sbt`)
* clone the repo (`git clone ...`)
* run the tests (`sbt aocJVM/test`)
  * this will run all tests (ScalaTest, ScalaCheck, ScalaMeter) and this might take a while (60 - 90 mins)
  * you can run specific tests with `sbt "aocJVM/testOnly aoc.Day01Spec"`
  * you can run `sbt "aocJVM/testOnly aoc.Day*Spec -- -l aoc.SlowTest"` to only run the *fast* tests (exclude the slow tests)
  * you can run `sbt "aocJVM/testOnly aoc.Day*Spec -- -n aoc.SolutionTest"` to only run the tests that will test for the correct solutions
* run the JVM main (`sbt aocJVM/run`)
* run the Native main (`sbt aocNative/run`)
* generate the [doc](http://www.tritsch.org/scala-aoc-2017) (`sbt doc`) and look at it (`open target/scala-2.10/api/index.html`)

Note: To [make Scala Native work](http://www.scala-native.org/en/latest/user/setup.html) you probably need to install a couple more packages.

Have fun!!!

## Benchmark

I also [benchmarked](https://docs.google.com/spreadsheets/d/1kHugZ-8mJczlmQRcda23YGvAgeqlJLt1I7cYlDD3Tws/edit?usp=sharing) the solutions [against each other](https://github.com/rolandtritsch/scala-aoc-2017/tree/master/results) (Scala, Native, Kotlin, Haskell, Eta).

![Benchmark](https://www.dropbox.com/s/5sfnqgl9u57kekp/benchmark.png?dl=0&raw=1)
