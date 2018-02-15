# Doing the [Advent of Code](https://adventofcode.com) - 2017

This is a JVM/Native crossproject.

To make this work you need to ...

* install git (`brew install git`)
* install sbt (`brew install sbt`)
* clone the repo (`git clone ...`)
* run the tests (`sbt aocJVM/test`)
* run the JVM main (`sbt aocJVM/run`)
* run the Native main (`sbt aocNative/run`)

Note: To [make Scala Native work](http://www.scala-native.org/en/latest/user/setup.html) you probably need to install a couple more packages.

Have fun!!!

## Todos

* add documentation (explain the main idea(s) behind the approach/algorithm)
* add pre-/post-conditions
* get rid of Part1/Part2 consistently/everywhere
* make test run fast (by excluding long-running test until a full test is requested)
* add property based testing for all days
* optimize implementations for performance (especially Day5)
* add/use visibility (right now all vals and defs and classes are public (which is not right)) 
* add a test flag to only run the solution tests
* use behaviour to make FlatSpec more readable/maintainable
* fix/remove the compile warnings about the conflicts
* add @todos/resolve @todos