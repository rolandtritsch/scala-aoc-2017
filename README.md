# Doing the [Advent of Code](https://adventofcode.com) - 2017

This is a JVM/Native crossproject.

To make this work you need to ...

* install git (`brew install git`)
* install sbt (`brew install sbt`)
* clone the repo (`git clone ...`)
* run the tests (`sbt aocJVM/test`)
* run the JVM main (`sbt aocJVM/run`)
* run the Native main (`sbt aocNative/run`)

Note: Right now you need to manually copy the `*.txt` files from the `src/main/resources` directory into `.jvm/target/scala-2.11/test-classes/aoc`.

Have fun!!!

## Todos

* add pre-/post-conditions
* use Part1/Part2 consistently/everywhere
* make test run fast (by excluding long-running test until a full test is requested)
* add property based testing for all days
* optimize implementations for performance (especially Day5)
* refactor the readInput into a package/utility object
* make sure we can find the input files (*input.txt)(or copy them as part of the build process into the right target dir)
* add/use visibility (right now all vals and defs and classes are public (which is not right)) 

