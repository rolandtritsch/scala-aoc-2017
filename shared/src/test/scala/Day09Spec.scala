package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day09Spec extends FlatSpec with Matchers {

  behavior of "readInput()"
  it should "read the input" in {
    Day09.input.take(10) should be("{{{{{},{<>".toList)
  }

  behavior of "solve() - Part1"
  it should "be able to process garbage" in {
    Day09.Part1.solve("{<>}".toList)._1 shouldBe 1
    Day09.Part1.solve("{<endanbca{dhbch}{{{{{!!!dndcn}}}}}}}}}<<<<<>}".toList)._1 shouldBe 1
    Day09.Part1.solve("{<<<<>}".toList)._1 shouldBe 1
    Day09.Part1.solve("{<{!>}>}".toList)._1 shouldBe 1
    Day09.Part1.solve("{<!!>}".toList)._1 shouldBe 1
    Day09.Part1.solve("{<!!!>>}".toList)._1 shouldBe 1
    Day09.Part1.solve("{<{o\"i!a,<{i<a>}".toList)._1 shouldBe 1
  }

  it should "solve the testcase(s)" in {
    val testcases = List(
      ("{}", 1, 1),
      ("{{{}}}", 6, 3),
      ("{{},{}}", 5, 3),
      ("{{{},{},{{}}}}", 16, 6),
      ("{<a>,<a>,<a>,<a>}", 1, 1),
      ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9, 5),
      ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9, 5),
      ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3, 2)
    )

    testcases.foreach { tc => {
      val (stream, sum, size) = tc
      val finalState = Day09.StateMachine(stream.toList).run
      finalState shouldBe a [Day09.OutOfGroup]
      finalState.stats.scores.sum shouldBe sum
      finalState.stats.scores.size shouldBe size
    }}
  }

  it should "solve the puzzle" taggedAs (SolutionTest, BuildTest) in {
    Day09.Part1.solve(Day09.input)._1 shouldBe 10800
  }

  behavior of "solve() - Part2"
  it should "solve the puzzle" taggedAs (SolutionTest, BuildTest) in {
    Day09.Part2.solve(Day09.input)._1 shouldBe 4522
  }
}
