package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day09Spec extends FlatSpec with Matchers {

  behavior of "readInput()"
  it should "read the input" in {
    Day09.input.take(10) should be("{{{{{},{<>".toList)
  }

  behavior of "score()"
  it should "return the right number of groups" in {
    Day09.score("{}".toList)._1.size shouldBe 1
    Day09.score("{{{}}}".toList)._1.size shouldBe 3
    Day09.score("{{},{}}".toList)._1.size shouldBe 3
    Day09.score("{{{},{},{{}}}}".toList)._1.size shouldBe 6
    Day09.score("{<{},{},{{}}>}".toList)._1.size shouldBe 1
    Day09.score("{<a>,<a>,<a>,<a>}".toList)._1.size shouldBe 1
    Day09.score("{{<a>},{<a>},{<a>},{<a>}}".toList)._1.size shouldBe 5
    Day09.score("{{<!>},{<!>},{<!>},{<a>}}".toList)._1.size shouldBe 2
  }

  behavior of "solve() - Part1"
  it should "solve the testcase(s)" in {
    Day09.Part1.solve("{}".toList) shouldBe 1
    Day09.Part1.solve("{{{}}}".toList) shouldBe 6
    Day09.Part1.solve("{{},{}}".toList) shouldBe 5
    Day09.Part1.solve("{{{},{},{{}}}}".toList) shouldBe 16
    Day09.Part1.solve("{<a>,<a>,<a>,<a>}".toList) shouldBe 1
    Day09.Part1.solve("{{<ab>},{<ab>},{<ab>},{<ab>}}".toList) shouldBe 9
    Day09.Part1.solve("{{<!!>},{<!!>},{<!!>},{<!!>}}".toList) shouldBe 9
    Day09.Part1.solve("{{<a!>},{<a!>},{<a!>},{<ab>}}".toList) shouldBe 3
  }

  it should "be able to process garbage" in {
    Day09.Part1.solve("{<>}".toList) shouldBe 1
    Day09.Part1.solve("{<endanbca{dhbch}{{{{{!!!dndcn}}}}}}}}}<<<<<>}".toList) shouldBe 1
    Day09.Part1.solve("{<<<<>}".toList) shouldBe 1
    Day09.Part1.solve("{<{!>}>}".toList) shouldBe 1
    Day09.Part1.solve("{<!!>}".toList) shouldBe 1
    Day09.Part1.solve("{<!!!>>}".toList) shouldBe 1
    Day09.Part1.solve("{<{o\"i!a,<{i<a>}".toList) shouldBe 1
  }

  it should "solve the puzzle" taggedAs (SolutionTest) in {
    Day09.Part1.solve(Day09.input) shouldBe 10800
  }

  behavior of "solve() - Part2"
  it should "solve the puzzle" taggedAs (SolutionTest) in {
    Day09.Part2.solve(Day09.input) shouldBe 4522
  }

  behavior of "run() - Part1"
  it should "solve the testcase(s)" in {
    val testcases = List(
      ("{}", 1),
      ("{{{}}}", 6),
      ("{{},{}}", 5),
      ("{{{},{},{{}}}}", 16),
      ("{<a>,<a>,<a>,<a>}", 1),
      ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9),
      ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9),
      ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3)
    )

    testcases.foreach { tc => {
      val (stream, expectedResult) = tc
      val finalState = Day09.StateMachine(stream.toList).run
      finalState.stats.scores.sum shouldBe expectedResult
    }}
  }

  it should "solve the puzzle" in {
    val finalState = Day09.StateMachine(Day09.input).run
    finalState shouldBe a [Day09.OutOfGroup]
    finalState.stats.scores.sum shouldBe 10800
  }
}
