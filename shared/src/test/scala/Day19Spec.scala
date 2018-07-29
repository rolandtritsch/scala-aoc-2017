package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day19Spec extends FlatSpec with Matchers {

  val testInput = Array(
    "      |         ",
    "      |  +--+   ",
    "      A  |  C   ",
    "  F---|----E|--+",
    "      |  |  |  D",
    "      +B-+  +--+"
  ).map(_.toCharArray)

  behavior of "readInput()"
  it should "have the first line in it" in {
    Day19.input.head.indexOf('|') shouldBe 163
  }

  behavior of "walkTheMaze()"
  it should "give the right result(s)" taggedAs(BuildTest) in {
    Day19.walkTheMaze(testInput) shouldBe ("ABCDEF", 38)
  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day19.Part1.solve(Day19.input)._1 shouldBe "PVBSCMEQHY"
  }

  behavior of "solve() - Part2"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day19.Part2.solve(Day19.input)._1 shouldBe 17736
  }
}
