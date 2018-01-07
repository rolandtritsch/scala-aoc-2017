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

  "the input" should "have the first line in it" in {
    Day19.in.head.indexOf('|') shouldBe 163
  }

  "walking the maze" should "give the right result(s)" in {
    Day19.walkTheMaze(testInput) shouldBe ("ABCDEF", 38)
  }

  it should "solve the puzzle" in {
    Day19.walkTheMaze(Day19.in) shouldBe ("PVBSCMEQHY", 17736)
  }
}