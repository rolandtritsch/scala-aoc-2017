package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day13Spec extends FlatSpec with Matchers {

  val testInput = List(
    "0: 3",
    "1: 2",
    "4: 4",
    "6: 4"
  )

  behavior of "readInput()"
  it should "be correct" in {
    Day13.input.head shouldBe "0: 4"
  }

  behavior of "parseInput()"
  it should "parse correctly" in {
    val parsed = Day13.parseInput(Day13.input)
    parsed(0) shouldBe 4
    parsed(98) shouldBe 18
  }

  behavior of "solve() - Part1"
  it should "solve the testcase(s)" in {
    Day13.Part1.solve(testInput) shouldBe 24
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day13.Part1.solve(Day13.input) shouldBe 1632
  }

  behavior of "solve() - Part2"
  it should "solve the testcase(s)" in {
    Day13.Part2.solve(testInput) shouldBe 10
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day13.Part2.solve(Day13.input) shouldBe 3834136
  }
}
