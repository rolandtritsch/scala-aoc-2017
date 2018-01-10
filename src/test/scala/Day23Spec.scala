package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day23Spec extends FlatSpec with Matchers {
  "the input" should "be correct" in {
    Day23.in.head shouldBe "set l 1"
  }

  "the parser" should "return the correct result(s)" in {
    Day23.parseInput(Day23.in).size shouldBe 33
  }

  "running the program" should "solve the puzzle" in {
    Day23.solveRun(Day23.Program(0, Day23.parseInput(Day23.in), Map.empty[Char, Long].withDefaultValue(0), Map.empty[String, Long].withDefaultValue(0))) shouldBe 6724
  }

  "running the program" should "solve the puzzle (again)" in {
    Day23.solveRun2(
      Day23.Program(
        0,
        Day23.parseInput(Day23.in),
        Map.empty[Char, Long].withDefaultValue(0L) + ('a' -> 1L),
        Map.empty[String, Long].withDefaultValue(0)
      )
    ) shouldBe 0
  }
}