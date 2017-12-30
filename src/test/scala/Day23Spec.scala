package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day23Spec extends FlatSpec with Matchers {
  "the input" should "be correct" in {
    Day23.in.head shouldBe "set b 84"
  }

  "the parser" should "return the correct result(s)" in {
    Day23.parseInput(Day23.in).size shouldBe 32
  }

  "running the program" should "solve the puzzle" in {
    //Day23.solveRun(Day23.Program(0, Day23.parseInput(Day23.in), Map.empty[Char, Int].withDefaultValue(0), Map.empty[String, Int].withDefaultValue(0))) shouldBe 0
  }
}