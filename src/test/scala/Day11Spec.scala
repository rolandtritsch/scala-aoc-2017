package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day11Spec extends FlatSpec with Matchers {

  behavior of "input"
  it should "be correct" in {
    Day11.in.take(5) should be (List("nw", "n", "n", "se", "ne"))
  }

  behavior of "calcSteps"
  it should "return the correct results" in {
    Day11.calcSteps(List("ne", "ne", "ne")) shouldBe (3, 3)
    Day11.calcSteps(List("ne", "ne", "sw", "sw")) shouldBe (0, 2)
    Day11.calcSteps(List("ne", "ne", "s", "s")) shouldBe (2, 2)
    Day11.calcSteps(List("se", "sw", "se", "sw", "sw")) shouldBe (3, 3)
  }

  it should "solve the puzzle" in {
    Day11.calcSteps(Day11.in) shouldBe (810, 1567)
  }
}
