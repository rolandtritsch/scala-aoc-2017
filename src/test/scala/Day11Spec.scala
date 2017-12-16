package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day11Spec extends FlatSpec with Matchers {

  "input" should "be correct" in {
    Day11.in.take(5) should be (List("nw", "n", "n", "se", "ne"))
  }

  "calcSteps" should "return the correct results" in {
    Day11.calcSteps(List("ne", "ne", "ne")) shouldBe 0
    Day11.calcSteps(List("ne", "ne", "sw", "sw")) shouldBe 0
    Day11.calcSteps(List("ne", "ne", "s", "s")) shouldBe 0
    Day11.calcSteps(List("se", "sw", "se", "sw", "sw")) shouldBe 0
  }

}
