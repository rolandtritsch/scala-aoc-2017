package aoc

import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day11Spec extends AnyFlatSpec with should.Matchers {

  behavior of "readInput()"
  it should "return the (beginning) of the input" in {
    Day11.input.take(5) should be (List("nw", "n", "n", "se", "ne"))
  }

  behavior of "calcSteps()"
  it should "solve the testcase(s)" in {
    Day11.calcSteps(List("ne", "ne", "ne")) shouldBe (3, 3)
    Day11.calcSteps(List("ne", "ne", "sw", "sw")) shouldBe (0, 2)
    Day11.calcSteps(List("ne", "ne", "s", "s")) shouldBe (2, 2)
    Day11.calcSteps(List("se", "sw", "se", "sw", "sw")) shouldBe (3, 3)
  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest, BuildTest) in {
    Day11.Part1.solve(Day11.input)._1 shouldBe 810
  }

  behavior of "solve() - Part2"
  it should "solve the puzzle" taggedAs(SolutionTest, BuildTest) in {
    Day11.Part2.solve(Day11.input)._1 shouldBe 1567
  }
}
