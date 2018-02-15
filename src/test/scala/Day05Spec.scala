package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day05Spec extends FlatSpec with Matchers {

  "readInput" should "read the instructions" in {
    val inputFirstTen = List(0, 1, 0, 0, 1, -3, 0, 0, 2, -2)
    Day05.in.take(10) should be (inputFirstTen)
  }

  val testStack = List(0, 3, 0, 1, -3)

  "Part 1 - countSteps" should "return the right result(s)" in {
    //Day05.Part1.countSteps(testStack, 0) shouldBe 5
  }

  ignore should "solve the puzzle" in {
    Day05.Part1.countSteps(Day05.in, 0) shouldBe 372139
  }

  "Part 2 - countSteps" should "return the right result(s)" in {
    //Day05.Part2.countSteps(testStack, 0) shouldBe (10, List(2, 3, 2, 3, -1))
  }

  ignore should "solve the puzzle" in {
    Day05.Part2.countSteps(Day05.in, 0)._1 shouldBe 29629538
  }
}