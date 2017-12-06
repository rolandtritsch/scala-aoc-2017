package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day6Spec extends FlatSpec with Matchers {
  val testBanks = List(0, 2, 7, 0)

  "redistribute" should "return the correct results" in {
    val step1 = Day6.Part1.redistribute(testBanks)
    step1 should be (List(2, 4, 1, 2))

    val step2 = Day6.Part1.redistribute(step1)
    step2 should be (List(3, 1, 2, 3))

    val step3 = Day6.Part1.redistribute(step2)
    step3 should be (List(0, 2, 3, 4))

    val step4 = Day6.Part1.redistribute(step3)
    step4 should be (List(1, 3, 4, 1))

    val step5 = Day6.Part1.redistribute(step4)
    step5 should be (step1)
  }

  "detectLoop" should "return the correct result(s)" in {
    Day6.Part1.detectLoop(testBanks) shouldBe (5, 4)
  }

  it should "solve the puzzle" in {
    Day6.Part1.detectLoop(Day6.in) shouldBe (14029, 2765)
  }
}

