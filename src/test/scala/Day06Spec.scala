package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day06Spec extends FlatSpec with Matchers {
  val testBanks = List(0, 2, 7, 0)

  behavior of "readInput"
  it should "read the input" in {
    Day06.in should be (List(10, 3, 15, 10, 5, 15, 5, 15, 9, 2, 5, 8, 5, 2, 3, 6))
  }
  behavior of "redistribute"
  it should "return the correct results" in {
    val step1 = Day06.Part1.redistribute(testBanks)
    step1 should be (List(2, 4, 1, 2))

    val step2 = Day06.Part1.redistribute(step1)
    step2 should be (List(3, 1, 2, 3))

    val step3 = Day06.Part1.redistribute(step2)
    step3 should be (List(0, 2, 3, 4))

    val step4 = Day06.Part1.redistribute(step3)
    step4 should be (List(1, 3, 4, 1))

    val step5 = Day06.Part1.redistribute(step4)
    step5 should be (step1)
  }

  behavior of "detectLoop"
  it should "return the correct result(s)" in {
    Day06.Part1.detectLoop(testBanks) shouldBe (5, 4)
  }

  it should "solve the puzzle" in {
    Day06.Part1.detectLoop(Day06.in) shouldBe (14029, 2765)
  }
}

