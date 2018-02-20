package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day06Spec extends FlatSpec with Matchers {
  val testBanks = List(0, 2, 7, 0)

  behavior of "readInput()"
  it should "read the input" in {
    Day06.input should be (List(10, 3, 15, 10, 5, 15, 5, 15, 9, 2, 5, 8, 5, 2, 3, 6))
  }

  behavior of "cycle()"
  it should "return the correct results" in {
    val step1 = Day06.cycle(testBanks)
    step1 should be (List(2, 4, 1, 2))

    val step2 = Day06.cycle(step1)
    step2 should be (List(3, 1, 2, 3))

    val step3 = Day06.cycle(step2)
    step3 should be (List(0, 2, 3, 4))

    val step4 = Day06.cycle(step3)
    step4 should be (List(1, 3, 4, 1))

    val step5 = Day06.cycle(step4)
    step5 should be (step1)
  }

  behavior of "solve() - Part1"
  it should "solve the testcase" in {
    Day06.Part1.solve(testBanks) shouldBe 5
    Day06.Part1.solve(List.fill(100)(10)) shouldBe 1465
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day06.Part1.solve(Day06.input) shouldBe 14029
  }

  behavior of "solve() - Part2"
  it should "solve the testcase" in {
    Day06.Part2.solve(testBanks) shouldBe 4
    Day06.Part2.solve(List.fill(100)(10)) shouldBe 100
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day06.Part2.solve(Day06.input) shouldBe 2765
  }
}