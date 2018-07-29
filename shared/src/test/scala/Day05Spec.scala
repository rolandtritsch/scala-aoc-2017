package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day05Spec extends FlatSpec with Matchers {

  behavior of "readInput()"
  it should "read the instructions" in {
    Day05.input.take(10) should be (List(0, 1, 0, 0, 1, -3, 0, 0, 2, -2))
  }

  val testStack = List(0, 3, 0, 1, -3)
  val testStack2= List.fill(100)(1)

  behavior of "solve() - Part1"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day05.Part1.solve(testStack)._1 shouldBe 5
    Day05.Part1.solve(testStack2)._1 shouldBe testStack2.size
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day05.Part1.solve(Day05.input)._1 shouldBe 372139
  }

  behavior of "solve() - Part2"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day05.Part2.solve(testStack)._1 shouldBe 10
    Day05.Part2.solve(testStack2)._1 shouldBe testStack2.size
  }

  it should "solve the puzzle" taggedAs(SolutionTest, SlowTest) in {
    Day05.Part2.solve(Day05.input)._1 shouldBe 29629538
  }
}
