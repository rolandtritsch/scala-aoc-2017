package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day02Spec extends FlatSpec with Matchers {

  val testSheet = List(
    List(5, 1, 9, 5),
    List(7, 5, 3),
    List(2, 4, 6, 8)
  )

  behavior of "readInput()"
  it should "read the (begining) of the input" in {
    Day02.input.head should be (List(737, 1866, 1565, 1452, 1908, 1874, 232, 1928, 201, 241, 922, 281, 1651, 1740, 1012, 1001))
  }

  behavior of "solve() - Part1"
  it should "solve the testcases" taggedAs(BuildTest) in {
    Day02.Part1.solve(testSheet) shouldBe 18
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day02.Part1.solve(Day02.input) shouldBe 34925
  }

  it should "throw an exception, if the sheet is malformed" in {
    an [IllegalArgumentException] should be thrownBy Day02.Part1.solve(List())
    an [IllegalArgumentException] should be thrownBy Day02.Part1.solve(List(List()))
    an [IllegalArgumentException] should be thrownBy Day02.Part1.solve(List(List()))
    an [IllegalArgumentException] should be thrownBy Day02.Part1.solve(List(List(0), List(), List(0)))
  }

  val testSheet2 = List(
    List(5, 9, 2, 8),
    List(9, 4, 7, 3),
    List(3, 8, 6, 5)
  )

  behavior of "solve() - Part2"
  it should "solve the testcases" taggedAs(BuildTest) in {
    Day02.Part2.solve(testSheet2) shouldBe 9
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day02.Part2.solve(Day02.input) shouldBe 221
  }
}