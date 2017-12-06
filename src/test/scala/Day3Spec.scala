package aoc

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.tagobjects.Slow

class Day3Spec extends FlatSpec with Matchers {

  val testGrid = Array(
    Array(17, 16, 15, 14, 13),
    Array(18,  5,  4,  3, 12),
    Array(19,  6,  1,  2, 11),
    Array(20,  7,  8,  9, 10),
    Array(21, 22, 23, 24, 25)
  )

  "moves" should "return the initial moves" in {
    Day3.moves(Day3.initalLoop).take(8) should be (Day3.initalLoop.flatten)
  }

  "calcDimension" should "return the required dimension to accomodate n" in {
    val results = List(
      (1, 1),
      (2, 3), (3, 3), (4, 3), (5, 3), (6, 3), (7, 3), (8, 3), (9, 3),
      (10, 5), (25, 5),
      (26, 7), (49, 7),
      (50, 8)
    )
    results.forall(r => Day3.Part1.calcDimensions(r._1) == r._2)
  }

  "init" should "return the test grid" in {
    Day3.Part1.initGrid(25) should be (testGrid)
  }

  "Part 1 - calcDistance" should "return the correct result(s)" in {
    Day3.Part1.calcDistanceFromLocToCenter(1, Day3.Part1.initGrid(1)) shouldBe 0
    Day3.Part1.calcDistanceFromLocToCenter(12, Day3.Part1.initGrid(12)) shouldBe 3
    Day3.Part1.calcDistanceFromLocToCenter(23, Day3.Part1.initGrid(23)) shouldBe 2
    Day3.Part1.calcDistanceFromLocToCenter(1024, Day3.Part1.initGrid(1024)) shouldBe 31
  }

  it should "solve the puzzle" in {
    Day3.Part1.calcDistanceFromLocToCenter(Day3.in, Day3.Part1.initGrid(Day3.in)) shouldBe 371
  }

  "Part 2 - findTheBiggestNumber" should "return the correct result(s)" in {
    Day3.Part2.findNextBiggestNumber(747) shouldBe 806
  }

  it should "solve the puzzle" in {
    Day3.Part2.findNextBiggestNumber(Day3.in) shouldBe 369601
  }
}
