package aoc

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.tagobjects.Slow

class Day03Spec extends FlatSpec with Matchers {

  val testGrid = Array(
    Array(17, 16, 15, 14, 13),
    Array(18,  5,  4,  3, 12),
    Array(19,  6,  1,  2, 11),
    Array(20,  7,  8,  9, 10),
    Array(21, 22, 23, 24, 25)
  )

  "moves" should "return the initial moves" in {
    Day03.moves(Day03.initalLoop).take(8) should be (Day03.initalLoop.flatten)
  }

  "calcDimension" should "return the required dimension to accomodate n" in {
    val results = List(
      (1, 1),
      (2, 3), (3, 3), (4, 3), (5, 3), (6, 3), (7, 3), (8, 3), (9, 3),
      (10, 5), (25, 5),
      (26, 7), (49, 7),
      (50, 8)
    )
    results.forall(r => Day03.Part1.calcDimensions(r._1) == r._2)
  }

  "init" should "return the test grid" in {
    Day03.Part1.initGrid(25) should be (testGrid)
  }

  "Part 1 - calcDistance" should "return the correct result(s)" in {
    Day03.Part1.calcDistanceFromLocToCenter(1, Day03.Part1.initGrid(1)) shouldBe 0
    Day03.Part1.calcDistanceFromLocToCenter(12, Day03.Part1.initGrid(12)) shouldBe 3
    Day03.Part1.calcDistanceFromLocToCenter(23, Day03.Part1.initGrid(23)) shouldBe 2
    Day03.Part1.calcDistanceFromLocToCenter(1024, Day03.Part1.initGrid(1024)) shouldBe 31
    Day03.Part1.calcDistanceFromLocToCenter(1000000, Day03.Part1.initGrid(1000000)) shouldBe 999
  }

  it should "solve the puzzle" in {
    Day03.Part1.calcDistanceFromLocToCenter(Day03.in, Day03.Part1.initGrid(Day03.in)) shouldBe 371
  }

  "Part 2 - findTheBiggestNumber" should "return the correct result(s)" in {
    Day03.Part2.findNextBiggestNumber(747) shouldBe 806
    Day03.Part2.findNextBiggestNumber(1000000) shouldBe 1009457
  }

  it should "solve the puzzle" in {
    Day03.Part2.findNextBiggestNumber(Day03.in) shouldBe 369601
  }
}
