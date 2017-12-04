package aoc

import aoc.Day3.Coordinates
import org.scalatest.{FlatSpec, Matchers}

class Day3Spec extends FlatSpec with Matchers {

  val testGrid = Array(
    Array(17, 16, 15, 14, 13),
    Array(18,  5,  4,  3, 12),
    Array(19,  6,  1,  2, 11),
    Array(20,  7,  8,  9, 10),
    Array(21, 22, 23, 24, 25)
  )

  val rotatedGridLeft = Array(
    Array(21, 20, 19, 18, 17),
    Array(22,  7,  6,  5, 16),
    Array(23,  8,  1,  4, 15),
    Array(24,  9,  2,  3, 14),
    Array(25, 10, 11, 12, 13)
  )

  "Circular2DArray" should "return the testgrid" in {
    val grid = Day3.Circular2DArray.build(25)
    //println(grid.grid.map(_.toList).toList)
    grid.grid should equal (testGrid)
  }

  it should "find the right numbers in the right location/coordinates" in {
    val grid = Day3.Circular2DArray.build(25)
    grid.find(25) should equal (Coordinates(2, 2))
    grid.find(1) should equal (Coordinates(0, 0))
  }

  it should "be able to be rotated" in {
    val grid = Day3.Circular2DArray.build(25)
    grid.grid = Day3.Circular2DArray.rotateLeft(grid.grid)
    grid.grid should equal (rotatedGridLeft)
    grid.grid = Day3.Circular2DArray.rotateRight(grid.grid)
    grid.grid should equal (testGrid)
  }

  "calcDimension" should "return the required dimension to accomodate n" in {
    val results = List(
      (1, 1),
      (2, 3), (3, 3), (4, 3), (5, 3), (6, 3), (7, 3), (8, 3), (9, 3),
      (10, 5), (25, 5),
      (26, 7), (49, 7),
      (50, 8)
    )
    results.forall(r => Day3.Circular2DArray.calcDimensions(r._1) == r._2)
  }

  "distance" should "return the correct result(s)" in {
    Day3.distance(1) shouldBe 0
    Day3.distance(12) shouldBe 3
    Day3.distance(23) shouldBe 2
    Day3.distance(1024) shouldBe 31
  }

  ignore should "solve the puzzle" in {
    Day3.distance(Day3.in) shouldBe 371
  }

  it should "throw an exception, if the input location is <= 0" in {
    an [IllegalArgumentException] should be thrownBy Day3.distance(0)
  }

  "moves" should "return the initial moves" in {
    Day3.moves.take(8).toList should be (Day3.initalLoop.flatten.toList)
  }

  "init" should "return the testGrid" in {
    Day3.initCircular2DArray(25) should be (testGrid)
  }

  ignore should "solve the puzzle" in {
    val grid = Day3.initCircular2DArray(Day3.in)
    val pos = Day3.findPos(Day3.in, grid)
    Day3.distanceToPort(pos) shouldBe 371
  }
}
