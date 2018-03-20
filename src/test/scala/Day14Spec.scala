package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day14Spec extends FlatSpec with Matchers {

  val testInput = "flqrgnkx"

  behavior of "readInput()"
  it should "read the input" in {
    Day14.input shouldBe "ugkiagan"
  }

  behavior of "buildGrid()"
  it should "return the correct grid for the testcase" in {
    val grid = Day14.buildGrid(testInput)
    grid(0).take(8) should be(List(true, true, false, true, false, true, false, false))
    grid(1).take(8) should be(List(false, true, false, true, false, true, false, true))
    grid(2).take(8) should be(List(false, false, false, false, true, false, true, false))
    grid(3).take(8) should be(List(true, false, true, false, true, true, false, true))
    grid(4).take(8) should be(List(false, true, true, false, true, false, false, false))
    grid(5).take(8) should be(List(true, true, false, false, true, false, false, true))
    grid(6).take(8) should be(List(false, true, false, false, false, true, false, false))
    grid(7).take(8) should be(List(true, true, false, true, false, true, true, false))
  }

  it should "return the correct grid for the input" taggedAs (SlowTest) in {
    val grid = Day14.buildGrid(Day14.input)
    grid(0).take(10) should be(List(false, false, true, true, true, true, true, true, true, false))
    grid(0).size shouldBe 128
    grid.size shouldBe 128
  }

  behavior of "hex2bin()"
  it should "return the correct result(s)" in {
    Day14.hex2bin("0") shouldBe "0000"
    Day14.hex2bin("1") shouldBe "0001"
    Day14.hex2bin("70") shouldBe "01110000"
    Day14.hex2bin("e") shouldBe "1110"
    Day14.hex2bin("f") shouldBe "1111"
    Day14.hex2bin("0a0c20170") shouldBe "000010100000110000100000000101110000"
  }

  behavior of "solve() - Part1"
  it should "solve the testcase(s)" taggedAs (SlowTest) in {
    Day14.Part1.solve(testInput) shouldBe 8108
  }

  it should "solve the puzzle" taggedAs (SolutionTest) in {
    Day14.Part1.solve(Day14.input) shouldBe 8292
  }

  behavior of "nextCoordinates()"
  it should "return the right coordinates" in {
    Day14.nextCoordinates(0, 0, 128) should be (List((0, 1), (1, 0)))
    Day14.nextCoordinates(0, 1, 128) should be (List((0,2), (0,0), (1,1)))
    Day14.nextCoordinates(1, 0, 128) should be (List((1,1), (2,0), (0,0)))
    Day14.nextCoordinates(1, 1, 128) should be (List((1,2), (1,0), (2,1), (0,1)))
  }

  behavior of "findRegion()"
  it should "find the first region in the testcase(s)" in {
    val grid = Day14.buildGrid(testInput)
    Day14.findRegion(0, 0, grid) should be (List((1,1), (0,1), (0,0)))
    Day14.findRegion(0, 3, grid) should be (List((1,3), (0,3)))
    Day14.findRegion(0, 5, grid) should be (List((1,5), (0,5)))
    Day14.findRegion(2, 4, grid) should be (List((5,4), (4,4), (3,5), (3,4), (2,4)))
  }

  behavior of "solve() - Part2"
  it should "solve the testcase(s)" taggedAs(SlowTest) in {
    Day14.Part2.solve(testInput) shouldBe 1242
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day14.Part2.solve(Day14.input) shouldBe 1069
  }
}
