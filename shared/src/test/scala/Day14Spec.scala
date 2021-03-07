package aoc

import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day14Spec extends AnyFlatSpec with should.Matchers {

  val testInput = "flqrgnkx"

  val testGrid = List(
    List(true, true, false, true, false, true, false, false),
    List(false, true, false, true, false, true, false, true),
    List(false, false, false, false, true, false, true, false),
    List(true, false, true, false, true, true, false, true),
    List(false, true, true, false, true, false, false, false),
    List(true, true, false, false, true, false, false, true),
    List(false, true, false, false, false, true, false, false),
    List(true, true, false, true, false, true, true, false)
  )

  behavior of "readInput()"
  it should "read the input" in {
    Day14.input shouldBe "ugkiagan"
  }

  behavior of "buildGrid()"
  it should "return the correct grid for the testcase" in {
    val grid = Day14.buildGrid(testInput)
    (0 until 8).forall(i => grid(i).take(8).sameElements(testGrid(i)))
  }

  it should "return the correct grid for the input" taggedAs(SlowTest) in {
    val grid = Day14.buildGrid(Day14.input)
    grid(0).take(10) should be(List(false, false, true, true, true, true, true, true, true, false))
    grid(0).size shouldBe 128
    grid.size shouldBe 128
  }

  behavior of "hex2bin()"
  it should "return the correct result(s)" taggedAs(BuildTest) in {
    Day14.hex2bin("0") shouldBe "0000"
    Day14.hex2bin("1") shouldBe "0001"
    Day14.hex2bin("70") shouldBe "01110000"
    Day14.hex2bin("e") shouldBe "1110"
    Day14.hex2bin("f") shouldBe "1111"
    Day14.hex2bin("0a0c20170") shouldBe "000010100000110000100000000101110000"
  }

  behavior of "solve() - Part1"
  it should "solve the testcase(s)" taggedAs(SlowTest) in {
    Day14.Part1.solve(testInput)._1 shouldBe 8108
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day14.Part1.solve(Day14.input)._1 shouldBe 8292
  }

  behavior of "nextCoordinates()"
  it should "return the right coordinates" in {
    Day14.nextCoordinates(0, 0, 128) should be (List((0, 1), (1, 0)))
    Day14.nextCoordinates(0, 1, 128) should be (List((0,2), (0,0), (1,1)))
    Day14.nextCoordinates(1, 0, 128) should be (List((1,1), (2,0), (0,0)))
    Day14.nextCoordinates(1, 1, 128) should be (List((1,2), (1,0), (2,1), (0,1)))
  }

  behavior of "findRegion()"
  it should "find the first region in the testcase(s)" taggedAs(BuildTest) in {
    Day14.findRegion(0, 0, testGrid) should be (List((1,1), (0,1), (0,0)))
    Day14.findRegion(0, 3, testGrid) should be (List((1,3), (0,3)))
    Day14.findRegion(0, 5, testGrid) should be (List((1,5), (0,5)))
    Day14.findRegion(2, 4, testGrid) should be (List((5,4), (4,4), (3,5), (3,4), (2,4)))
  }

  behavior of "solve() - Part2"
  it should "solve the testcase(s)" taggedAs(SlowTest) in {
    Day14.Part2.solve(testInput)._1 shouldBe 1242
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day14.Part2.solve(Day14.input)._1 shouldBe 1069
  }
}
