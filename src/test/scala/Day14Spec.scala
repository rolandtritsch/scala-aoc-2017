package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day14Spec extends FlatSpec with Matchers {

  val testInput = "flqrgnkx"

  behavior of "readInput()"
  it should "read the input" in {
    Day14.input shouldBe "ugkiagan"
  }

  behavior of "buildGrid()"
  it should "return the correct grid for the input" taggedAs(SlowTest) in {
    val grid = Day14.buildGrid(Day14.input)
    grid(0) shouldBe "3fa276af5ffda7fd6f2e1b467a2d7285"
    grid(0).size shouldBe 32
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
  it should "solve the testcase(s)" taggedAs(SlowTest) in {
    Day14.Part1.solve(testInput) shouldBe 8108
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day14.Part1.solve(Day14.input) shouldBe 8292
  }

  behavior of "solve() - Part2"
  it should "solve the testcase(s)" taggedAs(SlowTest) in {
    Day14.Part2.solve(testInput) shouldBe 1242
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day14.Part2.solve(Day14.input) shouldBe 0
  }
}
