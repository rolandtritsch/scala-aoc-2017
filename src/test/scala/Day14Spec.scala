package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day14Spec extends FlatSpec with Matchers {

  val testInput = "flqrgnkx"
  val testOutput = List(
    "##.#.#..",
    ".#.#.#.#",
    "....#.#.",
    "#.#.##.#",
    ".##.#...",
    "##..#..#",
    ".#...#..",
    "##.#.##."
  )

  behavior of "readInput"
  it should "read the input" in {
    Day14.in shouldBe "ugkiagan"
  }

  behavior of "hex2bin"
  it should "return the correct result(s)" in {
    Day14.hex2bin("0") shouldBe "0000"
    Day14.hex2bin("1") shouldBe "0001"
    Day14.hex2bin("70") shouldBe "01110000"
    Day14.hex2bin("e") shouldBe "1110"
    Day14.hex2bin("f") shouldBe "1111"
    Day14.hex2bin("0a0c20170") shouldBe "000010100000110000100000000101110000"
  }

  behavior of "used squares"
  ignore should "return the correct results" in {
    val squares = Day14.usedSquares(testInput)
    squares.map(_.take(8)) should be (testOutput)

    val numOfUsedSquares = squares.map(_.count(_ == '#')).sum
    numOfUsedSquares shouldBe 8108
  }
}
