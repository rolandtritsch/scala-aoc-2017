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

   "hex2bin" should "return the correct result(s)" in {
     Day14.hex2bin("0") shouldBe "0000"
     Day14.hex2bin("1") shouldBe "0001"
     Day14.hex2bin("70") shouldBe "01110000"
     Day14.hex2bin("e") shouldBe "1110"
     Day14.hex2bin("f") shouldBe "1111"
     Day14.hex2bin("0a0c20170") shouldBe "000010100000110000100000000101110000"
   }

  "used squares" should "return the correct results" in {
   }

  ignore should "return the correct results again" in {
     val squares = Day14.usedSquares(testInput)
     squares.map(_.take(8)) should be (testOutput)

     val numOfUsedSquares = squares.map(_.count(_ == '#')).sum
     numOfUsedSquares shouldBe 8108
   }
}
