package aoc

import aoc.Day15.Generator
import org.scalatest.{FlatSpec, Matchers}

class Day15Spec extends FlatSpec with Matchers {

  val testGenA = List(
    1092455,
    1181022009,
    245556042,
    1744312007,
    1352636452
  )

  val testGenB = List(
    430625591,
    1233683848,
    1431495498,
    137874439,
    285222916
  )

  val testBinPairs = List(
    (
      "00000000000100001010101101100111",
      "00011001101010101101001100110111"
    ),(
      "01000110011001001111011100111001",
      "01001001100010001000010110001000"
    ),(
      "00001110101000101110001101001010",
      "01010101010100101110001101001010"
    ),(
      "01100111111110000001011011000111",
      "00001000001101111100110000000111"
    ),(
      "01010000100111111001100000100100",
      "00010001000000000010100000000100"
    )
  )

  val testAstart = 65
  val testBstart = 8921

  "dec2bin" should "convert to bin correctly" in {
    Day15.dec2bin(1092455) shouldBe "00000000000100001010101101100111"
  }

  "a generator" should "produce a/the correct stream of numbers" in {
    val genA = Day15.Generator(testAstart, Day15.genAseed, Day15.defaultDevider)
    genA.numbers.take(5).toList shouldBe testGenA

    val genB = Day15.Generator(testBstart, Day15.genBseed, Day15.defaultDevider)
    genB.numbers.take(5).toList shouldBe testGenB
  }

  it should "produce the right pairs of binary numbers" in {
    val genA = Day15.Generator(testAstart, Day15.genAseed, Day15.defaultDevider)
    val genB = Day15.Generator(testBstart, Day15.genBseed, Day15.defaultDevider)

    val binPairGen = Day15.BinaryPairGenerator(genA, genB)
    binPairGen.numbers.take(5).toList should be (testBinPairs)
  }
}
