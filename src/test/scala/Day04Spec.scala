package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day04Spec extends FlatSpec with Matchers {

  "readInput" should "return the list of passphrases" in {
    Day04.in.head shouldBe "sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp"
  }

  "Part 1 - isValid" should "return the correct result(s)" in {
    Day04.Part1.isValid("aa bb cc dd ee") shouldBe true
    Day04.Part1.isValid("aa bb cc dd aa") shouldBe false
    Day04.Part1.isValid("aa bb cc dd aaa") shouldBe true
  }

  "Part 1 - countValid" should "solve the puzzle" in {
    Day04.Part1.countValid(Day04.in) shouldBe 383
  }

  "Part 2 - isValid" should "return the correct result(s)" in {
    Day04.Part2.isValid("abcde fghij") shouldBe true
    Day04.Part2.isValid("abcde xyz ecdab") shouldBe false
    Day04.Part2.isValid("a ab abc abd abf abj") shouldBe true
    Day04.Part2.isValid("iiii oiii ooii oooi oooo") shouldBe true
    Day04.Part2.isValid("oiii ioii iioi iiio") shouldBe false
  }

  "Part 2 - countValid" should "solve the puzzle" in {
    Day04.Part2.countValid(Day04.in) shouldBe 265
  }
}
