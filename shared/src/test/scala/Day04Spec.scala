package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day04Spec extends FlatSpec with Matchers {

  behavior of "readInput()"
  it should "return the first passphrase from the list of passphrases" in {
    Day04.input.head shouldBe ("sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp".split(' ').toList)
  }

  behavior of "isValid()"
  it should "return the correct result(s)" in {
    Day04.isValid("aa bb cc dd ee".split(' ').toList) shouldBe true
    Day04.isValid("aa bb cc dd aa".split(' ').toList) shouldBe false
    Day04.isValid("aa bb cc dd aaa".split(' ').toList) shouldBe true

    Day04.isValid("abcde fghij".split(' ').toList.map(_.sorted)) shouldBe true
    Day04.isValid("abcde xyz ecdab".split(' ').toList.map(_.sorted)) shouldBe false
    Day04.isValid("a ab abc abd abf abj".split(' ').toList.map(_.sorted)) shouldBe true
    Day04.isValid("iiii oiii ooii oooi oooo".split(' ').toList.map(_.sorted)) shouldBe true
    Day04.isValid("oiii ioii iioi iiio".split(' ').toList.map(_.sorted)) shouldBe false
  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest, BuildTest) in {
    Day04.Part1.solve(Day04.input)._1 shouldBe 383
  }

  behavior of "solve() - Part2"
  it should "solve the puzzle" taggedAs(SolutionTest, BuildTest) in {
    Day04.Part2.solve(Day04.input)._1 shouldBe 265
  }
}
