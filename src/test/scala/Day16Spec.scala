package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day16Spec extends FlatSpec with Matchers {
  val inputFirstFive = List(
    "x5/11",
    "s12",
    "x10/4",
    "pi/d",
    "x11/0"
  )

  val testInput = List(
    "s1",
    "x3/4",
    "pe/b"
  )

  val testOutput = List(
    Day16.Spin(1),
    Day16.Exchange(3, 4),
    Day16.Partner('e', 'b')
  )

  behavior of "reading the input"
  it should "produce the correct list of moves" in {
    Day16.in.take(5) should be (inputFirstFive)
    Day16.parseInput(testInput) should be (testOutput)
  }

  behavior of "executing the moves"
  it should "produce the correct result(s)" in {
    Day16.executeMoves("abcde", List(Day16.Spin(1))) shouldBe "eabcd"
    Day16.executeMoves("abcde", List(Day16.Spin(3))) shouldBe "cdeab"
    Day16.executeMoves("abcde", List(Day16.Exchange(0, 1))) shouldBe "bacde"
    Day16.executeMoves("abcde", List(Day16.Exchange(0, 4))) shouldBe "ebcda"
    Day16.executeMoves("abcde", List(Day16.Exchange(4, 0))) shouldBe "ebcda"
    Day16.executeMoves("abcde", List(Day16.Partner('a', 'b'))) shouldBe "bacde"
    Day16.executeMoves("bacde", List(Day16.Partner('a', 'b'))) shouldBe "abcde"
    Day16.executeMoves("abcde", List(Day16.Partner('a', 'e'))) shouldBe "ebcda"
    Day16.executeMoves("abcde", List(Day16.Partner('e', 'a'))) shouldBe "ebcda"
    Day16.executeMoves("abcde", Day16.parseInput(testInput)) shouldBe "baedc"
    Day16.executeMoves("baedc", Day16.parseInput(testInput)) shouldBe "ceadb"
    Day16.executeDance("abcde", Day16.parseInput(testInput), 1) shouldBe "baedc"
    Day16.executeDance("abcde", Day16.parseInput(testInput), 2) shouldBe "ceadb"
  }

  it should "solve the puzzle" in {
    Day16.executeMoves(Day16.programs, Day16.parseInput(Day16.in)) shouldBe "bijankplfgmeodhc"
    Day16.executeDance(Day16.programs, Day16.parseInput(Day16.in), 1) shouldBe "bijankplfgmeodhc"
  }

  ignore should "solve the big puzzle" in {
    Day16.executeDance(Day16.programs, Day16.parseInput(Day16.in), Day16.times) shouldBe ""
  }

  ignore should "solve the big puzzle again" in {
    Day16.executeDance2(Day16.programs, Day16.parseInput(Day16.in), Day16.times) shouldBe ""
  }

  ignore should "solve the big puzzle again and again" in {
    Day16.executeDance22(new StringBuilder(Day16.programs), Day16.parseInput(Day16.in), Day16.times) shouldBe ""
  }
}