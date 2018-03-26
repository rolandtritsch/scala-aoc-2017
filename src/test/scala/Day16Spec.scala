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

  behavior of "readInput()"
  it should "produce the correct list of moves" in {
    Day16.input.take(5) should be (inputFirstFive)
    Day16.parseInput(testInput) should be (testOutput)
  }

  behavior of "executeMoves()"
  it should "produce the correct result(s)" taggedAs(BuildTest) in {
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
  }

  behavior of "executeDance()"
  it should "solve the testcase(s)" in {
    Day16.executeDance("abcde", Day16.parseInput(testInput), 1) shouldBe "baedc"
    Day16.executeDance("abcde", Day16.parseInput(testInput), 2) shouldBe "ceadb"
  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day16.Part1.solve(Day16.input) shouldBe "bijankplfgmeodhc"
  }

  behavior of "solve() - Part2"
  ignore should "solve the puzzle" taggedAs(SolutionTest) in {
    Day16.Part2.solve(Day16.input) shouldBe ""
  }
}
