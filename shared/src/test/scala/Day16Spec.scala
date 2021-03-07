package aoc

import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day16Spec extends AnyFlatSpec with should.Matchers {

  val inputFirstFive = List(
    "x5/11",
    "s12",
    "x10/4",
    "pi/d",
    "x11/0"
  )

  val testProgram = ('a' to 'e').toArray

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
    Day16.executeMoves(testProgram, List(Day16.Spin(1))).mkString shouldBe "eabcd"
    Day16.executeMoves(testProgram, List(Day16.Spin(3))).mkString shouldBe "cdeab"
    Day16.executeMoves(testProgram, List(Day16.Exchange(0, 1))).mkString shouldBe "bacde"

    val firstExchange = Day16.executeMoves(testProgram, List(Day16.Exchange(0, 4)))
    val secondExchange = Day16.executeMoves(testProgram, List(Day16.Exchange(4, 0)))
    firstExchange.mkString shouldBe "ebcda"
    firstExchange.mkString shouldBe secondExchange.mkString

    val firstPartner = Day16.executeMoves(testProgram, List(Day16.Partner('a', 'b')))
    val secondPartner = Day16.executeMoves(firstPartner, List(Day16.Partner('a', 'b')))
    firstPartner.mkString shouldBe "bacde"
    secondPartner.mkString shouldBe testProgram.mkString

    Day16.executeMoves(testProgram, List(Day16.Partner('a', 'e'))).mkString shouldBe "ebcda"
    Day16.executeMoves(testProgram, List(Day16.Partner('e', 'a'))).mkString shouldBe "ebcda"

    val firstDance = Day16.executeMoves(testProgram, Day16.parseInput(testInput))
    val secondDance = Day16.executeMoves(firstDance, Day16.parseInput(testInput))
    firstDance.mkString shouldBe "baedc"
    secondDance.mkString shouldBe "ceadb"
  }

  behavior of "executeDance()"
  it should "solve the testcase(s)" in {
    Day16.executeDance(testProgram, Day16.parseInput(testInput), 1).mkString shouldBe "baedc"
    Day16.executeDance(testProgram, Day16.parseInput(testInput), 2).mkString shouldBe "ceadb"
  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day16.Part1.solve(Day16.input)._1 shouldBe "bijankplfgmeodhc"
  }

  behavior of "solve() - Part2"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day16.Part2.solve(Day16.input)._1 shouldBe "bpjahknliomefdgc"
  }
}
