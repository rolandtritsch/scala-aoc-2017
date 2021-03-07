package aoc

import org.scalatest.flatspec._
import org.scalatest.matchers._
import org.scalatest.tagobjects.Slow

class Day03Spec extends AnyFlatSpec with should.Matchers {

  val testGrid = Array(
    Array(17, 16, 15, 14, 13),
    Array(18,  5,  4,  3, 12),
    Array(19,  6,  1,  2, 11),
    Array(20,  7,  8,  9, 10),
    Array(21, 22, 23, 24, 25)
  )

  behavior of "readInput()"
  it should "read the input correctly" in {
    Day03.input shouldBe 368078
  }

  behavior of "moves()"
  it should "return the initial moves" in {
    Day03.moves(Day03.firstLevelMoves).take(8) should be (Day03.firstLevelMoves.flatten)
  }

  behavior of "solve() - Part1"
  it should "solve the testcases" taggedAs(BuildTest) in {
    Day03.Part1.solve(1)._1 shouldBe 0
    Day03.Part1.solve(12)._1 shouldBe 3
    Day03.Part1.solve(23)._1 shouldBe 2
    Day03.Part1.solve(1024)._1 shouldBe 31
    Day03.Part1.solve(1000000)._1 shouldBe 999
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day03.Part1.solve(Day03.input)._1 shouldBe 371
  }

  behavior of "solve() - Part2"
  it should "produce the (first 23) correct cell values" in {
    val values = List(1, 1, 2, 4, 5, 10, 11, 23, 25, 26, 54, 57, 59, 122, 133, 142, 147, 304, 330, 351, 362, 747, 806)
    val spiral = Day03.Part2.cells(Day03.moves(Day03.firstLevelMoves).iterator)
    spiral.take(values.size).map(_.value).toList should be (values)
  }

  it should "solve the testcases" taggedAs(BuildTest) in {
    Day03.Part2.solve(750)._1 shouldBe 806
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day03.Part2.solve(Day03.input)._1 shouldBe 369601
  }
}
