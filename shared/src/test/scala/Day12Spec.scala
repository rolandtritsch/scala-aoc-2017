package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day12Spec extends FlatSpec with Matchers {

  val testInput = List(
    "0 <-> 2",
    "1 <-> 1",
    "2 <-> 0, 3, 4",
    "3 <-> 2, 4",
    "4 <-> 2, 3, 6",
    "5 <-> 6",
    "6 <-> 4, 5"
  )

  behavior of "readInput()"
  it should "read the input" in {
    Day12.input.head shouldBe "0 <-> 46, 1376"
  }

  behavior of "parseInput()"
  it should "return the right result(s)" in {
    Day12.parseInput(testInput)(0) should be (List(2))
    Day12.parseInput(testInput)(4) should be (List(2, 3, 6))
  }

  it should "be able to process the input" in {
    Day12.parseInput(Day12.input)(40) should be (List(183, 333, 1032, 1405, 1587, 1649))
  }

  behavior of "findPrograms()"
  it should "return the right result(s)" taggedAs(BuildTest) in {
    Day12.findPrograms(0, Day12.parseInput(testInput)).size shouldBe 6
  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day12.Part1.solve(Day12.input)._1 shouldBe 152
  }

  behavior of "findGroups()"
  it should "return the right result(s)" taggedAs(BuildTest) in {
    Day12.findGroups(Day12.parseInput(testInput)).size shouldBe 2
  }

  behavior of "solve() - Part2"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day12.Part2.solve(Day12.input)._1 shouldBe 186
  }
}
