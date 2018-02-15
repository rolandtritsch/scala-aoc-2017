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

  behavior of "input"
  it should "be correct" in {
    Day12.in.head shouldBe "0 <-> 46, 1376"
  }

  behavior of "parsing the input"
  it should "return the right result(s)" in {
    Day12.parseInput(testInput)(0) should be (List(2))
    Day12.parseInput(testInput)(4) should be (List(2, 3, 6))
  }

  it should "be able to process the input" in {
    Day12.parseInput(Day12.in)(40) should be (List(183, 333, 1032, 1405, 1587, 1649))
  }

  behavior of "findPrograms"
  it should "return the right result(s)" in {
    Day12.findPrograms(0, Day12.parseInput(testInput)).size shouldBe 6
  }

  it should "solve the puzzle" in {
    Day12.findPrograms(0, Day12.parseInput(Day12.in)).size shouldBe 152
  }

  behavior of "findGroups"
  it should "return the right result(s)" in {
    Day12.findGroups(Day12.parseInput(testInput)).size shouldBe 2
  }

  it should "solve the puzzle" in {
    Day12.findGroups(Day12.parseInput(Day12.in)).size shouldBe 186
  }
}
