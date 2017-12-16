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

  "input" should "be correct" in {
    Day12.in.head shouldBe "0 <-> 46, 1376"
  }

  "parsing the input" should "return the right result(s)" in {
    Day12.parseInput(testInput)(0) should be (List(2))
    Day12.parseInput(testInput)(4) should be (List(2, 3, 6))
  }

  it should "be able to process the input" in {
    Day12.parseInput(Day12.in)(40) should be (List(183, 333, 1032, 1405, 1587, 1649))
  }

  "countPrograms" should "return the right result(s)" in {
    Day12.countPrograms(0, Day12.parseInput(testInput)) shouldBe 6
  }

  it should "solve the puzzle" in {
    Day12.countPrograms(0, Day12.parseInput(Day12.in)) shouldBe 152
  }
}
