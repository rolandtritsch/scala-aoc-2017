package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day13Spec extends FlatSpec with Matchers {

  val testInput = List(
    "0: 3",
    "1: 2",
    "4: 4",
    "6: 4"
  )

  behavior of "readInput()"
  it should "be correct" in {
    Day13.input.head shouldBe "0: 4"
  }

  behavior of "parseInput()"
  it should "parse correctly" in {
    val parsed = Day13.parseInput(Day13.input)
    parsed(0) shouldBe 4
    parsed(98) shouldBe 18
  }

  behavior of "buildFw()"
  it should "build a fw with the testcase(s)" in {
    Day13.buildFw(Day13.parseInput(testInput)) should be (List((0, 3), (1, 2), (2, 0), (3, 0), (4, 4), (5, 0), (6, 4)))
  }

  it should "build a fw with the input" in {
    val fw = Day13.buildFw(Day13.parseInput(Day13.input))
    fw.take(10) should be (List((0, 4), (1, 2), (2, 3), (3, 0), (4, 5), (5, 0), (6, 6), (7, 0), (8, 6), (9, 0)))
    fw.size shouldBe 99
    fw(98) shouldBe (98, 18)
  }

  behavior of "threatDetected()"
  it should "work correctly" in {
    val threats = Day13.buildFw(Day13.parseInput(testInput)).map {case(d, r) => Day13.threatDetected(d, r)}
    threats should be (List(true, false, false, false, false, false, true))
  }

  behavior of "solve() - Part1"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day13.Part1.solve(testInput)._1 shouldBe 24
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day13.Part1.solve(Day13.input)._1 shouldBe 1632
  }

  behavior of "solve() - Part2"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day13.Part2.solve(testInput)._1 shouldBe 10
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day13.Part2.solve(Day13.input)._1 shouldBe 3834136
  }
}
