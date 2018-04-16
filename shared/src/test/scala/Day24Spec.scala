package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day24Spec extends FlatSpec with Matchers {

  val testInput = List(
    "0/2",
    "2/2",
    "2/3",
    "3/4",
    "3/5",
    "0/1",
    "10/1",
    "9/10"
  )

  val testOutput1 = List(
    List(
      Day24.Component(0, 1),
      Day24.Component(1, 10),
      Day24.Component(10, 9)
    )
  )

  val testOutput2 = List(
    List(
      Day24.Component(0, 2),
      Day24.Component(2, 2),
      Day24.Component(2, 3),
      Day24.Component(3, 4)
    ),
    List(
      Day24.Component(0, 2),
      Day24.Component(2, 2),
      Day24.Component(2, 3),
      Day24.Component(3, 5)
    ),
      List(
      Day24.Component(0, 2),
      Day24.Component(2, 3),
      Day24.Component(3, 4)
    ),
    List(
      Day24.Component(0, 2),
      Day24.Component(2, 3),
      Day24.Component(3, 5)
    )
  )

  behavior of "the input"
  it should "be correct" in {
    Day24.in.head shouldBe "14/42"
  }

  behavior of "parse the input"
  it should "produce the correct result(s)" in {
    Day24.parseInput(testInput)(2) shouldBe Day24.Component(1, 10)
    Day24.parseInput(Day24.in).head shouldBe Day24.Component(0, 30)
    Day24.parseInput(Day24.in).distinct.size shouldBe Day24.in.size
  }

  behavior of "find zero-pin components"
  it should "return the correct result(s)" in {
    Day24.findZero(Day24.parseInput(testInput)) should be (List(Day24.Component(0, 2), Day24.Component(0, 1)))
    Day24.findZero(Day24.parseInput(Day24.in)) should be (List(Day24.Component(0, 30)))
  }

  behavior of "find path for a given zero"
  it should "return the correct result(s)" in {
    val components = Day24.parseInput(testInput)

    val zero1 = List(Day24.Component(0, 1))
    Day24.findPath(zero1, components.diff(zero1), List()) should be (testOutput1)

    val zero2 = List(Day24.Component(0, 2))
    Day24.findPath(zero2, components.diff(zero2), List()) should be (testOutput2)
  }

  behavior of "find all path for a given list of components"
  it should "return the correct result(s)" in {
    Day24.findPaths(Day24.parseInput(testInput)) should be (testOutput2 ++ testOutput1)
  }

  behavior of "find strongest path"
  it should "return the correct result(s)" in {
    Day24.findStrongestPath(Day24.findPaths(Day24.parseInput(testInput)))._1 shouldBe 31
  }

  ignore should "solve the puzzle" in {
    Day24.findStrongestPath(Day24.findPaths(Day24.parseInput(Day24.in)))._1 shouldBe 1695
  }

  behavior of "find longest/strongest path"
  it should "return the correct result(s)" in {
    Day24.findLongestPath(Day24.findPaths(Day24.parseInput(testInput)))._2 shouldBe 19
  }

  ignore should "solve the puzzle" in {
    Day24.findLongestPath(Day24.findPaths(Day24.parseInput(Day24.in)))._2 shouldBe 1673
  }
}