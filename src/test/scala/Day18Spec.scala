package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day18Spec extends FlatSpec with Matchers {
  val testInput = List(
    "set a 1",
    "add a 2",
    "mul a a",
    "mod a 5",
    "snd a",
    "set a 0",
    "rcv a",
    "jgz a -1",
    "set a 1",
    "jgz a -2"
  )

  val testOutput = List(
    Day18.Set('a', 1),
    Day18.Add('a', 2),
    Day18.MultiplyR('a', 'a'),
    Day18.Modulo('a', 5),
    Day18.Sound('a'),
    Day18.Set('a', 0),
    Day18.Recover('a'),
    Day18.JumpIfGreaterThanZero('a', -1),
    Day18.Set('a', 1),
    Day18.JumpIfGreaterThanZero('a', -2)
  )

  val resultInput = List(
    "set i 31",
    "set a 1",
    "mul p 17",
    "jgz p p",
    "mul a 2"
  )

  "the input" should "be correct" in {
    Day18.in.take(resultInput.size) should be (resultInput)
  }

  "the parser" should "return the correct result(s)" in {
    Day18.parseInput(testInput) should be (testOutput)
  }

  "running the program" should "return the correct result(s)" in {
    Day18.solveRun(Day18.Program(0, Day18.parseInput(testInput), Map.empty[Char, Int].withDefaultValue(0))) shouldBe 4
  }

  it should "solve the puzzle" in {
    Day18.solveRun(Day18.Program(0, Day18.parseInput(Day18.in), Map.empty[Char, Int].withDefaultValue(0))) shouldBe 3675
  }
}