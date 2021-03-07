package aoc

import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day18Spec extends AnyFlatSpec with should.Matchers {

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
    Day18.Send('a'),
    Day18.Set('a', 0),
    Day18.Receive('a'),
    Day18.JumpIfGreaterThanZero('a', -1),
    Day18.Set('a', 1),
    Day18.JumpIfGreaterThanZero('a', -2)
  )

  val resultInput = List(
    "set l 1",
    "set i 31",
    "set a 1",
    "mul p 17",
    "jgz p p",
    "mul a 2"
  )

  val deadlockInput = List(
    "set a 1",
    "set b 1",
    "set c 1",
    "set d 1",
    "set x 1",
    "set y 2",
    "snd x",
    "snd y",
    "snd p",
    "rcv a",
    "rcv b",
    "rcv c",
    "rcv d"
  )

  behavior of "readInput()"
  it should "be correct" in {
    Day18.input.take(resultInput.size) should be (resultInput)
  }

  behavior of "parseInput()"
  it should "return the correct result(s)" in {
    Day18.parseInput(testInput) should be (testOutput)
  }

  behavior of "solve() - Part1"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day18.Part1.solve(testInput)._1 shouldBe 4
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day18.Part1.solve(Day18.input)._1 shouldBe 3188
  }

  behavior of "solve() - Part2"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day18.Part2.solve(deadlockInput)._1 shouldBe 3
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day18.Part2.solve(Day18.input)._1 shouldBe 7112
  }
}
