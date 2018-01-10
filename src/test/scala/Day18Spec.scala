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

  behavior of "the input"
  it should "be correct" in {
    Day18.in.take(resultInput.size) should be (resultInput)
  }

  behavior of "the parser"
  it should "return the correct result(s)" in {
    Day18.parseInput(testInput) should be (testOutput)
  }

  behavior of "running the program"
  it should "return the correct result(s)" in {
    val channel = new java.util.concurrent.LinkedBlockingDeque[Long]()
    Day18.solveRun(Day18.Program(0, 0, Day18.parseInput(testInput), Map.empty[Char, Long].withDefaultValue(0L), channel, channel, 0, true)) shouldBe 4
  }

  it should "solve the puzzle" in {
    val channel = new java.util.concurrent.LinkedBlockingDeque[Long]()
    Day18.solveRun(Day18.Program(0, 0, Day18.parseInput(Day18.in), Map.empty[Char, Long].withDefaultValue(0L), channel, channel, 0, true)) shouldBe 3188
  }

  behavior of "running the program concurrently"
  ignore should "deadlock" in {
    // print 3
    Day18.concurrentRun(Day18.parseInput(deadlockInput))
  }

  ignore should "solve the puzzle (again)" in {
    // print 7112
    Day18.concurrentRun(Day18.parseInput(Day18.in))
  }
}