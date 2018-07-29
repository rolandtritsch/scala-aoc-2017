package aoc

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class Day25Spec extends FlatSpec with Matchers {

  behavior of "run()"
  it should "produce the right result(s)" taggedAs(BuildTest) in {
    val tape = Day25.Tape(mutable.ArrayBuffer.fill(101)(0))
    Day25.run(Day25.StateA(tape.size / 2, tape), 6).checkSum shouldBe 3
  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day25.Part1.solve(Day25.input)._1 shouldBe 4769
  }
}
