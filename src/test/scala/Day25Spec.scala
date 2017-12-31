package aoc

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class Day25Spec extends FlatSpec with Matchers {

  "run" should "produce the right result(s)" in {
    val tape = Day25.Tape(mutable.ArrayBuffer.fill(101)(0))
    Day25.run(Day25.StateA(tape.size / 2, tape), 6).checkSum shouldBe 3
  }

  it should "solve the puzzle" in {
    val tape = Day25.Tape(mutable.ArrayBuffer.fill(100001)(0))
    Day25.run(Day25.StateA(tape.size / 2, tape), Day25.in).checkSum shouldBe 4769
  }
}