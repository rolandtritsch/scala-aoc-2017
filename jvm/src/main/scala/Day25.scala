package aoc

/** Problem: [[http://adventofcode.com/2017/day/25]]
  *
  * Solution:
  *
  * General - And another [[State]] machine. Run the state machine for the
  * given number of steps/iterations (on the [[Tape]]).
  *
  * Part1 - Calc and return the [[Tape.checkSum]].
  *
  * Part2 - Merry Christmas :)
  *
  */
object Day25 {
  import scala.collection.mutable

  //val in = Util.readInput("Day25input.txt").head.toInt
  val input = 12667664

  case class Tape(private val init: mutable.ArrayBuffer[Int]) {
    def update(position: Int, value: Int): Tape = {
      init(position) = value
      this
    }

    def apply(position: Int) = init(position)

    val size = init.size

    def checkSum: Int = init.count(_ == 1)
  }

  abstract class State(programCounter: Int, tape: Tape) {
    def tick: State
    def checkSum = tape.checkSum
  }

  case class StateA(programCounter: Int, tape: Tape) extends State(programCounter, tape) {
    def tick: State = {
      if(tape(programCounter) == 0) StateB(programCounter + 1, tape.update(programCounter, 1))
      else if(tape(programCounter) == 1) StateC(programCounter - 1, tape.update(programCounter, 0))
      else {assert(false); StateA(0, Tape(mutable.ArrayBuffer()))}
    }
  }

  case class StateB(programCounter: Int, tape: Tape) extends State(programCounter, tape) {
    def tick: State = {
      if(tape(programCounter) == 0) StateA(programCounter - 1, tape.update(programCounter, 1))
      else if(tape(programCounter) == 1) StateD(programCounter + 1, tape.update(programCounter, 1))
      else {assert(false); StateA(0, Tape(mutable.ArrayBuffer()))}
    }
  }

  case class StateC(programCounter: Int, tape: Tape) extends State(programCounter, tape) {
    def tick: State = {
      if(tape(programCounter) == 0) StateB(programCounter - 1, tape.update(programCounter, 0))
      else if(tape(programCounter) == 1) StateE(programCounter - 1, tape.update(programCounter, 0))
      else {assert(false); StateA(0, Tape(mutable.ArrayBuffer()))}
    }
  }

  case class StateD(programCounter: Int, tape: Tape) extends State(programCounter, tape) {
    def tick: State = {
      if(tape(programCounter) == 0) StateA(programCounter + 1, tape.update(programCounter, 1))
      else if(tape(programCounter) == 1) StateB(programCounter + 1, tape.update(programCounter, 0))
      else {assert(false); StateA(0, Tape(mutable.ArrayBuffer()))}
    }
  }

  case class StateE(programCounter: Int, tape: Tape) extends State(programCounter, tape) {
    def tick: State = {
      if(tape(programCounter) == 0) StateF(programCounter - 1, tape.update(programCounter, 1))
      else if(tape(programCounter) == 1) StateC(programCounter - 1, tape.update(programCounter, 1))
      else {assert(false); StateA(0, Tape(mutable.ArrayBuffer()))}
    }
  }

  case class StateF(programCounter: Int, tape: Tape) extends State(programCounter, tape) {
    def tick: State = {
      if(tape(programCounter) == 0) StateD(programCounter + 1, tape.update(programCounter, 1))
      else if(tape(programCounter) == 1) StateA(programCounter + 1, tape.update(programCounter, 1))
      else {assert(false); StateA(0, Tape(mutable.ArrayBuffer()))}
    }
  }

  def run(state: State, steps: Int): State = {
    if(steps <= 0) state
    else run(state.tick, steps - 1)
  }

  object Part1 {
    def solve(input: Int): (Int, Long) = Util.measuredTimeMillis {
      val tape = Tape(mutable.ArrayBuffer.fill(100001)(0))
      run(StateA(tape.size / 2, tape), input).checkSum
    }
  }
}
