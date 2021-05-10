package aoc

/** Problem: [[https://adventofcode.com/2017/day/9]]
  *
  * Solution:
  *
  * General - My first implementation was based on a recursive processing
  * of the input stream. Hard to read. Hard to understand. Hard to extend/
  * maintain. And hard to run (needs a special -Xss setting (more/large
  * stack).
  *
  * This (refactored) implementation is using a state machine. The states
  * are OutOfGroup, InGroup, InGarbage, InCanceled ([[https://www.dropbox.com/s/pwogl9jhc8x7rqe/2018-02-21%2013.01.24.jpg?dl=0 pic]]). While I am running the
  * state machine I am collecting stats that will allow me (at the end) to
  * answer (the given) questions about the input stream.
  *
  * Part1 - Trivial. Collect and show the right stats.
  *
  * Part2 - Trivial. Collect and show the right stats.
  */
object Day09 {

  val input = Util.readInput("Day09input.txt").head.toList

  case class Statistics(scores: List[Int] = List.empty[Int], garbageCharCounter: Int = 0) {
    def collectScore(score: Int): Statistics = {
      this.copy(scores = score :: scores)
    }

    def collectGarbage: Statistics = {
      this.copy(garbageCharCounter = garbageCharCounter + 1)
    }
  }

  sealed trait State {
    val stats: Statistics
    val level: Int
    def next(c: Char): State
  }

  object BadState extends State {
    val level = 0
    val stats = Statistics(List.empty[Int], 0)
    def next(c: Char): State = {
      assert(false)
      BadState
    }
  }

  case class OutOfGroup(level: Int, stats: Statistics) extends State {
    def next(c: Char): State = c match {
      case '{' => InGroup(level + 1, stats)
      case _ => assert(false); BadState
    }
  }

  case class InGroup(level: Int, stats: Statistics) extends State {
    def next(c: Char): State = c match {
      case '{' => InGroup(level + 1, stats)
      case '}' if(level > 1) => InGroup(level - 1, stats.collectScore(level))
      case '}' => OutOfGroup(level - 1, stats.collectScore(level))
      case '<' => InGarbage(level, stats)
      case _ => InGroup(level, stats)
    }
  }

  case class InGarbage(level: Int, stats: Statistics) extends State {
    def next(c: Char): State = c match {
      case '!' => InCanceled(level, stats)
      case '>' => InGroup(level, stats)
      case _ => InGarbage(level, stats.collectGarbage)
    }
  }

  case class InCanceled(level: Int, stats: Statistics) extends State {
    def next(c: Char): State = c match {
      case _ => InGarbage(level, stats)
    }
  }

  case class StateMachine(stream: List[Char]) {
    def run: State = {
      stream.foldLeft(OutOfGroup(0, Statistics()).asInstanceOf[State])((currentState, c) => currentState.next(c))
    }
  }

  object Part1 {
    def solve(input: List[Char]): (Int, Long) = Util.measuredTimeMillis {
      val finalState = StateMachine(input).run
      assert(finalState.isInstanceOf[OutOfGroup], s"finalState.isInstanceOf[OutOfGroup] failed")
      finalState.stats.scores.sum
    }
  }

  object Part2 {
    def solve(input: List[Char]): (Int, Long) = Util.measuredTimeMillis {
      val finalState = StateMachine(input).run
      assert(finalState.isInstanceOf[OutOfGroup], s"finalState.isInstanceOf[OutOfGroup] failed")
      finalState.stats.garbageCharCounter
    }
  }
}
