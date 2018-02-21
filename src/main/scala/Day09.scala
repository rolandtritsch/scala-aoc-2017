package aoc

object Day09 {

  val input = Util.readInput("Day09input.txt").head.toList

  def score(stream: List[Char]): (List[Int], Int) = {
    def goGroup(stream: List[Char], level: Int, scores: List[Int], garbageCounter: Int): (List[Int], Int) = stream match {
      case Nil => assert(false); (List(), 0)
      case h :: Nil => assert(h == '}'); (scores, garbageCounter)
      case h :: rest if(h == '{') => goGroup(rest, level + 1, scores, garbageCounter)
      case h :: rest if(h == '}') => goGroup(rest, level - 1, level :: scores, garbageCounter)
      case h :: rest if(h == '<') => goGarbage(rest, level, scores, garbageCounter)
      case h :: rest => goGroup(rest, level, scores, garbageCounter)
      case _ => assert(false); (List(), 0)
    }

    def goGarbage(stream: List[Char], level: Int, scores: List[Int], garbageCounter: Int): (List[Int], Int) = stream match {
      case Nil => assert(false); (List(), 0)
      case h :: Nil => assert(false); (List(), 0)
      case h :: rest if(h == '!') => goCanceled(rest, level, scores, garbageCounter)
      case h :: rest if(h == '>') => goGroup(rest, level, scores, garbageCounter)
      case h :: rest => goGarbage(rest, level, scores, garbageCounter + 1)
      case _ => assert(false); (List(), 0)
    }

    def goCanceled(stream: List[Char], level: Int, scores: List[Int], garbageCounter: Int): (List[Int], Int) = stream match {
      case Nil => assert(false); (List(), 0)
      case h :: Nil => assert(false); (List(), 0)
      case h :: rest => goGarbage(rest, level, scores, garbageCounter)
      case _ => assert(false); (List(), 0)
    }

    goGroup(stream, 0, List(1), 0)
  }

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
    def solve(input: List[Char]): Int = {
      val finalState = StateMachine(input).run
      assert(finalState.isInstanceOf[OutOfGroup], s"finalState.isInstanceOf[OutOfGroup] failed")
      finalState.stats.scores.sum
    }
  }

  object Part2 {
    def solve(input: List[Char]): Int = {
      val finalState = StateMachine(input).run
      finalState.stats.garbageCharCounter
    }
  }
}
