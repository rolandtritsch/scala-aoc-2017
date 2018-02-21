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

  object Part1 {
    def solve(input: List[Char]): Int = {
      val (scores, _) = score(input)
      scores.sum
    }
  }

  object Part2 {
    def solve(input: List[Char]): Int = {
      val (_, nonCanceledChars) = score(input)
      nonCanceledChars
    }
  }
}
