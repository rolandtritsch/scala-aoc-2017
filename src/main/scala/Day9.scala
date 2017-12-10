package aoc

object Day9 {
  val fileURL = getClass.getResource(".") + "/Day9input.txt"
  val in = Util.readInputURL(fileURL).head.toList

  // Note: This is heavy on the stack. Had to increase -Xss to make it work.
  def score(stream: List[Char]): (List[Int], Int) = {
    def goGroup(s: List[Char], level: Int, garbageCounter: Int): (List[Int], Int) = s match {
      case Nil => assert(false); (List(), 0)
      case h :: Nil => assert(h == '}'); (List(level), garbageCounter)
      case h :: rest if(h == '{') => goGroup(rest, level + 1, garbageCounter)
      case h :: rest if(h == '}') => val (s, gc) = goGroup(rest, level - 1, garbageCounter); (level :: s, gc)
      case h :: rest if(h == '<') => goGarbage(rest, level, garbageCounter)
      case h :: rest => goGroup(rest, level, garbageCounter)
      case _ => assert(false); (List(), 0)
    }

    def goGarbage(s: List[Char], level: Int, garbageCounter: Int): (List[Int], Int) = s match {
      case Nil => assert(false); (List(), 0)
      case h :: Nil => assert(false); (List(), 0)
      case h :: rest if(h == '!') => goCanceled(rest, level, garbageCounter)
      case h :: rest if(h == '>') => goGroup(rest, level, garbageCounter)
      case h :: rest => goGarbage(rest, level, garbageCounter + 1)
      case _ => assert(false); (List(), 0)
    }

    def goCanceled(s: List[Char], level: Int, garbageCounter: Int): (List[Int], Int) = s match {
      case Nil => assert(false); (List(), 0)
      case h :: Nil => assert(false); (List(), 0)
      case h :: rest => goGarbage(rest, level, garbageCounter)
      case _ => assert(false); (List(), 0)
    }

    goGroup(stream, 0, 0)
  }
}
