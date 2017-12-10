package aoc

object Day9 {
  val fileURL = getClass.getResource(".") + "/Day9input.txt"

  def readInputURL(fileURL: String): List[String] = {
    scala.io.Source.fromURL(new java.net.URL(fileURL)).getLines().toList
  }

  val in = readInputURL(fileURL).head.toList

  // Note: This is heavy on the stack. Had to increase -Xss to make it work.
  def score(stream: List[Char]): List[Int] = {
    def goGroup(s: List[Char], level: Int): List[Int] = s match {
      case Nil => assert(false); List()
      case h :: Nil => assert(h == '}'); List(level)
      case h :: rest if(h == '{') => goGroup(rest, level + 1)
      case h :: rest if(h == '}') => level :: goGroup(rest, level - 1)
      case h :: rest if(h == '<') => goGarbage(rest, level)
      case h :: rest => goGroup(rest, level)
      case _ => assert(false); List()
    }

    def goGarbage(s: List[Char], level: Int): List[Int] = s match {
      case Nil => assert(false); List()
      case h :: Nil => assert(false); List()
      case h :: rest if(h == '!') => goCanceled(rest, level)
      case h :: rest if(h == '>') => goGroup(rest, level)
      case h :: rest => goGarbage(rest, level)
      case _ => assert(false); List()
    }

    def goCanceled(s: List[Char], level: Int): List[Int] = s match {
      case Nil => assert(false); List()
      case h :: Nil => assert(false); List()
      case h :: rest => goGarbage(rest, level)
      case _ => assert(false); List()
    }

    goGroup(stream, 0)
  }
}
