package aoc

object Day01 {
  val in = Util.readInput("Day01input.txt").head

  object Part1 {
    def captcha(in: String): Int = {
      require(in.nonEmpty, "in.nonEmpty failed")
      require(in.forall(_.isDigit), "in.forall(_.isDigit) failed")

      val circularIn = in :+ in.head
      val intIn = circularIn.map(_.toString.toInt).toList
      val pairs = intIn.sliding(2).toList
      pairs.filter(p => p.head == p.last).map(_.head).sum
    }
  }

  object Part2 {
    def captcha(in: String): Int = {
      require(in.nonEmpty, "in.nonEmpty failed")
      require(in.forall(_.isDigit), "in.forall(_.isDigit) failed")
      require(in.size % 2 == 0, "in.size % 2 == 0 failed")

      val circularIn = in ++ in
      val intIn = circularIn.map(_.toString.toInt).toList
      val pairs = for (i <- 0 until in.size) yield List(intIn(i), intIn(i + (in.size / 2)))
      pairs.filter(p => p.head == p.last).map(_.head).sum
    }
  }
}
