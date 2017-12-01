package aoc

object Day1 {
  def captcha(in: String): Int = {
    require(in.nonEmpty, "in.nonEmpty failed")
    require(in.forall(_.isDigit), "in.forall(_.isDigit) failed")

    val circularIn = in :+ in.head
    val intIn = circularIn.map(_.toString.toInt).toList
    val pairs = intIn.sliding(2).toList
    pairs.filter(p => p.head == p.last).map(_.head).sum
  }
}
