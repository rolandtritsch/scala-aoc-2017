package aoc

object Day01 {

  val in = Util.readInput("Day01input.txt").head

  def captcha(digits: String, offset: Int): Int = {
    require(offset <= digits.size, s"offset <= digits.size failed; with >${offset}< >${digits.size}<")

    val doubleDigits = (digits ++ digits).map(_.asDigit).toList
    val pairs = for (i <- 0 until digits.size) yield List(doubleDigits(i), doubleDigits(i + offset))
    pairs.filter(p => p.head == p.last).map(_.head).sum
  }

  object Part1 {
    def solve(in: String): Int = {
      require(in.nonEmpty, "in.nonEmpty failed")
      require(in.forall(_.isDigit), "in.forall(_.isDigit) failed")

      Day01.captcha(in, 1)
    }
  }

  object Part2 {
    def solve(in: String): Int = {
      require(in.nonEmpty, "in.nonEmpty failed")
      require(in.forall(_.isDigit), "in.forall(_.isDigit) failed")
      require(in.size % 2 == 0, "in.size % 2 == 0 failed")

      Day01.captcha(in, in.size / 2)
    }
  }
}
