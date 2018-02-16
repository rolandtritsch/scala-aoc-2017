package aoc

object Day01 {

  val input = Util.readInput("Day01input.txt").head

  def captcha(digits: String, offset: Int): Int = {
    require(offset <= digits.size, s"offset <= digits.size failed; with >${offset}< >${digits.size}<")

    val doubleDigits = (digits ++ digits).map(_.asDigit).toList
    val pairs = for (i <- 0 until digits.size) yield List(doubleDigits(i), doubleDigits(i + offset))
    pairs.filter(p => p.head == p.last).map(_.head).sum
  } ensuring(_ >= 0, s"_ >= 0 failed")

  object Part1 {
    def solve(input: String): Int = {
      require(input.nonEmpty, "input.nonEmpty failed")
      require(input.forall(_.isDigit), "input.forall(_.isDigit) failed")

      Day01.captcha(input, 1)
    }
  }

  object Part2 {
    def solve(input: String): Int = {
      require(input.nonEmpty, "input.nonEmpty failed")
      require(input.forall(_.isDigit), "input.forall(_.isDigit) failed")
      require(input.size % 2 == 0, "input.size % 2 == 0 failed")

      Day01.captcha(input, input.size / 2)
    }
  }
}
