package aoc

/** Problem: [[https://adventofcode.com/2017/day/1]]
  *
  * Solution:
  *
  * General - To avoid to have to deal with the "wrap-around" we
  * we are *just* doubling the size of the input. With that we can
  * build pairs/tuples of values (i, i + offset), find the pairs where
  * both numbers are the same and sum up the values of the pairs.
  *
  * Part 1 - Run the algorithm with an offset of 1.
  *
  * Part 2 - Run the algorithm with an offset of half the length
  * of the input string.
  */
object Day01 {

  val input = Util.readInput("Day01input.txt").head

  def captcha(digits: String, offset: Int): Int = {
    require(offset <= digits.size, s"offset <= digits.size failed; with >${offset}< >${digits.size}<")

    val doubleDigits = (digits ++ digits).map(_.asDigit).toList
    val pairs = for (i <- 0 until digits.size) yield (doubleDigits(i), doubleDigits(i + offset))
    pairs.filter{case (first, second) => first == second}.map{case (value, _) => value}.sum
  } ensuring(_ >= 0, s"_ >= 0 failed")

  object Part1 {
    def solve(input: String): (Int, Long) = Util.measuredTimeMillis {
      require(input.nonEmpty, "input.nonEmpty failed")
      require(input.forall(_.isDigit), "input.forall(_.isDigit) failed")

      Day01.captcha(input, 1)
    }
  }

  object Part2 {
    def solve(input: String): (Int, Long) = Util.measuredTimeMillis {
      require(input.nonEmpty, "input.nonEmpty failed")
      require(input.forall(_.isDigit), "input.forall(_.isDigit) failed")
      require(input.size % 2 == 0, "input.size % 2 == 0 failed")

      Day01.captcha(input, input.size / 2)
    }
  }
}
