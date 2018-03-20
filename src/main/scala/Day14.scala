package aoc

/** Problem: [[https://adventofcode.com/2017/day/14]]
  *
  * Solution:
  *
  * General - Using the solution from Day10, we are calculating
  * the 128x128 bit hash grid.
  *
  * Part1 - We are counting all the '1's in the hashes ('1' == '#').
  *
  * Part2 -
  */
object Day14 {

  val input = Util.readInput("Day14input.txt").head

  def buildGrid(input: String): List[String] = {
    (0 to 127).map(row => Day10.dense2hex(Day10.dense(Day10.sparse(Day10.encode(s"${input}-${row}", Day10.suffix), Day10.seed, Day10.rounds).hash))).toList
  }

  def hex2bin(hex: String): String = {
    (hex.foldLeft(List.empty[String])((acc, c) => BigInt(c.toString, 16).toString(2).reverse.padTo(4, '0').reverse :: acc)).reverse.mkString
  }

  object Part1 {
    def solve(input: String): Int = {
      val grid = buildGrid(input).map(hex2bin(_))
      grid.foldLeft(0)((sum, hash) => sum + hash.count(_ == '1'))
    }
  }

  object Part2 {
    def solve(input: String): Int = {
      val grid = buildGrid(input).map(hex2bin(_))

      0
    }
  }
}
