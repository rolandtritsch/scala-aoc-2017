package aoc

/** Problem: [[https://adventofcode.com/2017/day/2]]
  *
  * Solution:
  *
  * Part 1 - Straight forward. For every row, build a/the tuple
  * of the largest and the smallest value in that row. Calc the
  * diffs and sum them up.
  *
  * Part 2 - A little bit more interesting. Build the pairs of all
  * values in a row (but only the ones where x > y), filter for the
  * one pair where x is divisible by y with no remainder and sum up
  * the result of the division.
  */
private object Day02 {

  protected val input = Util.readInput("Day02input.txt").map(_.split('\t').toList).map(line => line.map(_.toInt))

  private def checksum(spreadSheet: List[List[Int]], processRow: List[List[Int]] => List[Int]): Int = {
    require(spreadSheet.nonEmpty, "spreadSheet.nonEmpty failed")
    require(spreadSheet.forall(_.nonEmpty), "spreadSheet.forall(_.nonEmpty) failed")

    processRow(spreadSheet).sum
  } ensuring(_ >= 0, s"_ >= 0 failed")

  private object Part1 {
    private def processRow(s: List[List[Int]]): List[Int] = {
      s.map(row => row.max - row.min)
    }

    protected def solve(spreadSheet: List[List[Int]]): Int = {
      Day02.checksum(spreadSheet, processRow)
    }
  }

  private object Part2 {
    private def processRow(s: List[List[Int]]): List[Int] = {
      s.map {row => {
        val pairs = for(x <- row; y <- row; if x > y) yield (x, y)
        val dividablePairs = pairs.filter{case (x, y) => (x % y == 0)}
        assert(dividablePairs.size == 1, s"dividablePairs.size == 1 failed; with >${dividablePairs}<")
        val (x, y) = dividablePairs.head
        x / y
      }}
    }

    protected def solve(spreadSheet: List[List[Int]]): Int = {
      Day02.checksum(spreadSheet, processRow)
    }
  }
}
