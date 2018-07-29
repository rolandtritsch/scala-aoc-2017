package aoc

/** Problem: [[http://adventofcode.com/2017/day/17]]
  *
  * Solution:
  *
  * General - Straight-forward. Take a mutable ListBuffer (for performance reasons) and
  * define the required operations on it.
  *
  * Part1 - Run the algorithm 2017 times with a step size of 371 (input).
  *
  * Part2 - A little bit more tricky. Build a/the list with 50000000 elements in it is
  * not possible (takes too long and takes too much main mem). But we can take a short-cut
  * by (just) moving through the list (virtually) and calculate the insert postions and
  * every time we insert a value after postion 0, we remember it. The last time we insert
  * something after position 0 gives us the solution.
  */
object Day17 {

  import scala.collection.mutable

  val input = Util.readInput("Day17input.txt").head.toInt

  val steps = input
  val times = 2017
  val times2 = 50000000

  def moveForward(position: Int, size: Int, steps: Int): Int = {
    require(position >= 0 && position <= size, s"position >= 0 && position <= size failed; with >${position}<")
    require(size >= 0, s"size >= 0 failed; with >${size}<")
    require(steps >= 0, s"steps >= 0 failed; with >${steps}<")

    if (steps <= 0) position
    else if (position + 1 >= size) moveForward(0, size, steps - 1)
    else moveForward(position + 1, size, steps - 1)
  }

  def insertAfter(position: Int, buffer: mutable.ListBuffer[Int], n: Int): mutable.ListBuffer[Int] = {
    require(position >= 0 && position <= buffer.size, s"position >= 0 && position <= buffer.size failed; with >${position}<")
    require(buffer.nonEmpty, s"buffer.nonEmpty failed")

    buffer.insert(position + 1, n)
    buffer
  }

  def nextBuffer(position: Int, buffer: mutable.ListBuffer[Int], steps: Int, n: Int): (Int, mutable.ListBuffer[Int]) = {
    require(position >= 0 && position <= buffer.size, s"position >= 0 && position <= buffer.size failed; with >${position}<")
    require(buffer.nonEmpty, s"buffer.nonEmpty failed")

    val insertPosition = moveForward(position, buffer.size, steps)
    val currentPosition = insertPosition + 1
    (currentPosition, insertAfter(insertPosition, buffer, n))
  }

  def buildBuffer(buffer: mutable.ListBuffer[Int], steps: Int, times: Int): (Int, mutable.ListBuffer[Int]) = {
    require(steps >= 1, s"step >= 1 failed; with >${steps}<")
    require(times >= 1, s"times >= 1 failed; with >${times}<")

    (1.to(times)).foldLeft(0, buffer)((current, n) => {
      val (currentPosition, currentBuffer) = current
      nextBuffer(currentPosition, currentBuffer, steps, n)
    })
  }

  object Part1 {
    def solve(steps: Int, times: Int): (Int, Long) = Util.measuredTimeMillis {
      val (position, buffer) = buildBuffer(mutable.ListBuffer(0), steps, times)
      buffer(position + 1)
    }
  }

  object Part2 {
    def solve(steps: Int, times: Int): (Int, Long) = Util.measuredTimeMillis {
      val (finalPosition, finalValue) = (1.to(times)).foldLeft(0, 0) { (current, size) => {
        val (currentPosition, currentValue) = current
        val nextPosition = moveForward(currentPosition, size, steps)
        if (nextPosition == 0) (nextPosition + 1, size)
        else (nextPosition + 1, currentValue)
      }}
      finalValue
    }
  }
}
