package aoc

object Day17 {
  import scala.collection.mutable

  val in = Util.readInput("Day17input.txt").head.toInt

  val steps = in
  val times = 2017
  val times2 = 50000000

  def moveForward(position: Int, size: Int, steps: Int): Int = {
    if (steps <= 0) position
    else if (position + 1 >= size) moveForward(0, size, steps - 1)
    else moveForward(position + 1, size, steps - 1)
  }

  def insertAfter(position: Int, buffer: mutable.ListBuffer[Int], n: Int): mutable.ListBuffer[Int] = {
    buffer.insert(position + 1, n)
    buffer
  }

  def nextBuffer(position: Int, buffer: mutable.ListBuffer[Int], steps: Int, n: Int): (Int, mutable.ListBuffer[Int]) = {
    val insertPosition = moveForward(position, buffer.size, steps)
    val currentPosition = insertPosition + 1
    (currentPosition, insertAfter(insertPosition, buffer, n))
  }

  def buildBuffer(buffer: mutable.ListBuffer[Int], steps: Int, times: Int): (Int, mutable.ListBuffer[Int]) = {
    require(steps >= 1, s"step >= 1 failed; with >${steps}<")
    require(times >= 1, s"times >= 1 failed; with >${times}<")
    (1 to times).foldLeft(0, buffer)((current, n) => {
      val (currentPosition, currentBuffer) = current
      //if(currentBuffer.size % 100000 == 0) println(currentBuffer.size)
      nextBuffer(currentPosition, currentBuffer, steps, n)
    })
  }

  object Part1 {
    def solve(steps: Int, times: Int): Int = {
      val (position, buffer) = buildBuffer(mutable.ListBuffer(0), steps, times)
      buffer(position + 1)
    }
  }

  object Part2 {
    def solve(steps: Int, times: Int): Int = {
      val (position, buffer) = buildBuffer(mutable.ListBuffer(0), steps, times)
      buffer(1)
    }
  }
}