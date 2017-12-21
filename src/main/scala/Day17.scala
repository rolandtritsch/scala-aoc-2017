package aoc

object Day17 {
  val steps = 371
  val times = 2017
  val times2 = 50000000

  def buildBuffer(buffer: List[Int], steps: Int, times: Int): (Int, List[Int]) = {
    require(steps >= 1, s"step >= 1 failed; with >${steps}<")
    require(times >= 1, s"times >= 1 failed; with >${times}<")
    (1 to times).foldLeft(0, buffer)((current, n) => {
      val (currentPosition, currentBuffer) = current
      nextBuffer(currentPosition, currentBuffer, steps, n)
    })
  }

  def moveForward(position: Int, size: Int, steps: Int): Int = {
    if (steps <= 0) position
    else if (position + 1 >= size) moveForward(0, size, steps - 1)
    else moveForward(position + 1, size, steps - 1)
  }

  def insertAfter(position: Int, buffer: List[Int], n: Int): List[Int] = {
    val (head, tail) = buffer.splitAt(position + 1)
    head ++ List(n) ++ tail
  }

  def nextBuffer(position: Int, buffer: List[Int], steps: Int, n: Int): (Int, List[Int]) = {
    val insertPosition = moveForward(position, buffer.size, steps)
    val currentPosition = insertPosition + 1
    (currentPosition, insertAfter(insertPosition, buffer, n))
  }

  object Part1 {
    def solve(steps: Int, times: Int): Int = {
      val (position, buffer) = buildBuffer(List(0), steps, times)
      buffer(position + 1)
    }
  }

  object Part2 {
    def solve(steps: Int, times: Int): Int = {
      val (position, buffer) = buildBuffer(List(0), steps, times)
      buffer(1)
    }
  }
}