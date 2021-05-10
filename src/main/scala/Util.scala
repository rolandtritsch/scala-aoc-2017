package aoc

object Util {
  def readInput(fileName: String): List[String] = {
    scala.io.Source.fromResource(fileName).getLines.toList
  }

  def measuredTimeMillis[R](block: => R): (R, Long) = {
    val startTime = System.currentTimeMillis()
    val result = block
    val endTime = System.currentTimeMillis()
    (result, endTime - startTime)
  }
}
