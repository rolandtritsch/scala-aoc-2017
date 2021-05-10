package aoc

object Util {
  def readInput(fileName: String): List[String] = {
    val path = scala.util.Properties.envOrElse("PWD", ".") + "/shared/src/main/resources/"
    scala.io.Source.fromFile(path + fileName).getLines().toList
  }

  def measuredTimeMillis[R](block: => R): (R, Long) = {
    val startTime = System.currentTimeMillis()
    val result = block
    val endTime = System.currentTimeMillis()
    (result, endTime - startTime)
  }
}
