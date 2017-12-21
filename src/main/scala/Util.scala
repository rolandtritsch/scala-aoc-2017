package aoc

object Util {
  def readInput(fileName: String): List[String] = {
    val path = "./src/main/resources/"
    scala.io.Source.fromFile(path + fileName).getLines().toList
  }
}