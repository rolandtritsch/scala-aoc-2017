package aoc

object Util {
  def readInputURL(fileURL: String): List[String] = {
    scala.io.Source.fromURL(new java.net.URL(fileURL)).getLines().toList
  }
}