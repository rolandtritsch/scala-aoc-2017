package aoc

import scala.util.Properties

object Util {
  def readInput(fileName: String): List[String] = {
    val path = scala.util.Properties.envOrElse("PWD", ".") + "/src/main/resources/"
    scala.io.Source.fromFile(path + fileName).getLines().toList
  }
}