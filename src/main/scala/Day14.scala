package aoc

object Day14 {

  val in = "ugkiagan"

  def hex2bin(hex: String): String = {
    (hex.foldLeft(List.empty[String])((acc, c) => BigInt(c.toString, 16).toString(2).reverse.padTo(4, '0').reverse :: acc)).reverse.mkString
  }

  def usedSquares(in: String): List[String] = {
    List.empty[String]
  }
}
