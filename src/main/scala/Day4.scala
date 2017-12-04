package aoc

object Day4 {

  //val fileName = getClass.getResource(".") + "/Day4input.txt"
  val fileName = "./src/main/resources" + "/Day4input.txt"

  def readInput(fileName: String): List[String] = {
    require(fileName.nonEmpty, s"fileName.nonEmpty failed; with >${fileName}<")
    require(new java.io.File(fileName).exists, s"java.io.File(fileName).exists failed; with >${fileName}<")

    scala.io.Source.fromFile(fileName).getLines().toList
  }

  val in = readInput(fileName)

  object Part1 {
    def isValid(passPhrase: String): Boolean = {
      val words = passPhrase.split(" ")
      val groupByWords = words.groupBy(w => w)
      val countByWords = groupByWords.map { case (k, v) => (k, v.size) }
      countByWords.forall { case (_, count) => count == 1 }
    }

    def countValid(passPhrases: List[String]): Int = passPhrases.count(isValid)
  }

  object Part2 {
    def isValid(passPhrase: String): Boolean = {
      val words = passPhrase.split(" ")
      val sortedWords = words.map(w => w.sorted)
      val groupByWords = sortedWords.groupBy(w => w)
      val countByWords = groupByWords.map { case (k, v) => (k, v.size) }
      countByWords.forall { case (_, count) => count == 1 }
    }

    def countValid(passPhrases: List[String]): Int = passPhrases.count(isValid)
  }
}
