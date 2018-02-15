package aoc

object Day04 {
  val in = Util.readInput("Day4input.txt")

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
