package aoc

/** Problem: [[https://adventofcode.com/2017/day/4]]
  *
  * Solution:
  *
  * General - Build a histogram of all words in the passphrase
  * and make sure that there are no duplicates and last but not
  * least count the valid passphrases.
  *
  * Part1 - Simple histogram.
  *
  * Part2 - Now we need to detect anagrams. We do that simply
  * by sorting the chars in the words (two words with the same
  * chars in it are an anagram, right).
  *
  * Note: Strictly speaking for Part2 you need to check Policy1
  * and Policy2 (this is how the problem/requirement is expressed),
  * but practically we only need to check Policy2, because Policy2
  * is a (stronger) superset of Policy1.
  */
object Day04 {

  val input = Util.readInput("Day04input.txt").map(_.split(' ').toList)

  type PassPhrase = List[String]

  def isValid(words: PassPhrase): Boolean = {
    require(words.nonEmpty, s"words.nonEmpty failed")

    val groupByWords = words.groupBy(identity)
    val countByWords = groupByWords.map { case (k, v) => (k, v.size) }
    countByWords.forall { case (_, count) => count == 1 }
  }

  def countValid(passPhrases: List[PassPhrase]): Int = {
    require(passPhrases.nonEmpty, s"passPhrases.nonEmpty")

    passPhrases.count(isValid)
  } ensuring(_ >= 0, s"_ >= 0 failed")

  object Part1 {
    def solve(passPhrases: List[PassPhrase]): (Int, Long) = Util.measuredTimeMillis {
      countValid(passPhrases)
    }
  }

  object Part2 {
    def solve(passPhrases: List[PassPhrase]): (Int, Long) = Util.measuredTimeMillis {
      countValid(passPhrases.map(_.map(_.sorted)))
    }
  }
}
