package aoc

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object Day04Meter extends Bench.OfflineReport {

  def randomString(n: Int) = util.Random.alphanumeric.take(n).mkString

  val passPhraseGen = for {
    numOfPhrases <- Gen.range("numOfPhrases")(10, 20, 2)
    numOfWordsInAPhrase <- Gen.range("numOfWordsInAPhrase")(10, 20, 2)
    lenghtOfWordsInAPhrase <- Gen.range("lenghtOfWordsInAPhrase")(5, 10, 2)
  } yield {
    List.fill(numOfPhrases)(List.fill(numOfWordsInAPhrase)(randomString(lenghtOfWordsInAPhrase)))
  }

  performance of "Part1" in {
    measure method "solve" in {
      using(passPhraseGen) in {
        pps => Day04.Part1.solve(pps)
      }
    }
  }

  performance of "Part2" in {
    measure method "solve" in {
      using(passPhraseGen) in {
        pps => Day04.Part2.solve(pps)
      }
    }
  }
}