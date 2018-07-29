package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day04Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  val minNumOfPassPhrases = 1
  val maxNumOfPassPhrases = 10
  val minNumOfWords = 1
  val maxNumOfWords = 10
  val minLenghtOfWords = 5
  val maxLengthOfWords = 10

  def randomString(n: Int) = util.Random.alphanumeric.take(n).mkString

  val passPhraseGen = for {
    numOfPhrases <- Gen.choose(minNumOfPassPhrases, maxNumOfPassPhrases)
    numOfWordsInAPhrase <- Gen.choose(minNumOfWords, maxNumOfWords)
    lenghtOfWordsInAPhrase <- Gen.choose(minLenghtOfWords, maxLengthOfWords)
  } yield {
    List.fill(numOfPhrases)(List.fill(numOfWordsInAPhrase)(randomString(lenghtOfWordsInAPhrase)))
  }

  property("All passphrases are valid") {
    forAll(passPhraseGen) {passPhrases => {
      Day04.Part1.solve(passPhrases)._1 shouldBe passPhrases.size
      Day04.Part2.solve(passPhrases)._1 shouldBe passPhrases.size
    }}
  }
}
