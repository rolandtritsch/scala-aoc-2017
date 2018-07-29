package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day01Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  val equalStrings = for {
    n <- Gen.choose(0, 9)
    l <- Gen.choose(1, 100)
  } yield List.fill(l * 2)(n.toString).mkString

  property("Any string of equal numbers should be <number>*<length of the string") {
    forAll(equalStrings) {input =>
      val expectedResult = input.head.asDigit * input.length
      Day01.Part1.solve(input)._1 shouldBe expectedResult
      Day01.Part2.solve(input)._1 shouldBe expectedResult
    }
  }
}
