package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day1Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  val equalStrings = for {
    n <- Gen.choose(0, 9)
    l <- Gen.choose(1, 100)
  } yield List.fill(l)(n.toString).mkString

  property("Any string of equal numbers should be <number>*<length of the string") {
    forAll(equalStrings) {in =>
      val expectedResult = in.head.toString.toInt * in.length
      Day01.Part1.captcha(in) shouldBe expectedResult
      //Day1.captcha2(in) shouldBe expectedResult
    }
  }
}
