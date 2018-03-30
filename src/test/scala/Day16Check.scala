package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day16Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  val inputGen = for {
    str <- Gen.alphaStr
    programs = str.distinct
    if (programs.nonEmpty)
    a <- Gen.choose(0, programs.size - 1)
    b <- Gen.choose(0, programs.size - 1)
  } yield {
    (programs, a, b)
  }

  property("Any input exhanged back and forth should give the input again") {
    forAll(inputGen) { case (programs, a, b) => {
      val m1 = Day16.executeMoves(programs.toArray, List(Day16.Exchange(a, b)))
      val m2 = Day16.executeMoves(programs.toArray, List(Day16.Exchange(b, a)))
      m1.mkString shouldBe m2.mkString
    }}
  }

  property("Any input partnered twice should give the input again") {
    forAll(inputGen) { case (programs, a, b) => {
      val m1 = Day16.executeMoves(programs.toArray, List(Day16.Partner(programs(a), programs(b))))
      val m2 = Day16.executeMoves(m1, List(Day16.Partner(programs(a), programs(b))))
      m2.mkString shouldBe programs
    }}
  }
}
