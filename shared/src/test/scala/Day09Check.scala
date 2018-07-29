package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day09Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  // {{{{}}}}
  val streamGen = for {
    n <- Gen.choose(1, 1000)
  } yield {
    List.fill(n)('{') ++ List.fill(n)('}')
  }

  // {}{}{}{}
  val streamGen2 = for {
    n <- Gen.choose(1, 1000)
  } yield {
    List.fill(n)("{}").mkString.toList
  }

  // {<!!x>...}
  val streamGen3 = for {
    n <- Gen.choose(1, 1000)
  } yield {
    (List('{') ++ List.fill(n)("<!!x>").mkString.toList ++ List('}'), n)
  }

  property("Any teststream should return respect result") {
    forAll(streamGen) {s => {
      Day09.Part1.solve(s)._1 shouldBe (1 to (s.size/2)).sum
    }}
  }

  property("Any teststream2 should return respect result") {
    forAll(streamGen2) {s => {
      Day09.Part1.solve(s)._1 shouldBe (s.size/2)
    }}
  }

  property("Any teststream3 should return respect result") {
    forAll(streamGen3) {case (s, n) => {
      Day09.Part2.solve(s)._1 shouldBe n
    }}
  }
}
