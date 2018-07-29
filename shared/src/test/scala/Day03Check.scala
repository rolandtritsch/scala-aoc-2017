package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day03Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  property("For any given number the result needs to be positive") {
    forAll(Gen.posNum[Int]) {n => {
      Day03.Part1.solve(n)._1 should be >= 0
      Day03.Part2.solve(n)._1 should be > n
    }}
  }
}
