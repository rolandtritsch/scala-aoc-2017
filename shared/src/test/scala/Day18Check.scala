package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day18Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  property("") {
    forAll(Gen.posNum[Int]) {n => {
    }}
  }
}
