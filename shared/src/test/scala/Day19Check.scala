package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day19Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  property("Dummy - do not know, what to test here") {
    forAll(Gen.posNum[Int]) {n => {
    }}
  }
}
