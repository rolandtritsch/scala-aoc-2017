package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day15Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  property("Any Int should return expected result") {
    forAll(Gen.posNum[Long]) {n => {
      Day15.matching(n, n) shouldBe true
      Day15.matching(n, n + 1) shouldBe false
    }}
  }
}
