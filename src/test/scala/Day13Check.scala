package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day13Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  val range1Gen = for {
    n <- Gen.choose(0, 100)
  } yield {
    (n, 1)
  }

  val layerGen = for {
    n <- Gen.choose(0, 100)
  } yield {
    (n, n)
  }

  property("All layers with a range of 1 will detect the packet") {
    forAll(range1Gen) {case (depth, range) => {
      Day13.threatDetected(depth, range) shouldBe true
    }}
  }

  property("All layers n with a range of n will not detect the packet") {
    forAll(layerGen) {case (depth, range) => {
      Day13.threatDetected(depth, range) shouldBe false
    }}
  }
}
