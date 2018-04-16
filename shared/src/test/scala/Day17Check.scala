package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day17Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  property("Moving forward any number of steps on a list of size 1 will always give us position 0") {
    forAll(Gen.posNum[Int]) {n => {
      Day17.moveForward(0, 1, n) shouldBe 0
    }}
  }
}
