package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day07Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  val treeGen = for {
    n <- Gen.choose(0, 0)
  } yield {
    n
  }

  property("Dummy - was not able to come up with an easy way to generate trees") {
    forAll(treeGen) {n => {
      true shouldBe true
    }}
  }
}
