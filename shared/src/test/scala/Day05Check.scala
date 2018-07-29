package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day05Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  val stackGen = for {
    stackSize <- Gen.choose(10, 100)
  } yield {
    List.fill(stackSize)(1)
  }

  property("Any teststack should return its size") {
    forAll(stackGen) {s => {
      Day05.Part1.solve(s)._1 shouldBe s.size
      Day05.Part2.solve(s)._1 shouldBe s.size
    }}
  }
}
