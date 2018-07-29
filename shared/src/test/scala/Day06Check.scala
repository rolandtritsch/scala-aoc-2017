package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day06Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  val membankGen = for {
    n <- Gen.choose(10, 10)
  } yield {
    List.fill(n * 10)(10)
  }

  property("Dummy - was not able to find a good property") {
    forAll(membankGen) {bank => {
      Day06.Part1.solve(bank)._1 shouldBe 1465
      Day06.Part2.solve(bank)._1 shouldBe bank.size
    }}
  }
}
