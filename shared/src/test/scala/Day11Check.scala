package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day11Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  // a path that is a star (going back and forth in all directions).
  // ending up in the center again.
  val starPathGen = for {
    n <- Gen.choose(1, 1000)
  } yield {
    List.fill(n)(List("n", "s", "ne", "sw", "nw", "se")).flatten
  }

  // random path
  val randomPathGen = for {
    n <- Gen.choose(1, 100)
    p <- Gen.listOfN(n, Gen.oneOf("n", "s", "ne", "sw", "nw", "se"))
  } yield p

  property("Any star path should end up in the center again") {
    forAll(starPathGen) { p => {
      Day11.Part1.solve(p)._1 shouldBe 0
    }}
  }

  property("For any star path the max distance is 1") {
    forAll(starPathGen) { p => {
      Day11.Part2.solve(p)._1 shouldBe 1
    }}
  }

  property("For any random stream max should be >= distance") {
    forAll(randomPathGen) { p => {
      val distance = Day11.Part1.solve(p)._1
      val max = Day11.Part2.solve(p)._1
      max should be >= distance
    }}
  }
}
