package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day21Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  lazy val genGrid = for {
    n <- Gen.posNum[Int]
    twoOrThree <- Gen.oneOf(2, 3)
    dim = n * twoOrThree
    grid <- Gen.listOfN(dim, Gen.listOfN(dim, Gen.oneOf('#', '.')))
  } yield {
    grid.map(_.toArray).toArray
  }

  property("For all possible grids, dividing it and joining it will give you the grid again") {
    forAll(genGrid) { grid => {
      val devided = Day21.divide(grid)
      val joined = Day21.join(devided)
      joined should be (grid)
    }}
  }
}
