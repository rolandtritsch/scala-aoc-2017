package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day14Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  val gridGen = for {
    row <- Gen.choose(0, 128)
    col <- Gen.choose(0, 128)
  } yield {
    val grid = (0 until 128).foldLeft(List.empty[List[Boolean]])((acc, _) => List.fill(128)(false) :: acc)
    val gridWithRow = (0 until 128).foldLeft(grid)((g, c) => g.updated(row, g(row).updated(c, true)))
    val gridWithRowAndCol = (0 until 128).foldLeft(gridWithRow)((g, r) => g.updated(r, g(r).updated(col, true)))
    gridWithRowAndCol
  }

  property("Any testgrid should return respect result") {
    forAll(gridGen) {g => {
      g.foldLeft(0)((sum, hash) => sum + hash.count(identity)) shouldBe 128 * 2 -1
      Day14.findRegions(g).size shouldBe 1
    }}
  }
}
