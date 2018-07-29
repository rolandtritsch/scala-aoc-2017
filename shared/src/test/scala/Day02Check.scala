package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day02Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  val maxDimension = 100 * 2 // needs to be divisble by 2
  val minDimension = 2
  def maxValueForMin(d: Int) = (d / 2) - 1
  def minValueForMax(d: Int) = (d / 2) + 1

  val sheetGen = for {
    dimension <- Gen.choose(minDimension, maxDimension)
    min <- Gen.choose(0, maxValueForMin(dimension))
    max <- Gen.choose(minValueForMax(dimension), dimension)
    minPos <- Gen.choose(0, dimension - 1)
    maxPos <- Gen.choose(0, dimension - 1)
    if minPos != maxPos
  } yield {
    val sheet = (for(_ <- 1 to dimension) yield List.fill(dimension)(dimension / 2).updated(minPos, min).updated(maxPos, max)).toList
    (sheet, dimension * (max - min))
  }

  property("Any testsheet should return the expected result") {
    forAll(sheetGen) {case(sheet, result) => {
      Day02.Part1.solve(sheet)._1 shouldBe result
    }}
  }
}
