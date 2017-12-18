package aoc

import aoc.Day15.Generator
import org.scalatest.{FlatSpec, Matchers}

class Day15Spec extends FlatSpec with Matchers {

  val testGenA = List(
    1092455,
    1181022009,
    245556042,
    1744312007,
    1352636452
  )

  val testGenB = List(
    430625591,
    1233683848,
    1431495498,
    137874439,
    285222916
  )

  val testAstart = 65
  val testBstart = 8921

  "the generators" should "return the right values" in {
    val g0 = Day15.Generator(testAstart, Day15.genAseed, Day15.devider)
    g0.current shouldBe testGenA(0)
    g0.next.current shouldBe testGenA(1)
    g0.next.next.current shouldBe testGenA(2)
    g0.next.next.next.current shouldBe testGenA(3)
    g0.next.next.next.next.current shouldBe testGenA(4)

    val g0result = (0 to 3).foldLeft(g0, List(g0.current))((acc, _) => {
      val g = Day15.Generator(acc._2.head, acc._1.factor, acc._1.devider)
      (g, g.current :: acc._2)
    })
    g0result._2.reverse should be (testGenA)

    val g1 = Day15.Generator(testBstart, Day15.genBseed, Day15.devider)
    val g1result = (0 to 3).foldLeft(g1, List(g1.current))((acc, _) => {
      val g = Day15.Generator(acc._2.head, acc._1.factor, acc._1.devider)
      (g, g.current :: acc._2)
    })
    g1result._2.reverse should be (testGenB)
  }
}
