package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day10Spec extends FlatSpec with Matchers {

  "knot" should "return the right hash" in {
    Day10.knot(List(3), List.range(0, 5)) should be (List(2, 1, 0, 3, 4))
    Day10.knot(List(3, 4), List.range(0, 5)) should be (List(4, 3, 0, 1, 2))
    Day10.knot(List(3, 4, 1), List.range(0, 5)) should be (List(4, 3, 0, 1, 2))
    Day10.knot(List(3, 4, 1, 5), List.range(0, 5)) should be (List(3, 4, 2, 1, 0))
  }

  ignore should "solve the puzzle" in {
    Day10.knot(Day10.in, List.range(0, 256)) should be (List())
  }
}