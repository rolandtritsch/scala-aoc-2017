package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day17Spec extends FlatSpec with Matchers {

  val testResult = List(
    (0, List(0)),
    (1, List(0, 1)),
    (1, List(0, 2, 1)),
    (2, List(0, 2, 3, 1)),
    (2, List(0, 2, 4, 3, 1)),
    (1, List(0, 5, 2, 4, 3, 1)),
    (1, List(0, 5, 2, 4, 3, 6, 1)),
    (2, List(0, 5, 7, 2, 4, 3, 6, 1)),
    (6, List(0, 5, 7, 2, 4, 3, 8, 6, 1)),
    (1, List(0, 9, 5, 7, 2, 4, 3, 8, 6, 1))
  )

  "moveForward" should "return the right result(s)" in {
    Day17.moveForward(0, 1, 3) shouldBe 0
    Day17.moveForward(1, 2, 3) shouldBe 0
    Day17.moveForward(1, 3, 3) shouldBe 1
    Day17.moveForward(2, 4, 3) shouldBe 1
  }

  "insertAfter" should "return the right result(s)" in {
    Day17.insertAfter(0, List(0), 1) should be (List(0, 1))
    Day17.insertAfter(0, List(0, 1), 2) should be (List(0, 2, 1))
    Day17.insertAfter(1, List(0, 2, 1), 3) should be (List(0, 2, 3, 1))
  }

  "nextBuffer" should "return the next buffer" in {
    val step1 = Day17.nextBuffer(0, List(0), 3, 1)
    step1 should be (1, List(0, 1))

    val step2 = Day17.nextBuffer(step1._1, step1._2, 3, 2)
    step2 should be (1, List(0, 2, 1))

    val step3 = Day17.nextBuffer(step2._1, step2._2, 3, 3)
    step3 should be (2, List(0, 2, 3, 1))
  }

  "buildBuffer" should "return the right result(s)" in {
    Day17.buildBuffer(List(0), 3, 1) should be (1, List(0, 1))
    Day17.buildBuffer(List(0), 3, 2) should be (1, List(0, 2, 1))
    Day17.buildBuffer(List(0), 3, 3) should be (2, List(0, 2, 3, 1))

    val (position, buffer) = Day17.buildBuffer(List(0), 3, Day17.times)
    buffer(position) shouldBe Day17.times
    buffer.slice(position - 3, position + 3 + 1) should be (List(1512, 1134, 151, 2017, 638, 1513, 851))
    buffer(position + 1) shouldBe 638
  }

  it should "solve the puzzle" in {
    val (position, buffer) = Day17.buildBuffer(List(0), Day17.steps, Day17.times)
    buffer(position) shouldBe Day17.times
    buffer(position + 1) shouldBe 1311
  }

  ignore should "solve the puzzle (Part2)" in {
    val (position, buffer) = Day17.buildBuffer(List(0), Day17.steps, Day17.times2)
    buffer(position) shouldBe Day17.times2
    buffer(1) shouldBe 0
  }
}