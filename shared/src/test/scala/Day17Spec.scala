package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day17Spec extends FlatSpec with Matchers {
  import scala.collection.mutable

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

  behavior of "readInput()"
  it should "read the input" in {
    Day17.input shouldBe 371
  }

  behavior of "moveForward()"
  it should "return the right result(s)" in {
    Day17.moveForward(0, 1, 3) shouldBe 0
    Day17.moveForward(1, 2, 3) shouldBe 0
    Day17.moveForward(1, 3, 3) shouldBe 1
    Day17.moveForward(2, 4, 3) shouldBe 1
  }

  behavior of "insertAfter()"
  it should "return the right result(s)" in {
    Day17.insertAfter(0, mutable.ListBuffer(0), 1) should be (List(0, 1))
    Day17.insertAfter(0, mutable.ListBuffer(0, 1), 2) should be (List(0, 2, 1))
    Day17.insertAfter(1, mutable.ListBuffer(0, 2, 1), 3) should be (List(0, 2, 3, 1))
  }

  behavior of "nextBuffer()"
  it should "return the next buffer" in {
    val step1 = Day17.nextBuffer(0, mutable.ListBuffer(0), 3, 1)
    step1 should be (1, List(0, 1))

    val step2 = Day17.nextBuffer(step1._1, step1._2, 3, 2)
    step2 should be (1, List(0, 2, 1))

    val step3 = Day17.nextBuffer(step2._1, step2._2, 3, 3)
    step3 should be (2, List(0, 2, 3, 1))
  }

  behavior of "buildBuffer()"
  it should "return the right result(s)" in {
    Day17.buildBuffer(mutable.ListBuffer(0), 3, 1) should be (1, List(0, 1))
    Day17.buildBuffer(mutable.ListBuffer(0), 3, 2) should be (1, List(0, 2, 1))
    Day17.buildBuffer(mutable.ListBuffer(0), 3, 3) should be (2, List(0, 2, 3, 1))

    val (position, buffer) = Day17.buildBuffer(mutable.ListBuffer(0), 3, Day17.times)
    buffer(position) shouldBe Day17.times
    buffer.slice(position - 3, position + 3 + 1) should be (List(1512, 1134, 151, 2017, 638, 1513, 851))
    buffer(position + 1) shouldBe 638
  }

  behavior of "solve() - Part1"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day17.Part1.solve(3, Day17.times)._1 shouldBe 638
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day17.Part1.solve(Day17.steps, Day17.times)._1 shouldBe 1311
  }

  behavior of "solve() - Part2"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day17.Part2.solve(3, 1)._1 shouldBe 1
    Day17.Part2.solve(3, 2)._1 shouldBe 2
    Day17.Part2.solve(3, 3)._1 shouldBe 2
    Day17.Part2.solve(3, 4)._1 shouldBe 2
    Day17.Part2.solve(3, 5)._1 shouldBe 5
    Day17.Part2.solve(3, 9)._1 shouldBe 9
  }

  it should "solve the puzzle" taggedAs(SolutionTest, SlowTest) in {
    Day17.Part2.solve(Day17.steps, Day17.times2)._1 shouldBe 39170601
  }
}
