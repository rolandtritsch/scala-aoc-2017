package aoc

import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day20Spec extends AnyFlatSpec with should.Matchers {

  val testInput = List(
    "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>",
    "p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"
  )

  val tick1Result = List(
    "p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>",
    "p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>"
  )

  val tick2Result = List(
    "p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>",
    "p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>"
  )

  val tick3Result = List(
    "p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>",
    "p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>"
  )

  val test2Input = List(
    "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>",
    "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>",
    "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>",
    "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"
  )

  behavior of "readInput()"
  it should "be correct" in {
    Day20.input.head shouldBe "p=<1199,-2918,1457>, v=<-13,115,-8>, a=<-7,8,-10>"
  }

  behavior of "parseInput()"
  it should "parse the input correctly" in {
    Day20.parseInput(testInput).head shouldBe Day20.Particle(Day20.Position(3,0,0), Day20.Velocity(2,0,0), Day20.Acceleration(-1, 0, 0))
  }

  behavior of "run()"
  it should "return the right results for the testcase(s)" taggedAs(BuildTest) in {
    Day20.run(Day20.parseInput(testInput), 1) should be (Day20.parseInput(tick1Result))
    Day20.run(Day20.parseInput(testInput), 2) should be (Day20.parseInput(tick2Result))
    Day20.run(Day20.parseInput(testInput), 3) should be (Day20.parseInput(tick3Result))
  }

  behavior of "findClosest()"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day20.findClosest(Day20.run(Day20.parseInput(testInput), 10)) shouldBe 0
  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day20.Part1.solve(Day20.input)._1 shouldBe 243
  }

  behavior of "runWithCollisionDetection"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day20.runWithCollisionDetection(Day20.parseInput(testInput), 3).size shouldBe 2
    Day20.runWithCollisionDetection(Day20.parseInput(test2Input), 3).size shouldBe 1
  }

  behavior of "solve() - Part2"
  it should "solve() - Part2" taggedAs(SolutionTest) in {
    Day20.Part2.solve(Day20.input)._1 shouldBe 648
  }
}
