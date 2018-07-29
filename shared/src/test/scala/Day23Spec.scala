package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day23Spec extends FlatSpec with Matchers {

  behavior of "readInput()"
  it should "be correct" in {
    Day23.input.head shouldBe "set l 1"
  }

  behavior of "parseInput()"
  it should "return the correct result(s)" in {
    Day23.parseInput(Day23.input).size shouldBe 33
  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day23.Part1.solve(Day23.input)._1 shouldBe 6724
  }

  behavior of "isPrime()"
  it should "return the correct result(s)" in {
    val primes = List(1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31)
    primes.forall(Day23.Part2.findPrime(_)) shouldBe true
  }

  behavior of "solve() - Part2"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day23.Part2.solve(Day23.input)._1 shouldBe 903
  }
}
