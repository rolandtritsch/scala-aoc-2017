package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day01Spec extends FlatSpec with Matchers {

  behavior of "readInput()"
  it should "return the (the beginnig) of the correct input" in {
    Day01.input.take(10) shouldBe "7773699185"
  }

  behavior of "solve() - Part1"
  it should "solve the testcases" in {
    Day01.Part1.solve("1122") shouldBe 3
    Day01.Part1.solve("1111") shouldBe 4
    Day01.Part1.solve("1234") shouldBe 0
    Day01.Part1.solve("91212129") shouldBe 9
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day01.Part1.solve(Day01.input) shouldBe 1223
  }

  it should "throw an exception, if the input is empty" in {
    assertThrows[IllegalArgumentException](Day01.Part1.solve(""))
  }

  it should "throw an exception, if the input is non-numeric" in {
    assertThrows[IllegalArgumentException](Day01.Part1.solve("123a"))
    assertThrows[IllegalArgumentException](Day01.Part1.solve("123 "))
  }

  behavior of "solve() - Part2"
  it should "solve the testcases" in {
    Day01.Part2.solve("1212") shouldBe 6
    Day01.Part2.solve("1221") shouldBe 0
    Day01.Part2.solve("123425") shouldBe 4
    Day01.Part2.solve("123123") shouldBe 12
    Day01.Part2.solve("12131415") shouldBe 4
    Day01.Part2.solve("111111") shouldBe 6
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day01.Part2.solve(Day01.input) shouldBe 1284
  }

  it should "throw an exception, if the input is empty" in {
    assertThrows[IllegalArgumentException](Day01.Part2.solve(""))
  }

  it should "throw an exception, if the input is non-numeric" in {
    assertThrows[IllegalArgumentException](Day01.Part2.solve("123a"))
    assertThrows[IllegalArgumentException](Day01.Part2.solve("123 "))
  }

  it should "throw an exception, if the input is not of even length" in {
    assertThrows[IllegalArgumentException](Day01.Part2.solve("123"))
    assertThrows[IllegalArgumentException](Day01.Part2.solve("12345"))
  }
}
