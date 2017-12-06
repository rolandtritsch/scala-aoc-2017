package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day1Spec extends FlatSpec with Matchers {

  "Part1 - captcha" should "return the right result" in {
    Day1.Part1.captcha("1122") shouldBe 3
    Day1.Part1.captcha("1111") shouldBe 4
    Day1.Part1.captcha("1234") shouldBe 0
    Day1.Part1.captcha("91212129") shouldBe 9
  }

  it should "solve the puzzle" in {
    Day1.Part1.captcha(Day1.in) shouldBe 1223
  }

  it should "throw an exception, if the input is empty" in {
    assertThrows[IllegalArgumentException](Day1.Part1.captcha(""))
  }

  it should "throw an exception, if the input is non-numeric" in {
    assertThrows[IllegalArgumentException](Day1.Part1.captcha("123a"))
    assertThrows[IllegalArgumentException](Day1.Part1.captcha("123 "))
  }

  "Part2 - captcha" should "return the right result" in {
    Day1.Part2.captcha("1212") shouldBe 6
    Day1.Part2.captcha("1221") shouldBe 0
    Day1.Part2.captcha("123425") shouldBe 4
    Day1.Part2.captcha("123123") shouldBe 12
    Day1.Part2.captcha("12131415") shouldBe 4
    Day1.Part2.captcha("111111") shouldBe 6
  }

  it should "solve the puzzle" in {
    Day1.Part2.captcha(Day1.in) shouldBe 1284
  }

  it should "throw an exception, if the input is empty" in {
    assertThrows[IllegalArgumentException](Day1.Part2.captcha(""))
  }

  it should "throw an exception, if the input is non-numeric" in {
    assertThrows[IllegalArgumentException](Day1.Part2.captcha("123a"))
    assertThrows[IllegalArgumentException](Day1.Part2.captcha("123 "))
  }

  it should "throw an exception, if the input is not of even length" in {
    assertThrows[IllegalArgumentException](Day1.Part2.captcha("123"))
    assertThrows[IllegalArgumentException](Day1.Part2.captcha("12345"))
  }
}
