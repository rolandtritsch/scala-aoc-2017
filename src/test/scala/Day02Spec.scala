package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day02Spec extends FlatSpec with Matchers {

  val testSheet = List(
    List(5, 1, 9, 5),
    List(7, 5, 3),
    List(2, 4, 6, 8)
  )

  behavior of "Reading the input"
  it should "read the (begining) of the input" in {
    Day02.in.head should be (List(737, 1866, 1565, 1452, 1908, 1874, 232, 1928, 201, 241, 922, 281, 1651, 1740, 1012, 1001))
  }

  behavior of "Part 1 - checksum"
  it should "return the correct checksum" in {
    Day02.checksum(testSheet) shouldBe 18
  }

  it should "solve the puzzle" in {
    Day02.checksum(Day02.in) shouldBe 34925
  }

  it should "throw an exception, if the sheet is malformed" in {
    an [IllegalArgumentException] should be thrownBy Day02.checksum(List())
    an [IllegalArgumentException] should be thrownBy Day02.checksum(List(List()))
    an [IllegalArgumentException] should be thrownBy Day02.checksum(List(List()))
    an [IllegalArgumentException] should be thrownBy Day02.checksum(List(List(0), List(), List(0)))
  }

  val testSheet2 = List(
    List(5, 9, 2, 8),
    List(9, 4, 7, 3),
    List(3, 8, 6, 5)
  )

  behavior of "Part 2 - checksum"
  it should "return the correct checksum" in {
    Day02.checksum2(testSheet2) shouldBe 9
  }

  it should "solve the puzzle" in {
    Day02.checksum2(Day02.in) shouldBe 221
  }

  it should "throw an exception, if the sheet is malformed" in {
    an [IllegalArgumentException] should be thrownBy Day02.checksum2(List())
    an [IllegalArgumentException] should be thrownBy Day02.checksum2(List(List()))
    an [IllegalArgumentException] should be thrownBy Day02.checksum2(List(List()))
    an [IllegalArgumentException] should be thrownBy Day02.checksum2(List(List(0), List(), List(0)))
  }
}