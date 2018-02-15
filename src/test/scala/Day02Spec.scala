package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day02Spec extends FlatSpec with Matchers {
  val testSheet = List(
    List(5, 1, 9, 5),
    List(7, 5, 3),
    List(2, 4, 6, 8)
  )

  "checksum" should "return the correct checksum" in {
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

  "checksum2" should "return the correct checksum" in {
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