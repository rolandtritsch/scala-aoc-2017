package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day10Spec extends FlatSpec with Matchers {

  behavior of "readInput"
  it should "read the input" in {
    Day10.in should be (List(165, 1, 255, 31, 87, 52, 24, 113, 0, 91, 148, 254, 158, 2, 73, 153))
  }

  behavior of "shiftLeft"
  an [IllegalArgumentException] should be thrownBy Day10.shiftLeft(List(), 0)
  an [IllegalArgumentException] should be thrownBy Day10.shiftLeft(List(0), -1)
  it should "shift a list/hash to the left" in {
    Day10.shiftLeft(List(1, 2, 3), 0) should be (List(1, 2, 3))
    Day10.shiftLeft(List(1, 2, 3), 1) should be (List(2, 3, 1))
    Day10.shiftLeft(List(1, 2, 3), 2) should be (List(3, 1, 2))
  }

  behavior of "shiftRight"
  it should "shift a list/hash to the right" in {
    Day10.shiftRight(List(1, 2, 3), 0) should be (List(1, 2, 3))
    Day10.shiftRight(List(1, 2, 3), 1) should be (List(3, 1, 2))
    Day10.shiftRight(List(1, 2, 3), 2) should be (List(2, 3, 1))
  }

  behavior of "reverse"
  an [IllegalArgumentException] should be thrownBy Day10.reverse(List(), 0)
  an [IllegalArgumentException] should be thrownBy Day10.reverse(List(0), -1)
  an [IllegalArgumentException] should be thrownBy Day10.reverse(List(0), 2)
  it should "reverse a given hash" in {
    Day10.reverse(List(1, 2, 3, 4, 5), 2) should be (List(2, 1, 3, 4, 5))
    Day10.reverse(List(1, 2, 3, 4, 5), 3) should be (List(3, 2, 1, 4, 5))
    Day10.reverse(List(1, 2, 3, 4, 5), 4) should be (List(4, 3, 2, 1, 5))
  }

  behavior of "knot"
  it should "return the right test hashes" in {
    Day10.knot(List(3), Day10.Hash(List.range(0, 5), 0, 0)).hash should be (List(2, 1, 0, 3, 4))
    Day10.knot(List(3, 4), Day10.Hash(List.range(0, 5), 0, 0)).hash should be (List(4, 3, 0, 1, 2))
    Day10.knot(List(3, 4, 1), Day10.Hash(List.range(0, 5), 0, 0)).hash should be (List(4, 3, 0, 1, 2))
    Day10.knot(List(3, 4, 1, 5), Day10.Hash(List.range(0, 5), 0, 0)).hash should be (List(3, 4, 2, 1, 0))
  }

  it should "return the right hash" in {
    Day10.knot(Day10.in, Day10.Hash(List.range(0, 256), 0, 0)).hash should be (List(121, 34, 35, 36, 37, 38, 39, 40, 41, 126, 125, 243, 242, 241, 240, 239, 215, 216, 79, 80, 81, 82, 83, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 214, 213, 212, 211, 210, 209, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 33, 32, 31, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 85, 84, 57, 55, 56, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42, 70, 69, 217, 78, 77, 76, 75, 74, 73, 72, 71, 128, 127, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 255, 254, 253, 252, 251, 250, 249, 248, 247, 246, 245, 244, 124, 123, 122))
  }

  behavior of "solve"
  it should "solve the puzzle" in {
    Day10.solve(Day10.in, List.range(0, 256)) shouldBe 4114
  }
}