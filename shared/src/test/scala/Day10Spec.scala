package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day10Spec extends FlatSpec with Matchers {

  behavior of "readInput()"
  it should "read the input" in {
    Day10.input shouldBe "165,1,255,31,87,52,24,113,0,91,148,254,158,2,73,153"
  }

  behavior of "shiftLeft()"
  an [IllegalArgumentException] should be thrownBy Day10.shiftLeft(List(), 0)
  an [IllegalArgumentException] should be thrownBy Day10.shiftLeft(List(0), -1)
  it should "shift a list/hash to the left" in {
    Day10.shiftLeft(List(1, 2, 3), 0) should be (List(1, 2, 3))
    Day10.shiftLeft(List(1, 2, 3), 1) should be (List(2, 3, 1))
    Day10.shiftLeft(List(1, 2, 3), 2) should be (List(3, 1, 2))
  }

  behavior of "shiftRight()"
  it should "shift a list/hash to the right" in {
    Day10.shiftRight(List(1, 2, 3), 0) should be (List(1, 2, 3))
    Day10.shiftRight(List(1, 2, 3), 1) should be (List(3, 1, 2))
    Day10.shiftRight(List(1, 2, 3), 2) should be (List(2, 3, 1))
  }

  behavior of "reverse()"
  an [IllegalArgumentException] should be thrownBy Day10.reverse(List(), 0)
  an [IllegalArgumentException] should be thrownBy Day10.reverse(List(0), -1)
  an [IllegalArgumentException] should be thrownBy Day10.reverse(List(0), 2)
  it should "reverse a given hash" in {
    Day10.reverse(List(1, 2, 3, 4, 5), 2) should be (List(2, 1, 3, 4, 5))
    Day10.reverse(List(1, 2, 3, 4, 5), 3) should be (List(3, 2, 1, 4, 5))
    Day10.reverse(List(1, 2, 3, 4, 5), 4) should be (List(4, 3, 2, 1, 5))
  }

  behavior of "knot()"
  it should "return the right test hashes" taggedAs(BuildTest) in {
    val seed = Day10.Hash(List.range(0, 5), 0, 0)
    Day10.knot(List(3), seed).hash should be (List(2, 1, 0, 3, 4))
    Day10.knot(List(3, 4), seed).hash should be (List(4, 3, 0, 1, 2))
    Day10.knot(List(3, 4, 1), seed).hash should be (List(4, 3, 0, 1, 2))
    Day10.knot(List(3, 4, 1, 5), seed).hash should be (List(3, 4, 2, 1, 0))
  }

  it should "return the right hash" in {
    Day10.knot(Day10.input2Lengths(Day10.input), Day10.seed).hash should be (List(121, 34, 35, 36, 37, 38, 39, 40, 41, 126, 125, 243, 242, 241, 240, 239, 215, 216, 79, 80, 81, 82, 83, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 214, 213, 212, 211, 210, 209, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 33, 32, 31, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 85, 84, 57, 55, 56, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42, 70, 69, 217, 78, 77, 76, 75, 74, 73, 72, 71, 128, 127, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 255, 254, 253, 252, 251, 250, 249, 248, 247, 246, 245, 244, 124, 123, 122))
  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day10.Part1.solve(Day10.input)._1 shouldBe 4114
  }

  behavior of "encode()"
  it should "process the testcase(s)" in {
    Day10.encode("1,2,3", Day10.suffix) should be (List(49, 44, 50, 44, 51) ++ Day10.suffix)
  }

  behavior of "sparse()"
  it should "process the testcase(s)" in {
    val input = List(3, 4, 1, 5) ++ Day10.suffix
    Day10.sparse(input, Day10.seed, Day10.rounds).hash should be (List(170, 24, 100, 99, 11, 128, 219, 202, 119, 50, 204, 105, 2, 187, 40, 205, 233, 102, 46, 34, 33, 245, 76, 222, 164, 209, 55, 151, 15, 91, 206, 176, 37, 167, 141, 86, 112, 111, 166, 96, 64, 7, 174, 214, 10, 162, 148, 207, 178, 25, 203, 153, 82, 246, 78, 41, 212, 39, 244, 241, 217, 254, 152, 243, 192, 220, 14, 13, 238, 6, 115, 114, 110, 88, 198, 21, 247, 53, 120, 19, 52, 89, 48, 9, 155, 138, 92, 248, 27, 218, 58, 67, 1, 213, 184, 104, 249, 71, 90, 234, 51, 252, 17, 190, 116, 18, 113, 42, 47, 197, 255, 226, 165, 173, 139, 95, 183, 172, 171, 103, 182, 194, 156, 65, 188, 83, 228, 160, 31, 143, 69, 8, 93, 124, 59, 54, 101, 23, 189, 79, 229, 131, 239, 125, 98, 196, 134, 108, 87, 122, 60, 251, 216, 161, 26, 117, 106, 232, 201, 123, 150, 28, 70, 68, 154, 121, 145, 227, 30, 223, 74, 140, 186, 157, 85, 45, 62, 179, 57, 35, 136, 147, 81, 146, 75, 20, 177, 132, 163, 3, 142, 195, 84, 109, 127, 36, 129, 200, 175, 215, 181, 180, 5, 185, 169, 126, 0, 49, 22, 80, 97, 44, 32, 61, 118, 208, 250, 77, 211, 210, 191, 158, 221, 230, 66, 199, 231, 107, 240, 29, 144, 130, 137, 224, 225, 38, 236, 237, 43, 235, 159, 94, 56, 63, 72, 73, 12, 193, 168, 149, 16, 242, 135, 133, 253, 4))
  }

  behavior of "xorHashSlice()"
  it should "process the testcase(s)" in {
    Day10.xorHashSlice(List(65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22)) shouldBe 64
  }

  behavior of "dense()"
  it should "process the testcase(s)" in {
    val input = List(3, 4, 1, 5) ++ Day10.suffix
    val sparse = Day10.sparse(input, Day10.seed, Day10.rounds).hash
    Day10.dense(sparse) should be (List(147, 58, 76, 128, 186, 93, 164, 9, 133, 128, 65, 200, 8, 28, 153, 46))
  }

  behavior of "dense2hex()"
  it should "process the testcase(s)" in {
    Day10.dense2hex(List(64, 7, 255)) shouldBe "4007ff"
    Day10.dense2hex(List(1, 7, 255)) shouldBe "0107ff"
  }

  behavior of "solve() - Part2"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day10.Part2.solve("")._1 shouldBe "a2582a3a0e66e6e86e3812dcb672a272"
    Day10.Part2.solve("AoC 2017")._1 shouldBe "33efeb34ea91902bb2f59c9920caa6cd"
    Day10.Part2.solve("1,2,3")._1 shouldBe "3efbe78a8d82f29979031a4aa0b16a9d"
    Day10.Part2.solve("1,2,4")._1 shouldBe "63960835bcdc130f0b66d7ff4f6a5a8e"
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day10.Part2.solve(Day10.input)._1 shouldBe "2f8c3d2100fdd57cec130d928b0fd2dd"
  }
}
