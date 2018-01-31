package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day21Spec extends FlatSpec with Matchers {

  val testInput = List(
    "../.# => ##./#../...",
    ".#./..#/### => #..#/..../..../#..#"
  )

  behavior of "the input"
  it should "be correct" in {
    Day21.in.head shouldBe "../.. => ###/.##/#.."
  }

  behavior of "parsing the input"
  it should "produce the correct results" in {
    Day21.parseInput(testInput).size shouldBe 16
    Day21.parseInput(Day21.in).size shouldBe 864
  }

  behavior of "flipping the grid horizontally"
  it should "produce the correct result(s)" in {
    val in = Array(
      "###".toCharArray,
      "#..".toCharArray,
      "...".toCharArray
    )

    val out = Array(
      "...".toCharArray,
      "#..".toCharArray,
      "###".toCharArray
    )

    Day21.flipHorizontal(in) should be (out)
  }

  behavior of "flipping the grid vertically"
  it should "produce the correct result(s)" in {
    val in = Array(
      "###".toCharArray,
      "#..".toCharArray,
      "...".toCharArray
    )

    val out = Array(
      "###".toCharArray,
      "..#".toCharArray,
      "...".toCharArray
    )

    Day21.flipVertical(in) should be (out)
  }

  behavior of "rotating the grid clockwise"
  it should "produce the correct result(s)" in {
    val in = Array(
      "###".toCharArray,
      "#..".toCharArray,
      "...".toCharArray
    )

    val out = Array(
      ".##".toCharArray,
      "..#".toCharArray,
      "..#".toCharArray
    )

    Day21.rotateClockWise(in) should be (out)
  }

  behavior of "divide a given grid"
  // divide 3 -> 3
  // enhance 3 -> 4 -> 4
  // devide 4 -> 2x2
  // enhance 2x2 -> 3x3 -> 9
  // devide 9 -> 3x3
  // enhance 3x3 -> 4x4 -> 16
  // divide 16 -> 2x2x2x2
  // enhance 2x2x2x2 -> 3x3x3x3 -> 81
  // devide 81 -> 3x3x3x3
  // enhance 3x3x3x3 -> 4x4x4x4 -> 256
  // devide 256 -> 2x2x2x2x2x2x2x2
  // enhance 2x2x2x2x2x2x2x2 -> 3x3x3x3x3x3x3x3 -> 6561

  it should "return the correct result(s) (for 4x4)" in {
    val in = Array(
      "#..#".toCharArray,
      "....".toCharArray,
      "....".toCharArray,
      "#..#".toCharArray
    )

    val out = List(
      "#./..",
      ".#/..",
      "../#.",
      "../.#"
    )

    val grids = Day21.divide(List(in))
    grids should have size 4
    grids.map(Day21.Grid.toString(_)) should be (out)
  }

  behavior of  "enhancing the grid"
  // divide 3 -> 3
  // enhance 3 -> 4 -> 4
  // devide 4 -> 2x2
  // enhance 2x2 -> 3x3 -> 9
  // devide 9 -> 3x3
  // enhance 3x3 -> 4x4 -> 16
  // divide 16 -> 2x2x2x2
  // enhance 2x2x2x2 -> 3x3x3x3 -> 81
  // devide 81 -> 3x3x3x3
  // enhance 3x3x3x3 -> 4x4x4x4 -> 256
  // devide 256 -> 2x2x2x2x2x2x2x2
  // enhance 2x2x2x2x2x2x2x2 -> 3x3x3x3x3x3x3x3 -> 6561
  it should "produce the correct result(s)" in {
    val grids = Day21.enhance(List(Day21.start), Day21.parseInput(testInput))
    grids should have size 1
    Day21.Grid.toString(grids.head) shouldBe "#..#/..../..../#..#"
  }

  ignore should "produce more correct result(s)" in {
    val rules = Day21.parseInput(testInput)
    val grids = Day21.enhance(List(Day21.start), rules)
    grids should have size 1
    Day21.Grid.toString(grids.head) shouldBe "#..#/..../..../#..#"

    val nextGrids = Day21.divide(grids)
    nextGrids should have size 4
    val squares = Day21.enhance(nextGrids, rules)
    squares should have size 4
    val head = Day21.Grid.toString(squares.head)
    head shouldBe "##./#../..."
  }

  behavior of "running the simulation"
  it should "return the correct results" in {
    val grid2 = Day21.run(List(Day21.start), Day21.parseInput(testInput), 2)
    grid2.flatten.flatten.count(_ == '#') shouldBe 12
  }

  it should "solve the puzzle" in {
    val grid5 = Day21.run(List(Day21.start), Day21.parseInput(Day21.in), 5)
    grid5.flatten.flatten.count(_ == '#') shouldBe 205
  }

  ignore should "show all the right numbers on the way to the solution" in {
    //val counts = List(5, 8, 25, 58, 78, 205, 516, 730, 1874, 4645, 6538, 16853, 41876, 58832, 151557, 376588, 529586, 1364380)
    val counts = List(5, 8, 25, 58, 78, 205)
    (0 to counts.size - 1).map (n => {
      Day21.run(List(Day21.start), Day21.parseInput(Day21.in), n).flatten.flatten.count(_ == '#')
    }).toList should be (counts)

  }
}
