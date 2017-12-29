package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day21Spec extends FlatSpec with Matchers {

  val testInput = List(
    "../.# => ##./#../...",
    ".#./..#/### => #..#/..../..../#..#"
  )
  "the input" should "be correct" in {
    Day21.in.head shouldBe "../.. => ###/.##/#.."
  }

  "parsing the input" should "produce the correct results" in {
    Day21.parseInput(testInput).size shouldBe 24
    Day21.parseInput(Day21.in).size shouldBe 1296
  }

  "flipping the grid horizontally" should "produce the correct result(s)" in {
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

  "flipping the grid vertically" should "produce the correct result(s)" in {
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

  "rotating the grid clockwise" should "produce the correct result(s)" in {
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

  "divide a given grid" should "return the correct result(s) (for 4x4)" in {
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

    Day21.divide(in).map(Day21.Grid.toString(_)) should be (out)
  }

  it should "return the correct result(s) (for 6x6)" in {
    val in = Array(
      "#....#".toCharArray,
      "......".toCharArray,
      "......".toCharArray,
      "......".toCharArray,
      "......".toCharArray,
      "#....#".toCharArray
    )

    /*
    val out = List(
      "#./..",
      "../..",
      ".#/..",
      "../..",
      "../..",
      "../..",
      "../#.",
      "../..",
      "../.#"
    )
    */

    val out = List(
      "#../.../...",
      "..#/.../...",
      ".../.../#..",
      ".../.../..#"
    )

    Day21.divide(in).map(Day21.Grid.toString(_)) should be (out)
  }

  it should "return the correct result(s) (for 9x9)" in {
    val in = Array(
      "#.......#".toCharArray,
      ".........".toCharArray,
      ".........".toCharArray,
      ".........".toCharArray,
      ".........".toCharArray,
      ".........".toCharArray,
      ".........".toCharArray,
      ".........".toCharArray,
      "#.......#".toCharArray
    )

    val out = List(
      "#../.../...",
      ".../.../...",
      "..#/.../...",
      ".../.../...",
      ".../.../...",
      ".../.../...",
      ".../.../#..",
      ".../.../...",
      ".../.../..#"
    )

    Day21.divide(in).map(Day21.Grid.toString(_)) should be (out)
  }

  "enhancing the grid" should "produce the correct result(s)" in {
    val grid = Day21.enhance(Day21.start, Day21.parseInput(testInput))
    Day21.Grid.toString(grid) shouldBe "#..#/..../..../#..#"
  }

  "joining the grid" should "create the correct result(s)" in {
    val rules = Day21.parseInput(testInput)
    val grid = Day21.enhance(Day21.start, rules)
    Day21.Grid.toString(grid) shouldBe "#..#/..../..../#..#"

    val squares = Day21.divide(grid).map(Day21.enhance(_, rules))
    val head = Day21.Grid.toString(squares.head)
    head shouldBe "##./#../..."
    squares.forall(Day21.Grid.toString(_) == head) shouldBe true
    Day21.Grid.toString(Day21.join(squares)) shouldBe "##.##./#..#../....../##.##./#..#../......"
  }

  "running the simulation" should "return the correct results" in {
    val grid2 = Day21.run(Day21.start, Day21.parseInput(testInput), 2)
    Day21.Grid.toString(grid2) shouldBe "##.##./#..#../....../##.##./#..#../......"
    Day21.Grid.toString(grid2).count(_ == '#') shouldBe 12
  }

  ignore should "solve the puzzle" in {
    val grid5 = Day21.run(Day21.start, Day21.parseInput(Day21.in), 5)
    //Day21.Grid.toString(grid5) shouldBe ""
    Day21.Grid.toString(grid5).count(_ == '#') shouldBe 0
  }
}
