package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day21Spec extends FlatSpec with Matchers {

  val testInput = List(
    "../.# => ##./#../...",
    ".#./..#/### => #..#/..../..../#..#"
  )

  behavior of "readInput()"
  it should "be correct" in {
    Day21.input.head shouldBe "../.. => ###/.##/#.."
  }

  behavior of "parseInput()"
  it should "produce the correct results" in {
    Day21.parseInput(testInput).size shouldBe 16
    Day21.parseInput(Day21.input).size shouldBe 864
  }

  behavior of "flipHorizontal()"
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

  behavior of "rotateClockwise()"
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

  behavior of "divide()"
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

    val grids = Day21.divide(in)
    grids should have size 4
    grids.forall(_.flatten.size == 4) shouldBe true
    grids.map(Day21.Grid.toString) should be (out)
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

    val grids = Day21.divide(in)
    grids should have size 9
    grids.forall(_.flatten.size == 4) shouldBe true
    grids.map(Day21.Grid.toString) should be (out)
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

    val grids = Day21.divide(in)
    grids should have size 9
    grids.forall(_.flatten.size == 9) shouldBe true
    grids.map(Day21.Grid.toString) should be (out)
  }

  behavior of "enhance()"
  it should "produce the correct result(s)" in {
    val grids = Day21.enhance(List(Day21.start), Day21.parseInput(testInput))
    grids should have size 1
    Day21.Grid.toString(grids.head) shouldBe "#..#/..../..../#..#"
  }

  it should "produce more correct result(s)" in {
    val rules = Day21.parseInput(testInput)
    val grids = Day21.enhance(List(Day21.start), rules)
    grids should have size 1
    Day21.Grid.toString(grids.head) shouldBe "#..#/..../..../#..#"

    val nextGrids = Day21.divide(Day21.join(grids))
    nextGrids should have size 4
    val squares = Day21.enhance(nextGrids, rules)
    squares should have size 4
    val head = Day21.Grid.toString(squares.head)
    head shouldBe "##./#../..."
  }

  behavior of "join()"
  it should "join four 2x2 grids" in {
    val in = List(
      Array(
        "#.".toCharArray(),
        "..".toCharArray()
      ),
      Array(
        ".#".toCharArray(),
        "..".toCharArray()
      ),
      Array(
        "..".toCharArray(),
        "#.".toCharArray()
      ),
      Array(
        "..".toCharArray(),
        ".#".toCharArray()
      )
    )

    val out = Array(
      "#..#".toCharArray,
      "....".toCharArray,
      "....".toCharArray,
      "#..#".toCharArray
    )

    Day21.join(in) should be (out)
  }

  it should "join nine 2x2 grids" in {
    val in = List(
      Array(
        "#.".toCharArray(),
        "..".toCharArray()
      ),
      Array(
        "..".toCharArray(),
        "..".toCharArray()
      ),
      Array(
        ".#".toCharArray(),
        "..".toCharArray()
      ),
      Array(
        "..".toCharArray(),
        "..".toCharArray()
      ),
      Array(
        "..".toCharArray(),
        "..".toCharArray()
      ),
      Array(
        "..".toCharArray(),
        "..".toCharArray()
      ),
      Array(
        "..".toCharArray(),
        "#.".toCharArray()
      ),
      Array(
        "..".toCharArray(),
        "..".toCharArray()
      ),
      Array(
        "..".toCharArray(),
        ".#".toCharArray()
      )
    )

    val out = Array(
      "#....#".toCharArray,
      "......".toCharArray,
      "......".toCharArray,
      "......".toCharArray,
      "......".toCharArray,
      "#....#".toCharArray
    )

    Day21.join(in) should be (out)
  }

  behavior of "run()"
  it should "return the correct results for the testcase(s)" taggedAs(BuildTest) in {
    val grid2 = Day21.run(Day21.start, Day21.parseInput(testInput), 2)
    grid2.flatten.count(_ == '#') shouldBe 12
  }

  it should "solve the puzzle" in {
    val grid5 = Day21.run(Day21.start, Day21.parseInput(Day21.input), 5)
    grid5.flatten.count(_ == '#') shouldBe 205
  }

  it should "show all the right numbers on the way to the solution" taggedAs(SlowTest) in {
    //val counts = List(5, 8, 25, 58, 78, 205, 516, 730, 1874, 4645, 6538, 16853, 41876, 58832, 151557, 376588, 529586, 1364380)
    val counts = List(5, 8, 25, 58, 78, 205, 516, 730, 1874, 4645, 6538)
    (0 to counts.size - 1).map (n => {
      val result = Day21.run(Day21.start, Day21.parseInput(Day21.input), n)
      result.flatten.count(_ == '#')
    }).toList should be (counts)
  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day21.Part1.solve(Day21.input)._1 shouldBe 205
  }

  behavior of "solve() - Part2"
  it should "solve the puzzle" taggedAs(SolutionTest, SlowTest) in {
    Day21.Part2.solve(Day21.input)._1 shouldBe 3389823
  }
}
