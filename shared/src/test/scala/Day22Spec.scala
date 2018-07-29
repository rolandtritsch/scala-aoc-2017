package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day22Spec extends FlatSpec with Matchers {

  val testInput = Array(
    "..#".toCharArray,
    "#..".toCharArray,
    "...".toCharArray
  )

  behavior of "readInput()"
  it should "be correct" in {
    Day22.input.head shouldBe "...###.#.#.##...##.#..##."
  }

  behavior of "parseInput()"
  it should "produce the correct result(s)" in {
    Day22.parseInput(Day22.input).head.mkString shouldBe "...###.#.#.##...##.#..##."
  }

  behavior of "SimpleGrid.midpoint()"
  it should "calculate the correct result(s)" in {
    Day22.SimpleGrid(5).midPoint shouldBe 2
  }

  behavior of "building a SimpleGrid"
  it should "produce the correct result(s)" in {
    Day22.SimpleGrid(5).mkString(1) shouldBe "...\n...\n..."
  }

  behavior of "mapping the SimpleGrid"
  it should "produce the correct result(s)" in {
    Day22.SimpleGrid(5).mapInput(testInput).mkString(2) shouldBe ".....\n...#.\n.#...\n.....\n....."
  }

  behavior of "moving through the SimpleGrid"
  it should "produce the correct result(s)" in {
    val tick0Output = List(
      ".........",
      ".........",
      ".........",
      ".....#...",
      "...#.....",
      ".........",
      ".........",
      ".........",
      "........."
    )

    val tick1Output = List(
      ".........",
      ".........",
      ".........",
      ".....#...",
      "...##....",
      ".........",
      ".........",
      ".........",
      "........."
    )

    val tick2Output = List(
      ".........",
      ".........",
      ".........",
      ".....#...",
      "....#....",
      ".........",
      ".........",
      ".........",
      "........."
    )

    val tick6Output = List(
      ".........",
      ".........",
      ".........",
      "..##.#...",
      "..###....",
      ".........",
      ".........",
      ".........",
      "........."
    )

    val tick7Output = List(
      ".........",
      ".........",
      ".........",
      "..#..#...",
      "..###....",
      ".........",
      ".........",
      ".........",
      "........."
    )

    val grid = Day22.SimpleGrid(9).mapInput(testInput)
    grid.mkString(4) shouldBe tick0Output.mkString("\n")

    grid.tick
    grid.mkString(4) shouldBe tick1Output.mkString("\n")

    grid.tick
    grid.mkString(4) shouldBe tick2Output.mkString("\n")

    grid.tick.tick.tick.tick
    grid.mkString(4) shouldBe tick6Output.mkString("\n")

    grid.tick
    grid.mkString(4) shouldBe tick7Output.mkString("\n")
    grid.numOfTicks shouldBe 7
    grid.numOfInfections shouldBe 5
  }

  it should "return the right number of infections for all testcases" taggedAs(BuildTest) in {
    Day22.run(Day22.SimpleGrid(9).mapInput(testInput), 7).numOfInfections shouldBe 5
    Day22.run(Day22.SimpleGrid(11).mapInput(testInput), 70).numOfInfections shouldBe 41
    Day22.run(Day22.SimpleGrid(1001).mapInput(testInput), 10000).numOfInfections shouldBe 5587
  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day22.Part1.solve(Day22.input)._1 shouldBe 5305
  }

  behavior of "moving through the AdvancdedGrid"
  it should "return the right number of infections for all testcases" in {
    Day22.run(Day22.AdvancedGrid(101).mapInput(testInput), 100).numOfInfections shouldBe 26
    Day22.run(Day22.AdvancedGrid(1001).mapInput(testInput), 10000000).numOfInfections shouldBe 2511944
  }

  behavior of "solve() - Part2"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day22.Part2.solve(Day22.input)._1 shouldBe 2511424
  }
}
