package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day22Spec extends FlatSpec with Matchers {

  val testInput = Array(
    "..#".toCharArray,
    "#..".toCharArray,
    "...".toCharArray
  )

  behavior of "the input"
  it should "be correct" in {
    Day22.in.head shouldBe "...###.#.#.##...##.#..##."
  }

  behavior of "parsing the input"
  it should "produce the correct result(s)" in {
    Day22.parseInput(Day22.in).head.mkString shouldBe "...###.#.#.##...##.#..##."
  }

  behavior of "midpoint"
  it should "calculate the correct result(s)" in {
    Day22.SimpleGrid(5).midPoint shouldBe 2
  }

  behavior of "building a SimpleGrid"
  it should "produce the correct result(s)" in {
    Day22.SimpleGrid(5).mkString(1) shouldBe "...\n...\n..."
  }

  behavior of "map the SimpleGrid"
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

  it should "return the right number of infections" in {
    Day22.run(Day22.SimpleGrid(9).mapInput(testInput), 7).numOfInfections shouldBe 5
    Day22.run(Day22.SimpleGrid(11).mapInput(testInput), 70).numOfInfections shouldBe 41
    Day22.run(Day22.SimpleGrid(1001).mapInput(testInput), 10000).numOfInfections shouldBe 5587
  }

  it should "solve the puzzle" in {
    Day22.run(Day22.SimpleGrid(1001).mapInput(Day22.parseInput(Day22.in)), Day22.Default.ticks1).numOfInfections shouldBe 5305
  }

  behavior of "moving through the AdvancdedGrid"
  it should "return the right number of infections" in {
    Day22.run(Day22.AdvancedGrid(101).mapInput(testInput), 100).numOfInfections shouldBe 26
    Day22.run(Day22.AdvancedGrid(1001).mapInput(testInput), 10000000).numOfInfections shouldBe 2511944
  }

  it should "solve the puzzle" in {
    Day22.run(Day22.AdvancedGrid(1001).mapInput(Day22.parseInput(Day22.in)), Day22.Default.ticks2).numOfInfections shouldBe 2511424
  }
}