package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day08Spec extends FlatSpec with Matchers {

  val testInput = List(
    "b inc 5 if a > 1",
    "a inc 1 if b < 5",
    "c dec -10 if a >= 1",
    "c inc -20 if c == 10"
  )

  val testInstructions = List(
    Day08.Instruction("b", "inc", 5, "a", ">", 1),
    Day08.Instruction("a", "inc", 1, "b", "<", 5),
    Day08.Instruction("c", "dec", -10, "a", ">=", 1),
    Day08.Instruction("c", "inc", -20, "c", "==", 10)
  )

  behavior of "readInput()"
  it should "read the input" in {
    Day08.input.head shouldBe "g dec 231 if bfx > -10"
  }

  behavior of "parseInput()"
  it should "return the list of instructions" in {
    Day08.parseInput(testInput) should be (testInstructions)
  }

  behavior of "runProgram()"
  it should "return the stack of registers" in {
    val instructions = Day08.parseInput(testInput)
    val registers = Day08.buildRegisters(instructions)
    val stack = Day08.runProgram(instructions, registers)
    stack.head.toList.sorted should be (List(("a", 1), ("b", 0), ("c", -10)))
  }

  behavior of "solve() - Part1"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day08.Part1.solve(testInput)._1 shouldBe 1
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day08.Part1.solve(Day08.input)._1 shouldBe 4163
  }

  behavior of "solve() - Part2"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day08.Part2.solve(testInput)._1 shouldBe 10
  }

  it should "should solve the puzzle" taggedAs(SolutionTest) in {
    Day08.Part2.solve(Day08.input)._1 shouldBe 5347
  }
}
