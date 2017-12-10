package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day8Spec extends FlatSpec with Matchers {
  val testInput = List(
    "b inc 5 if a > 1",
    "a inc 1 if b < 5",
    "c dec -10 if a >= 1",
    "c inc -20 if c == 10"
  )

  val testInstructions = List(
    Day8.Instruction("b", "inc", 5, "a", ">", 1),
    Day8.Instruction("a", "inc", 1, "b", "<", 5),
    Day8.Instruction("c", "dec", -10, "a", ">=", 1),
    Day8.Instruction("c", "inc", -20, "c", "==", 10)
  )

  "parseInput" should "return the list of instructions" in {
    Day8.parseInput(testInput) should be (testInstructions)
  }

  "runProgram" should "return the stack of registers" in {
    val instructions = Day8.parseInput(testInput)
    val registers = Day8.buildRegisters(instructions)
    val stack = Day8.runProgram(instructions, registers)
    stack.head.toList.sorted should be (List(("a", 1), ("b", 0), ("c", -10)))
  }

  "maxRegister" should "return the max of all registers" in {
    val instructions = Day8.parseInput(testInput)
    val registers = Day8.buildRegisters(instructions)
    val stack = Day8.runProgram(instructions, registers)
    Day8.maxRegister(stack.head) shouldBe 1
  }

  it should "solve the puzzle" in {
    val instructions = Day8.parseInput(Day8.in)
    val registers = Day8.buildRegisters(instructions)
    val stack = Day8.runProgram(instructions, registers)
    Day8.maxRegister(stack.head) shouldBe 4163
  }
}
