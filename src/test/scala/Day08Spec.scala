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

  "parseInput" should "return the list of instructions" in {
    Day08.parseInput(testInput) should be (testInstructions)
  }

  "runProgram" should "return the stack of registers" in {
    val instructions = Day08.parseInput(testInput)
    val registers = Day08.buildRegisters(instructions)
    val stack = Day08.runProgram(instructions, registers)
    stack.head.toList.sorted should be (List(("a", 1), ("b", 0), ("c", -10)))
  }

  "maxRegister" should "return the max of all registers" in {
    val instructions = Day08.parseInput(testInput)
    val registers = Day08.buildRegisters(instructions)
    val stack = Day08.runProgram(instructions, registers)
    Day08.maxRegister(stack.head) shouldBe 1
  }

  it should "solve the puzzle" in {
    val instructions = Day08.parseInput(Day08.in)
    val registers = Day08.buildRegisters(instructions)
    val stack = Day08.runProgram(instructions, registers)
    Day08.maxRegister(stack.head) shouldBe 4163
  }

  "maxStack" should "return the max of all register values in the stack" in {
    val instructions = Day08.parseInput(testInput)
    val registers = Day08.buildRegisters(instructions)
    val stack = Day08.runProgram(instructions, registers)
    Day08.maxStack(stack) shouldBe 10
  }

  it should "should solve the puzzle" in {
    val instructions = Day08.parseInput(Day08.in)
    val registers = Day08.buildRegisters(instructions)
    val stack = Day08.runProgram(instructions, registers)
    Day08.maxStack(stack) shouldBe 5347
  }
}
