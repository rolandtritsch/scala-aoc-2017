package aoc

/** Problem: [[https://adventofcode.com/2017/day/8]]
  *
  * Solution:
  *
  * General - Very simple idea. We have a set of (immutable) instructions (no jumps)
  * and a set of registers. We just need to go through all instructions and update
  * the set of registers on the way (with every instruction). While we do this, we
  * collect all register states. The final register state is the head of the collected
  * register states.
  *
  * Part1 - Simple. The max of the register values in the final/last register state/set.
  *
  * Part2 - Simple. The max of all register values in all collected register states/sets.
  */
object Day08 {

  val input = Util.readInput("Day08input.txt")

  case class Instruction(register: String, operation: String, operand: Int, conditionRegister: String, condition: String, conditionOperand: Int)

  def parseInput(lines: List[String]): List[Instruction] = {
    def parseLine(line: String): Instruction = {
      // b inc 5 if a > 1
      val tokens = line.split("[ ]")
      val register = tokens(0)
      val operation = tokens(1)
      val operand = tokens(2).toInt
      val conditionRegister = tokens(4)
      val condition = tokens(5)
      val conditionOperand = tokens(6).toInt

      Instruction(
        register,
        operation,
        operand,
        conditionRegister,
        condition,
        conditionOperand
      )
    }

    lines.map(parseLine)
  }

  def buildRegisters(instructions: List[Instruction]): Map[String, Int] = {
    require(instructions.nonEmpty, s"instructions.nonEmpty failed")
    instructions.map(i => (i.register, 0)).toMap
  } ensuring(_.nonEmpty, s"_.nonEmpty failed")

  def evalCondition(i: Instruction, registers: Map[String, Int]): Boolean = {
    require(List("==", "!=", "<", ">", "<=", ">=").contains(i.condition), s"List(conditions).contains(i.condition) failed; with >${i.condition}<")

    i.condition match {
      case "==" if(registers(i.conditionRegister) == i.conditionOperand) => true
      case "!=" if(registers(i.conditionRegister) != i.conditionOperand) => true
      case "<" if(registers(i.conditionRegister) < i.conditionOperand) => true
      case ">" if(registers(i.conditionRegister) > i.conditionOperand) => true
      case "<=" if(registers(i.conditionRegister) <= i.conditionOperand) => true
      case ">=" if(registers(i.conditionRegister) >= i.conditionOperand) => true
      case _ => false
    }
  }

  def executeInstruction(i: Instruction, registers: Map[String, Int]): Map[String, Int] = {
    require(List("inc", "dec").contains(i.operation), s"List(operations).contains(i.operation) failed; with >${i.operation}<")

    if(evalCondition(i, registers)) {
      i.operation match {
        case "inc" => registers + (i.register -> (registers(i.register) + i.operand))
        case "dec" => registers + (i.register -> (registers(i.register) - i.operand))
        case _ => assert(false); Map.empty[String, Int]
      }
    } else {
      registers
    }
  }

  def runProgram(instructions: List[Instruction], registers: Map[String, Int]): List[Map[String, Int]] = {
    require(instructions.nonEmpty, s"instructions.nonEmpty")
    require(registers.nonEmpty, s"registers.nonEmpty")

    instructions.foldLeft(List(registers))((currentRegisters, i) => {
      val newRegisters = executeInstruction(i, currentRegisters.head)
      newRegisters :: currentRegisters
    })
  }

  object Part1 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      val instructions = parseInput(input)
      val registers = buildRegisters(instructions)
      val allRegisterStates = runProgram(instructions, registers)

      allRegisterStates.head.values.max
    }
  }

  object Part2 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      val instructions = parseInput(input)
      val registers = buildRegisters(instructions)
      val allRegisterStates = runProgram(instructions, registers)

      allRegisterStates.flatMap(_.values).max
    }
  }
}
