package aoc

object Day08 {
  val in = Util.readInput("Day8input.txt")

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
    instructions.map(i => (i.register, 0)).toMap
  }

  def evalCondition(i: Instruction, registers: Map[String, Int]): Boolean = {
    //println(s"${i.conditionRegister}(${registers(i.conditionRegister)})/${i.condition}/${i.conditionOperand}")
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
    if(evalCondition(i, registers)) {
      //println(s"${i.operation}/${i.operand}/${i.register}/${registers(i.register)}")
      i.operation match {
        case "inc" => registers + (i.register -> (registers(i.register) + i.operand))
        case "dec" => registers + (i.register -> (registers(i.register) - i.operand))
      }
    } else {
      registers
    }
  }

  def runProgram(instructions: List[Instruction], registers: Map[String, Int]): List[Map[String, Int]] = {
    instructions.foldLeft(List(registers))((acc, i) => {
      val newRegisters = executeInstruction(i, acc.head)
      newRegisters :: acc
    })
  }

  def maxRegister(registers: Map[String, Int]): Int = {
    registers.values.max
  }

  def maxStack(stack: List[Map[String, Int]]): Int = {
    stack.flatMap(_.values).max
  }

  def findMaxRegister(in: List[String]): Int = {
    val instructions = parseInput(in)
    val registers = buildRegisters(instructions)
    val stack = runProgram(instructions, registers)
    maxRegister(stack.head)
  }

  def findMaxStack(in: List[String]): Int = {
    val instructions = parseInput(in)
    val registers = buildRegisters(instructions)
    val stack = runProgram(instructions, registers)
    maxStack(stack)
  }
}
