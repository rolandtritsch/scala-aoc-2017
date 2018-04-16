package aoc

object Day23 {

  val in = Util.readInput("Day23input.txt")

  sealed abstract class Operation
  case class Set(register: Char, value: Long) extends Operation
  case class SetR(register: Char, value: Char) extends Operation
  case class Sub(register: Char, value: Long) extends Operation
  case class SubR(register: Char, value: Char) extends Operation
  case class Mul(register: Char, value: Long) extends Operation
  case class MulR(register: Char, value: Char) extends Operation
  case class JumpIfNotZero(register: Char, value: Int) extends Operation

  val registerRange = ('a' to 'h').toList

  def parseInput(in: List[String]): List[Operation] = {
    in.map(l => {
      val tokens = l.split(' ')
      assert(tokens.size >= 2)
      val operation = tokens(0)
      val register = tokens(1).charAt(0)
      operation match {
        case "set" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if(registerRange.contains(operand.charAt(0))) SetR(register, operand.charAt(0))
          else Set(register, operand.toLong)
        }
        case "sub" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if(registerRange.contains(operand.charAt(0))) SubR(register, operand.charAt(0))
          else Sub(register, operand.toLong)
        }
        case "mul" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if(registerRange.contains(operand.charAt(0))) MulR(register, operand.charAt(0))
          else Mul(register, operand.toLong)
        }
        case "jnz" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          JumpIfNotZero(register, operand.toInt)
        }
        case _ => {
          assert(false)
          Set(' ', 0)
        }
      }
    })
  }

  case class Program(counter: Int, instructions: List[Operation], register: Map[Char, Long], instructionCounter: Map[String, Long])

  def run(program: Program, done: Program => Boolean, exit: Program => Long): Long = {
    //println(s"${program.counter}/${program.instructions(program.counter)}/${program.register.toList}")
    //println(s"${program.counter}/${program.instructions(program.counter)}/${program.instructionCounter("mul")}")
    if(done(program)) exit(program)
    else {
      val next = program.instructions(program.counter) match {
        case Set(r, v) => {
          Program(
            program.counter + 1,
            program.instructions,
            program.register + (r -> v),
            program.instructionCounter + ("set" -> (program.instructionCounter("set") + 1))
          )
        }
        case SetR(r, v) => {
          Program(
            program.counter + 1,
            program.instructions,
            program.register + (r -> program.register(v)),
            program.instructionCounter + ("set" -> (program.instructionCounter("set") + 1))
          )
        }
        case Sub(r, v) => {
          Program(
            program.counter + 1,
            program.instructions,
            program.register + (r -> (program.register(r) - v)),
            program.instructionCounter + ("sub" -> (program.instructionCounter("sub") + 1))
          )
        }
        case SubR(r, v) => {
          Program(
            program.counter + 1,
            program.instructions,
            program.register + (r -> (program.register(r) - program.register(v))),
            program.instructionCounter + ("sub" -> (program.instructionCounter("sub") + 1))
          )
        }
        case Mul(r, v) => {
          Program(
            program.counter + 1,
            program.instructions,
            program.register + (r -> (program.register(r) * v)),
            program.instructionCounter + ("mul" -> (program.instructionCounter("mul") + 1))
          )
        }
        case MulR(r, v) => {
          Program(
            program.counter + 1,
            program.instructions,
            program.register + (r -> (program.register(r) * program.register(v))),
            program.instructionCounter + ("mul" -> (program.instructionCounter("mul") + 1))
          )
        }
        case JumpIfNotZero(r, v) => {
          if(program.register(r) != 0) {
            Program(
              program.counter + v,
              program.instructions,
              program.register,
              program.instructionCounter + ("jnz" -> (program.instructionCounter("jnz") + 1))
            )
          }
          else {
            Program(
              program.counter + 1,
              program.instructions,
              program.register,
              program.instructionCounter + ("jnz" -> (program.instructionCounter("jnz") + 1))
            )
          }
        }
      }
      run(next, done, exit)
    }
  }

  def fullRun(program: Program): Long = {
    def done(p: Program) = p.counter < 0 || p.counter >= p.instructions.size
    def exit(p: Program) = -1

    run(program, done, exit)
  }

  def solveRun(program: Program): Long = {
    def done(p: Program) = p.counter < 0 || p.counter >= p.instructions.size
    def exit(p: Program) = p.instructionCounter("mul")

    run(program, done, exit)
  }

  def solveRun2(program: Program): Long = {
    def done(p: Program) = p.counter < 0 || p.counter >= p.instructions.size
    def exit(p: Program) = p.register('h')

    run(program, done, exit)
  }
}