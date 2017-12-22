package aoc

object Day18 {
  val in = Util.readInput("Day18input.txt")

  sealed abstract class Operation
  case class Sound(register: Char) extends Operation
  case class Set(register: Char, value: Int) extends Operation
  case class SetR(register: Char, value: Char) extends Operation
  case class Add(register: Char, value: Int) extends Operation
  case class AddR(register: Char, value: Char) extends Operation
  case class Multiply(register: Char, value: Int) extends Operation
  case class MultiplyR(register: Char, value: Char) extends Operation
  case class Modulo(register: Char, value: Int) extends Operation
  case class ModuloR(register: Char, value: Char) extends Operation
  case class Recover(register: Char) extends Operation
  case class JumpIfGreaterThanZero(register: Char, value: Int) extends Operation
  case class JumpIfGreaterThanZeroR(register: Char, value: Char) extends Operation

  val registerRange = ('a' to 'z').toList

  def parseInput(in: List[String]): List[Operation] = {
    in.map(l => {
      val tokens = l.split(' ')
      assert(tokens.size >= 2)
      val operation = tokens(0)
      val register = tokens(1).charAt(0)
      operation match {
        case "snd" => Sound(register)
        case "set" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if(registerRange.contains(operand.charAt(0))) SetR(register, operand.charAt(0))
          else Set(register, operand.toInt)
        }
        case "add" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if(registerRange.contains(operand.charAt(0))) AddR(register, operand.charAt(0))
          else Add(register, operand.toInt)
        }
        case "mul" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if(registerRange.contains(operand.charAt(0))) MultiplyR(register, operand.charAt(0))
          else Multiply(register, operand.toInt)
        }
        case "mod" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if(registerRange.contains(operand.charAt(0))) ModuloR(register, operand.charAt(0))
          else Modulo(register, operand.toInt)
        }
        case "rcv" => Recover(register)
        case "jgz" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if(registerRange.contains(operand.charAt(0))) JumpIfGreaterThanZeroR(register, operand.charAt(0))
          else JumpIfGreaterThanZero(register, operand.toInt)
        }
        case _ => assert(false); Sound('a')
      }
    })
  }

  case class Program(counter: Int, instructions: List[Operation], register: Map[Char, Int])

  val recoverRegister = '!'

  def run(program: Program, done: Program => Boolean, exit: Program => Int): Int = {
    if(done(program)) exit(program)
    else {
      val next = program.instructions(program.counter) match {
        case Sound(r) => {
          Program(program.counter + 1, program.instructions, program.register + (recoverRegister -> program.register(r)))
        }
        case Set(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> v))
        }
        case SetR(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> program.register(v)))
        }
        case Add(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> (program.register(r) + v)))
        }
        case AddR(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> (program.register(r) + program.register(v))))
        }
        case Multiply(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> (program.register(r) * v)))
        }
        case MultiplyR(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> (program.register(r) * program.register(v))))
        }
        case Modulo(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> (program.register(r) % v)))
        }
        case ModuloR(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> (program.register(r) % program.register(v))))
        }
        case Recover(r) => {
          if(program.register(r) != 0) Program(program.counter + 1, program.instructions, program.register + (r -> program.register(recoverRegister)))
          else Program(program.counter + 1, program.instructions, program.register)
        }
        case JumpIfGreaterThanZero(r, v) => {
          if(program.register(r) > 0) Program(program.counter + v, program.instructions, program.register)
          else Program(program.counter + 1, program.instructions, program.register)
        }
        case JumpIfGreaterThanZeroR(r, v) => {
          if(program.register(r) > 0) Program(program.counter + program.register(v), program.instructions, program.register)
          else Program(program.counter + 1, program.instructions, program.register)
        }
      }
      run(next, done, exit)
    }
  }

  def fullRun(program: Program): Int = {
    def done(p: Program) = p.counter < 0 || p.counter >= p.instructions.size
    def exit(p: Program) = -1

    run(program, done, exit)
  }

  def solveRun(program: Program): Int = {
    def done(p: Program) = {
      if(p.counter < 0 || p.counter >= p.instructions.size) true
      else p.instructions(p.counter) match {
        case Recover(r) if(p.register(r) > 0) => true
        case _ => false
      }
    }

    def exit(p: Program) = {
      if(p.counter < 0 || p.counter >= p.instructions.size) -1
      else p.register(recoverRegister)
    }

    run(program, done, exit)
  }
}