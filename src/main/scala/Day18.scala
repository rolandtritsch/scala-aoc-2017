package aoc

// @note The instruction on line 35 is >jgz 1 3<. Not sure, if this is a typo. I changed it to >jgz l 3< and added >set l 1< as the first instruction.
object Day18 {
  import java.util.concurrent.{LinkedBlockingDeque, TimeUnit}

  val in = Util.readInput("Day18input.txt")

  sealed abstract class Operation
  case class Send(register: Char) extends Operation
  case class Set(register: Char, value: Long) extends Operation
  case class SetR(register: Char, value: Char) extends Operation
  case class Add(register: Char, value: Long) extends Operation
  case class AddR(register: Char, value: Char) extends Operation
  case class Multiply(register: Char, value: Long) extends Operation
  case class MultiplyR(register: Char, value: Char) extends Operation
  case class Modulo(register: Char, value: Long) extends Operation
  case class ModuloR(register: Char, value: Char) extends Operation
  case class Receive(register: Char) extends Operation
  case class JumpIfGreaterThanZero(register: Char, value: Int) extends Operation
  case class JumpIfGreaterThanZeroR(register: Char, value: Char) extends Operation

  val registerRange = ('a' to 'z').toList

  def parseInput(in: List[String]): List[Operation] = {
    in.map(l => {
      val tokens = l.split(' ')
      assert(tokens.size >= 2)
      val operation = tokens(0)
      val register = tokens(1).charAt(0)
      assert(registerRange.contains(register))
      operation match {
        case "snd" => Send(register)
        case "set" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if(registerRange.contains(operand.charAt(0))) SetR(register, operand.charAt(0))
          else Set(register, operand.toLong)
        }
        case "add" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if(registerRange.contains(operand.charAt(0))) AddR(register, operand.charAt(0))
          else Add(register, operand.toLong)
        }
        case "mul" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if(registerRange.contains(operand.charAt(0))) MultiplyR(register, operand.charAt(0))
          else Multiply(register, operand.toLong)
        }
        case "mod" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if(registerRange.contains(operand.charAt(0))) ModuloR(register, operand.charAt(0))
          else Modulo(register, operand.toLong)
        }
        case "rcv" => Receive(register)
        case "jgz" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if(registerRange.contains(operand.charAt(0))) JumpIfGreaterThanZeroR(register, operand.charAt(0))
          else JumpIfGreaterThanZero(register, operand.toInt)
        }
        case _ => assert(false); Send('a')
      }
    })
  }

  case class Program(counter: Int, instructions: List[Operation], register: Map[Char, Long], readChannel: LinkedBlockingDeque[Long], writeChannel: LinkedBlockingDeque[Long])

  def run(program: Program, done: Program => Boolean, exit: Program => Long): Long = {
    if(done(program)) exit(program)
    else {
      val next = program.instructions(program.counter) match {
        case Send(r) => {
          assert(program.writeChannel.offerFirst(program.register(r)))
          Program(program.counter + 1, program.instructions, program.register, program.readChannel, program.writeChannel)
        }
        case Set(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> v), program.readChannel, program.writeChannel)
        }
        case SetR(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> program.register(v)), program.readChannel, program.writeChannel)
        }
        case Add(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> (program.register(r) + v)), program.readChannel, program.writeChannel)
        }
        case AddR(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> (program.register(r) + program.register(v))), program.readChannel, program.writeChannel)
        }
        case Multiply(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> (program.register(r) * v)), program.readChannel, program.writeChannel)
        }
        case MultiplyR(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> (program.register(r) * program.register(v))), program.readChannel, program.writeChannel)
        }
        case Modulo(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> (program.register(r) % v)), program.readChannel, program.writeChannel)
        }
        case ModuloR(r, v) => {
          Program(program.counter + 1, program.instructions, program.register + (r -> (program.register(r) % program.register(v))), program.readChannel, program.writeChannel)
        }
        case Receive(r) => {
          if(program.register(r) != 0) {
            val value = program.readChannel.pollLast(5, TimeUnit.SECONDS)
            Program(program.counter + 1, program.instructions, program.register + (r -> value), program.readChannel, program.writeChannel)
          } else Program(program.counter + 1, program.instructions, program.register, program.readChannel, program.writeChannel)
        }
        case JumpIfGreaterThanZero(r, v) => {
          if(program.register(r) > 0) Program(program.counter + v, program.instructions, program.register, program.readChannel, program.writeChannel)
          else Program(program.counter + 1, program.instructions, program.register, program.readChannel, program.writeChannel)
        }
        case JumpIfGreaterThanZeroR(r, v) => {
          if(program.register(r) > 0) Program(program.counter + program.register(v).toInt, program.instructions, program.register, program.readChannel, program.writeChannel)
          else Program(program.counter + 1, program.instructions, program.register, program.readChannel, program.writeChannel)
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
    def done(p: Program) = {
      if(p.counter < 0 || p.counter >= p.instructions.size) true
      else p.instructions(p.counter) match {
        case Receive(r) if(p.register(r) > 0) => true
        case _ => false
      }
    }

    def exit(p: Program) = {
      if(p.counter < 0 || p.counter >= p.instructions.size) -1L
      else p.readChannel.pollFirst(5, TimeUnit.SECONDS)
    }

    run(program, done, exit)
  }
}