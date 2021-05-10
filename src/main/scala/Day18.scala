package aoc

/** Problem: [[https://adventofcode.com/2017/day/18]]
  *
  * Solution:
  *
  * General - First we need to implement all of the operations. We then
  * execute the progrom by executing all instructions (the input). Running
  * the program can be parameterized by `when is the program done?` and
  * `what do we do when we exit?`.
  *
  * Note: To solve the puzzle(s) we need two program instances to talk
  * to each other (send/receive register values (frequencies)). For that
  * to happen I am using a blocking queue between the instances.
  *
  * Note: The instruction on line 35 is >jgz 1 3<. Not sure, if this is
  * a typo. I changed it to >jgz l 3< and added >set l 1< as the first
  * instruction in the input file.
  *
  * Part1 - Simple. Run the program. When we exit we need to return the
  * value of the recovered frequency (the value of the most recently
  * played sound; basically most recent value in the queue). We are done,
  * the first time a receive instruction is executed with a non-zero value.
  *
  * Part2 - Also (kind of) simple (after some refactoring; using Future and
  * replacing LinkedBlockingDeque with Queue). Run until both sides are waiting
  * (i.e. are deadlocked) and (at that point in time) return/exit with the
  * writeCount.
  *
  * Note: This solution will not compile with scala native (because there
  * is no implementation of LinkedBlockingDequeue in scala native).
  */
object Day18 {

  val input = Util.readInput("Day18input.txt")

  def parseInput(input: List[String]): List[Operation] = {
    require(input.nonEmpty, s"input.nonEmpty failed")

    val registerRange = ('a'to('z')).toList

    input.map(l => {
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
          if (registerRange.contains(operand.charAt(0))) SetR(register, operand.charAt(0))
          else Set(register, operand.toLong)
        }
        case "add" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if (registerRange.contains(operand.charAt(0))) AddR(register, operand.charAt(0))
          else Add(register, operand.toLong)
        }
        case "mul" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if (registerRange.contains(operand.charAt(0))) MultiplyR(register, operand.charAt(0))
          else Multiply(register, operand.toLong)
        }
        case "mod" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if (registerRange.contains(operand.charAt(0))) ModuloR(register, operand.charAt(0))
          else Modulo(register, operand.toLong)
        }
        case "rcv" => Receive(register)
        case "jgz" => {
          assert(tokens.size == 3)
          val operand = tokens(2)
          if (registerRange.contains(operand.charAt(0))) JumpIfGreaterThanZeroR(register, operand.charAt(0))
          else JumpIfGreaterThanZero(register, operand.toInt)
        }
        case _ => assert(false); Send('a')
      }
    })
  } ensuring(_.nonEmpty, s"_.nonEmpty failed")

  import java.util.concurrent.{LinkedBlockingDeque, TimeUnit}
  case class Program(
    id: Int,
    counter: Int,
    instructions: List[Operation],
    register: Map[Char, Long],
    readChannel: LinkedBlockingDeque[Long],
    writeChannel: LinkedBlockingDeque[Long],
    writeCount: Int,
    checkRegisterOnReceive: Boolean,
    deadlocked: Boolean
  )

  sealed abstract class Operation {
    def execute(program: Program): Program
  }

  case class Send(r: Char) extends Operation {
    def execute(program: Program): Program = {
      val success = program.writeChannel.offerFirst(program.register(r))
      assert(success)
      program.copy(
        counter = program.counter + 1,
        writeCount = if(program.id == 1) program.writeCount + 1 else program.writeCount
      )
    }
  }

  case class Set(r: Char, v: Long) extends Operation {
    def execute(program: Program): Program = {
      program.copy(
        counter = program.counter + 1,
        register = program.register + (r -> v)
      )
    }
  }

  case class SetR(r: Char, v: Char) extends Operation {
    def execute(program: Program): Program = {
      program.copy(
        counter = program.counter + 1,
        register = program.register + (r -> program.register(v))
      )
    }
  }

  case class Add(r: Char, v: Long) extends Operation {
    def execute(program: Program): Program = {
      program.copy(
        counter = program.counter + 1,
        register = program.register + (r -> (program.register(r) + v))
      )
    }
  }

  case class AddR(r: Char, v: Char) extends Operation {
    def execute(program: Program): Program = {
      program.copy(
        counter = program.counter + 1,
        register = program.register + (r -> (program.register(r) + program.register(v)))
      )
    }
  }

  case class Multiply(r: Char, v: Long) extends Operation {
    def execute(program: Program): Program = {
      program.copy(
        counter = program.counter + 1,
        register = program.register + (r -> (program.register(r) * v))
      )
    }
  }

  case class MultiplyR(r: Char, v: Char) extends Operation {
    def execute(program: Program): Program = {
      program.copy(
        counter = program.counter + 1,
        register = program.register + (r -> (program.register(r) * program.register(v)))
      )
    }
  }

  case class Modulo(r: Char, v: Long) extends Operation {
    def execute(program: Program): Program = {
      program.copy(
        counter = program.counter + 1,
        register = program.register + (r -> (program.register(r) % v))
      )
    }
  }

  case class ModuloR(r: Char, v: Char) extends Operation {
    def execute(program: Program): Program = {
      program.copy(
        counter = program.counter + 1,
        register = program.register + (r -> (program.register(r) % program.register(v)))
      )
    }
  }

  case class Receive(r: Char) extends Operation {
    def execute(program: Program): Program = {
      if (program.checkRegisterOnReceive) {
        if (program.register(r) != 0) {
          Option(program.readChannel.pollLast(1, TimeUnit.SECONDS)) match {
            case Some(v) => {
              program.copy(
                counter = program.counter + 1,
                register = program.register + (r -> v)
              )
            }

            case None => {
              program.copy(
                counter = program.counter + 1,
                deadlocked = true
              )
            }
          }
        } else {
          program.copy(
            counter = program.counter + 1
          )
        }
      } else {
        Option(program.readChannel.pollLast(1, TimeUnit.SECONDS)) match {
          case Some(v) => {
            program.copy(
              counter = program.counter + 1,
              register = program.register + (r -> v)
            )

          }

          case None => {
            program.copy(
              counter = program.counter + 1,
              deadlocked = true
            )
          }
        }
      }
    }
  }

  case class JumpIfGreaterThanZero(r: Char, v: Int) extends Operation {
    def execute(program: Program): Program = {
      if (program.register(r) > 0) {
        program.copy(
          counter = program.counter + v
        )
      }
      else {
        program.copy(
          counter = program.counter + 1
        )
      }
    }
  }

  case class JumpIfGreaterThanZeroR(r: Char, v: Char) extends Operation {
    def execute(program: Program): Program = {
      if (program.register(r) > 0) {
        program.copy(
          counter = program.counter + program.register(v).toInt
        )
      }
      else {
        program.copy(
          counter = program.counter + 1
        )
      }
    }
  }

  def run(program: Program, done: Program => Boolean, exit: Program => Long): Long = {
    if (done(program)) exit(program)
    else run(program.instructions(program.counter).execute(program), done, exit)
  }

  object Part1 {
    def solve(input: List[String]): (Long, Long) = Util.measuredTimeMillis {
      def done(p: Program): Boolean = {
        if (p.counter < 0 || p.counter >= p.instructions.size) true
        else p.instructions(p.counter) match {
          case Receive(r) if (p.register(r) > 0) => true
          case _ => false
        }
      }

      def exit(p: Program): Long = {
        if (p.counter < 0 || p.counter >= p.instructions.size) -1L
        else p.readChannel.pollFirst(5, TimeUnit.SECONDS)
      }

      val instructions = parseInput(input)
      val registers = Map.empty[Char, Long].withDefaultValue(0L)
      val channel = new LinkedBlockingDeque[Long]()
      val program = Program(0, 0, instructions, registers, channel, channel, 0, true, false)

      run(program, done, exit)
    }
  }

  object Part2 {
    def solve(input: List[String]): (Long, Long) = Util.measuredTimeMillis {
      def done(p: Program): Boolean = {
        p.counter < 0 || p.counter >= p.instructions.size || p.deadlocked
      }

      def exit(p: Program): Long = {
        if(p.deadlocked) p.writeCount else -1L
      }

      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.{Future, Await}
      import scala.concurrent.duration._
      import scala.language.postfixOps

      val instructions = parseInput(input)
      val p0Registers = Map.empty[Char, Long].withDefaultValue(0L) + ('p' -> 0L)
      val p1Registers = Map.empty[Char, Long].withDefaultValue(0L) + ('p' -> 1L)
      val p0Channel = new LinkedBlockingDeque[Long]()
      val p1Channel = new LinkedBlockingDeque[Long]()
      val p0 = Program(0, 0, instructions, p0Registers, p1Channel, p0Channel, 0, false, false)
      val p1 = Program(1, 0, instructions, p1Registers, p0Channel, p1Channel, 0, false, false)

      val thread0 = Future { run(p0, done, exit) }
      val thread1 = Future { run(p1, done, exit) }
      val result0 = Await.result(thread0, 1 minute)
      val result1 = Await.result(thread1, 1 minute)
      result0 + result1
    }
  }
}
