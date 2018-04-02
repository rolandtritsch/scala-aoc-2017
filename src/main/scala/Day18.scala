package aoc

// @note The instruction on line 35 is >jgz 1 3<. Not sure, if this is a typo. I changed it to >jgz l 3< and added >set l 1< as the first instruction.
// @todo This needs to be refactored
object Day18 {

  import java.util.concurrent.{LinkedBlockingDeque, TimeUnit}

  val input = Util.readInput("Day18input.txt")

  sealed abstract class Operation {
    def execute(program: Program): Program

  }

  case class Send(r: Char) extends Operation {
    def execute(program: Program): Program = {
      assert(program.writeChannel.offerFirst(program.register(r)))
      Program(
        program.id,
        program.counter + 1,
        program.instructions,
        program.register,
        program.readChannel,
        program.writeChannel,
        if(program.id == 1) program.writeCount + 1 else program.writeCount,
        program.checkRegisterOnReceive
      )
    }
  }

  case class Set(r: Char, v: Long) extends Operation {
    def execute(program: Program): Program = {
      Program(
        program.id,
        program.counter + 1,
        program.instructions,
        program.register + (r -> v),
        program.readChannel,
        program.writeChannel,
        program.writeCount,
        program.checkRegisterOnReceive
      )
    }
  }

  case class SetR(r: Char, v: Char) extends Operation {
    def execute(program: Program): Program = {
      Program(
        program.id,
        program.counter + 1,
        program.instructions,
        program.register + (r -> program.register(v)),
        program.readChannel,
        program.writeChannel,
        program.writeCount,
        program.checkRegisterOnReceive
      )
    }
  }

  case class Add(r: Char, v: Long) extends Operation {
    def execute(program: Program): Program = {
      Program(
        program.id,
        program.counter + 1,
        program.instructions,
        program.register + (r -> (program.register(r) + v)),
        program.readChannel,
        program.writeChannel,
        program.writeCount,
        program.checkRegisterOnReceive
      )
    }
  }

  case class AddR(r: Char, v: Char) extends Operation {
    def execute(program: Program): Program = {
      Program(
        program.id,
        program.counter + 1,
        program.instructions,
        program.register + (r -> (program.register(r) + program.register(v))),
        program.readChannel,
        program.writeChannel,
        program.writeCount,
        program.checkRegisterOnReceive
      )
    }
  }

  case class Multiply(r: Char, v: Long) extends Operation {
    def execute(program: Program): Program = {
      Program(
        program.id,
        program.counter + 1,
        program.instructions,
        program.register + (r -> (program.register(r) * v)),
        program.readChannel,
        program.writeChannel,
        program.writeCount,
        program.checkRegisterOnReceive
      )
    }
  }

  case class MultiplyR(r: Char, v: Char) extends Operation {
    def execute(program: Program): Program = {
      Program(
        program.id,
        program.counter + 1,
        program.instructions,
        program.register + (r -> (program.register(r) * program.register(v))),
        program.readChannel,
        program.writeChannel,
        program.writeCount,
        program.checkRegisterOnReceive
      )
    }
  }

  case class Modulo(r: Char, v: Long) extends Operation {
    def execute(program: Program): Program = {
      Program(
        program.id,
        program.counter + 1,
        program.instructions,
        program.register + (r -> (program.register(r) % v)),
        program.readChannel,
        program.writeChannel,
        program.writeCount,
        program.checkRegisterOnReceive
      )
    }
  }

  case class ModuloR(r: Char, v: Char) extends Operation {
    def execute(program: Program): Program = {
      Program(
        program.id,
        program.counter + 1,
        program.instructions,
        program.register + (r -> (program.register(r) % program.register(v))),
        program.readChannel,
        program.writeChannel,
        program.writeCount,
        program.checkRegisterOnReceive
      )
    }
  }

  case class Receive(r: Char) extends Operation {
    def execute(program: Program): Program = {
      if (program.checkRegisterOnReceive) {
        if (program.register(r) != 0) {
          val value = Option(program.readChannel.pollLast(5, TimeUnit.SECONDS)) match {
            case Some(v) => v
            case None => {
              // @todo Find a way to return the writeCount
              println(s"Deadlock detected (${program.id}/${program.writeCount})")
              // @todo Do not *just* stop the thread. Find a way to terminate gracefully
              Thread.currentThread.stop()
              0L
            }
          }

          Program(
            program.id,
            program.counter + 1,
            program.instructions,
            program.register + (r -> value),
            program.readChannel,
            program.writeChannel,
            program.writeCount,
            program.checkRegisterOnReceive
          )
        } else {
          Program(
            program.id,
            program.counter + 1,
            program.instructions,
            program.register,
            program.readChannel,
            program.writeChannel,
            program.writeCount,
            program.checkRegisterOnReceive
          )
        }
      } else {
        val value = Option(program.readChannel.pollLast(5, TimeUnit.SECONDS)) match {
          case Some(v) => v
          case None => {
            println(s"Deadlock detected (${program.id}/${program.writeCount})")
            Thread.currentThread.stop()
            0L
          }
        }

        Program(
          program.id,
          program.counter + 1,
          program.instructions,
          program.register + (r -> value),
          program.readChannel,
          program.writeChannel,
          program.writeCount,
          program.checkRegisterOnReceive
        )
      }
    }
  }

  case class JumpIfGreaterThanZero(r: Char, v: Int) extends Operation {
    def execute(program: Program): Program = {
      if (program.register(r) > 0) {
        Program(
          program.id,
          program.counter + v,
          program.instructions,
          program.register,
          program.readChannel,
          program.writeChannel,
          program.writeCount,
          program.checkRegisterOnReceive
        )
      }
      else {
        Program(
          program.id,
          program.counter + 1,
          program.instructions,
          program.register,
          program.readChannel,
          program.writeChannel,
          program.writeCount,
          program.checkRegisterOnReceive
        )
      }
    }
  }

  case class JumpIfGreaterThanZeroR(r: Char, v: Char) extends Operation {
    def execute(program: Program): Program = {
      if (program.register(r) > 0) {
        Program(
          program.id,
          program.counter + program.register(v).toInt,
          program.instructions,
          program.register,
          program.readChannel,
          program.writeChannel,
          program.writeCount,
          program.checkRegisterOnReceive
        )
      }
      else {
        Program(
          program.id,
          program.counter + 1,
          program.instructions,
          program.register,
          program.readChannel,
          program.writeChannel,
          program.writeCount,
          program.checkRegisterOnReceive
        )
      }
    }
  }

  def parseInput(in: List[String]): List[Operation] = {
    val registerRange = ('a'to('z')).toList

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
  }

  // @todo Refactor Program. This class is too big. It is passing around to much state. Consider to pass along some of the parameters as implicits.
  case class Program(id: Int, counter: Int, instructions: List[Operation], register: Map[Char, Long], readChannel: LinkedBlockingDeque[Long], writeChannel: LinkedBlockingDeque[Long], writeCount: Int, checkRegisterOnReceive: Boolean)

  // @todo This should be/could be a method on Program.
  def run(program: Program, done: Program => Boolean, exit: Program => Long): Long = {
    if (done(program)) exit(program)
    else run(program.instructions(program.counter).execute(program), done, exit)
  }

  def fullRun(program: Program): Long = {
    def done(p: Program) = p.counter < 0 || p.counter >= p.instructions.size

    def exit(p: Program) = -1L

    run(program, done, exit)
  }

  object Part1 {
    def solve(input: List[String]): Long = {
      def done(p: Program) = {
        if (p.counter < 0 || p.counter >= p.instructions.size) true
        else p.instructions(p.counter) match {
          case Receive(r) if (p.register(r) > 0) => true
          case _ => false
        }
      }

      def exit(p: Program) = {
        if (p.counter < 0 || p.counter >= p.instructions.size) -1L
        else p.readChannel.pollFirst(5, TimeUnit.SECONDS)
      }

      val instructions = parseInput(input)
      val registers = Map.empty[Char, Long].withDefaultValue(0L)
      val channel = new java.util.concurrent.LinkedBlockingDeque[Long]()
      val program = Program(0, 0, instructions, registers, channel, channel, 0, true)

      run(program, done, exit)
    }
  }

  object Part2 {
    def solve(input: List[String]): Unit = {
      val instructions = parseInput(input)
      val p0Registers = Map.empty[Char, Long].withDefaultValue(0L) + ('p' -> 0L)
      val p1Registers = Map.empty[Char, Long].withDefaultValue(0L) + ('p' -> 1L)
      val p0Channel = new LinkedBlockingDeque[Long]()
      val p1Channel = new LinkedBlockingDeque[Long]()
      val p0 = Program(0, 0, instructions, p0Registers, p1Channel, p0Channel, 0, false)
      val p1 = Program(1, 0, instructions, p1Registers, p0Channel, p1Channel, 0, false)

      // @todo Consider using Futures instead of Threads
      val threads = List(
        new Thread(new Runner(p0)),
        new Thread(new Runner(p1))
      )
      threads.foreach(t => t.start())
      threads.foreach(t => t.join())
    }

    class Runner(program: Program) extends Runnable {
      override def run(): Unit = {
        fullRun(program)
      }
    }
  }
}

