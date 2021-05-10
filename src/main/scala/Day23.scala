package aoc

/** Problem: [[http://adventofcode.com/2017/day/23]]
  *
  * Solution:
  *
  * General - This was a difficult one (at least for me). Part1 is
  * straightforward enough, but I really struggled to get Part2 done.
  * For Part1 I implemented a *normal* [[Operation]]-interpreter to
  * run the [[Program]]. But this prooved to be way too slow to solve
  * Part2. From the discussion group I then realized that I have to
  * reverse engineer the code to find out what it is doing and write
  * a compilable version of that code. First I annotated the code
  * in Day23Input.txt.annotated. From this you can deduct the following
  * code ...
  * {{{
  * b = c = 84
  * if (a != 0)
  *     b = b * 100 + 100000
  *     c = b + 17000
  * do
  *     f = 1
  *     d = 2
  *     do
  *         e = 2
  *         do
  *             g = d * e - b
  *             if (g == 0)
  *                 f = 0
  *             e = e + 1
  *             g = e - b
  *         while g != 0
  *         d = d + 1
  *         g = d - b
  *     while g != 0
  *     if (f != 0)
  *         h = h + 1
  *     g = b - c
  *     if (g != 0)
  *         break
  *     b = b + 17
  * while (true)
  * }}}
  * Note that, for Part1 a == 0 and for Part2 a == 1, means for Part1
  * `b` and `c` get initialized to 84, but for Part2 `b` and `c` get
  * initialized to ...
  * {{{
  * b = 84 * 100 + 100000
  * c = b + 17000
  * }}}
  * ... which explains, why Part2 is running very slow, because the algorithm
  * will look for all non-prime numbers between `b` and `c` (with a stepsize
  * of 17) ...
  * {{{
  * from = 84 * 100 + 100000
  * to = from + 17000
  * stepsize = 17
  * for (n in range(from, to , stepsize) {
  *     found = false
  *     for first in range(2, n, 1) {
  *         for (second in range(2, n, 1)
  *             if (first * second == n) found = true
  *         }
  *     }
  *     if (found) nonPrime= nonPrime + 1
  * }
  * }}}
  *
  * Part1 - Build a list of [[Operation]]s to run the [[Program]].
  *
  * Part2 - Implement the algorithm above in [[Part2.solve]].
  *
  */
object Day23 {

  val input = Util.readInput("Day23input.txt")

  sealed abstract class Operation
  case class Set(register: Char, value: Long) extends Operation
  case class SetR(register: Char, value: Char) extends Operation
  case class Sub(register: Char, value: Long) extends Operation
  case class SubR(register: Char, value: Char) extends Operation
  case class Mul(register: Char, value: Long) extends Operation
  case class MulR(register: Char, value: Char) extends Operation
  case class JumpIfNotZero(register: Char, value: Int) extends Operation

  val registerRange = ('a' to 'h').toList

  def parseInput(input: List[String]): List[Operation] = {
    require(input.nonEmpty, "input.nonEmpty failed")

    input.map(l => {
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
  } ensuring(_.nonEmpty, "_.nonEmpty failed")

  case class Program(counter: Int, instructions: List[Operation], register: Map[Char, Long], instructionCounter: Map[String, Long])

  def run(program: Program, done: Program => Boolean, exit: Program => Long): Long = {
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

  object Part1 {
    def done(p: Program) = p.counter < 0 || p.counter >= p.instructions.size
    def exit(p: Program) = p.instructionCounter("mul")

    val program = Program(0, parseInput(input), Map.empty[Char, Long].withDefaultValue(0), Map.empty[String, Long].withDefaultValue(0))

    def solve(input: List[String]): (Long, Long) = Util.measuredTimeMillis {
      run(program, done, exit)
    }
  }

  object Part2 {
    def findPrime(n: Int): Boolean = !(2 until n).exists(x => n % x == 0)

    val seed = 84
    val start = seed * 100 + 100000
    val end = start + 17000
    val stepsize = 17

    def solve(input: List[String]): (Long, Long) = Util.measuredTimeMillis {
      (start to end by stepsize).count(!findPrime(_))
    }
  }
}
