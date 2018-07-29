package aoc

/** Problem: [[https://adventofcode.com/2017/day/16]]
  *
  * Solution:
  *
  * General - Kind of simple. Take a program and a move and execute
  * the move on the program. Take all moves and execute all of them
  * one after the other (executeMoves). Then (for Part2) execute all
  * moves on a/the program 1000000000 times.
  *
  * Part1 - execute the moves (once).
  *
  * Part2 - execute the moves 1000000000 times (execute the dance).
  * The big trick here is to realize that there is a loop in the
  * dance and that the dance is repeating itself after N iterations,
  * means you can ignore the first M times the dance is executed and
  * need todo the remainder to find the final position.
  */
object Day16 {

  val input = Util.readInput("Day16input.txt").head.split(',').toList

  val initial = ('a' to 'p').toArray
  val times = 1000000000

  sealed abstract class Move
  case class Spin(size: Int) extends Move
  case class Exchange(thiz: Int, thaz: Int) extends Move
  case class Partner(thiz: Char, thaz: Char) extends Move

  def parseInput(input: List[String]): List[Move] = {
    require(input.nonEmpty, s"input.nonEmpty failed")

    input.map(l => l(0) match {
      case 's' => {
        val s = l.substring(1).toInt
        assert(s >= 1 && s <= initial.size, s"s >= 1 && s <= initial.size failed; with >${s}}<")
        Spin(s)
      }

      case 'x' => {
        val ps = l.substring(1).split('/')
        assert(ps.size == 2)
        val thiz = ps(0).toInt
        assert(thiz >= 0 && thiz <= initial.size - 1, s"thiz >= 0 && thiz <= initial.size - 1 failed; with >${thiz}}<")
        val thaz = ps(1).toInt
        assert(thaz >= 0 && thaz <= initial.size - 1, s"thaz >= 0 && thaz <= initial.size - 1 failed; with >${thaz}}<")
        assert(thiz != thaz, s"thiz != thaz failed; with >${thiz}</>${thaz}<")
        Exchange(thiz, thaz)
      }

      case 'p' => {
        val ps = l.substring(1).split('/')
        assert(ps.size == 2)
        val thiz = ps(0).charAt(0)
        assert(initial.contains(thiz), s"initial.contains(thiz) failed; with >${thiz}<")
        val thaz = ps(1).charAt(0)
        assert(initial.contains(thaz), s"initial.contains(thaz) failed; with >${thaz}<")
        assert(thiz != thaz, s"thiz != thaz failed; with >${thiz}</>${thaz}<")
        Partner(thiz, thaz)
      }
    })
  }

  @scala.annotation.tailrec
  def executeMoves(programs: Array[Char], moves: List[Move]): Array[Char] = {
    require(programs.nonEmpty, s"programs.nonEmpty failed")

    def executeMove(programs: Array[Char], move: Move): Array[Char] = move match {
      case Spin(s) => {
        // Note: The Spin rotates counter-clockwise
        val (head, tail) = programs.splitAt(programs.size - s)
        tail ++ head
      }

      case Exchange(thiz, thaz) => {
        val thizProgram = programs.charAt(thiz)
        val thazProgram = programs.charAt(thaz)
        programs.updated(thiz, thazProgram).updated(thaz, thizProgram)
      }

      case Partner(thiz, thaz) => {
        val thizPos = programs.indexOf(thiz)
        val thazPos = programs.indexOf(thaz)
        programs.updated(thizPos, thaz).updated(thazPos, thiz)
      }
    }

    moves match {
      case Nil => programs
      case m :: ms => executeMoves(executeMove(programs, m), ms)
    }
  }

  @scala.annotation.tailrec
  def executeDance(programs: Array[Char], moves: List[Move], times: BigInt): Array[Char] = {
    require(programs.nonEmpty, s"programs.nonEmpty failed")
    require(moves.nonEmpty, s"moves.nonEmpty failed")

    if(times <= 0) programs
    else executeDance(executeMoves(programs, moves), moves, times - 1)
  }

  @scala.annotation.tailrec
  def findLoop(programs: Array[Char], moves: List[Move], times: BigInt): BigInt = {
    require(programs.nonEmpty, s"programs.nonEmpty failed")
    require(moves.nonEmpty, s"moves.nonEmpty failed")

    if(programs.sameElements(initial)) times
    else findLoop(executeMoves(programs, moves), moves, times + 1)
  }

  object Part1 {
    def solve(input: List[String]): (String, Long) = Util.measuredTimeMillis {
      executeMoves(initial, parseInput(input)).mkString
    }
  }

  object Part2 {
    def solve(input: List[String]): (String, Long) = Util.measuredTimeMillis {
      val moves = parseInput(input)
      val loopTimes = findLoop(executeMoves(initial, moves), moves,1)
      executeDance(initial, moves, times % loopTimes).mkString
    }
  }
}
