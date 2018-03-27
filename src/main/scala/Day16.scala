package aoc

/** Problem: [[https://adventofcode.com/2017/day/16]]
  *
  * Solution:
  *
  * General - Kind of simple. Take a program and a move and execute
  * the move on the program. Take all moves and execute all of them
  * one after the other (executeMoves; moves.foldLeft). Then (for
  * Part2) execute all moves on a/the program 1000000000 times.
  *
  * Part1 - execute the moves (once).
  *
  * Part2 - execute the moves 1000000000 times (execute the dance).
  */
object Day16 {

  val input = Util.readInput("Day16input.txt").head.split(',').toList

  val programs = ('a' to 'p').toString.toCharArray
  val times = 1000000000

  sealed abstract class Move
  case class Spin(size: Int) extends Move
  case class Exchange(thiz: Int, thaz: Int) extends Move
  case class Partner(thiz: Char, thaz: Char) extends Move

  def parseInput(input: List[String]): List[Move] = {
    input.map(l => l(0) match {
      case 's' => {
        val s = l.substring(1).toInt
        assert(s >= 1 && s <= programs.size, s"s >= 1 && s <= programs.size failed; with >${s}}<")
        Spin(s)
      }

      case 'x' => {
        val ps = l.substring(1).split('/')
        assert(ps.size == 2)
        val thiz = ps(0).toInt
        assert(thiz >= 0 && thiz <= programs.size - 1, s"thiz >= 0 && thiz <= programs.size - 1 failed; with >${thiz}}<")
        val thaz = ps(1).toInt
        assert(thaz >= 0 && thaz <= programs.size - 1, s"thaz >= 0 && thaz <= programs.size - 1 failed; with >${thaz}}<")
        assert(thiz != thaz, s"thiz != thaz failed; with >${thiz}</>${thaz}<")
        Exchange(thiz, thaz)
      }

      case 'p' => {
        val ps = l.substring(1).split('/')
        assert(ps.size == 2)
        val thiz = ps(0).charAt(0)
        assert(programs.contains(thiz), s"programs.contains(thiz) failed; with >${thiz}<")
        val thaz = ps(1).charAt(0)
        assert(programs.contains(thaz), s"programs.contains(thaz) failed; with >${thaz}<")
        assert(thiz != thaz, s"thiz != thaz failed; with >${thiz}</>${thaz}<")
        Partner(thiz, thaz)
      }
    })
  }

  def executeMoves(programs: Array[Char], moves: List[Move]): Array[Char] = {
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

    moves.foldLeft(programs)((current, move) => executeMove(current, move))
  }

  def executeDance(programs: Array[Char], moves: List[Move], times: BigInt): Array[Char] = {
    (BigInt(1) to times).foldLeft(programs)((current, _) => executeMoves(current, moves))
  }

  object Part1 {
    def solve(input: List[String]): String = {
      executeMoves(programs, parseInput(input)).mkString
    }
  }

  object Part2 {
    def solve(input: List[String]): String = {
      executeDance(programs, parseInput(input), 100000).mkString
    }
  }
}
