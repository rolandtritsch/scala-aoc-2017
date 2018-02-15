package aoc

object Day16 {

  val in = Util.readInput("Day16input.txt").head.split(',').toList

  val programs = ('a' to 'p').mkString
  val times = 1000000000

  sealed abstract class Move
  case class Spin(size: Int) extends Move
  case class Exchange(thiz: Int, thaz: Int) extends Move
  case class Partner(thiz: Char, thaz: Char) extends Move

  def parseInput(in: List[String]): List[Move] = {
    in.map(l => l(0) match {
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

  def executeMoves(programs: String, moves: List[Move]): String = {
    def executeMove(programs: String, move: Move): String = move match {
      case Spin(s) => {
        // Note: The Spin rotates counter-clockwise
        val (head, tail) = programs.splitAt(programs.size - s)
        tail + head
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

  def executeMoves2(programs: StringBuilder, moves: List[Move]): StringBuilder = {
    def executeMove2(programs: StringBuilder, move: Move): StringBuilder = move match {
      case Spin(s) => {
        // Note: The Spin rotates counter-clockwise
        val (head, tail) = programs.splitAt(programs.size - s)
        new scala.collection.mutable.StringBuilder((tail ++ head).toString)
      }
      case Exchange(thiz, thaz) => {
        val thizProgram = programs.charAt(thiz)
        val thazProgram = programs.charAt(thaz)
        programs.update(thiz, thazProgram)
        programs.update(thaz, thizProgram)
        programs
      }
      case Partner(thiz, thaz) => {
        val thizPos = programs.indexOf(thiz)
        val thazPos = programs.indexOf(thaz)
        programs.update(thizPos, thaz)
        programs.update(thazPos, thiz)
        programs
      }
    }

    moves.foldLeft(programs)((current, move) => executeMove2(current, move))
  }

  def executeDance(programs: String, moves: List[Move], times: BigInt): String = {
    (BigInt(1) to times).foldLeft(programs)((current, _) => executeMoves(current, moves))
  }

  def executeDance2(programs: String, moves: List[Move], times: BigInt): String = {
    if(times % 100000 == 0) println(times)
    if(times <= 1) executeMoves(programs, moves)
    else executeDance2(executeMoves(programs, moves), moves, times - 1)
  }

  def executeDance22(programs: StringBuilder, moves: List[Move], times: BigInt): String = {
    if(times % 100000 == 0) println(times)
    if(times <= 1) executeMoves2(programs, moves).result
    else executeDance22(executeMoves2(programs, moves), moves, times - 1)
  }
}