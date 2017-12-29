package aoc

import scala.collection.mutable

object Day22 {
  val in = Util.readInput("Day22input.txt")

  def parseInput(in: List[String]): Array[Array[Char]] = {
    in.map(_.toCharArray).toArray
  }

  case class Grid(dimension: Int) {
    private val grid = Array.fill(dimension)(mutable.ArrayBuffer.fill(dimension)(State.CLEAN))
    private var currentPosition = (midPoint, midPoint)
    private var currentDirection = Direction.UP
    private def currentIsInfected = grid(currentPosition._1)(currentPosition._2) == State.INFECTED
    private def turnLeft = currentDirection match {
      case Direction.UP => Direction.LEFT
      case Direction.DOWN => Direction.RIGHT
      case Direction.LEFT => Direction.DOWN
      case Direction.RIGHT => Direction.UP
    }
    private def turnRight = currentDirection match {
      case Direction.UP => Direction.RIGHT
      case Direction.DOWN => Direction.LEFT
      case Direction.LEFT => Direction.UP
      case Direction.RIGHT => Direction.DOWN
    }

    var numOfTicks = 0
    var numOfInfections = 0

    object Direction {
      val UP = "UP"
      val DOWN = "DOWN"
      val LEFT = "LEFT"
      val RIGHT = "RIGHT"
    }

    object State {
      val CLEAN = '.'
      val INFECTED = '#'
    }

    def midPoint: Int = grid.size / 2

    def mapInput(in: Array[Array[Char]]): Grid = {
      val offset = (grid.size - in.size) / 2

      for {
        r <- 0 until in.size
        c <- 0 until in.size
      } grid(r + offset)(c + offset) = in(r)(c)

      this
    }

    def tick: Grid = {
      numOfTicks = numOfTicks + 1
      val (row, col) = currentPosition
      currentDirection = if(currentIsInfected) turnRight else turnLeft
      grid(row)(col) = if(currentIsInfected) State.CLEAN else {numOfInfections = numOfInfections +1; State.INFECTED}
      currentPosition = currentDirection match {
        case Direction.UP => (row - 1, col)
        case Direction.DOWN => (row + 1, col)
        case Direction.LEFT => (row, col - 1)
        case Direction.RIGHT => (row, col + 1)
      }
      this
    }

    def mkString(radius: Int): String = {
      val area = for {
        r <- midPoint - radius to midPoint + radius
      } yield for {
        c <- midPoint - radius to midPoint + radius
      } yield grid(r)(c)
      area.map(_.mkString).mkString("\n")
    }
  }

  def run(grid: Grid, ticks: Int): Grid = {
    if(ticks <= 0) grid
    else run(grid.tick, ticks - 1)
  }
}