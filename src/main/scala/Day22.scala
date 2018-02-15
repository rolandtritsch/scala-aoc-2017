package aoc

import scala.collection.mutable

object Day22 {

  val in = Util.readInput("Day22input.txt")

  def parseInput(in: List[String]): Array[Array[Char]] = {
    in.map(_.toCharArray).toArray
  }

  object Default {
    val ticks1 = 10000
    val ticks2 = 10000000
  }

  abstract class Grid(private val dimension: Int) {
    // @todo Make the grid infinite (not fixed)
    protected val grid = Array.fill(dimension)(mutable.ArrayBuffer.fill(dimension)(State.CLEAN))
    protected var currentPosition = (midPoint, midPoint)
    protected var currentDirection = Direction.UP
    protected def currentIsInfected = grid(currentPosition._1)(currentPosition._2) == State.INFECTED
    protected def turnLeft = currentDirection match {
      case Direction.UP => Direction.LEFT
      case Direction.DOWN => Direction.RIGHT
      case Direction.LEFT => Direction.DOWN
      case Direction.RIGHT => Direction.UP
    }
    protected def turnRight = currentDirection match {
      case Direction.UP => Direction.RIGHT
      case Direction.DOWN => Direction.LEFT
      case Direction.LEFT => Direction.UP
      case Direction.RIGHT => Direction.DOWN
    }

    object State {
      val CLEAN = '.'
      val INFECTED = '#'
      val WEAKEND = 'W'
      val FLAGGED = 'F'
    }

    var numOfTicks = 0
    var numOfInfections = 0

    object Direction {
      val UP = "UP"
      val DOWN = "DOWN"
      val LEFT = "LEFT"
      val RIGHT = "RIGHT"
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

    def mkString(radius: Int): String = {
      val area = for {
        r <- midPoint - radius to midPoint + radius
      } yield for {
        c <- midPoint - radius to midPoint + radius
      } yield grid(r)(c)
      area.map(_.mkString).mkString("\n")
    }

    def tick: Grid
  }

  case class SimpleGrid(private val d: Int) extends Grid(d) {
    def tick: SimpleGrid = {
      numOfTicks = numOfTicks + 1
      val (row, col) = currentPosition

      currentDirection = grid(row)(col) match {
        case State.CLEAN => turnLeft
        case State.INFECTED => turnRight
      }

      grid(row)(col) = grid(row)(col) match {
        case State.CLEAN => {numOfInfections = numOfInfections + 1; State.INFECTED}
        case State.INFECTED => State.CLEAN
      }

      currentPosition = currentDirection match {
        case Direction.UP => (row - 1, col)
        case Direction.DOWN => (row + 1, col)
        case Direction.LEFT => (row, col - 1)
        case Direction.RIGHT => (row, col + 1)
      }

      this
    }
  }

  case class AdvancedGrid(private val d: Int) extends Grid(d) {
    def doNotTurn = currentDirection
    def turnAround = currentDirection match {
      case Direction.UP => Direction.DOWN
      case Direction.DOWN => Direction.UP
      case Direction.LEFT => Direction.RIGHT
      case Direction.RIGHT => Direction.LEFT
    }

    def tick: AdvancedGrid = {
      numOfTicks = numOfTicks + 1
      val (row, col) = currentPosition

      currentDirection = grid(row)(col) match {
        case State.CLEAN => turnLeft
        case State.WEAKEND => doNotTurn
        case State.INFECTED => turnRight
        case State.FLAGGED => turnAround
      }

      grid(row)(col) = grid(row)(col) match {
        case State.CLEAN => State.WEAKEND
        case State.WEAKEND => {numOfInfections = numOfInfections + 1; State.INFECTED}
        case State.INFECTED => State.FLAGGED
        case State.FLAGGED => State.CLEAN
      }

      currentPosition = currentDirection match {
        case Direction.UP => (row - 1, col)
        case Direction.DOWN => (row + 1, col)
        case Direction.LEFT => (row, col - 1)
        case Direction.RIGHT => (row, col + 1)
      }

      this
    }
  }

  def run(grid: Grid, ticks: Int): Grid = {
    if(ticks <= 0) grid
    else run(grid.tick, ticks - 1)
  }
}