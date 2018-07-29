package aoc

/** Problem: [[http://adventofcode.com/2017/day/22]]
  *
  * Solution:
  *
  * General - Basicly I am implementing two grids (with a static/
  * pre-allocated size (not good; needs to be fixed)) and walk
  * these grids follwoing the rules in the problem statement.
  *
  * Part1 - Walk the [[SimpleGrid]] and count the infections.
  *
  * Part2 - Walk the [[AdvancedGrid]] and count the infections.
  *
  */
object Day22 {

  import scala.collection.mutable

  val input = Util.readInput("Day22input.txt")

  def parseInput(input: List[String]): Array[Array[Char]] = {
    input.map(_.toCharArray).toArray
  }

  object Default {
    val ticks1 = 10000
    val ticks2 = 10000000
  }

  abstract class Grid(private val dimension: Int) {
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

    def mapInput(input: Array[Array[Char]]): Grid = {
      val offset = (grid.size - input.size) / 2

      for {
        r <- 0 until input.size
        c <- 0 until input.size
      } grid(r + offset)(c + offset) = input(r)(c)

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

  object Part1 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      run(Day22.SimpleGrid(1001).mapInput(parseInput(input)), Default.ticks1).numOfInfections
    }
  }

  object Part2 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      run(Day22.AdvancedGrid(1001).mapInput(parseInput(input)), Default.ticks2).numOfInfections
    }
  }
}
