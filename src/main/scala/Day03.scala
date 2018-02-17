package aoc

/** Problem: [[https://adventofcode.com/2017/day/3]]
  *
  * @see [[https://oeis.org/A033951]]
  * @see [[https://oeis.org/A141481]]
  *
  * Solution:
  *
  * General - This is a/the refactored solution. I was only able to come
  * up with it after I saw Part2. My initial approach made the implementation
  * of Part2 hard (and/or at least not elegant).
  *
  * Now the idea is ...
  *
  * - build a stream of moves
  * - from these moves build a stream of cells
  * - a cell has an index (starting from 0), coordinates (x, y - showing/
  * describing the position of the cell relative to the center) and a value
  * (the value of the cell)
  * - Note: For Part1 constructing the cell value is trivial (based on the
  * index). For Part2 it is more elaborate and requires to search the already
  * evaluated stream for the 8 coordinates/values surrounding the cell that
  * needs to be calculated.
  *
  * Part 1 - find the cell with the given value. Take the coordinates of
  * the cell and use them to calculate the Manhatten-Distance to the center
  * from it (realizing that the coordinates are effectively the Manhatten-
  * Distance :)).
  *
  * Part 2 - find the cell after the one with the given value and return
  * the value of that cell.
  */
object Day03 {

  val input = Util.readInput("Day03input.txt").head.toInt

  case class Move(x: Int, y: Int)

  object Move {
    val up = Move(-1, 0)
    val down = Move(1, 0)
    val left = Move(0, -1)
    val right = Move(0, 1)
  }

  type Moves = List[List[Move]]

  val firstLevelMoves = List(
    List(Move.right),
    List(Move.up),
    List(Move.left, Move.left),
    List(Move.down, Move.down),
    List(Move.right, Move.right)
  )

  def nextLevelMoves(currentLevelMoves: Moves): Moves = List(
    currentLevelMoves(0),
    currentLevelMoves(1) ++ List(Move.up, Move.up),
    currentLevelMoves(2) ++ List(Move.left, Move.left),
    currentLevelMoves(3) ++ List(Move.down, Move.down),
    currentLevelMoves(4) ++ List(Move.right, Move.right)
  )

  def moves(seed: Moves): Stream[Move] = {
    def go(ms: Moves): Stream[Move] = ms.flatten.toStream #::: go(nextLevelMoves(ms))
    go(seed)
  }

  case class Cell(index: Int, value: Int, coordinates: (Int, Int))

  object Part1 {
    def cells(msi: Iterator[Move]): Stream[Cell] = {
      def go(c: Cell): Stream[Cell] = {
        val m = msi.next
        val (x, y) = c.coordinates
        c #:: go(Cell(c.index + 1, c.value + 1, (x + m.x, y + m.y)))
      }
      go(Cell(0, 1, (0, 0)))
    }

    def solve(cellValueToFind: Int): Int = {
      val spiral = cells(moves(firstLevelMoves).toIterator)
      val (x, y) = spiral.find(c => c.value == cellValueToFind).get.coordinates
      Math.abs(x) + Math.abs(y)
    }
  }

  object Part2 {
    def calcValue(currentCoordinates: (Int, Int), cellsSoFar: Map[(Int, Int), Cell]): Int = {
      val (x, y) = currentCoordinates
      cellsSoFar(x - 1, y).value +
        cellsSoFar(x - 1, y - 1).value +
        cellsSoFar(x - 1, y + 1).value +
        cellsSoFar(x + 1, y).value +
        cellsSoFar(x + 1, y - 1).value +
        cellsSoFar(x + 1, y + 1).value +
        cellsSoFar(x, y - 1).value +
        cellsSoFar(x, y + 1).value
    }

    def cells(moves: Iterator[Move]): Stream[Cell] = {
      def go(c: Cell, cellsSoFar: Map[(Int, Int), Cell]): Stream[Cell] = {
        val move = moves.next
        val thisIndex = c.index + 1
        val thisCoordinates = (c.coordinates._1 + move.x, c.coordinates._2 + move.y)
        val thisValue = calcValue(thisCoordinates, cellsSoFar)
        val thisCell = Cell(thisIndex, thisValue, thisCoordinates)
        c #:: go(thisCell, cellsSoFar + (thisCoordinates -> thisCell))
      }

      val centerCell = Cell(0, 1, (0, 0))
      val initMap = Map.empty[(Int, Int), Cell].withDefaultValue(Cell(0, 0, (0, 0))) + (centerCell.coordinates -> centerCell)
      go(centerCell, initMap)
    }

    def solve(cellValueToFind: Int): Int = {
      val spiral = cells(moves(firstLevelMoves).toIterator)
      spiral.find(c => c.value > cellValueToFind).get.value
    }
  }
}