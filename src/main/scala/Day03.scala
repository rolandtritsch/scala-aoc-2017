package aoc

/** Problem: [[https://adventofcode.com/2017/day/3]]
  *
  * Solution:
  *
  * General - This is a/the refactored solution. I was only able to come
  * up with it after I saw Part2. My initial approach made the implementation
  * of Part2 hard (and/or at least not elegant).
  *
  * Now the idea is ...
  *
  *   - build a stream of moves
  *   - from these moves build a stream of cells
  *   - a cell has an index (starting from 0), coordinates (x, y - showing/
  *     describing the position of the cell relative to the center) and a value
  *     (the value of the cell)
  *   - Note: For Part1 constructing the cell value is trivial (based on the
  *     index). For Part2 it is more elaborate and requires to search the already
  *     evaluated stream for the 8 coordinates/values surrounding the cell that
  *     needs to be calculated.
  *
  * Part 1 - find the cell with the given value. Take the coordinates of
  * the cell and use them to calculate the Manhatten-Distance to the center
  * from it (realizing that the coordinates are effectively the Manhatten-
  * Distance :)).
  *
  * Part 2 - find the cell after the one with the given value and return
  * the value of that cell.
  *
  * @see [[https://oeis.org/A033951]]
  * @see [[https://oeis.org/A141481]]
  *
  */
object Day03 {

  val input = Util.readInput("Day03input.txt").head.toInt

  case class Move(x: Int, y: Int)

  object Advance {
    val up = Move(-1, 0)
    val down = Move(1, 0)
    val left = Move(0, -1)
    val right = Move(0, 1)
  }

  type Moves = List[List[Move]]

  val firstLevelMoves = List(
    List(Advance.right),
    List(Advance.up),
    List(Advance.left, Advance.left),
    List(Advance.down, Advance.down),
    List(Advance.right, Advance.right)
  )

  def nextLevelMoves(currentLevelMoves: Moves): Moves = {
    require(currentLevelMoves.nonEmpty, s"currentLevelMoves.nonEmpty failed")
    require(currentLevelMoves.forall(_.nonEmpty), s"currentLevelMoves.forall(_.nonEmpty) failed")

    List(
      currentLevelMoves(0),
      currentLevelMoves(1) ++ List(Advance.up, Advance.up),
      currentLevelMoves(2) ++ List(Advance.left, Advance.left),
      currentLevelMoves(3) ++ List(Advance.down, Advance.down),
      currentLevelMoves(4) ++ List(Advance.right, Advance.right)
    )
  } ensuring(_.flatten.size == currentLevelMoves.flatten.size + 8)

  def moves(seed: Moves): LazyList[Move] = {
    def go(ms: Moves): LazyList[Move] = ms.flatten.to(LazyList) #::: go(nextLevelMoves(ms))
    go(seed)
  }

  case class Cell(index: Int, value: Int, coordinates: (Int, Int))

  object Part1 {
    def cells(moves: Iterator[Move]): LazyList[Cell] = {
      def go(previousCell: Cell): LazyList[Cell] = {
        val move = moves.next

        val thisCellIndex = previousCell.index + 1
        val thisCellCoordinates = (previousCell.coordinates._1 + move.x, previousCell.coordinates._2 + move.y)
        val thisCellValue = previousCell.value + 1
        val thisCell = Cell(thisCellIndex, thisCellValue, thisCellCoordinates)

        previousCell #:: go(thisCell)
      }

      val centerCell = Cell(0, 1, (0, 0))
      go(centerCell)
    }

    def solve(cellValueToFind: Int): (Int, Long) = Util.measuredTimeMillis {
      val spiral = cells(moves(firstLevelMoves).iterator)
      val coordinates = spiral.find(c => c.value == cellValueToFind).get.coordinates
      calcManhattenDistance(coordinates)
    }

    def calcManhattenDistance(coordinates: (Int, Int)): Int = {
      val (x, y) = coordinates
      Math.abs(x) + Math.abs(y)
    }
  }

  object Part2 {
    /** Using stream of moves to produce stream of cells.
      *
      * But ... this time around creating the cell value (the sum of all cell values
      * around the cell we are creating) is not straight forward.
      *
      * To make this work we need to maintain a map of the cell values that we have
      * already created (and need to be able to look them up by coordinates).
      *
      * We also need to cater for the case that (while we calc the cell value from
      * the 8 cells around a given cell) some of the 8 cells (on the "outerside")
      * have not been created yet. We do this, by giving the map a default value
      * of 0 (that means I can just *blindly* look up all 8 cell cooordinates
      * around the given cell and for the once that do not exist yet, I am just
      * getting a value of 0).
      */
    def cells(moves: Iterator[Move]): LazyList[Cell] = {
      def go(previousCell: Cell, valuesSoFar: Map[(Int, Int), Int]): LazyList[Cell] = {
        val move = moves.next

        val thisCellIndex = previousCell.index + 1
        val thisCellCoordinates = (previousCell.coordinates._1 + move.x, previousCell.coordinates._2 + move.y)
        val thisCellValue = calcValue(thisCellCoordinates, valuesSoFar)
        val thisCell = Cell(thisCellIndex, thisCellValue, thisCellCoordinates)

        previousCell #:: go(thisCell, valuesSoFar + (thisCellCoordinates -> thisCellValue))
      }

      val centerCell = Cell(0, 1, (0, 0))
      val valuesSoFar = Map.empty[(Int, Int), Int].withDefaultValue(0) + (centerCell.coordinates -> centerCell.value)
      go(centerCell, valuesSoFar)
    }

    def solve(cellValueToFind: Int): (Int, Long) = Util.measuredTimeMillis {
      val spiral = cells(moves(firstLevelMoves).iterator)
      spiral.find(c => c.value > cellValueToFind).get.value
    }

    def calcValue(currentCoordinates: (Int, Int), valuesSoFar: Map[(Int, Int), Int]): Int = {
      require(valuesSoFar.nonEmpty, s"valuesSoFar.nonEmpty failed")

      val (x, y) = currentCoordinates
      valuesSoFar(x - 1, y) + valuesSoFar(x - 1, y - 1) + valuesSoFar(x - 1, y + 1) +
        valuesSoFar(x + 1, y) + valuesSoFar(x + 1, y - 1) + valuesSoFar(x + 1, y + 1) +
        valuesSoFar(x, y - 1) + valuesSoFar(x, y + 1)
    } ensuring(result => result == 1 || result > valuesSoFar.values.max)
  }
}
