package aoc

/** Day3 - building circular arrays and doing stuff with them/in them.
  *
  * Part 1
  *
  */
object Day3 {

  val in = 368078

  type Location = Int
  type Distance = Int
  type Grid = Array[Array[Int]]

  /** The x and y coordinates of a cell.
    *
    * The center is 0, 0 and will always contain 0.
    *
    * @param x - x coordinate
    * @param y - y coordinate
    */
  case class Coordinates(x: Int, y: Int)

  object Circular2DArray {

    def build(location: Location): Circular2DArray = {
      val dimension = calcDimensions(location)
      new Circular2DArray(dimension)
    }

    /** Calculate the dimension (the size of the NxN array)
      * that we need to hold an array that contains loc.
      *
      * e.g. for 25 we need a 5x5 array. For 26 we need 7x7.
      *
      * @param loc the loc we need to find
      * @return the number of dimensions we need
      */
    def calcDimensions(loc: Location): Int = {
      require(loc >= 1, s"loc >= 1 failed; with n = ${loc}")

      def nearestSqrt(n: Int): Int = {
        require(n >= 1, s"n >= 1 failed; with n = ${n}")

        def go(n: Int, r: Int): Int = {
          if (r * r >= n) r
          else go(n, r + 1)
        }

        go(n, 1)
      }

      val n = nearestSqrt(loc)
      if (n % 2 == 0) n + 1 else n
    }

    def rotateLeft(a: Grid): Grid = {
      a.transpose.map(_.reverse)
    }

    def rotateRight(a: Grid): Grid = {
      rotateLeft(rotateLeft(rotateLeft(a)))
    }
  }

  /** Build a/the NxN circular array.
    *
    * @param dimension the dimensions of the array
    */
  class Circular2DArray(dimension: Int) {
    require(dimension >= 1, s"dimension >= 1 failed; with dimension = ${dimension}")
    require(dimension % 2 == 1, s"dimension % 2 == 1 failed; with dimension = ${dimension}")

    var grid = Array.ofDim[Int](dimension, dimension)

    /** The algorithm goes like this ...
      *
      * - start from the outside (and with n = maximum value)
      * - go backwards writing values into cells
      * - and everytime you hit the corner
      * - rotate the 2D array and keep on writing
      * - until you have rotated 4 times
      * - and then go one ring down and do it again
      * - until you hit the middle of the array
      */
    var n = dimension * dimension
    var downTo = 0
    var startFrom = dimension - 1
    for (ring <- dimension to 1 by (-2)) {
      grid(startFrom)(startFrom) = n
      n = n - 1
      for (rotate <- 0 to 3) {
        downTo = if (rotate == 3) downTo + 1 else downTo
        for (i <- startFrom - 1 to downTo by (-1)) {
          grid(startFrom)(i) = n
          n = n - 1
        }
        grid = Circular2DArray.rotateRight(grid)
      }
      startFrom = startFrom - 1
    }

    /** Find the coordinate for a given location.
      *
      * @param location the location to find
      * @return the coordinates of the location
      */
    def find(location: Location): Coordinates = {
      val row = grid.indexWhere(a => a.contains(location))
      val col = grid(row).indexOf(location)
      val offset = (dimension / 2).toInt
      Coordinates(row - offset, col - offset)
    }
  }

  def distance(start: Location): Distance = {
    require(start >= 1, s"square >= 1 failed; with square = ${start}")

    val grid = Circular2DArray.build(start)
    val coordinates = grid.find(start)
    Math.abs(coordinates.x) + Math.abs(coordinates.y)
  }

  case class Move(x: Int, y: Int)

  object Move {
    val up = Move(-1, 0)
    val down = Move(1, 0)
    val left = Move(0, -1)
    val right = Move(0, 1)
  }

  val initalLoop = List(
    List(Move.right),
    List(Move.up),
    List(Move.left, Move.left),
    List(Move.down, Move.down),
    List(Move.right, Move.right)
  )

  def nextLevelLoop(loop: List[List[Move]]): List[List[Move]] = List(
    loop(0),
    loop(1) ++ List(Move.up, Move.up),
    loop(2) ++ List(Move.left, Move.left),
    loop(3) ++ List(Move.down, Move.down),
    loop(4) ++ List(Move.right, Move.right)
  )

  val grid = Array.ofDim[Int](101, 101)

  def moves: List[Move] = {
    def go(current: List[Move], sofar: List[List[Move]], level: Int): List[Move] = {
      if(level <= 0) current
      else {
        val next = nextLevelLoop(sofar)
        go(current ++ next.flatten.toList, next, level - 1)
      }
    }
    go(initalLoop.flatten.toList, initalLoop, 399)
  }

  class ArrayIterator(start: (Int, Int), moves: List[Move]) {
    var currentField = start
    var currentMoves = moves

    def next: (Int, Int) = {
      currentField = (currentField._1 + currentMoves.head.x, currentField._2 + currentMoves.head.y)
      currentMoves = currentMoves.tail
      currentField
    }
  }

  def initCircular2DArray(loc: Location): Grid = {
    val dimension = Circular2DArray.calcDimensions(loc)
    val grid = Array.ofDim[Int](dimension, dimension)
    val start = (dimension/2, dimension/2)
    grid(start._1)(start._2) = 1
    val iter = new ArrayIterator(start, moves)

    for(n <- 2 to dimension * dimension) {
      val pos = iter.next
      grid(pos._1)(pos._2) = n
    }
    grid
  }

  def findPos(location: Location, grid: Grid): (Int, Int) = {
    val row = grid.indexWhere(a => a.contains(location))
    val col = grid(row).indexOf(location)
    val offset = (grid.size / 2).toInt
    (row - offset, col - offset)
  }

  def distanceToPort(pos: (Int, Int)): Int = {
    Math.abs(pos._1) + Math.abs(pos._2)
  }
}

