package aoc

/** Day3 - building circular arrays and doing stuff with them/in them.
  *
  * Part 1 - build a circular array (with monoton increasing numbers),
  * find a number/location in it and calculate the manhatten distance
  * to the access port/the center.
  *
  * Part 2 - build a circular array (calc the numbers based on the sum
  * of all cells around a given cell) and find the first value that is
  * bigger than a given value.
  *
  * To make this work, I am using a slightly unorthodox approach.
  *
  * First I am creating a grid (NxN, where N needs to be >= N && isOdd).
  *
  * Then I am creating a Stream of moves/coordinates that walk from the
  * center of the grid in spirals to the outside of the grid and put the
  * approbriate values into the fields.
  *
  * I am then using the resulting/initialized grids to solve the problems.
  */
object Day03 {

  val in = Util.readInput("Day03input.txt").head.toInt

  type Grid = Array[Array[Int]]
  type Moves = List[List[Move]]

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

  def nextLevelLoop(loop: Moves): Moves = List(
    loop(0),
    loop(1) ++ List(Move.up, Move.up),
    loop(2) ++ List(Move.left, Move.left),
    loop(3) ++ List(Move.down, Move.down),
    loop(4) ++ List(Move.right, Move.right)
  )

  def moves(seed: Moves): Stream[Move] = {
    def go(ms: Moves): Stream[Move] = ms.flatten.toStream #::: go(nextLevelLoop(ms))
    go(seed)
  }

  def positions(center: (Int, Int), msi: Iterator[Move]): Stream[(Int, Int)] = {
    def go(p: (Int, Int)): Stream[(Int, Int)] = {
      val m = msi.next
      val (x, y) = p
      (x, y) #:: go((x + m.x, y + m.y))
    }
    go(center)
  }

  def center(grid: Grid): (Int, Int) = (grid.size / 2, grid.size / 2)

  object Part1 {
    def calcDimensions(loc: Int): Int = {
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

    def initGrid(loc: Int): Grid = {
      val dimension = calcDimensions(loc)
      val grid = Array.ofDim[Int](dimension, dimension)

      lazy val msi = moves(initalLoop).iterator
      lazy val psi = positions(center(grid), msi).iterator

      for (n <- 1 to dimension * dimension) {
        val (x, y) = psi.next
        grid(x)(y) = n
      }
      grid
    }

    def calcDistanceFromLocToCenter(location: Int, grid: Grid): Int = {
      val x = grid.indexWhere(a => a.contains(location))
      val y = grid(x).indexOf(location)
      val offset = (grid.size / 2)
      Math.abs(x - offset) + Math.abs(y - offset)
    }
  }

  object Part2 {
    def findNextBiggestNumber(number: Int): Int = {
      // @todo D... it. Lost some code. Need to get rid of the hardcoded Array ... again.
      val grid = Array.ofDim[Int](11, 11)

      lazy val msi = moves(initalLoop).iterator
      lazy val psi = positions(center(grid), msi).iterator

      var done = false
      var n = 0
      while (!done) {
        val (x, y) = psi.next

        if((x, y) == center(grid)) grid(x)(y) = 1
        else grid(x)(y) = grid(x + 1)(y + 0) + grid(x + 1)(y + 1) + grid(x + 0)(y + 1) + grid(x + 1)(y - 1) + grid(x - 1)(y - 0) + grid(x - 1)(y - 1) + grid(x - 0)(y - 1) + grid(x - 1)(y + 1)

        if (grid(x)(y) > number) {
          done = true
          n = grid(x)(y)
        }
      }
      n
    }
  }
}