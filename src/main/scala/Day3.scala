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
object Day3 {

  val in = 368078

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

  def moves(seed: Moves): List[Move] = {
    def go(current: List[Move], sofar: Moves, level: Int): List[Move] = {
      if (level <= 0) current
      else {
        val next = nextLevelLoop(sofar)
        go(current ++ next.flatten.toList, next, level - 1)
      }
    }

    // Note: This needs to be 399b
    go(seed.flatten.toList, seed, 99)
  }

  val ms = moves(initalLoop)

  def positions(center: (Int, Int), moves: List[Move]): List[(Int, Int)] = {
    def go(ms: List[Move], ps: List[(Int, Int)]): List[(Int, Int)] = {
      if (ms.isEmpty) ps
      else go(ms.tail, ps :+ (ms.head.x + ps.last._1, ms.head.y + ps.last._2))
    }

    go(moves.tail, List((moves.head.x + center._1, moves.head.y + center._2)))
  }

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

      val center = (grid.size / 2, grid.size / 2)
      grid(center._1)(center._2) = 1

      //val ms = moves(initalLoop)
      val ps = positions(center, ms)

      for (n <- 2 to dimension * dimension) {
        val pos = ps(n - 2)
        grid(pos._1)(pos._2) = n
      }
      grid
    }

    def calcDistanceFromLocToCenter(location: Int, grid: Grid): Int = {
      val row = grid.indexWhere(a => a.contains(location))
      val col = grid(row).indexOf(location)
      val offset = (grid.size / 2).toInt
      Math.abs(row - offset) + Math.abs(col - offset)
    }
  }

  object Part2 {

    def findNextBiggestNumber(number: Int): Int = {
      val grid = Array.ofDim[Int](101, 101)
      val center = (grid.size / 2, grid.size / 2)
      grid(center._1)(center._2) = 1

      //val ms = moves(initalLoop)
      val ps = positions(center, ms)

      var done = false
      var i = 0
      var n = 0
      while (!done) {
        val p = ps(i)
        n = grid(p._1 + 1)(p._2 + 0) + grid(p._1 + 1)(p._2 + 1) + grid(p._1 + 0)(p._2 + 1) + grid(p._1 + 1)(p._2 - 1) + grid(p._1 - 1)(p._2 - 0) + grid(p._1 - 1)(p._2 - 1) + grid(p._1 - 0)(p._2 - 1) + grid(p._1 - 1)(p._2 + 1)
        grid(p._1)(p._2) = n

        i = i + 1
        if (n > number) done = true
      }
      n
    }
  }

}