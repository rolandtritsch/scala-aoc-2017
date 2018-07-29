package aoc

/** Problem: [[https://adventofcode.com/2017/day/14]]
  *
  * Solution:
  *
  * General - Using the solution from Day10, we are calculating
  * the 128x128 bit hash grid. Note: Instead of a grid of chars
  * [#.] I am using a grid of booleans.
  *
  * Part1 - We are counting all the trues in the hashes.
  *
  * Part2 - Is a little bit more tricky. I decided to look at it
  * as a tree search problem. I visit every square and if it is
  * used I recursively visit all adjacent squares until there are
  * no more squares to visit (means I have now visited and collected
  * all squares of the region). I do this for the entire grid and
  * will end up with a/the list of root nodes (coordinates) for
  * all regions.
  */
object Day14 {

  val input = Util.readInput("Day14input.txt").head

  def buildGrid(input: String): List[List[Boolean]] = {
    require(input.nonEmpty, s"input.nonEmpty failed")

    val grid = (0 to 127).map(row => Day10.dense2hex(Day10.dense(Day10.sparse(Day10.encode(s"${input}-${row}", Day10.suffix), Day10.seed, Day10.rounds).hash))).toList
    grid.map(hex2bin(_)).map(row => row.map(_ == '1').toList)
  } ensuring(result => result.size == 128 && result.forall(_.size == 128))

  def hex2bin(hex: String): String = {
    require(hex.nonEmpty, s"hex.nonEmpty failed")

    (hex.foldLeft(List.empty[String])((acc, c) => BigInt(c.toString, 16).toString(2).reverse.padTo(4, '0').reverse :: acc)).reverse.mkString
  } ensuring(result => result.size == hex.size * 4)

  object Part1 {
    def solve(input: String): (Int, Long) = Util.measuredTimeMillis {
      buildGrid(input).foldLeft(0)((sum, hash) => sum + hash.count(identity))
    }
  }

  def findRegions(grid: List[List[Boolean]]): List[(Int, Int)] = {
    require(grid.nonEmpty, s"grid.nonEmpty failed")

    def i2RowCol(i: Int, size: Int): (Int, Int) = {
      val row = i / size
      val col = i % size
      (row, col)
    }

    val (regionRoots, allNodesVisted) = (0 until grid.size * grid(0).size).foldLeft(List.empty[(Int, Int)], List.empty[(Int, Int)]) {(acc, i) => {
      val (roots, alreadyVisited) = acc
      val (row, col) = i2RowCol(i, grid.size)
      if(grid(row)(col) && !alreadyVisited.contains(row, col)) {
        val nodes = findRegion(row, col, grid)
        ((row, col) :: roots, nodes ++ alreadyVisited)
      } else acc
    }}

    regionRoots
  } ensuring(_.nonEmpty)

  // Get the next coordinates. Ignore the coordinates that are outside the grid.
  def nextCoordinates(row: Int, col: Int, size: Int): List[(Int, Int)] = {
    require(row >= 0 && row <= size, s"row >= 0 && row <= size failed; with >${row}<")
    require(col >= 0 && col <= size, s"col >= 0 && col <= size failed; with >${col}<")

    List((0, 1), (0, -1), (1, 0), (-1, 0)).map {
      case(rOffset, cOffset) => (row + rOffset, col + cOffset)
    }.filterNot {
      case(row, col) => row < 0 || col < 0 || row >= size || col >= size
    }
  }

  def findRegion(row: Int, col: Int, grid: List[List[Boolean]]): List[(Int, Int)] = {
    require(row >= 0 && row <= grid.size, s"row >= 0 && row <= grid.size failed; with >${row}<")
    require(col >= 0 && col <= grid(0).size, s"col >= 0 && col <= grid(0).size failed; with >${col}<")
    require(grid.nonEmpty, s"grid.nonEmpty failed")

    def go(grid: List[List[Boolean]], coordinates: List[(Int, Int)], alreadyVisited: List[(Int, Int)]): List[(Int, Int)] = {
      val toVisit = coordinates.diff(alreadyVisited)
      toVisit.foldLeft(alreadyVisited) {(alreadyVisited, coordinate) => {
        val (row, col) = coordinate
        if(grid(row)(col)) go(grid, nextCoordinates(row, col, grid.size), coordinate :: alreadyVisited)
        else alreadyVisited
      }}
    }

    go(grid, nextCoordinates(row, col, grid.size), List((row, col)))
  } ensuring(result => result.nonEmpty && result.contains(row, col))

  object Part2 {
    def solve(input: String): (Int, Long) = Util.measuredTimeMillis {
      findRegions(buildGrid(input)).size
    }
  }
}
