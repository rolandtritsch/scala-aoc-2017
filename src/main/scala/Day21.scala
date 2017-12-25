package aoc

object Day21 {
  val in = Util.readInput("Day21input.txt")

  val start = Array(
    ".#.".toCharArray,
    "..#".toCharArray,
    "###".toCharArray
  )

  type Grid = Array[Array[Char]]

  object Grid {
    def toString(thiz: Grid): String = {
      thiz.map(_.mkString).mkString("/")
    }
  }

  case class Rule(from: String, to: Grid)

  def parseInput(in: List[String]): List[Rule] = {
    in.map(l => {
      // ../.. => ###/.##/#..
      val from = l.substring(0, l.indexOf('=') - 1)
      val to = l.substring(l.indexOf('=') + 3)
      val fromGrid = from.split('/').map(_.toCharArray)
      val toGrid = to.split('/').map(_.toCharArray)
      List(
        Rule(Grid.toString(fromGrid), toGrid),
        Rule(Grid.toString(flipVertical(fromGrid)), toGrid),
        Rule(Grid.toString(flipHorizontal(fromGrid)), toGrid),

        Rule(Grid.toString(rotateClockWise(fromGrid)), toGrid),
        Rule(Grid.toString(flipVertical(rotateClockWise(fromGrid))), toGrid),
        Rule(Grid.toString(flipHorizontal(rotateClockWise(fromGrid))), toGrid),

        Rule(Grid.toString(rotateClockWise(rotateClockWise(fromGrid))), toGrid),
        Rule(Grid.toString(flipVertical(rotateClockWise(rotateClockWise(fromGrid)))), toGrid),
        Rule(Grid.toString(flipHorizontal(rotateClockWise(rotateClockWise(fromGrid)))), toGrid),

        Rule(Grid.toString(rotateClockWise(rotateClockWise(rotateClockWise(fromGrid)))), toGrid),
        Rule(Grid.toString(flipVertical(rotateClockWise(rotateClockWise(rotateClockWise(fromGrid))))), toGrid),
        Rule(Grid.toString(flipHorizontal(rotateClockWise(rotateClockWise(rotateClockWise(fromGrid))))), toGrid)
      )
    }).flatten
  }

  def flipHorizontal(thiz: Grid): Grid = {
    thiz.reverse
  }

  def flipVertical(thiz: Grid): Grid = {
    thiz.map(_.reverse)
  }

  def rotateClockWise(thiz: Grid): Grid = {
    thiz.transpose.map(_.reverse)
  }

  def copy(row: Int, col: Int, size: Int, thiz: Grid): Grid = {
    (for (r <- row until row + size) yield for (c <- col until col + size) yield thiz(r)(c)).toArray.map(_.toArray)
  }

  def divide(thiz: Grid): List[Grid] = {
    if (thiz.size % 2 == 0) {
      if (thiz.size == 2) List(thiz)
      else {
        val splitAt = thiz.size / 2
        val upperLeft = divide(copy(0, 0, splitAt, thiz))
        val upperRight = divide(copy(0, splitAt, splitAt, thiz))
        val lowerLeft = divide(copy(splitAt, 0, splitAt, thiz))
        val lowerRight = divide(copy(splitAt, splitAt, splitAt, thiz))
        upperLeft ++ upperRight ++ lowerLeft ++ lowerRight
      }
    } else if (thiz.size % 3 == 0) {
      if (thiz.size == 3) List(thiz)
      else {
        val splitAt = thiz.size / 3
        val splitAt2 = thiz.size / 3 * 2
        val upperLeft = divide(copy(0, 0, splitAt, thiz))
        val upperMiddle = divide(copy(0, splitAt, splitAt, thiz))
        val upperRight = divide(copy(0, splitAt2, splitAt, thiz))
        val midLeft = divide(copy(splitAt, 0, splitAt, thiz))
        val midMiddle = divide(copy(splitAt, splitAt, splitAt, thiz))
        val midRight = divide(copy(splitAt, splitAt2, splitAt, thiz))
        val lowerLeft = divide(copy(splitAt2, 0, splitAt, thiz))
        val lowerMiddle = divide(copy(splitAt2, splitAt, splitAt, thiz))
        val lowerRight = divide(copy(splitAt2, splitAt2, splitAt, thiz))
        upperLeft ++ upperMiddle ++ upperRight ++
          midLeft ++ midMiddle ++ midRight ++
          lowerLeft ++ lowerMiddle ++ lowerRight
      }
    } else {
      assert(false)
      List()
    }
  }

  def enhance(thiz: Grid, rules: List[Rule]): Grid = {
    val rule = rules.find(r => r.from == Grid.toString(thiz)).getOrElse {
      println(Grid.toString(thiz))
      assert(false)
      Rule("", Array(Array.empty[Char]))
    }
    rule.to
  }

  def join(grids: List[Grid]): Grid = {
    require(grids.size == 1 || grids.size == 4, s"grids.size == 1 || grids.size == 4; with >${grids.size}<")
    //require(grids.head.size == 2 || grids.head.size == 3, s"grids.head.size == 2 || grids.head.size == 3; with >${grids.head.size}<")
    if (grids.size == 1) grids.head
    else if (grids.head.size == 2) {
      Array(
        grids(0)(0) ++ grids(1)(0),
        grids(0)(1) ++ grids(1)(1),
        grids(2)(0) ++ grids(3)(0),
        grids(2)(1) ++ grids(3)(1)
      )
    } else if (grids.head.size == 3) {
      Array(
        grids(0)(0) ++ grids(1)(0),
        grids(0)(1) ++ grids(1)(1),
        grids(0)(2) ++ grids(1)(2),
        grids(2)(0) ++ grids(3)(0),
        grids(2)(1) ++ grids(3)(1),
        grids(2)(2) ++ grids(3)(2)
      )
    } else if (grids.head.size == 4) {
      Array(
        grids(0)(0) ++ grids(1)(0),
        grids(0)(1) ++ grids(1)(1),
        grids(0)(2) ++ grids(1)(2),
        grids(0)(3) ++ grids(1)(3),
        grids(2)(0) ++ grids(3)(0),
        grids(2)(1) ++ grids(3)(1),
        grids(2)(2) ++ grids(3)(2),
        grids(2)(3) ++ grids(3)(3)
      )
    } else {
      println(grids.map(Grid.toString(_)).mkString("\n"))
      assert(false)
      Array(Array.empty[Char])
    }
  }

  def run(current: Grid, rules: List[Rule], iterations: Int): Grid = {
    if (iterations <= 0) current
    else run(join(divide(current).map(enhance(_, rules))), rules, iterations - 1)
  }
}