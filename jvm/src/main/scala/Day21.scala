package aoc

/** Problem: [[http://adventofcode.com/2017/day/21]]
  *
  * Solution:
  *
  * General - This was a tricky one. First, you have to get the transformations
  * right. Means you read all of the rules from the input, but then you have to
  * *add* rules for all of the [[https://en.wikipedia.org/wiki/Dihedral_group_of_order_8 transformations ]]
  * (by flipping and rotating the input pattern).
  *
  * We then need to implement a/the divide-enhance-join functions and call them
  * recursively for N iterations.
  *
  * Part1 - Run divide-enhance-join for 5 iterations.
  *
  * Part2 - Run divide-enhance-join for 18 iterations.
  *
  */
object Day21 {

  val input = Util.readInput("Day21input.txt")

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

  def parseInput(input: List[String]): List[Rule] = {
    require(input.nonEmpty, "input.nonEmpty failed")

    def rotations(fromGrid: Grid): List[Grid] = {
      (1 to 3).scanLeft(fromGrid)((g, _) => rotateClockWise(g)).toList
    }

    def flips(fromGrid: Grid): List[Grid] = {
      rotations(fromGrid) ++
      rotations(flipHorizontal(fromGrid))
    }

    input.flatMap(l => {
      // ../.. => ###/.##/#..
      val from = l.substring(0, l.indexOf('=') - 1)
      val to = l.substring(l.indexOf('=') + 3)
      val fromGrid = from.split('/').map(_.toCharArray)
      val toGrid = to.split('/').map(_.toCharArray)

      flips(fromGrid).map(g => Rule(Grid.toString(g), toGrid))
    })
  } ensuring(_.size >= input.size)

  def flipHorizontal(thiz: Grid): Grid = {
    thiz.reverse
  }

  def rotateClockWise(thiz: Grid): Grid = {
    thiz.transpose.map(_.reverse)
  }

  def copy(row: Int, col: Int, size: Int, thiz: Grid): Grid = {
    require(row >= 0 && row < thiz.size, s"row >= 0 && row < thiz.size failed; with >${row}<")
    require(col >= 0 && col < thiz.size, s"col >= 0 && col < thiz.size failed; with >${col}<")
    require(size >= 1 && size <= thiz.size, s"size >= 1 && size <= thiz.size; with >${size}<")

    (for {
      r <- row until row + size
    } yield {
      for {
        c <- col until col + size
      } yield {
        thiz(r)(c)
      }
    }).map(_.toArray).toArray
  } ensuring(_.size == size)

  def divide(thiz: Grid): List[Grid] = {
    val stepSize =
      if(thiz.size % 2 == 0) 2
      else if(thiz.size % 3 == 0) 3
      else {
        assert(false)
        0
      }

    val grids = for {
      row <- 0 until thiz.size by stepSize
      col <- 0 until thiz.size by stepSize
    } yield {
      copy(row, col, stepSize, thiz)
    }

    grids.toList
  } ensuring(gs => gs.nonEmpty && gs.head.size <= thiz.size)

  def enhance(thiz: List[Grid], rules: List[Rule]): List[Grid] = {
    require(thiz.nonEmpty, "thiz.nonEmpty failed")
    require(rules.nonEmpty, "rules.nonEmpty failed")

    val result = thiz.map { g => {
      val rule = rules.find(r => r.from == Grid.toString(g)).getOrElse {
        assert(false)
        Rule("", Array(Array.emptyCharArray))
      }
      rule.to
    }}

    result
  } ensuring(gs => gs.nonEmpty && gs.head.size > thiz.head.size)

  def join(thiz: List[Grid]): Grid = {
    require(thiz.nonEmpty, "thiz.nonEmpty failed")

    val grid = for {
      group <- thiz.grouped(Math.sqrt(thiz.size).toInt)
      row <- 0 until thiz(0).size
      line = group.map(g => g(row)).flatten
    } yield {
      line
    }

    grid.map(_.toArray).toArray
  } ensuring(g => g.size == Math.sqrt(thiz.size).toInt * thiz(0).size)

  def run(current: Grid, rules: List[Rule], iterations: Int): Grid = {
    require(current.nonEmpty, "current.nonEmpty failed")
    require(rules.nonEmpty, "rules.nonEmpty failed")

    if(iterations <= 0) current
    else run(join(enhance(divide(current), rules)), rules, iterations - 1)
  }

  object Part1 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      run(start, parseInput(input), 5).flatten.count(_ == '#')
    }
  }

  object Part2 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      run(start, parseInput(input), 18).flatten.count(_ == '#')
    }
  }
}
