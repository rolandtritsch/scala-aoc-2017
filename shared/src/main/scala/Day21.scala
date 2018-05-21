package aoc

/** Problem: [[http://adventofcode.com/2017/day/21]]
  *
  * Solution:
  *
  * General -
  * and an [[Acceleration]]. With every *tick* the Particle will get
  * a new Position. Run a/the simulation for a while (defaultDepth).
  *
  * Part1 -
  *
  * Part2 -
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
    def rotations(fromGrid: Grid): List[Grid] = {
      (1 to 3).scanLeft(fromGrid)((g, _) => rotateClockWise(g)).toList
    }

    def flips(fromGrid: Grid): List[Grid] = {
      List(fromGrid) ++
      rotations(fromGrid) ++
      rotations(flipVertical(fromGrid)) ++
      rotations(flipHorizontal(fromGrid)) ++
      List(flipVertical(fromGrid)) ++
      List(flipHorizontal(fromGrid))
    }

    input.flatMap(l => {
      // ../.. => ###/.##/#..
      val from = l.substring(0, l.indexOf('=') - 1)
      val to = l.substring(l.indexOf('=') + 3)
      val fromGrid = from.split('/').map(_.toCharArray)
      val toGrid = to.split('/').map(_.toCharArray)

      flips(fromGrid).map(g => Rule(Grid.toString(g), toGrid))
    })
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
    (for {
      r <- row until row + size
    } yield {
      for {
        c <- col until col + size
      } yield {
        thiz(r)(c)
      }
    }).map(_.toArray).toArray
  }

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

    println(s"${grids.size} -> ${grids(0).size}x${grids(0)(0).size} -> ${Math.sqrt(grids.size).toInt * grids(0).size}")
    grids.toList
  }

  def divide2(thiz: List[Grid]): List[Grid] = {
    thiz.flatMap { g => {
      if(g.size % 2 == 0) {
        if(g.size == 2) List(g)
        else {
          val splitAt = g.size / 2
          val upperLeft = copy(0, 0, splitAt, g)
          val upperRight = copy(0, splitAt, splitAt, g)
          val lowerLeft = copy(splitAt, 0, splitAt, g)
          val lowerRight = copy(splitAt, splitAt, splitAt, g)
          List(upperLeft) ++ List(upperRight) ++ List(lowerLeft) ++ List(lowerRight)
        }
      } else if(g.size % 3 == 0) {
        if(g.size == 3) List(g)
        else {
          assert(false)
          List(Array(Array.emptyCharArray))
        }
      } else {
        assert(false)
        List(Array(Array.emptyCharArray))
      }
    }}
  }

  def enhance(thiz: List[Grid], rules: List[Rule]): List[Grid] = {
    val result = thiz.map { g => {
      val rule = rules.find(r => r.from == Grid.toString(g)).getOrElse {
        assert(false)
        Rule("", Array(Array.emptyCharArray))
      }
      rule.to
    }}

    result
  }

  def join(thiz: List[Grid]): Grid = {
    val gridDimensions = thiz(0)(0).size

    val elements = (for {
      row <- 0 until gridDimensions
    } yield {
      thiz.map(g => g(row)).flatten
    }).flatten

    val joinedGridDimensions = Math.sqrt(elements.size).toInt

    val rows = elements.sliding(joinedGridDimensions, joinedGridDimensions).toList
    rows.map(_.toArray).toArray
  }

  def run(current: Grid, rules: List[Rule], iterations: Int): Grid = {
    if(iterations <= 0) current
    else run(join(enhance(divide(current), rules)), rules, iterations - 1)
  }

  object Part1 {
    def solve(input: List[String]): Int = {
      run(start, parseInput(input), 5).flatten.count(_ == '#')
    }
  }

  object Part2 {
    def solve(input: List[String]): Int = {
      run(start, parseInput(input), 38).flatten.count(_ == '#')
    }
  }
}
