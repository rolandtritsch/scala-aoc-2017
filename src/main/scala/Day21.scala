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
    def rotations(fromGrid: Grid) = (1 to 3).scanLeft(fromGrid)((g, _) => rotateClockWise(g))
    //def flips(fromGrid: Grid) = rotations(fromGrid) ++ rotations(flipVertical(fromGrid))  ++ rotations(flipHorizontal(fromGrid))
    def flips(fromGrid: Grid) = rotations(fromGrid) ++ rotations(flipVertical(fromGrid))

    in.flatMap(l => {
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
    (for (r <- row until row + size) yield for (c <- col until col + size) yield thiz(r)(c)).toArray.map(_.toArray)
  }

  def divide(thiz: List[Grid]): List[Grid] = {
    thiz.flatMap {g => {
      if (g.size % 2 == 0) {
        if (g.size == 2) List(g)
        else {
          val splitAt = g.size / 2
          val upperLeft = copy(0, 0, splitAt, g)
          val upperRight = copy(0, splitAt, splitAt, g)
          val lowerLeft = copy(splitAt, 0, splitAt, g)
          val lowerRight = copy(splitAt, splitAt, splitAt, g)
          List(upperLeft) ++ List(upperRight) ++ List(lowerLeft) ++ List(lowerRight)
        }
      } else if (g.size % 3 == 0) {
        if (g.size == 3) List(g)
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
    val result = thiz.map {g => {
      val rule = rules.find(r => r.from == Grid.toString(g)).getOrElse {
        assert(false)
        Rule("", Array(Array.emptyCharArray))
      }
      rule.to
    }}
    //println(thiz.map(Grid.toString(_)).mkString("\n"))
    //println("---")
    //println(result.map(Grid.toString(_)).mkString("\n"))
    //println("***")
    result
  }

  def run(current: List[Grid], rules: List[Rule], iterations: Int): List[Grid] = {
    if (iterations <= 0) current
    else run(enhance(divide(current), rules), rules, iterations - 1)
  }
}