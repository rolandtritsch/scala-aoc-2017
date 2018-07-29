package aoc

/** Problem: [[https://adventofcode.com/2017/day/11]]
  *
  * Solution:
  *
  * General - Calculating the [[https://en.wikipedia.org/wiki/Taxicab_geometry manhatten distance]] on a [[https://stackoverflow.com/a/47749887/2374327 hexagon grid]]
  * is a well understood problem. The implementation of the algorithm will go
  * through the grid and will collect/calculate the finalDistance and the maxDistance
  * that was found/encountered on the way to the final tile.
  *
  * Part1 - Return the (manhatten) distance of the final tile
  * from the center (tile).
  *
  * Part2 - Return the max distance that was encountered while
  * getting/going to the final/target tile.
  */
object Day11 {

  val input = Util.readInput("Day11input.txt").head.split(',').toList

  case class Tile(x: Int, y: Int, z: Int) {
    def distance(that: Tile): Int = {
      (Math.abs(this.x - that.x) + Math.abs(this.y - that.y) + Math.abs(this.z - that.z)) / 2
    }
  }

  /** @see [[https://stackoverflow.com/a/47749887/2374327]]
    */
  def calcSteps(path: List[String]): (Int, Int) = {
    val centerTile = Tile(0, 0, 0)
    val (finalTile, finalMax) = path.foldLeft(centerTile, 0)((current, move) => {
      val (currentTile, currentMax) = current
      val nextTile = move match {
        case "n" => Tile(currentTile.x, currentTile.y + 1, currentTile.z - 1)
        case "ne" => Tile(currentTile.x + 1, currentTile.y, currentTile.z - 1)
        case "nw" => Tile(currentTile.x - 1 , currentTile.y + 1, currentTile.z)
        case "s" => Tile(currentTile.x, currentTile.y - 1, currentTile.z + 1)
        case "se" => Tile(currentTile.x + 1, currentTile.y - 1, currentTile.z)
        case "sw" => Tile(currentTile.x - 1, currentTile.y, currentTile.z + 1)
        case _ => {assert(false); Tile(0, 0, 0)}
      }
      (nextTile, Math.max(currentMax, centerTile.distance(nextTile)))
    })
    (centerTile.distance(finalTile), finalMax)
  } ensuring(result => result._1 >= 0 && result._2 >= result._1)

  object Part1 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      calcSteps(input)._1
    }
  }

  object Part2 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      calcSteps(input)._2
    }
  }
}
