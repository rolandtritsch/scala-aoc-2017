package aoc

// @note https://stackoverflow.com/a/47749887/2374327
object Day11 {

  val in = Util.readInput("Day11input.txt").head.split(',').toList

  case class Tile(x: Int, y: Int, z: Int) {
    def distance(that: Tile): Int = {
      (Math.abs(this.x - that.x) + Math.abs(this.y - that.y) + Math.abs(this.z - that.z)) / 2
    }
  }

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
  }
}