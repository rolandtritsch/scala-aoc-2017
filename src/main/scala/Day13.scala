package aoc

/** Problem: [[http://adventofcode.com/2017/day/13]]
  *
  * Solution:
  *
  * General - Let's build a simulation. The simulation has [[aoc.Day13.SecurityScanner]]s.
  * The scanners are working on a [[aoc.Day13.Layer]]. The layer has a depth (it's *location*
  * in the firewall) and a range. The [[aoc.Day13.FireWall]] is constructed from layers.
  * With every [[aoc.Day13.FireWall.tick]] (aka. picosecond) the entire simulation makes
  * a step forward. Means all layers get updated, the packet moves to the next later and
  * we check, if the packet was caught/detected and if so, we calculate the security score.
  *
  * Part1 - Just run the simulation and report the security score.
  *
  * Part2 - Run the simulation with increasing delays, until we can get through undetected.
  */
object Day13 {

  val input = Util.readInput("Day13input.txt")

  def parseInput(lines: List[String]): Map[Int, Int] = {
    lines.map(l => {
      val tokens = l.split("[ :]")
      val depth = tokens(0).toInt
      val range = tokens(2).toInt
      (depth, range)
    }).toMap
  }

  object Direction extends Enumeration {
    type Direction = Value
    val UP, DOWN = Value
  }

  case class SecurityScanner(range: Int, pos: Int = 1, direction: Direction.Direction = Direction.DOWN) {
    def tick: SecurityScanner = {
      if (range > 1)
        if (direction == Direction.DOWN)
          if (pos + 1 > range) SecurityScanner(range, pos - 1, Direction.UP)
          else SecurityScanner(range, pos + 1, Direction.DOWN)
        else
          if (pos - 1 == 0) SecurityScanner(range, pos + 1, Direction.DOWN)
          else SecurityScanner(range, pos - 1, Direction.UP)
      else
        this
    }

    def isTop: Boolean = (pos == 1 && range > 0)
  }

  case class Layer(depth: Int, securityScanner: SecurityScanner) {
    def tick: Layer = Layer(depth, securityScanner.tick)
  }

  object FireWall {
    /** Build a firewall.
      *
      * @note If the layer has no range (is not defined in the input), a [[Layer]] with range 0 is created.
      * @note For all layers the security scanner starts at position 1 (the top).
      */
    def build(inputLayers: Map[Int, Int], delay: Int): FireWall = {
      val layers = (for {
        l <- 0 to inputLayers.keys.max
        r = inputLayers.getOrElse(l, 0)
      } yield Layer(l, SecurityScanner(r))).toList
      FireWall(layers, delay * (-1), 0, false)
    }

    def runSimulation(fw: FireWall): FireWall = {
      def go(fw: FireWall, count: Int): FireWall = {
        if (count <= 0) fw.tick
        else go(fw.tick, count - 1)
      }

      go(fw, Math.abs(fw.threatPosition) + fw.layers.size)
    }
  }

  case class FireWall(layers: List[Layer], threatPosition: Int, securityScore: Int, threatDetected: Boolean) {
    def tick: FireWall = {
      val newThreats = layers.map(l => {
        if (threatPosition == l.depth && l.securityScanner.isTop) (l.depth * l.securityScanner.range, true)
        else (0, false)
      })
      val newSecurityScore = newThreats.map(_._1).sum
      val newThreatDetected = !(newThreats.map(_._2).forall(!_))
      FireWall(layers.map(_.tick), threatPosition + 1, securityScore + newSecurityScore, threatDetected || newThreatDetected)
    }

    override def toString: String = {
      layers.map(l => {
        val depthStr = if (l.depth == threatPosition) s"(${l.depth}): " else s" ${l.depth} : "
        val rangeStr = (for (r <- 1 to l.securityScanner.range) yield
          if (l.securityScanner.pos == r) "[S]" else "[ ]"
          ).mkString
        depthStr + rangeStr
      }).mkString("\n")
    }
  }

  def findWayThrough(input: Map[Int, Int]): Int = {
    def go(input: Map[Int, Int], delay: Int): Int = {
      val fw = FireWall.runSimulation(FireWall.build(input, delay))
      if (fw.threatDetected) go(input, delay + 1)
      else delay
    }

    go(input, 0)
  }

  object Part1 {
    def solve(input: List[String]): Int = {
      FireWall.runSimulation(FireWall.build(parseInput(input), 0)).securityScore
    }
  }

  object Part2 {
    def solve(input: List[String]): Int = {
      findWayThrough(parseInput(input))
    }
  }
}
