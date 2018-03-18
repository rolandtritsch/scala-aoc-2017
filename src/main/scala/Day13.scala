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

  def buildFw(input: Map[Int, Int]): List[(Int, Int)] = {
    (for {
      d <- 0 to input.keys.max
      r = input.getOrElse(d, 0)
    } yield (d, r)).toList
  }

  def threatDedected(depth: Int, range: Int): Boolean = {
    if(range == 0) false
    else depth % ((range - 1) * 2) == 0
  }

  def calcSecScore(fw: List[(Int, Int)]): Int = {
    fw.foldLeft(0) {(secScore, layer) => {
      val (depth, range) = layer
      if(threatDedected(depth, range)) secScore + depth * range
      else secScore
    }}
  }

  object Part1 {
    def solve(input: List[String]): Int = {
      calcSecScore(buildFw(parseInput(input)))
    }
  }

  def buildNextFw(fw: List[(Int, Int)]): List[(Int, Int)] = {
    List((0, 0)) ++ fw.map {case (d, r) => (d + 1, r)}
  }

  object Part2 {
    def solve(input: List[String]): Int = {
      def go(fw: List[(Int, Int)], delay: Int): Int = {
        if(calcSecScore(fw) == 0) delay
        else go(buildNextFw(fw), delay + 1)
      }

      go(buildFw(parseInput(input)), 0)
    }
  }
}
