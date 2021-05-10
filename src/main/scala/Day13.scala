package aoc

/** Problem: [[http://adventofcode.com/2017/day/13]]
  *
  * Solution:
  *
  * General - First implementation was a simulation. That turned out to be too slow
  * for Part2. Second implementation is calculating the solution/collisions. The main
  * idea is that the layers correspond to the number of picosecs, after 0 picosecs we
  * are on layer 0, after 1 on layer 1, and so on. With that it is easy to calculate,
  * if the packet gets dedected at/on that layer in the firewall ((number of picosecs)
  * modolo (the range of the scanner)).
  *
  * Part1 - Fold through the layers of the firewall and sum up the security score of
  * every layer, where the threat gets detected.
  *
  * Part2 - Add an offset/delay to the depth of the layers of the firewall. Increase
  * the delay until we can pass through the firewall undetected.
  */
object Day13 {

  val input = Util.readInput("Day13input.txt")

  def parseInput(lines: List[String]): Map[Int, Int] = {
    require(lines.nonEmpty, s"lines.nonEmpty failed")

    lines.map(l => {
      val tokens = l.split("[ :]")
      val depth = tokens(0).toInt
      val range = tokens(2).toInt
      (depth, range)
    }).toMap
  }

  def buildFw(input: Map[Int, Int]): List[(Int, Int)] = {
    require(input.nonEmpty, s"input.nonEmpty failed")

    (for {
      d <- 0 to input.keys.max
      r = input.getOrElse(d, 0)
    } yield (d, r)).toList.sorted
  } ensuring(result => result.size == input.keys.max + 1)

  def threatDetected(depth: Int, range: Int): Boolean = {
    require(depth >= 0, s"depth >= 0 failed; with >${depth}<")
    require(range >= 0, s"range >= 0 failed; with >${range}<")

    // Here we go: If the layer on the current depth has no range
    // the packet can never be caught (the layer is not able to
    // catch the packet, right). And if the range of the layer is
    // 1 the packet will always be caught. Otherwise we just do the
    // modolo operation, but ... we need to take into consideration
    // that the scanner is moving down and then up again (this is why
    // it is "*2".
    if(range == 0) false
    else if(range == 1) true
    else depth % ((range - 1) * 2) == 0
  }

  def calcSecScore(fw: List[(Int, Int)]): Int = {
    require(fw.nonEmpty, s"fw.nonEmpty failed")

    fw.foldLeft(0) {(secScore, layer) => {
      val (depth, range) = layer
      if(threatDetected(depth, range)) secScore + depth * range
      else secScore
    }}
  } ensuring(result => result >= 0 && result <= fw.map {case (d, r) => d * r}.sum)

  object Part1 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      calcSecScore(buildFw(parseInput(input)))
    }
  }

  def passThrough(fw: List[(Int, Int)], delay: Int): Boolean = {
    require(fw.nonEmpty, s"fw.nonEmpty failed")

    fw.forall {case (depth, range) => {
      !threatDetected(depth + delay, range)
    }}
  }

  object Part2 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      def go(fw: List[(Int, Int)], delay: Int): Int = {
        if(passThrough(fw, delay)) delay
        else go(fw, delay + 1)
      }

      go(buildFw(parseInput(input)), 0)
    }
  }
}
