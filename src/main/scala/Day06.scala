package aoc

object Day06 {

  val in = Util.readInput("Day06input.txt").head.split('\t').map(_.toInt).toList

  object Part1 {
    def redistribute(banks: List[Int]): List[Int] = {
      val maxBlocks = banks.max
      val mostBlocksIndex = banks.indexWhere(b => b == maxBlocks)
      var newBanks = banks.updated(mostBlocksIndex, 0)
      for {
        i <- 1 to maxBlocks
        n = ((mostBlocksIndex + i) % banks.size)
      } {
        newBanks = newBanks.updated(n, newBanks(n) + 1)
      }
      newBanks
    }

    def detectLoop(banks: List[Int]): (Int, Int) = {
      def go(distribution: List[Int], sofar: List[List[Int]], step: Int): (Int, Int) = {
        val newDistribution = redistribute(distribution)
        if (sofar.contains(newDistribution)) {
          val cycles = sofar.size - sofar.indexOf(newDistribution)
          (step + 1, cycles)
        }
        else go(newDistribution, sofar ++ List(newDistribution), step + 1)
      }

      go(banks, List(List()), 0)
    }
  }
}
