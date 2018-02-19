package aoc

/** Problem: [[https://adventofcode.com/2017/day/6]]
  *
  * General - This is a [[https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_Tortoise_and_Hare cycle detection problem]].
  *
  * Part1 - Is lambda + mu.
  *
  * Part2 - Is just lambda.
  */
object Day06 {

  val input = Util.readInput("Day06input.txt").head.split('\t').map(_.toInt).toList

  type MemoryBanks = List[Int]

  def cycle(banks: MemoryBanks): MemoryBanks = {
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

  def floyd(banks: MemoryBanks, cycle: MemoryBanks => MemoryBanks): (Int, Int) = {
    var tortoise = cycle(banks)
    var hare = cycle(cycle(banks))
    while(tortoise != hare) {
      tortoise = cycle(tortoise)
      hare = cycle(cycle(hare))
    }

    var mu = 0
    tortoise = banks
    while(tortoise != hare) {
      tortoise = cycle(tortoise)
      hare = cycle(hare)
      mu = mu + 1
    }

    var lambda = 1
    hare = cycle(tortoise)
    while(tortoise != hare) {
      hare = cycle(hare)
      lambda = lambda + 1
    }

    return(lambda, mu)
  }

  object Part1 {
    def solve(banks: MemoryBanks): Int = {
      val (lambda, mu) = floyd(banks, cycle)
      lambda + mu
    }
  }

  object Part2 {
    def solve(banks: MemoryBanks): Int = {
      val (lambda, mu) = floyd(banks, cycle)
      lambda
    }
  }
}
