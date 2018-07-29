package aoc

/** Problem: [[https://adventofcode.com/2017/day/6]]
  *
  * General - This is a [[https://en.wikipedia.org/wiki/Cycle_detection cycle detection problem]].
  *
  * Part1 - Is lambda + mu.
  *
  * Part2 - Is just lambda.
  *
  * @see [[https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_Tortoise_and_Hare]]
  */
object Day06 {

  val input = Util.readInput("Day06input.txt").head.split('\t').map(_.toInt).toList

  type MemoryBanks = List[Int]

  def cycle(banks: MemoryBanks): MemoryBanks = {
    val maxBlocks = banks.max
    val mostBlocksIndex = banks.indexWhere(b => b == maxBlocks)

    (1 to maxBlocks).foldLeft(banks.updated(mostBlocksIndex, 0)) {(currentBanks, i) => {
      val n = (mostBlocksIndex + i) % banks.size
      currentBanks.updated(n, currentBanks(n) + 1)
    }}
  }

  def floyd(banks: MemoryBanks, cycle: MemoryBanks => MemoryBanks): (Int, Int) = {
    def phase1(tortoise: MemoryBanks, hare: MemoryBanks): MemoryBanks = {
      if(tortoise == hare) hare
      else phase1(cycle(tortoise), cycle(cycle(hare)))
    }

    val hare = phase1(cycle(banks), cycle(cycle(banks)))

    def phase2(tortoise: MemoryBanks, hare: MemoryBanks, mu: Int): (MemoryBanks, Int) = {
      if(tortoise == hare) (tortoise, mu)
      else phase2(cycle(tortoise), cycle(hare), mu + 1)
    }

    val (tortoise, mu) = phase2(banks, hare, 0)

    def phase3(tortoise: MemoryBanks, hare: MemoryBanks, lambda: Int): Int = {
      if(tortoise == hare) lambda
      else phase3(tortoise, cycle(hare), lambda + 1)
    }

    val lambda = phase3(tortoise, cycle(tortoise), 1)

    return(lambda, mu)
  }

  object Part1 {
    def solve(banks: MemoryBanks): (Int, Long) = Util.measuredTimeMillis {
      val (lambda, mu) = floyd(banks, cycle)
      lambda + mu
    }
  }

  object Part2 {
    def solve(banks: MemoryBanks): (Int, Long) = Util.measuredTimeMillis {
      val (lambda, mu) = floyd(banks, cycle)
      lambda
    }
  }
}
