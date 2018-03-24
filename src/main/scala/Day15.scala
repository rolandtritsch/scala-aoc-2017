package aoc

/** Problem: [[http://adventofcode.com/2017/day/15]]
  *
  * Solution:
  *
  * General -
  *
  * Part1 -
  *
  * Part2 -
  */

object Day15 {
  // val in = ... - the only one with no *in*, means I am not reading Day15input.txt.
  // I just copied the values from it :).

  object Default {
    val factorA = 16807
    val factorB = 48271
    val devider = Int.MaxValue // 2147483647
    val depth = 40000000
    val depth2 = 5000000

    val startA = 703
    val startB = 516
  }

  case class Generator(previous: Long, factor: Long, devider: Long = Default.devider) {
    def numbers: Stream[Long] = next((previous * factor) % devider, factor, devider)
    def next(p: Long, f: Long, d: Long): Stream[Long] = p #:: next((p*f) % d, f, d)
  }

  case class BinaryPairGenerator(genA: Generator, genB: Generator) {
    val numbers: Stream[(Long, Long)] = genA.numbers.zip(genB.numbers)
  }

  def matching(pair: (Long, Long)): Boolean = {
    val (thiz, thaz) = pair
    val mask = 0xffff
    (thiz & mask) == (thaz & mask)
  }

  def countMatchingPair(binPairs: Stream[(Long, Long)], depth: Int): Int = {
    binPairs.take(depth).count(matching(_))
  }

  object Part1 {
    def solve: Int = {
      val genA = Generator(Default.startA, Default.factorA)
      val genB = Generator(Default.startB, Default.factorB)

      val binPairGen = BinaryPairGenerator(genA, genB)

      countMatchingPair(binPairGen.numbers, 4000000)
    }
  }

  object Part2 {
    def solve: Int = {
      0
    }
  }
}
