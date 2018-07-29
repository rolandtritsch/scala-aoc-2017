package aoc

/** Problem: [[http://adventofcode.com/2017/day/15]]
  *
  * Solution:
  *
  * General - This was heavily refactored. Twice :). I started with a stream
  * based/recursive approach. Then I tried to *just* use the stream (with take/count),
  * but stream is an IteraterableAgain and that means it keeps the front of the stream
  * around and that means we (sooner or later) run out of mem (initially using Strings
  * did not help either; with the mem and the performance). The right approach is
  * obviously to use an Iterator. And then do a recursion.
  *
  * Part1 - solve the puzzle with a modolo of (1, 1) and a depth of 40 * 10e6.
  *
  * Part2 - solve the puzzle with a modolo of (4, 8) and a depth of 5 * 10e6.
  */

object Day15 {
  // val in = ... - the only one with no *in*, means I am not reading Day15input.txt.
  // I just copied the values from it :).

  object Default {
    val startA = 703
    val startB = 516

    val factorA = 16807
    val factorB = 48271

    val devider = 2147483647 //Int.MaxValue

    def next(c: Long, f: Long, d: Long, m: Long): Long = {
      val n = (c * f) % d
      if(n % m == 0) n
      else next(n, f, d, m)
    }
  }

  case class GeneratorConfig(start: Long, factor: Long, devider: Long, modolo: Long, next: (Long, Long, Long, Long) => Long)

  def generator(a: GeneratorConfig, b: GeneratorConfig): Iterator[(Long, Long)] = {
    Iterator.iterate(a.start, b.start)(current => (a.next(current._1, a.factor, a.devider, a.modolo), b.next(current._2, b.factor, b.devider, b.modolo)))
  }

  def matching(pair: (Long, Long)): Boolean = {
    (pair._1 & 0xffff) == (pair._2 & 0xffff)
  }

  def countMatchingPairs(gen: Iterator[(Long, Long)], depth: Int): Int = {
    def go(g: Iterator[(Long, Long)], d: Int, c: Int): Int = {
      if(d <= 0) c
      else if(matching(g.next)) go(g, d - 1, c + 1)
      else go(g, d - 1, c)
    }

    go(gen, depth, 0)
  }

  def run(modolo: (Long, Long), depth: Int): Int = {
    val genA = GeneratorConfig(Default.startA, Default.factorA, Default.devider, modolo._1, Default.next)
    val genB = GeneratorConfig(Default.startB, Default.factorB, Default.devider, modolo._2, Default.next)
    val gen = generator(genA, genB)

    countMatchingPairs(gen, depth)
  }

  object Part1 {
    def solve: (Int, Long) = Util.measuredTimeMillis {
      run((1, 1), 40000000)
    }
  }

  object Part2 {
    def solve: (Int, Long) = Util.measuredTimeMillis {
      run((4, 8), 5000000)
    }
  }
}
