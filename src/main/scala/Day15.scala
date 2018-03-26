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
    val startA = 703
    val startB = 516

    val factorA = 16807
    val factorB = 48271

    val devider = Int.MaxValue // 2147483647

    val modolo = 1
    val modoloA2 = 4
    val modoloB2 = 8

    def next(c: Long, f: Long, d: Long, m: Long): Long = {
      val n = (c * f) % d
      if(n % m == 0) n
      else next(n, f, d, m)
    }

    val depth = 40000000
    val depth2 = 5000000
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

  object Part1 {
    def solve: Int = {
      val genA1 = GeneratorConfig(Default.startA, Default.factorA, Default.devider, Default.modolo, Default.next)
      val genB1 = GeneratorConfig(Default.startB, Default.factorB, Default.devider, Default.modolo, Default.next)
      val gen = generator(genA1, genB1)

      countMatchingPairs(gen, Default.depth)
    }
  }

  object Part2 {
    def solve: Int = {
      val genA2 = GeneratorConfig(Default.startA, Default.factorA, Default.devider, Default.modoloA2, Default.next)
      val genB2 = GeneratorConfig(Default.startB, Default.factorB, Default.devider, Default.modoloB2, Default.next)
      val gen = generator(genA2, genB2)

      countMatchingPairs(gen, Default.depth2)
    }
  }
}
