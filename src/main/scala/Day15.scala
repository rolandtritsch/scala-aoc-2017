package aoc

object Day15 {
  object Default {
    val factorA = BigInt(16807)
    val factorB = BigInt(48271)
    val devider = BigInt(2147483647)
    val depth = BigInt(40000000)
    val depth2 = BigInt(5000000)

    val startA = BigInt(703)
    val startB = BigInt(516)
  }

  def dec2bin(in: BigInt): String = {
    in.toString(2).reverse.padTo(32, '0').reverse
  }

  case class Generator(previous: BigInt, factor: BigInt, devider: BigInt = Default.devider) {
    def numbers: Stream[BigInt] = next((previous * factor) % devider, factor, devider)
    def next(p: BigInt, f: BigInt, d: BigInt): Stream[BigInt] = p #:: next((p*f) % d, f, d)
  }

  case class BinaryPairGenerator(genA: Generator, genB: Generator) {
    val numbers: Stream[(String, String)] = genA.numbers.zip(genB.numbers).map{case (nA, nB) => (dec2bin(nA), dec2bin(nB))}
  }

  def matchingStr(pair: (String, String)): Boolean = {
    val (thiz, thaz) = pair
    thiz.endsWith(thaz.substring(16))
  }

  def matching(pair: (BigInt, BigInt)): Boolean = {
    val (thiz, thaz) = pair
    dec2bin(thiz).endsWith(dec2bin(thaz).substring(16))
  }

  def countMatchingPair(binPairs: Stream[(String, String)], depth: BigInt, count: Int = 0): Int = {
    if(depth <= 0) count
    else {
      if(matchingStr(binPairs.head)) countMatchingPair(binPairs.tail, depth - 1, count + 1)
      else countMatchingPair(binPairs.tail, depth - 1, count)
    }
  }

  object Part1 {
    def countMatchingPairs(start: (BigInt, BigInt), depth: BigInt): Int = {
      def go(previous: (BigInt, BigInt), depth: BigInt, count: Int): Int = {
        if(depth <= 0) count
        else {
          val (previousA, previousB) = previous
          val nextA = (previousA * Default.factorA) % Default.devider
          val nextB = (previousB * Default.factorB) % Default.devider
          val next = (nextA, nextB)
          if(matching(previous)) go(next, depth - 1, count + 1)
          else go(next, depth - 1, count)
        }
      }
      go(start, depth, 0)
    }
  }

  object Part2 {
    def countMatchingPairs(start: (BigInt, BigInt), depth: BigInt): Int = {
      def findNext(previous: BigInt, factor: BigInt, devider: BigInt, modolo: Int): BigInt = {
        val next = (previous * factor) % devider
        if(next % modolo == 0) next
        else findNext(next, factor, devider, modolo)
      }

      def go(previous: (BigInt, BigInt), depth: BigInt, count: Int): Int = {
        if(depth <= 0) count
        else {
          val (previousA, previousB) = previous
          val nextA = findNext(previousA, Default.factorA, Default.devider, 4)
          val nextB = findNext(previousB, Default.factorB, Default.devider, 8)
          val next = (nextA, nextB)
          if(matching(previous)) go(next, depth - 1, count + 1)
          else go(next, depth - 1, count)
        }
      }
      go(start, depth, 0)
    }
  }
}
