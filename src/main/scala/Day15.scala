package aoc

object Day15 {
  val genAseed = BigInt(16807)
  val genBseed = BigInt(48271)
  val defaultDevider = BigInt(2147483647)

  val genAstart = 703
  val genBstart = 516

  def dec2bin(in: BigInt): String = {
    in.toString(2).reverse.padTo(32, '0').reverse
  }

  case class Generator(previous: BigInt, factor: BigInt, devider: BigInt) {
    def numbers: Stream[BigInt] = next((previous * factor) % devider, factor, devider)
    def next(p: BigInt, f: BigInt, d: BigInt): Stream[BigInt] = p #:: next((p*f) % d, f, d)
  }

  case class BinaryPairGenerator(genA: Generator, genB: Generator) {
    val numbers: Stream[(String, String)] = genA.numbers.zip(genB.numbers).map{case (nA, nB) => (dec2bin(nA), dec2bin(nB))}
  }
}
