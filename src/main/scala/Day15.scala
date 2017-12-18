package aoc

object Day15 {
  val genAseed = 16807
  val genBseed = 48271
  val devider = 2147483647

  val genAstart = 703
  val genBstart = 516

  def dec2bin(in: BigInt): String = {
    in.toString(2)
  }

  case class Generator(previous: BigInt, factor: BigInt, devider: BigInt) {
    val current: BigInt = (previous * factor) % devider
    def next: Generator = Generator(current, factor, devider)
  }
}
