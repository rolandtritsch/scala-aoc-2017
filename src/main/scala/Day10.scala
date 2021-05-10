package aoc

/** Problem: [[https://adventofcode.com/2017/day/10]]
  *
  * Solution:
  *
  * General - Nothing special here. Just very careful reading
  * of the problem statement and the requirements that come
  * with it.
  *
  * Part1 - Implement the knot.
  *
  * Part2 - Implement the sparse/dense hash.
  */
object Day10 {

  val input = Util.readInput("Day10input.txt").head.filterNot(_.isWhitespace)

  def shiftLeft(hash: List[Int], times: Int): List[Int] = {
    require(hash.nonEmpty, s"hash.nonEmpty failed; with >${hash}<")
    require(times >= 0, s"times >= 0 failed; with >${times}<")

    def shiftLeftOnce(hash: List[Int]): List[Int] = hash match {
      case head :: rest => rest ++ List(head)
      case Nil => hash
    }

    if (times == 0) hash
    else shiftLeft(shiftLeftOnce(hash), times - 1)
  }

  def shiftRight(hash: List[Int], times: Int): List[Int] = {
    shiftLeft(hash, hash.size - times)
  }

  def reverse(hash: List[Int], length: Int): List[Int] = {
    require(hash.nonEmpty, s"hash.nonEmpty failed; with >${hash}<")
    require(length >= 0 && length <= hash.size, s"length >= 0 && length <= hash.size failed; with >${length}<")

    val (front, end) = hash.splitAt(length)
    front.reverse ++ end
  }

  case class Hash(hash: List[Int], position: Int, skip: Int) {
    def next(length: Int): Hash = {
      val nextHash = shiftRight(reverse(shiftLeft(hash, position), length), position)
      val nextPosition = (position + length + skip) % hash.size
      val nextSkip = skip + 1
      Hash(nextHash, nextPosition, nextSkip)
    }
  }

  def input2Lengths(input: String): List[Int] = {
    input.split(',').map(_.toInt).toList
  }

  val seed = Hash(List.range(0, 256), 0, 0)
  def knot(lengths: List[Int], seed: Hash): Hash = {
    lengths.foldLeft(seed)((currentHash, length) => currentHash.next(length))
  }

  object Part1 {
    def solve(input: String): (Int, Long) = Util.measuredTimeMillis {
      val hash = knot(input2Lengths(input), seed).hash
      hash(0) * hash(1)
    }
  }

  val suffix = List(17, 31, 73, 47, 23)
  def encode(input: String, suffix: List[Int]): List[Int] = {
    input.map(_.toInt).toList ++ suffix
  }

  val rounds = 64
  def sparse(lengths: List[Int], seed: Hash, rounds: Int): Hash = {
    require(rounds >= 1, s"rounds >= 1 failed; with >${rounds}<")

    if(rounds == 1) knot(lengths, seed)
    else sparse(lengths, knot(lengths, seed), rounds - 1)
  } ensuring(_.hash.forall(n => n >= 0 && n < 256))

  def xorHashSlice(slice: List[Int]): Int = {
    slice.foldLeft(0)((acc, i) => acc ^ i)
  }

  val sliceSize = 16
  def dense(hash: List[Int]): List[Int] = {
    require(hash.size % sliceSize == 0, s"hash.size % sliceSize == 0 failed; with >${hash.size}<")
    require(hash.forall(n => n >= 0 && n < hash.size), s"hash.forall(n => n >= 0 && n < hash.size) failed")

    val hashSlices = hash.grouped(sliceSize).toList
    hashSlices.map(xorHashSlice(_))
  } ensuring(result => result.size == hash.size / sliceSize)

  def dense2hex(hash: List[Int]): String = {
    hash.foldLeft(List.empty[String])((acc, i) => acc :+ f"${i}%02x").mkString
  }

  object Part2 {
    def solve(input: String): (String, Long) = Util.measuredTimeMillis {
      dense2hex(dense(sparse(encode(input, suffix), seed, rounds).hash))
    }
  }
}
