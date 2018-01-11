package aoc

object Day10 {
  val in = List(165, 1, 255, 31, 87, 52, 24, 113, 0, 91, 148, 254, 158, 2, 73, 153)

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

  def knot(lengths: List[Int], seed: Hash): Hash = {
    lengths.foldLeft(seed)((currentHash, length) => currentHash.next(length))
  }

  def solve(lengths: List[Int], seed: List[Int]): Int = {
    val hash = knot(lengths, Hash(seed, 0, 0)).hash
    hash(0) * hash(1)
  }
}