package aoc

object Day10 {
  val in = List(165, 1, 255, 31, 87, 52, 24, 113, 0, 91, 148, 254, 158, 2, 73, 153)

  type Hash = List[Int]

  def knot(lengths: List[Int], seed: Hash): Hash = {
    def go(lengths: List[Int], currentHash: Hash, currentPosition: Int, currentSkip: Int): Hash = lengths match {
      case Nil => currentHash
      case l :: restLengths => {
        // So that I do not need to do the wrap-around,
        // I am just doubleing the hash so that I can write
        // over the end.
        val buffer = currentHash ++ currentHash
        val head = buffer.slice(0, currentPosition)
        val toBeReversed = buffer.slice(currentPosition, currentPosition + l)
        val tail = buffer.slice(currentPosition + l, buffer.size)
        val newBuffer = head ++ toBeReversed.reverse ++ tail
        //println(s"${currentPosition}/${l}/${currentSkip}")
        //println(s"${head} ++ ${toBeReversed.reverse} ++ ${tail}")
        val newHash = if(currentPosition + l > currentHash.size) {
          // assemble the new hash from the buffer
          val (firstHalf, secondHalf) = newBuffer.splitAt(currentHash.size)
          val (firstHalfOld, firstHalfNew) = firstHalf.splitAt(currentPosition)
          val (secondHalfNew, secondHalfOld) = secondHalf.splitAt(((currentPosition + l) % currentHash.size) + 1)
          //println(firstHalf, secondHalf)
          //println(firstHalfOld, firstHalfNew)
          //println(secondHalfNew, secondHalfOld)
          //println(secondHalfNew ++ firstHalfNew)
          //println("---")
          secondHalfNew ++ firstHalfNew
        } else {
          // use the first half of the buffer
          //println(newBuffer.splitAt(currentHash.size)._1)
          //println("---")
          newBuffer.splitAt(currentHash.size)._1
        }
        go(restLengths, newHash, (currentPosition + l + currentSkip) % newHash.size, currentSkip + 1)
      }
    }

    go(lengths, seed, 0, 0)
  }
}