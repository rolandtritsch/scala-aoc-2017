package aoc

object Day02 {

  val in = Util.readInput("Day02input.txt").map(_.split('\t').toList).map(line => line.map(_.toInt))

  def checksum(spreadSheet: List[List[Int]]): Int = {
    require(spreadSheet.nonEmpty, "spreadSheet.nonEmpty failed")
    require(spreadSheet.forall(_.nonEmpty), "spreadSheet.forall(_.nonEmpty) failed")
    //require(spreadSheet.forall(row => row.size == spreadSheet.head.size))

    val pairs = spreadSheet.map(row => (row.max, row.min))
    val diffs = pairs.map(p => p._1 - p._2)
    diffs.sum
  }

  def checksum2(spreadSheet: List[List[Int]]): Int = {
    require(spreadSheet.nonEmpty, "spreadSheet.nonEmpty failed")
    require(spreadSheet.forall(_.nonEmpty), "spreadSheet.forall(_.nonEmpty) failed")
    require(spreadSheet.forall(row => row.size == spreadSheet.head.size), "spreadSheet.forall(row => row.size == spreadSheet.head.size)")

    val divs = spreadSheet.map {row => {
      val pairs = for(x <- row; y <- row; if x > y) yield (x, y)
      val dividablePairs = pairs.filter(p => (p._1 % p._2 == 0))
      assert(dividablePairs.size == 1, s"dividablePairs.size == 1 failed with ${dividablePairs}")
      dividablePairs.head._1 / dividablePairs.head._2
    }}
    divs.sum
  }
}
