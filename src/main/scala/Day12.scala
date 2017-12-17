package aoc

object Day12 {
  val fileURL = getClass.getResource(".") + "/Day12input.txt"
  val in = Util.readInputURL(fileURL)

  def parseInput(in: List[String]): Map[Int, List[Int]] = {
    in.map(line => {
      val tokens = line.split("[ ,]")
      val from = tokens(0).toInt
      val to = (for(i <- 2 until tokens.size by 2) yield tokens(i).toInt).toList
      from -> to
    }).toMap
  }

  type Mesh = Map[Int, List[Int]]

  def findPrograms(start: Int, graph: Mesh): List[Int] = {
    def go(node: Int, graph: Mesh, seenAlready: List[Int]): List[Int] = {
      if(seenAlready.contains(node)) seenAlready
      else {
        val nodes = graph(node)
        nodes.foldLeft(node :: seenAlready)((seen: List[Int], n: Int) => {
          go(n, graph, seen)
        })
      }
    }
    go(start, graph, List.empty[Int])
  }

  def findGroups(graph: Mesh): List[List[Int]] = {
    def go(nodes: List[Int], graph: Mesh, groups: List[List[Int]]): List[List[Int]] = {
      if(nodes.isEmpty) groups
      else {
        val nextGroup = findPrograms(nodes.head, graph)
        go(nodes.diff(nextGroup), graph, nextGroup :: groups)
      }
    }
    val nodes = graph.keys.toList
    val groupZero = findPrograms(0, graph)
    go(nodes.diff(groupZero), graph, List(groupZero))
  }
}
