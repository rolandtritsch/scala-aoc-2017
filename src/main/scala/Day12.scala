package aoc

/** Problem: [[https://adventofcode.com/2017/day/12]]
  *
  * Solution:
  *
  * General - A pipe is just an entry in a map that expresses
  * which program can communicate with which other program.
  *
  * Part1 - To find the number of programs that are connected
  * to program 0, we just walk the tree. We start with the root (0)
  * and visit all children until we have seen all nodes. When we hit
  * a node that we have already seen we can stop looking (because we
  * know, that we have hit a loop). At the end we just need to return
  * the number of nodes we collected while we traversed the tree.
  *
  * Part2 - To find the number of groups, we first take all nodes, then
  * we find all nodes that belong to group 0 (the group with the root 0).
  * Then we get rid of all nodes that are part of group 0, pick the first
  * node of the rest and build the next group. Then we diff the remaining
  * nodes with the ones that we just found group N. Until no nodes are
  * left over.
  */
object Day12 {

  val input = Util.readInput("Day12input.txt")

  def parseInput(in: List[String]): Map[Int, List[Int]] = {
    in.map(line => {
      val tokens = line.split("[ ,]")
      val from = tokens(0).toInt
      val to = (for(i <- 2 until tokens.size by 2) yield tokens(i).toInt).toList
      from -> to
    }).toMap
  }

  type Pipes = Map[Int, List[Int]]

  def findPrograms(start: Int, graph: Pipes): List[Int] = {
    def go(node: Int, graph: Pipes, seenAlready: List[Int]): List[Int] = {
      if(seenAlready.contains(node)) seenAlready
      else {
        val nodes = graph(node)
        nodes.foldLeft(node :: seenAlready)((seen: List[Int], n: Int) => go(n, graph, seen))
      }
    }
    go(start, graph, List.empty[Int])
  }

  def findGroups(graph: Pipes): List[List[Int]] = {
    def go(nodes: List[Int], graph: Pipes, groups: List[List[Int]]): List[List[Int]] = {
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

  object Part1 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      findPrograms(0, parseInput(input)).size
    }
  }

  object Part2 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      findGroups(parseInput(input)).size
    }
  }
}
