package aoc

/** Problem: [[http://adventofcode.com/2017/day/24]]
  *
  * Solution:
  *
  * General - ???
  *
  * Part1 - ???
  *
  * Part2 - ???
  *
  */
object Day24 {

  val input = Util.readInput("Day24input.txt")

  case class Component(left: Int, right: Int) {
    def matches(port: Int) = left == port || right == port

    def appendTo(l: List[Component]): List[Component] = {
      if (l.last.right == left) l :+ Component(left, right)
      else if (l.last.right == right) l :+ Component(right, left)
      else {
        assert(false); List()
      }
    }
  }

  def parseInput(in: List[String]): List[Component] = {
    in.map(l => {
      val tokens = l.split('/')
      assert(tokens.size == 2)
      val port1 = tokens(0).toInt
      val port2 = tokens(1).toInt
      if (port1 <= port2) Component(port1, port2)
      else Component(port2, port1)
    }).sortBy(_.left)
  }

  def findZero(components: List[Component]): List[Component] = {
    components.filter(c => c.left == 0)
  }

  def findPath(head: List[Component], rest: List[Component], all: List[List[Component]]): List[List[Component]] = {
    if (rest.isEmpty) all :+ head
    else {
      rest.filter(_.matches(head.last.right)) match {
        case Nil => all :+ head
        case cs => cs.foldLeft(all)((acc, next) => findPath(next.appendTo(head), rest.diff(List(next)), acc))
      }
    }
  }

  def findPaths(components: List[Component]): List[List[Component]] = {
    findZero(components).flatMap(z => findPath(List(z), components.diff(List(z)), List()))
  }

  def findStrongestPath(paths: List[List[Component]]): (Int, List[Component]) = {
    val strength = paths.map(p => (p.foldLeft(0)((acc, c) => acc + c.left + c.right), p))
    strength.maxBy(_._1)
  }

  def findLongestPath(paths: List[List[Component]]): (Int, Int, List[Component]) = {
    val length = paths.map(p => (p.size, p.foldLeft(0)((acc, c) => acc + c.left + c.right), p))
    val maxLength = length.maxBy(_._1)._1
    length.filter(l => l._1 == maxLength).maxBy(_._2)
  }

  object Part1 {
    def solve(input: List[String]): Int = {
      val (maxStrength, _) = findStrongestPath(findPaths(parseInput(input)))
      maxStrength
    }
  }

  object Part2 {
    def solve(input: List[String]): Int = {
      val (_, maxLength, _) = findLongestPath(findPaths(parseInput(input)))
      maxLength
    }
  }
}
