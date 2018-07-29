package aoc

/** Problem: [[https://adventofcode.com/2017/day/7]]
  *
  * Solution:
  *
  * General - This is a tree searching problem.
  *
  * Part1 - The root is the only node with no parent, means
  * it is never referenced as a child, means we can find it
  * by doing the diff between all nodes and all children.
  *
  * Part2 - Find the root. Go through the tree (top-down) and
  * find the (bad) node, where the tree unbalanced (for the
  * first time). Find the wrong weight (the one that is
  * different to the other ones) and calc you to correct it.
  */

object Day07 {

  val input = Util.readInput("Day07input.txt")

  abstract class ParseElement {
    def name: String
    def weight: Int
  }
  case class ParseNode(name: String, weight: Int, children: List[String]) extends ParseElement
  case class ParseLeaf(name: String, weight: Int) extends ParseElement

  def parseInput(lines: List[String]): List[ParseElement] = {
    def parseLine(line: String): ParseElement = {
      // fwft (72) -> ktlj, cntj, xhth
      val tokens = line.split("[ (),]")
      val name = tokens(0)
      val weight = tokens(2).toInt
      if(tokens.size > 3) {
        val children= tokens.drop(5).filterNot(_.isEmpty).toList
        ParseNode(name, weight, children)
      } else {
        ParseLeaf(name, weight)
      }
    }
    lines.map(parseLine)
  }

  object Tree {
    abstract class Element {
      def name: String
      def weight: Int
      def toString(level: Int): String
      override def toString: String = s"${name}/${weight}"
    }
    case class Node(name: String, weight: Int, children: List[Element]) extends Element {
      def toString(level: Int): String = {
        val indent = List.fill(level)("  ").mkString
        indent + s"${name}/${weight}" + "\n" + children.map(_.toString(level + 1)).mkString("\n")
      }
    }
    case class Leaf(name: String, weight: Int) extends Element {
      def toString(level: Int): String = {
        val indent = List.fill(level)("  ").mkString
        indent + s"${name}/${weight}"
      }
    }

    def findRoot(pnodes: List[ParseElement]): String = {
      require(pnodes.nonEmpty, s"pnodes.nonEmpty failed")

      val allNames = pnodes.map(_.name)
      val allChildrenNames = pnodes.collect {case n: ParseNode => n.children}.flatten
      assert(allNames.size >= allChildrenNames.size, s"failed; with >${allNames.size}< >${allChildrenNames.size}<")

      val rootNames = allNames.diff(allChildrenNames)
      assert(rootNames.size == 1, s"rootNames.size == 1 failed; with >${rootNames.size}<")

      rootNames.head
    } ensuring(!pnodes.collect {case n: ParseNode => n.children}.flatten.contains(_), s"!pnodes.collect {case n: ParseNode => n.children}.flatten.contains(_) failed")

    def build(name: String, pnodes: List[ParseElement]): Element = {
      require(name.nonEmpty, s"name.nonEmpty failed")
      require(pnodes.nonEmpty, s"pnodes.nonEmpty failed")
      require(pnodes.map(_.name).contains(name), s"pnodes.map(_.name).contains(name) failed")

      val pn = pnodes.find(p => p.name == name).get
      pn match {
        case ParseLeaf(n, w) => Leaf(n, w)
        case ParseNode(n, w, c) => Node(n, w, c.map(build(_, pnodes)))
      }
    }

    def calcWeight(node: Element): Int = node match {
      case Leaf(_, w) => w
      case Node(_, w, c) => w + c.map(calcWeight).sum
    }

    def isBalanced(node: Element): Boolean = node match {
      case Leaf(_, _) => true
      case Node(_, _, c) => {
        val checkSum = calcWeight(c.head)
        c.forall(calcWeight(_) == checkSum)
      }
    }

    def findBadNode(node: Element): Node = node match {
      case l: Leaf => assert(false); Node("", 0, List())
      case n: Node => {
        if(!isBalanced(n) && n.children.forall(isBalanced)) n
        else n.children.find(!isBalanced(_)) match {
          case None => assert(false); Node("", 0, List())
          case Some(n) => findBadNode(n)
        }
      }
    }

    def toString(node: Node): String = {
      node + " -> " + node.children.map(c => c.toString + s"/${calcWeight(c)}").mkString("-")
    }
  }

  object Part1 {
    def solve(input: List[String]): (String, Long) = Util.measuredTimeMillis {
      Tree.findRoot(parseInput(input))
    }
  }

  object Part2 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      val pnodes = parseInput(input)
      val root = Tree.build(Day07.Tree.findRoot(pnodes), pnodes)
      val badNode = Tree.findBadNode(root)

      val nodesByWeight = badNode.children.groupBy(Tree.calcWeight(_))
      assert(nodesByWeight.size == 2, s"nodesByWeight.size == 2 failed; with >${nodesByWeight.size}<")

      val nodesByOccurences = nodesByWeight.map {case (w, ns) => (w, ns.size, ns.head)}
      val (_, _, bad) = nodesByOccurences.find {case (_, occurences, _) => occurences == 1}.get
      val (_, _, good) = nodesByOccurences.find {case (_, occurences, _) => occurences > 1}.get
      val (name, badWeight, correctWeight) = (bad.name, bad.weight, bad.weight - (Tree.calcWeight(bad) - Tree.calcWeight(good)))

      correctWeight
    }
  }
}
