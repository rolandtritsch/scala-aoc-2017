package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day07Spec extends FlatSpec with Matchers {

  val testInput = List(
    "pbga (66)",
    "xhth (57)",
    "ebii (61)",
    "havc (66)",
    "ktlj (57)",
    "fwft (72) -> ktlj, cntj, xhth",
    "qoyq (66)",
    "padx (45) -> pbga, havc, qoyq",
    "tknk (41) -> ugml, padx, fwft",
    "jptl (61)",
    "ugml (68) -> gyxo, ebii, jptl",
    "gyxo (61)",
    "cntj (57)"
  )

  val testInputBalanced = List(
    "pbga (66)",
    "xhth (57)",
    "ebii (61)",
    "havc (66)",
    "ktlj (57)",
    "fwft (72) -> ktlj, cntj, xhth",
    "qoyq (66)",
    "padx (45) -> pbga, havc, qoyq",
    "tknk (41) -> ugml, padx, fwft",
    "jptl (61)",
    "ugml (60) -> gyxo, ebii, jptl",
    "gyxo (61)",
    "cntj (57)"
  )

  val testNodes = List(
    Day07.ParseLeaf("pbga", 66),
    Day07.ParseLeaf("xhth", 57),
    Day07.ParseLeaf("ebii", 61),
    Day07.ParseLeaf("havc", 66),
    Day07.ParseLeaf("ktlj", 57),
    Day07.ParseNode("fwft", 72, List("ktlj", "cntj", "xhth")),
    Day07.ParseLeaf("qoyq", 66),
    Day07.ParseNode("padx", 45, List("pbga", "havc", "qoyq")),
    Day07.ParseNode("tknk", 41, List("ugml", "padx", "fwft")),
    Day07.ParseLeaf("jptl", 61),
    Day07.ParseNode("ugml", 68, List("gyxo", "ebii", "jptl")),
    Day07.ParseLeaf("gyxo", 61),
    Day07.ParseLeaf("cntj", 57)
  )

  behavior of "readInput()"
  it should "read the input" in {
    Day07.input.take(1) should be(List("occxa (60)"))
  }

  behavior of "parseInput()"
  it should "return a/the list of nodes" in {
    Day07.parseInput(testInput).size shouldBe testInput.size
    Day07.parseInput(testInput) should be(testNodes)

    Day07.parseInput(Day07.input).size shouldBe Day07.input.size
  }

  behavior of "solve() - Part1"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day07.Part1.solve(testInput)._1 shouldBe "tknk"
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day07.Part1.solve(Day07.input)._1 shouldBe "uownj"
  }

  behavior of "build()"
  it should "build the tree" in {
    val pnodes = Day07.parseInput(testInput)
    val root = Day07.Tree.build(Day07.Tree.findRoot(pnodes), pnodes)
    root.name shouldBe "tknk"
  }

  it should "build the tree from the input" in {
    val pnodes = Day07.parseInput(Day07.input)
    val root = Day07.Tree.build(Day07.Tree.findRoot(pnodes), pnodes)
    root.name shouldBe "uownj"
  }

  behavior of "isBalanced()"
  it should "fail on the unbalanced testInput" in {
    val pnodes = Day07.parseInput(testInput)
    val root = Day07.Tree.build(Day07.Tree.findRoot(pnodes), pnodes)
    Day07.Tree.isBalanced(root) shouldBe false
  }

  it should "succeed on the balanced testInput" in {
    val pnodes = Day07.parseInput(testInputBalanced)
    val root = Day07.Tree.build(Day07.Tree.findRoot(pnodes), pnodes)
    Day07.Tree.isBalanced(root) shouldBe true
  }

  it should "fail on the input" in {
    val pnodes = Day07.parseInput(Day07.input)
    val root = Day07.Tree.build(Day07.Tree.findRoot(pnodes), pnodes)
    Day07.Tree.isBalanced(root) shouldBe false
  }

  behavior of "solve() - Part2"
  it should "solve the testcase(s)" taggedAs(BuildTest) in {
    Day07.Part2.solve(testInput)._1 shouldBe 60
  }

  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day07.Part2.solve(Day07.input)._1 shouldBe 596
  }
}
