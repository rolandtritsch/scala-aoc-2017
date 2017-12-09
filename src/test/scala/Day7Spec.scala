package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day7Spec extends FlatSpec with Matchers {

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
    Day7.ParseLeaf("pbga", 66),
    Day7.ParseLeaf("xhth", 57),
    Day7.ParseLeaf("ebii", 61),
    Day7.ParseLeaf("havc", 66),
    Day7.ParseLeaf("ktlj", 57),
    Day7.ParseNode("fwft", 72, List("ktlj", "cntj", "xhth")),
    Day7.ParseLeaf("qoyq", 66),
    Day7.ParseNode("padx", 45, List("pbga", "havc", "qoyq")),
    Day7.ParseNode("tknk", 41, List("ugml", "padx", "fwft")),
    Day7.ParseLeaf("jptl", 61),
    Day7.ParseNode("ugml", 68, List("gyxo", "ebii", "jptl")),
    Day7.ParseLeaf("gyxo", 61),
    Day7.ParseLeaf("cntj", 57)
  )

  "readInput" should "read the input" in {
    val input = List("occxa (60)")
    Day7.readInput(Day7.fileName).take(1) should be(input)
  }

  "parseInput" should "return a/the list of nodes" in {
    Day7.parseInput(testInput).size shouldBe testInput.size
    Day7.parseInput(testInput) should be(testNodes)

    Day7.parseInput(Day7.in).size shouldBe Day7.in.size
  }

  "findRoot" should "return the right result(s)" in {
    Day7.Tree.findRoot(Day7.parseInput(testInput)) shouldBe "tknk"
  }

  it should "solve the puzzle" in {
    Day7.Tree.findRoot(Day7.parseInput(Day7.in)) shouldBe "uownj"
  }

  "build" should "build the tree" in {
    val pnodes = Day7.parseInput(testInput)
    val root = Day7.Tree.build(Day7.Tree.findRoot(pnodes), pnodes)
    root.name shouldBe "tknk"
    //println(root.toString(0))
  }

  it should "build the tree from the input" in {
    val pnodes = Day7.parseInput(Day7.in)
    val root = Day7.Tree.build(Day7.Tree.findRoot(pnodes), pnodes)
    root.name shouldBe "uownj"
    //println(root.toString(0))
  }

  "isBalanced" should "fail on the testInput" in {
    val pnodes = Day7.parseInput(testInput)
    val root = Day7.Tree.build(Day7.Tree.findRoot(pnodes), pnodes)
    Day7.Tree.isBalanced(root) shouldBe false
  }

  it should "succeed on the balanced testInput" in {
    val pnodes = Day7.parseInput(testInputBalanced)
    val root = Day7.Tree.build(Day7.Tree.findRoot(pnodes), pnodes)
    Day7.Tree.isBalanced(root) shouldBe true
  }

  it should "fail on the input" in {
    val pnodes = Day7.parseInput(Day7.in)
    val root = Day7.Tree.build(Day7.Tree.findRoot(pnodes), pnodes)
    Day7.Tree.isBalanced(root) shouldBe false
  }

  "solve" should "find the bad node" in {
    Day7.Tree.solve(testInput) shouldBe ("ugml", 68, 60)
  }

  it should "find the bad node in the input" in {
    Day7.Tree.solve(Day7.in) shouldBe ("mfzpvpj", 604, 596)
  }
}
