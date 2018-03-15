package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day13Spec extends FlatSpec with Matchers {

  val testInput = List(
    "0: 3",
    "1: 2",
    "4: 4",
    "6: 4"
  )

  behavior of "readInput()"
  it should "be correct" in {
    Day13.input.head shouldBe "0: 4"
  }

  behavior of "parseInput()"
  it should "parse correctly" in {
    val parsed = Day13.parseInput(Day13.input)
    parsed(0) shouldBe 4
    parsed(98) shouldBe 18
  }

  behavior of "a SecurityScanner()"
  it should "behave as expected" in {
    val ss0 = Day13.SecurityScanner(0)
    ss0.isTop shouldBe false
    ss0.tick.isTop shouldBe false

    val ss1 = Day13.SecurityScanner(1)
    ss1.isTop shouldBe true
    ss1.tick.isTop shouldBe true
    ss1.tick.direction shouldBe Day13.Direction.DOWN

    val ss2 = Day13.SecurityScanner(3)
    ss2.isTop shouldBe true
    ss2.tick.isTop shouldBe false
    ss2.tick.direction shouldBe Day13.Direction.DOWN
    ss2.tick.tick.isTop shouldBe false
    ss2.tick.tick.direction shouldBe Day13.Direction.DOWN
    ss2.tick.tick.tick.isTop shouldBe false
    ss2.tick.tick.tick.direction shouldBe Day13.Direction.UP
    ss2.tick.tick.tick.tick.isTop shouldBe true
    ss2.tick.tick.tick.tick.direction shouldBe Day13.Direction.UP
    ss2.tick.tick.tick.tick.tick.isTop shouldBe false
    ss2.tick.tick.tick.tick.tick.direction shouldBe Day13.Direction.DOWN
  }

  behavior of "a Layer()"
  it should "behave as expected" in {
    val l0 = Day13.Layer(0, Day13.SecurityScanner(1))
    l0.tick.securityScanner.isTop shouldBe true
  }

  behavior of "building a Firewall()"
  it should "return a/the correct firewall" in {
    val fw = Day13.FireWall.build(Day13.parseInput(testInput), 0)
    fw.threatDetected shouldBe false
    fw.tick.threatDetected shouldBe true
  }

  behavior of "a Firewall()"
  it should "have the correct states" in {
    val fw = Day13.FireWall.build(Day13.parseInput(testInput), 0)
    fw.securityScore shouldBe 0
    fw.threatDetected shouldBe false

    val fw1 = fw.tick
    fw1.securityScore shouldBe 0
    fw1.threatDetected shouldBe true

    val fw2 = fw1.tick
    fw2.securityScore shouldBe 0
    fw2.threatDetected shouldBe true

    val fw3 = fw2.tick
    fw3.securityScore shouldBe 0
    fw3.threatDetected shouldBe true

    val fw4 = fw3.tick
    fw4.securityScore shouldBe 0
    fw4.threatDetected shouldBe true

    val fw5 = fw4.tick
    fw5.securityScore shouldBe 0
    fw5.threatDetected shouldBe true

    val fw6 = fw5.tick
    fw6.securityScore shouldBe 0
    fw6.threatDetected shouldBe true

    val fw7 = fw6.tick
    fw7.securityScore shouldBe 24
    fw7.threatDetected shouldBe true
  }

  it should "solve the puzzle" in {
    val fw = Day13.FireWall.build(Day13.parseInput(Day13.input), 0)
    val resultFw = fw.layers.foldLeft(fw)((previousFw, _) => previousFw.tick).tick
    resultFw.securityScore shouldBe 1632
    resultFw.threatDetected shouldBe true
  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day13.Part1.solve(Day13.input) shouldBe 1632
  }

  behavior of "solve() - Part2"
  it should "solve the testcase(s)" in {
    Day13.Part2.solve(testInput) shouldBe 10
  }

  ignore should "solve the puzzle" taggedAs(SolutionTest) in {
    Day13.Part2.solve(Day13.input) shouldBe 0
  }
}
