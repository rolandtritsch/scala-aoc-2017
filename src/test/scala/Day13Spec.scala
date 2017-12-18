package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day13Spec extends FlatSpec with Matchers {
  val testInput = List(
    "0: 3",
    "1: 2",
    "4: 4",
    "6: 4"
  )

  "the input" should "be correct" in {
    Day13.in.head shouldBe "0: 4"
  }

  it should "parse correctly" in {
    val parsed = Day13.parseInput(Day13.in)
    parsed(0) shouldBe 4
    parsed(98) shouldBe 18
  }

  "a security scanner" should "behave as expected" in {
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

  "a layer" should "behave as expected" in {
    val l0 = Day13.Layer(0, Day13.SecurityScanner(1))
    l0.tick.securityScanner.isTop shouldBe true
  }

  "building a firewall" should "return a/the correct firewall" in {
    val fw = Day13.FireWall.build(Day13.parseInput(testInput), 0)
    //println(fw); println("---")
    fw.threatDetected shouldBe false
    //println(fw); println("---")
    fw.tick.threatDetected shouldBe true
  }

  "the firewall" should "have the correct states" in {
    val fw = Day13.FireWall.build(Day13.parseInput(testInput), 0)
    fw.securityScore shouldBe 0
    fw.threatDetected shouldBe false

    val fw1 = fw.tick
    //println(fw1); println("---")
    fw1.securityScore shouldBe 0
    fw1.threatDetected shouldBe true

    val fw2 = fw1.tick
    //println(fw2); println("---")
    fw2.securityScore shouldBe 0
    fw2.threatDetected shouldBe true

    val fw3 = fw2.tick
    //println(fw3); println("---")
    fw3.securityScore shouldBe 0
    fw3.threatDetected shouldBe true

    val fw4 = fw3.tick
    //println(fw4); println("---")
    fw4.securityScore shouldBe 0
    fw4.threatDetected shouldBe true

    val fw5 = fw4.tick
    //println(fw5); println("---")
    fw5.securityScore shouldBe 0
    fw5.threatDetected shouldBe true

    val fw6 = fw5.tick
    //println(fw6); println("---")
    fw6.securityScore shouldBe 0
    fw6.threatDetected shouldBe true

    val fw7 = fw6.tick
    //println(fw7); println("---")
    fw7.securityScore shouldBe 24
    fw7.threatDetected shouldBe true
  }

  it should "solve the puzzle" in {
    val fw = Day13.FireWall.build(Day13.parseInput(Day13.in), 0)
    val resultFw = fw.layers.foldLeft(fw)((previousFw, _) => previousFw.tick).tick
    resultFw.securityScore shouldBe 1632
    resultFw.threatDetected shouldBe true

    val simResultFw = Day13.FireWall.runSimulation(fw)
    simResultFw.securityScore shouldBe 1632
    simResultFw.threatDetected shouldBe true
  }

  it should "be able to get through the test input with a delay of 10" in {
    val fw = Day13.FireWall.build(Day13.parseInput(testInput), 10)
    val resultFw = fw.layers.foldLeft(fw)((previousFw, _) => previousFw.tick).tick
    resultFw.securityScore shouldBe 0
    resultFw.threatDetected shouldBe false

    val simResultFw = Day13.FireWall.runSimulation(fw)
    simResultFw.securityScore shouldBe 0
    simResultFw.threatDetected shouldBe false
  }

  "findWayThrough" should "find the smallest possible delay" in {
    Day13.findWayThrough(Day13.parseInput(testInput)) shouldBe 10
  }

  ignore should "solve the puzzle" in {
    Day13.findWayThrough(Day13.parseInput(Day13.in)) shouldBe 0
  }
}
