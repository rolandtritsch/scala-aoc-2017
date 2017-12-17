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

  "the firewall" should "have the correct states" in {
    val fw = Day13.FireWall.build(Day13.parseInput(testInput))
    //println(fw); println("---")
    fw.securityScore shouldBe 0
    val fw1 = fw.tick
    //println(fw1); println("---")
    fw1.securityScore shouldBe 0
    val fw2 = fw1.tick
    //println(fw2); println("---")
    fw2.securityScore shouldBe 0
    val fw3 = fw2.tick
    //println(fw3); println("---")
    fw3.securityScore shouldBe 0
    val fw4 = fw3.tick
    //println(fw4); println("---")
    fw4.securityScore shouldBe 0
    val fw5 = fw4.tick
    //println(fw5); println("---")
    fw5.securityScore shouldBe 0
    val fw6 = fw5.tick
    //println(fw6); println("---")
    fw6.securityScore shouldBe 0
    val fw7 = fw6.tick
    //println(fw7); println("---")
    fw7.securityScore shouldBe 24
  }

  it should "solve the puzzle" in {
    val fw = Day13.FireWall.build(Day13.parseInput(Day13.in))
    val lastFw = fw.layers.foldLeft(fw)((previousFw, _) => previousFw.tick)
    lastFw.tick.securityScore shouldBe 1632
  }
}
