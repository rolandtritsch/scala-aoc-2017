package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day15Spec extends FlatSpec with Matchers {

  val testAstart = 65
  val testBstart = 8921

  val testDecPairs = List(
    (testAstart, testBstart),
    (1092455,430625591),
    (1181022009,1233683848),
    (245556042,1431495498),
    (1744312007,137874439),
    (1352636452,285222916)
  )

  val test2DecPairs = List(
    (testAstart, testBstart),
    (1352636452, 1233683848),
    (1992081072, 862516352),
    (530830436, 1159784568),
    (1980017072, 1616057672),
    (740335192, 412269392)
  )

  behavior of "generator()"
  it should "produce the correct pairs of numbers for the testcase(s)" in {
    val genA1 = Day15.GeneratorConfig(testAstart, Day15.Default.factorA, Day15.Default.devider, 1, Day15.Default.next)
    val genB1 = Day15.GeneratorConfig(testBstart, Day15.Default.factorB, Day15.Default.devider, 1, Day15.Default.next)
    Day15.generator(genA1, genB1).take(6).toList should be (testDecPairs)

    val genA2 = Day15.GeneratorConfig(testAstart, Day15.Default.factorA, Day15.Default.devider, 4, Day15.Default.next)
    val genB2 = Day15.GeneratorConfig(testBstart, Day15.Default.factorB, Day15.Default.devider, 8, Day15.Default.next)
    Day15.generator(genA2, genB2).take(6).toList should be (test2DecPairs)
  }

  behavior of "countMatchingPairs()"
  it should "return the correct result(s) for the testcase(s)" taggedAs (BuildTest) in {
    val genA1 = Day15.GeneratorConfig(testAstart, Day15.Default.factorA, Day15.Default.devider, 1, Day15.Default.next)
    val genB1 = Day15.GeneratorConfig(testBstart, Day15.Default.factorB, Day15.Default.devider, 1, Day15.Default.next)
    val gen3 = Day15.generator(genA1, genB1)
    Day15.countMatchingPairs(gen3, 3) shouldBe 0

    val gen4 = Day15.generator(genA1, genB1)
    Day15.countMatchingPairs(gen4, 4) shouldBe 1

    val gen1000000 = Day15.generator(genA1, genB1)
    Day15.countMatchingPairs(gen1000000, 1000000) shouldBe 12

    val genDefaultDepth = Day15.generator(genA1, genB1)
    Day15.countMatchingPairs(genDefaultDepth, 40000000) shouldBe 588

    val genA2 = Day15.GeneratorConfig(testAstart, Day15.Default.factorA, Day15.Default.devider, 4, Day15.Default.next)
    val genB2 = Day15.GeneratorConfig(testBstart, Day15.Default.factorB, Day15.Default.devider, 8, Day15.Default.next)
    val genDefaultDepth2 = Day15.generator(genA2, genB2)
    Day15.countMatchingPairs(genDefaultDepth2, 5000000) shouldBe 309

  }

  behavior of "solve() - Part1"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day15.Part1.solve._1 shouldBe 594
  }

  behavior of "solve() - Part2"
  it should "solve the puzzle" taggedAs(SolutionTest) in {
    Day15.Part2.solve._1 shouldBe 328
  }
}
