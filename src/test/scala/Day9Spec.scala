package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day9Spec extends FlatSpec with Matchers {

  "score" should "return the right results" in {
    Day9.score("{}".toList)._1.sum shouldBe 1
    Day9.score("{{{}}}".toList)._1.sum shouldBe 6
    Day9.score("{{},{}}".toList)._1.sum shouldBe 5
    Day9.score("{{{},{},{{}}}}".toList)._1.sum shouldBe 16
    Day9.score("{<a>,<a>,<a>,<a>}".toList)._1.sum shouldBe 1
    Day9.score("{{<ab>},{<ab>},{<ab>},{<ab>}}".toList)._1.sum shouldBe 9
    Day9.score("{{<!!>},{<!!>},{<!!>},{<!!>}}".toList)._1.sum shouldBe 9
    Day9.score("{{<a!>},{<a!>},{<a!>},{<ab>}}".toList)._1.sum shouldBe 3
  }

  it should "be able to process garbage" in {
    Day9.score("{<>}".toList)._1.sum shouldBe 1
    Day9.score("{<endanbca{dhbch}{{{{{!!!dndcn}}}}}}}}}<<<<<>}".toList)._1.sum shouldBe 1
    Day9.score("{<<<<>}".toList)._1.sum shouldBe 1
    Day9.score("{<{!>}>}".toList)._1.sum shouldBe 1
    Day9.score("{<!!>}".toList)._1.sum shouldBe 1
    Day9.score("{<!!!>>}".toList)._1.sum shouldBe 1
    Day9.score("{<{o\"i!a,<{i<a>}".toList)._1.sum shouldBe 1
  }

  it should "return the right number of groups" in {
    Day9.score("{}".toList)._1.size shouldBe 1
    Day9.score("{{{}}}".toList)._1.size shouldBe 3
    Day9.score("{{},{}}".toList)._1.size shouldBe 3
    Day9.score("{{{},{},{{}}}}".toList)._1.size shouldBe 6
    Day9.score("{<{},{},{{}}>}".toList)._1.size shouldBe 1
    Day9.score("{<a>,<a>,<a>,<a>}".toList)._1.size shouldBe 1
    Day9.score("{{<a>},{<a>},{<a>},{<a>}}".toList)._1.size shouldBe 5
    Day9.score("{{<!>},{<!>},{<!>},{<a>}}".toList)._1.size shouldBe 2
  }

  it should "solve the puzze" in {
    val (scores, garbageCounter) = Day9.score(Day9.in)
    scores.sum shouldBe 10800
    garbageCounter shouldBe 4522
  }
}
