package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day09Spec extends FlatSpec with Matchers {

  behavior of "readInput"
  it should "read the input" in {
    Day09.in.take(10) should be ("{{{{{},{<>".toList)
  }

  behavior of "score"
  it should "return the right results" in {
    Day09.score("{}".toList)._1.sum shouldBe 1
    Day09.score("{{{}}}".toList)._1.sum shouldBe 6
    Day09.score("{{},{}}".toList)._1.sum shouldBe 5
    Day09.score("{{{},{},{{}}}}".toList)._1.sum shouldBe 16
    Day09.score("{<a>,<a>,<a>,<a>}".toList)._1.sum shouldBe 1
    Day09.score("{{<ab>},{<ab>},{<ab>},{<ab>}}".toList)._1.sum shouldBe 9
    Day09.score("{{<!!>},{<!!>},{<!!>},{<!!>}}".toList)._1.sum shouldBe 9
    Day09.score("{{<a!>},{<a!>},{<a!>},{<ab>}}".toList)._1.sum shouldBe 3
  }

  it should "be able to process garbage" in {
    Day09.score("{<>}".toList)._1.sum shouldBe 1
    Day09.score("{<endanbca{dhbch}{{{{{!!!dndcn}}}}}}}}}<<<<<>}".toList)._1.sum shouldBe 1
    Day09.score("{<<<<>}".toList)._1.sum shouldBe 1
    Day09.score("{<{!>}>}".toList)._1.sum shouldBe 1
    Day09.score("{<!!>}".toList)._1.sum shouldBe 1
    Day09.score("{<!!!>>}".toList)._1.sum shouldBe 1
    Day09.score("{<{o\"i!a,<{i<a>}".toList)._1.sum shouldBe 1
  }

  it should "return the right number of groups" in {
    Day09.score("{}".toList)._1.size shouldBe 1
    Day09.score("{{{}}}".toList)._1.size shouldBe 3
    Day09.score("{{},{}}".toList)._1.size shouldBe 3
    Day09.score("{{{},{},{{}}}}".toList)._1.size shouldBe 6
    Day09.score("{<{},{},{{}}>}".toList)._1.size shouldBe 1
    Day09.score("{<a>,<a>,<a>,<a>}".toList)._1.size shouldBe 1
    Day09.score("{{<a>},{<a>},{<a>},{<a>}}".toList)._1.size shouldBe 5
    Day09.score("{{<!>},{<!>},{<!>},{<a>}}".toList)._1.size shouldBe 2
  }

  it should "solve the puzze" in {
    val (scores, garbageCounter) = Day09.score(Day09.in)
    scores.sum shouldBe 10800
    garbageCounter shouldBe 4522
  }
}
