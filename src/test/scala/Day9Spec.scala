package aoc

import org.scalatest.{FlatSpec, Matchers}

class Day9Spec extends FlatSpec with Matchers {

  "score" should "return the right results" in {
    Day9.score("{}".toList).sum shouldBe 1
    Day9.score("{{{}}}".toList).sum shouldBe 6
    Day9.score("{{},{}}".toList).sum shouldBe 5
    Day9.score("{{{},{},{{}}}}".toList).sum shouldBe 16
    Day9.score("{<a>,<a>,<a>,<a>}".toList).sum shouldBe 1
    Day9.score("{{<ab>},{<ab>},{<ab>},{<ab>}}".toList).sum shouldBe 9
    Day9.score("{{<!!>},{<!!>},{<!!>},{<!!>}}".toList).sum shouldBe 9
    Day9.score("{{<a!>},{<a!>},{<a!>},{<ab>}}".toList).sum shouldBe 3
  }

  it should "be able to process garbage" in {
    Day9.score("{<>}".toList).sum shouldBe 1
    Day9.score("{<endanbca{dhbch}{{{{{!!!dndcn}}}}}}}}}<<<<<>}".toList).sum shouldBe 1
    Day9.score("{<<<<>}".toList).sum shouldBe 1
    Day9.score("{<{!>}>}".toList).sum shouldBe 1
    Day9.score("{<!!>}".toList).sum shouldBe 1
    Day9.score("{<!!!>>}".toList).sum shouldBe 1
    Day9.score("{<{o\"i!a,<{i<a>}".toList).sum shouldBe 1
  }

  it should "return the right number of groups" in {
    Day9.score("{}".toList).size shouldBe 1
    Day9.score("{{{}}}".toList).size shouldBe 3
    Day9.score("{{},{}}".toList).size shouldBe 3
    Day9.score("{{{},{},{{}}}}".toList).size shouldBe 6
    Day9.score("{<{},{},{{}}>}".toList).size shouldBe 1
    Day9.score("{<a>,<a>,<a>,<a>}".toList).size shouldBe 1
    Day9.score("{{<a>},{<a>},{<a>},{<a>}}".toList).size shouldBe 5
    Day9.score("{{<!>},{<!>},{<!>},{<a>}}".toList).size shouldBe 2
  }

  it should "solve the puzze" in {
    Day9.score(Day9.in).sum shouldBe 10800
  }


}
