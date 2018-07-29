package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day12Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  // 0 <-> 1, 2, ...
  // 1 <-> 1
  // 2 <-> 2
  val pipeGen = for {
    n <- Gen.choose(1, 10)
  } yield {
    (List(s"0 <-> ${(1 to n).toList.mkString(", ")}") ++ (for(i <- 1 to n) yield s"${i} <-> ${i}").toList, n)
  }

  property("Any testpipe should return n") {
    forAll(pipeGen) {case (input, n) => {
      Day12.Part1.solve(input)._1 shouldBe (n + 1)
      Day12.Part2.solve(input)._1 shouldBe 1
    }}
  }
}
