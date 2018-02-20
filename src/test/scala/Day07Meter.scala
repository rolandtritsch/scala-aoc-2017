package aoc

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object Day07Meter extends Bench.OfflineReport {

  val treeGen = for {
    n <- Gen.range("trees")(1, 10, 1)
  } yield {
    n
  }

  performance of "Part1" in {
    measure method "solve" in {
      using(treeGen) in {
        n => Day07.Part1.solve(Day07.input)
      }
    }
  }

  performance of "Part2" in {
    measure method "solve" in {
      using(treeGen) in {
        n => Day07.Part2.solve(Day07.input)
      }
    }
  }
}