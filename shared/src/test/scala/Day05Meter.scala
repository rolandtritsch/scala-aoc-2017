package aoc

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object Day05Meter extends Bench.OfflineReport {

  val testStack = List(0, 3, 0, 1, -3)

  val nullGen = Gen.single("null")(0)

  performance of "Part1" in {
    measure method "solve" in {
      using(nullGen) in {
        _ => Day05.Part1.solve(testStack)
      }
    }
  }

  performance of "Part2" in {
    measure method "solve" in {
      using(nullGen) in {
        _ => Day05.Part2.solve(testStack)
      }
    }
  }
}