package aoc

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object Day03Meter extends Bench.OfflineReport {

  val startRange = 100
  val numGen = for {
    num <- Gen.range("num")(startRange, startRange + 10, 1)
  } yield {
    num
  }

  performance of "Part1" in {
    measure method "solve" in {
      using(numGen) in {
        n => Day03.Part1.solve(n)
      }
    }
  }

  performance of "Part2" in {
    measure method "solve" in {
      using(numGen) in {
        n => Day03.Part2.solve(n)
      }
    }
  }
}
