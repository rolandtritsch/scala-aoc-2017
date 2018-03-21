package aoc

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object Day14Meter extends Bench.OfflineReport {

  val nGen = for {
    n <- Gen.range("n")(1, 5, 1)
  } yield {
    n
  }

  performance of "Part1" in {
    measure method "solve" in {
      using(nGen) in {
        _ => Day14.Part1.solve(Day14.input)
      }
    }
  }

  performance of "Part2" in {
    measure method "solve" in {
      using(nGen) in {
        _ => Day14.Part2.solve(Day14.input)
      }
    }
  }
}