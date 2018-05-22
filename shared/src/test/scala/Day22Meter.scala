package aoc

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object Day22Meter extends Bench.OfflineReport {

  val nGen = for {
    n <- Gen.range("n")(1, 1, 1)
  } yield {
    n
  }

  performance of "Part1" in {
    measure method "solve" in {
      using(nGen) in {
        _ => Day22.Part1.solve(Day22.input)
      }
    }
  }

  performance of "Part2" in {
    measure method "solve" in {
      using(nGen) in {
        _ => Day22.Part2.solve(Day22.input)
      }
    }
  }
}
