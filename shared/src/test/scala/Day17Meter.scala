package aoc

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object Day17Meter extends Bench.OfflineReport {

  val nGen = for {
    n <- Gen.range("n")(1, 1, 1)
  } yield {
    n
  }

  performance of "Part1" in {
    measure method "solve" in {
      using(nGen) in {
        _ => Day17.Part1.solve(Day17.steps, Day17.times)
      }
    }
  }

  performance of "Part2" in {
    measure method "solve" in {
      using(nGen) in {
        _ => Day17.Part2.solve(Day17.steps, Day17.times2)
      }
    }
  }
}
