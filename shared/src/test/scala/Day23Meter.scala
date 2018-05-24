package aoc

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object Day23Meter extends Bench.OfflineReport {

  val nGen = for {
    n <- Gen.range("n")(1, 1, 1)
  } yield {
    n
  }

  performance of "Part1" in {
    measure method "solve" in {
      using(nGen) in {
        _ => Day23.Part1.solve(Day23.input)
      }
    }
  }

  performance of "Part1" in {
    measure method "solve" in {
      using(nGen) in {
        _ => Day23.Part1.solve(Day23.input)
      }
    }
  }
}
