package aoc

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object Day08Meter extends Bench.OfflineReport {

  val nullGen = for {
    n <- Gen.range("n")(0, 0, 1)
  } yield {
    n
  }

  performance of "Part1" in {
    measure method "solve" in {
      using(nullGen) in {
        n => Day08.Part1.solve(Day08.input)
      }
    }
  }

  performance of "Part2" in {
    measure method "solve" in {
      using(nullGen) in {
        n => Day08.Part2.solve(Day08.input)
      }
    }
  }
}