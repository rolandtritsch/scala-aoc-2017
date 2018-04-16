package aoc

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object Day06Meter extends Bench.OfflineReport {

  val membankGen = for {
    numOfBanks <- Gen.range("numOfBanks")(10, 20, 1)
  } yield {
    List.fill(numOfBanks)(10)
  }

  performance of "Part1" in {
    measure method "solve" in {
      using(membankGen) in {
        b => Day06.Part1.solve(b)
      }
    }
  }

  performance of "Part2" in {
    measure method "solve" in {
      using(membankGen) in {
        b => Day06.Part2.solve(b)
      }
    }
  }
}