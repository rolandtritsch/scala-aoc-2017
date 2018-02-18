package aoc

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object Day02Meter extends Bench.OfflineReport {

  val sheetGen = for {
    rows <- Gen.range("rows")(2, 10, 1)
    cols <- Gen.range("cols")(2, 10, 1)
    number <- Gen.range("number")(0, 99, 1)
  } yield {
    (for(_ <- 1 to rows) yield List.fill(cols)(number)).toList
  }

  performance of "Part1" in {
    measure method "solve" in {
      using(sheetGen) in {
        s => Day02.Part1.solve(s)
      }
    }
  }
}