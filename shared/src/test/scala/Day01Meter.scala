package aoc

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object Day01Meter extends Bench.OfflineReport {

  def randomDigitString(size: Int): String = {
    val r = new util.Random(System.currentTimeMillis)
    (for(_ <- 1 to size) yield r.nextInt(10)).mkString
  }

  val ss = for {
    size <- Gen.range("size")(2, 100, 2)
    string <- Gen.single("string")(randomDigitString(size))
  } yield string

  performance of "Part1" in {
    measure method "captcha" in {
      using(ss) in {
        s => Day01.Part1.solve(s)
      }
    }
  }

  performance of "Part2" in {
    measure method "captcha" in {
      using(ss) in {
        s => Day01.Part2.solve(s)
      }
    }
  }
}