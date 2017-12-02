package aoc

object Main {
  def main(args: Array[String]): Unit = {
    println(s"Day1: Captcha -> ${Day1.captcha(Day1.in)}")
    println(s"Day1: Captcha2 -> ${Day1.captcha2(Day1.in)}")
    println(s"Day2: CheckSum -> ${Day2.checksum(Day2.in)}")
    println(s"Day2: CheckSum2 -> ${Day2.checksum2(Day2.in)}")
  }
}
