package aoc

object Main {
  def main(args: Array[String]): Unit = {
    println(s"Day1: Captcha -> ${Day1.captcha(Day1.in)}")
    println(s"Day1: Captcha2 -> ${Day1.captcha2(Day1.in)}")
    println(s"Day2: CheckSum -> ${Day2.checksum(Day2.in)}")
    println(s"Day2: CheckSum2 -> ${Day2.checksum2(Day2.in)}")
    println(s"Day3: CalcDistance -> ${Day3.Part1.calcDistanceFromLocToCenter(Day3.in, Day3.Part1.initGrid(Day3.in))}")
    println(s"Day3: FindNumber -> ${Day3.Part2.findNextBiggestNumber(Day3.in)}")
    println(s"Day4: CountValid -> ${Day4.Part1.countValid(Day4.in)}")
    println(s"Day4: CountValid2 -> ${Day4.Part2.countValid(Day4.in)}")
    println(s"Day5: CountSteps -> ${Day5.Part1.countSteps(Day5.in, 0)}")
    println(s"Day5: CountSteps2 -> ${Day5.Part2.countSteps(Day5.in, 0)}")
  }
}
