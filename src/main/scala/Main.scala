package aoc

object Main {
  def main(args: Array[String]): Unit = {
    println(s"Day1: Captcha -> ${Day1.Part1.captcha(Day1.in)}")
    println(s"Day1: Captcha2 -> ${Day1.Part2.captcha(Day1.in)}")
    println(s"Day2: CheckSum -> ${Day2.checksum(Day2.in)}")
    println(s"Day2: CheckSum2 -> ${Day2.checksum2(Day2.in)}")
    println(s"Day3: CalcDistance -> ${Day3.Part1.calcDistanceFromLocToCenter(Day3.in, Day3.Part1.initGrid(Day3.in))}")
    println(s"Day3: FindNumber -> ${Day3.Part2.findNextBiggestNumber(Day3.in)}")
    println(s"Day4: CountValid -> ${Day4.Part1.countValid(Day4.in)}")
    println(s"Day4: CountValid2 -> ${Day4.Part2.countValid(Day4.in)}")
    println(s"Day5: CountSteps -> ${Day5.Part1.countSteps(Day5.in, 0)}")
    println(s"Day5: CountSteps2 -> ${Day5.Part2.countSteps(Day5.in, 0)._1}")
    println(s"Day6: DetectLoops -> ${Day6.Part1.detectLoop(Day6.in)._1}")
    println(s"Day6: DetectLoops2 -> ${Day6.Part1.detectLoop(Day6.in)._2}")
    println(s"Day7: FindRoot -> ${Day7.Tree.findRoot(Day7.parseInput(Day7.in))}")
    println(s"Day7: FindBadNode -> ${Day7.Tree.solve(Day7.in)}")
    println(s"Day8: FindMaxRegister -> ${Day8.findMaxRegister(Day8.in)}")
    println(s"Day8: FindMaxStack -> ${Day8.findMaxStack(Day8.in)}")
    println(s"Day9: StreamScore -> ${Day9.score(Day9.in)._1.sum}")
    println(s"Day9: StreamGarbageCounter -> ${Day9.score(Day9.in)._2}")
    // @todo Day10
    // @todo Day11
    println(s"Day12: FindPrograms -> ${Day12.findPrograms(0, Day12.parseInput(Day12.in))}")
    println(s"Day12: FindGroups -> ${Day12.findGroups(Day12.parseInput(Day12.in))}")
    println(s"Day13: SecurityScore -> ${Day13.FireWall.runSimulation(Day13.FireWall.build(Day13.parseInput(Day13.in), 0))}")
    // @todo Day13 - Part2 - println(s"Day13: FindWayThrough -> ${Day13.findWayThrough(Day13.parseInput(Day13.in))}")
  }
}
