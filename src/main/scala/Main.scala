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
    // @todo Too slow - println(s"Day5: CountSteps -> ${Day5.Part1.countSteps(Day5.in, 0)}")
    // @todo Too slow - println(s"Day5: CountSteps2 -> ${Day5.Part2.countSteps(Day5.in, 0)._1}")
    println(s"Day6: DetectLoops -> ${Day6.Part1.detectLoop(Day6.in)._1}")
    println(s"Day6: DetectLoops2 -> ${Day6.Part1.detectLoop(Day6.in)._2}")
    println(s"Day7: FindRoot -> ${Day7.Tree.findRoot(Day7.parseInput(Day7.in))}")
    println(s"Day7: FindBadNode -> ${Day7.Tree.solve(Day7.in)._1}")
    println(s"Day8: FindMaxRegister -> ${Day8.findMaxRegister(Day8.in)}")
    println(s"Day8: FindMaxStack -> ${Day8.findMaxStack(Day8.in)}")
    println(s"Day9: StreamScore -> ${Day9.score(Day9.in)._1.sum}")
    println(s"Day9: StreamGarbageCounter -> ${Day9.score(Day9.in)._2}")
    println(s"Day10: KnotTheHash -> ${Day10.solve(Day10.in, List.range(0, 256))}")
    // @todo Day10 - Part 2
    println(s"Day11: CalcSteps -> ${Day11.calcSteps(Day11.in)._1}")
    println(s"Day11: CalcMaxSteps -> ${Day11.calcSteps(Day11.in)._2}")
    println(s"Day12: FindPrograms -> ${Day12.findPrograms(0, Day12.parseInput(Day12.in)).size}")
    println(s"Day12: FindGroups -> ${Day12.findGroups(Day12.parseInput(Day12.in)).size}")
    println(s"Day13: SecurityScore -> ${Day13.FireWall.runSimulation(Day13.FireWall.build(Day13.parseInput(Day13.in), 0)).securityScore}")
    // @todo Day13 - Part2 - println(s"Day13: FindWayThrough -> ${Day13.findWayThrough(Day13.parseInput(Day13.in))}")
    // @todo Day14
    // @todo Too slow - println(s"Day15: CountMatchingPairs -> ${Day15.Part1.countMatchingPairs((Day15.Default.startA, Day15.Default.startB), Day15.Default.depth)}")
    // @todo Too slow - println(s"Day15: CountMatchingPairs(modolo) -> ${Day15.Part2.countMatchingPairs((Day15.Default.startA, Day15.Default.startB), Day15.Default.depth2)}")
    println(s"Day16: ExecuteMoves -> ${Day16.executeMoves(Day16.programs, Day16.parseInput(Day16.in))}")
    // @todo Too slow - println(s"Day16: ExecuteDance -> ${Day16.executeDance(Day16.programs, Day16.parseInput(Day16.in), Day16.times)}")
    println(s"Day17: BuildBuffer -> ${Day17.Part1.solve(Day17.steps, Day17.times)}")
    // @todo Too slow - println(s"Day17: BuildBuffer (big) -> ${Day17.Part2.solve(Day17.steps, Day17.times2)}")
    // @todo Refactor to make signature smaller (get rid of the channels) - println(s"Day18: RunProgram -> ${Day18.solveRun(Day18.Program(0, Day18.parseInput(Day18.in), Map.empty[Char, Long].withDefaultValue(0), readChannel, writeChannel))}")
    // @todo Day18 - Part2
    println(s"Day19: WalkTheMaze (path) -> ${Day19.walkTheMaze(Day19.in)._1}")
    println(s"Day19: WalkTheMaze (steps) -> ${Day19.walkTheMaze(Day19.in)._2}")
    println(s"Day20: FindClosest -> ${Day20.findClosest(Day20.run(Day20.parseInput(Day20.in), 1000))}")
    println(s"Day20: NoCollisions -> ${Day20.runWithCollisionDetection(Day20.parseInput(Day20.in), 1000).size}")
    // @todo Day21
    println(s"Day22: SimpleGrid -> ${Day22.run(Day22.SimpleGrid(1001).mapInput(Day22.parseInput(Day22.in)), Day22.Default.ticks1).numOfInfections}")
    println(s"Day22: AdvancedGrid -> ${Day22.run(Day22.AdvancedGrid(1001).mapInput(Day22.parseInput(Day22.in)), Day22.Default.ticks2).numOfInfections}")
    println(s"Day23: RunProgram -> ${Day23.solveRun(Day23.Program(0, Day23.parseInput(Day23.in), Map.empty[Char, Long].withDefaultValue(0), Map.empty[String, Long].withDefaultValue(0)))}")
    // @todo - Day23 - Part2
    // @todo - Day24 - too slow - println(s"Day24: FindStrongestPath -> ${Day24.findStrongestPath(Day24.findPaths(Day24.parseInput(Day24.in)))._1}")
    // @todo - Day24 - too slow - println(s"Day24: FindLongestPath -> ${Day24.findLongestPath(Day24.findPaths(Day24.parseInput(Day24.in)))._2}")
    // @todo - This does not compile with scala-native - println(s"Day25: RunProgram -> ${val tape = Day25.Tape(scala.collection.mutable.ArrayBuffer.fill(100001)(0)); Day25.run(Day25.StateA(tape.size / 2, tape), Day25.in).checkSum}")
  }
}
