package aoc

import scala.annotation.tailrec

object Day5 {
  //val fileName = getClass.getResource(".") + "/Day4input.txt"
  val fileName = "./src/main/resources" + "/Day5input.txt"

  def readInput(fileName: String): List[Int] = {
    require(fileName.nonEmpty, s"fileName.nonEmpty failed; with >${fileName}<")
    require(new java.io.File(fileName).exists, s"java.io.File(fileName).exists failed; with >${fileName}<")

    scala.io.Source.fromFile(fileName).getLines().toList.map(_.toInt)
  }

  val in = readInput(fileName)

  object Part1 {
    @tailrec
    def countSteps(stack: List[Int], stackCounter: Int, steps: Int = 1): Int = {
      if(stackCounter + stack(stackCounter) >= stack.size) steps
      else countSteps(stack.updated(stackCounter, stack(stackCounter) + 1), stackCounter + stack(stackCounter), steps + 1)
    }
  }

  object Part2 {
    @tailrec
    def countSteps(stack: List[Int], stackCounter: Int, steps: Int = 1): (Int, List[Int]) = {
      if(stackCounter + stack(stackCounter) >= stack.size) (steps, stack)
      else countSteps(
        stack.updated(
          stackCounter,
          if(stack(stackCounter) >= 3) stack(stackCounter) - 1 else stack(stackCounter) + 1
        ),
        stackCounter + stack(stackCounter),
        steps + 1
      )
    }
  }
}
