package aoc

import scala.annotation.tailrec

object Day5 {
  val fileURL = getClass.getResource(".") + "/Day5input.txt"
  val in = Util.readInputURL(fileURL).map(_.toInt)

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
