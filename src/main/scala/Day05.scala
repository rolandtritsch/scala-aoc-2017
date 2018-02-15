package aoc

object Day05 {
  val in = Util.readInput("Day5input.txt").map(_.toInt)

  object Part1 {
    def countSteps(stack: List[Int], stackCounter: Int, steps: Int = 1): Int = {
      if(stackCounter + stack(stackCounter) >= stack.size) steps
      else countSteps(stack.updated(stackCounter, stack(stackCounter) + 1), stackCounter + stack(stackCounter), steps + 1)
    }
  }

  object Part2 {
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
