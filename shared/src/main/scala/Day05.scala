package aoc

/** Problem: [[https://adventofcode.com/2017/day/5]]
  *
  * Solution:
  *
  * General - Walk through the stack and update the stack with the offset()
  * function until the stack counter is/runs out of bounds (which indicates
  * that the "program" has exited.
  *
  * Note: The given program will exit at the higher end of the stack, but
  * for completeness we are also checking for the lower bound.
  *
  * Note: Picking the right [[https://docs.scala-lang.org/overviews/collections/performance-characteristics.html datastructure]] is important (and
  * in our case right means Vector, because it updates in constant time,
  * not linear time (like e.g. List)).
  *
  * Part1 - Trivial. Just increase the stack counter by 1.
  *
  * Part2 - Increase the stack counter as described in the problem statement.
  */
object Day05 {

  val input = Util.readInput("Day05input.txt").map(_.toInt)

  @scala.annotation.tailrec
  def countSteps(stack: Vector[Int], stackCounter: Int, steps: Int, offset: Int => Int): Int = {
    require(stack.nonEmpty, s"stack.nonEmpty failed")
    require(steps >= 1, s"steps >= 1 failed; with >${steps}<")

    def outOfBounds(stack: Vector[Int], stackCounter: Int) = stackCounter + stack(stackCounter) < 0 || stackCounter + stack(stackCounter) >= stack.size

    if(outOfBounds(stack, stackCounter)) steps
    else countSteps(stack.updated(stackCounter, offset(stack(stackCounter))), stackCounter + stack(stackCounter), steps + 1, offset)
  } // can't do this; need tail recursion; ensuring(_ >= 0, s"_ >= 0")

  object Part1 {
    def offset(sc: Int) = sc + 1
    def solve(stack: List[Int]): (Int, Long) = Util.measuredTimeMillis {
      countSteps(stack.toVector, 0, 1, offset)
    }
  }

  object Part2 {
    def offset(sc: Int) = if(sc >= 3) sc - 1 else sc + 1
    def solve(stack: List[Int]): (Int, Long) = Util.measuredTimeMillis {
      countSteps(stack.toVector, 0, 1, offset)
    }
  }
}
