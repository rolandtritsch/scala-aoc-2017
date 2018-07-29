package aoc

/** Problem: [[http://adventofcode.com/2017/day/19]]
  *
  * Solution:
  *
  * General - Using a very simple state machine, that takes the current field
  * and the current direction and (based on these two inputs) decide what the
  * next state is. We can then (recursively) process states until we get out
  * of the maze.
  *
  * Part1 - Collect all letters, while walking the maze.
  *
  * Part2 - Count the steps, while walking the maze.
  *
  */
object Day19 {

  val input = Util.readInput("Day19input.txt").map(_.toCharArray).toArray

  object Direction {
    val UP = 'U'
    val DOWN = 'D'
    val LEFT = 'L'
    val RIGHT = 'R'
  }

  type DirectionType = Char

  case class State(row: Int, col: Int, direction: DirectionType, maze: Array[Array[Char]], steps: Int, path: String, done: Boolean) {
    def next: State = maze(row)(col) match {
      case '|' if(direction == Direction.UP) => State(row - 1, col, Direction.UP, maze, steps + 1, path, false)
      case '|' if(direction == Direction.DOWN) => State(row + 1, col, Direction.DOWN, maze, steps + 1, path, false)
      case '|' if(direction == Direction.RIGHT) => State(row, col + 1, Direction.RIGHT, maze, steps + 1, path, false)
      case '|' if(direction == Direction.LEFT) => State(row, col - 1, Direction.LEFT, maze, steps + 1, path, false)
      case '-' if(direction == Direction.UP) => State(row - 1, col, Direction.UP, maze, steps + 1, path, false)
      case '-' if(direction == Direction.DOWN) => State(row + 1, col, Direction.DOWN, maze, steps + 1, path, false)
      case '-' if(direction == Direction.RIGHT) => State(row, col + 1, Direction.RIGHT, maze, steps + 1, path, false)
      case '-' if(direction == Direction.LEFT) => State(row, col - 1, Direction.LEFT, maze, steps + 1, path, false)
      case '+' => {
        if(direction == Direction.LEFT || direction == Direction.RIGHT) {
          if(row > 0 && maze(row - 1)(col) != ' ') State(row - 1, col, Direction.UP, maze, steps + 1, path, false)
          else if(row < maze.size - 1 && maze(row + 1)(col) != ' ') State(row + 1, col, Direction.DOWN, maze, steps + 1, path, false)
          else {
            assert(false)
            State(0, 0, 0, Array(), 0, "", true)
          }
        } else if(direction == Direction.UP || direction == Direction.DOWN) {
          if(col > 0 && maze(row)(col - 1) != ' ') State(row, col - 1, Direction.LEFT, maze, steps + 1, path, false)
          else if(col < maze(row).size - 1 && maze(row)(col + 1) != ' ') State(row, col + 1, Direction.RIGHT, maze, steps + 1, path, false)
          else {
            assert(false)
            State(0, 0, 0, Array(), 0, "", true)
          }
        } else {
          assert(false)
          State(0, 0, 0, Array(), 0, "", true)
        }
      }
      case _ => {
        assert(('A' to 'Z').contains(maze(row)(col)))

        if(direction == Direction.DOWN) {
          if(row == maze.size - 1 || maze(row + 1)(col) == ' ') State(row, col, direction, maze, steps + 1, path + maze(row)(col), true)
          else State(row + 1, col, direction, maze, steps + 1, path + maze(row)(col), false)
        } else if(direction == Direction.UP) {
          if(row == 0 || maze(row - 1)(col) == ' ') State(row, col, direction, maze, steps + 1, path + maze(row)(col), true)
          else State(row - 1, col, direction, maze, steps + 1, path + maze(row)(col), false)
        } else if(direction == Direction.LEFT) {
          if(col == 0 || maze(row)(col - 1) == ' ') State(row, col, direction, maze, steps + 1, path + maze(row)(col), true)
          else State(row, col - 1, direction, maze, steps + 1, path + maze(row)(col), false)
        } else if(direction == Direction.RIGHT) {
          if(col == maze(row).size - 1 || maze(row)(col + 1) == ' ') State(row, col, direction, maze, steps + 1, path + maze(row)(col), true)
          else State(row, col + 1, direction, maze, steps + 1, path + maze(row)(col), false)
        } else {
          assert(false)
          State(0, 0, 0, Array(), 0, "", false)
        }
      }
    }
  }

  def walkTheMaze(maze: Array[Array[Char]]): (String, Int) = {
    def go(s: State): State = {
      if(s.done) s
      else go(s.next)
    }

    val finalState = go(State(0, maze(0).indexOf('|'), Direction.DOWN, maze, 0, "", false))
    (finalState.path, finalState.steps)
  }

  object Part1 {
    def solve(input: Array[Array[Char]]): (String, Long) = Util.measuredTimeMillis {
      walkTheMaze(input)._1
    }
  }

  object Part2 {
    def solve(input: Array[Array[Char]]): (Int, Long) = Util.measuredTimeMillis {
      walkTheMaze(input)._2
    }
  }
}
