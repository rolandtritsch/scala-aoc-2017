package aoc

/** Problem: [[http://adventofcode.com/2017/day/20]]
  *
  * Solution:
  *
  * General - A [[Particle]] got a [[Position]] and a [[Velocity]]
  * and an [[Acceleration]]. With every *tick* the Particle will get
  * a new Position. Run a/the simulation for a while (defaultDepth).
  *
  * Part1 - Check on/in the resulting list of particles, where the
  * minimum is and find the closest particle to the center.
  *
  * Part2 - Run a/the simulation again, but filter out all collisions.
  * And then *just* take teh size of the resulting list.
  *
  */
object Day20 {

  val input = Util.readInput("Day20input.txt")

  case class Position(x: Int, y: Int, z: Int) {
    def add(v: Velocity): Position = {
      Position(x + v.x, y + v.y, z + v.z)
    }
    def distance: Int = Math.abs(x) + Math.abs(y) + Math.abs(z)
  }

  case class Velocity(x: Int, y: Int, z: Int) {
    def add(a: Acceleration): Velocity = {
      Velocity(x + a.x, y + a.y, z + a.z)
    }
  }

  case class Acceleration(x: Int, y: Int, z: Int)

  case class Particle(p: Position, v: Velocity, a: Acceleration) {
    def tick: Particle = {
      Particle(p.add(v.add(a)), v.add(a), a)
    }
  }

  def parseInput(input: List[String]): List[Particle] = {
    require(input.nonEmpty, "input.nonEmpty failed")

    input.map(l => {
      // p=<1199,-2918,1457>, v=<-13,115,-8>, a=<-7,8,-10>
      val tokens = l.split("[<>,]").map(_.trim)
      val p = Position(tokens(1).toInt, tokens(2).toInt, tokens(3).toInt)
      val v = Velocity(tokens(6).toInt, tokens(7).toInt, tokens(8).toInt)
      val a = Acceleration(tokens(11).toInt, tokens(12).toInt, tokens(13).toInt)
      Particle(p, v, a)
    })
  } ensuring(_.nonEmpty, "_.nonEmpty failed")

  private val defaultDepth = 1000
  def run(ps: List[Particle], depth: Int = defaultDepth): List[Particle] = {
    if(depth <= 0) ps
    else run(ps.map(_.tick), depth - 1)
  }

  def findClosest(ps: List[Particle]): Int = {
    require(ps.nonEmpty, "ps.nonEmpty failed")

    ps.indexOf(ps.minBy(_.p.distance))
  } ensuring(_ >= 0, "_ >= 0 failed")

  def removeCollisions(ps: List[Particle]): List[Particle] = {
    require(ps.nonEmpty, "ps.nonEmpty failed")

    ps.groupBy(_.p).map { case (k, v) => (k, v, v.size) }.filter { case (_, v, s) => s == 1 }.toList.map { case (_, v, _) => v.head }
  } ensuring(_.nonEmpty, "_.nonEmpty failed")

  def runWithCollisionDetection(ps: List[Particle], depth: Int = defaultDepth): List[Particle] = {
    if(depth <= 0) ps
    else runWithCollisionDetection(removeCollisions(ps.map(_.tick)), depth - 1)
  }

  object Part1 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      findClosest(run(parseInput(input)))
    }
  }

  object Part2 {
    def solve(input: List[String]): (Int, Long) = Util.measuredTimeMillis {
      runWithCollisionDetection(parseInput(input)).size
    }
  }
}
