package aoc

object Day20 {

  val in = Util.readInput("Day20input.txt")

  case class Position(x: Int, y: Int, z: Int) {
    def add(v: Velocity): Position = {
      Position(x + v.x, y + v.y, z + v.z)
    }
    def sumAbs: Int = Math.abs(x) + Math.abs(y) + Math.abs(z)
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

  def parseInput(in: List[String]): List[Particle] = {
    in.map(l => {
      // p=<1199,-2918,1457>, v=<-13,115,-8>, a=<-7,8,-10>
      val tokens = l.split("[<>,]").map(_.trim)
      val p = Position(tokens(1).toInt, tokens(2).toInt, tokens(3).toInt)
      val v = Velocity(tokens(6).toInt, tokens(7).toInt, tokens(8).toInt)
      val a = Acceleration(tokens(11).toInt, tokens(12).toInt, tokens(13).toInt)
      Particle(p, v, a)
    })
  }

  def run(ps: List[Particle], depth: Int): List[Particle] = {
    if(depth <= 0) ps
    else run(ps.map(_.tick), depth - 1)
  }

  def findClosest(ps: List[Particle]): Int = {
    val min = ps.map(_.p.sumAbs).min
    ps.indexWhere(p => p.p.sumAbs == min)
  }

  def removeCollisions(ps: List[Particle]): List[Particle] = {
    ps.groupBy(_.p).map{case (k, v) => (k, v, v.size)}.filter{case (_, v, s) => s == 1}.toList.map{case (_, v, _) => v.head}
  }

  def runWithCollisionDetection(ps: List[Particle], depth: Int): List[Particle] = {
    if(depth <= 0) ps
    else runWithCollisionDetection(removeCollisions(ps.map(_.tick)), depth - 1)
  }
}