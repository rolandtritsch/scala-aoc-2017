package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day20Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  import scala.util.Random

  lazy val genParticle = for {
    _ <- Gen.posNum[Int]
    x = Random.nextInt
    y = Random.nextInt
    z = Random.nextInt
  } yield {
    Day20.Particle(Day20.Position(x, y, z), Day20.Velocity(0, 0, 0), Day20.Acceleration(0, 0, 0))
  }

  lazy val genParticles = Gen.listOf(genParticle)

  property("For all possible lists of particles, the nullParticle is always the closest") {
    forAll(genParticles) {ps => {
      val nullParticle = Day20.Particle(Day20.Position(0, 0, 0), Day20.Velocity(0, 0, 0), Day20.Acceleration(0, 0, 0))
      val pss = nullParticle :: ps
      val closest = Day20.findClosest(pss)
      pss(closest) should be (nullParticle)
    }}
  }
}
