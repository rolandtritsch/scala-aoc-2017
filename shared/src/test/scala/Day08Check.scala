package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class Day08Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  // disable shrinking (to make debugging easier)
  import org.scalacheck.Shrink
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  def instructionGen = for {
    register <- Gen.alphaChar
    operation <- Gen.oneOf("dec", "inc")
    operand <- Gen.choose(0, 100)
    //conditionRegister <- Gen.alphaChar
    condition <- Gen.oneOf("==", "!=", "<", ">", "<=", ">=")
    conditionOperand <- Gen.choose(0, 100)
  } yield {
    //s"${register} ${operation} ${operand} if ${conditionRegister} ${condition} ${conditionOperand}"
    s"${register} ${operation} ${operand} if ${register} ${condition} ${conditionOperand}"
  }

  def programGen = Gen.listOfN(100, instructionGen)

  property("Every program will end") {
    forAll(programGen) {p => {
      Day08.Part1.solve(p)._1 should be >= 0
    }}
  }
}
