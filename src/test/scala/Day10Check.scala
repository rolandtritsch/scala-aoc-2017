package aoc

import org.scalatest.PropSpec
import org.scalacheck.{Gen, Prop}

class Day10Check extends PropSpec {

  property("for all hashes of one, the result should be the list itself (independent from the number of shifts)") {
    Prop.forAll(Gen.listOfN(1, Gen.posNum[Int]), Gen.posNum[Int])((hash, times) => {
      Day10.shiftLeft(hash, times) == hash
    })
  }

  property("for all hashes, the result should be the hash itself (if we are shifting by the size of the hash)") {
    Prop.forAll(Gen.nonEmptyListOf(Gen.posNum[Int]))(hash => {
      Day10.shiftLeft(hash, hash.size) == hash
    })
  }

  property("for all hashes, shifting it left and then right again should give us the hash again") {
    Prop.forAll(Gen.nonEmptyListOf(Gen.posNum[Int]), Gen.posNum[Int])((hash, times) => {
      Day10.shiftLeft(Day10.shiftRight(hash, times), times) == hash
    })
  }

  property("for all hashes, reversing a hash of size one, gives the original hash") {
    Prop.forAll(Gen.listOfN(1, Gen.posNum[Int]))(hash => {
      Day10.reverse(hash, hash.size) == hash
    })
  }

  property("for all hashes, reversing the hash should ... work :)") {
    Prop.forAll(Gen.nonEmptyListOf(Gen.posNum[Int]))(hash => {
      Day10.reverse(hash, hash.size) == hash.reverse
    })
  }

  property("for all hashes, reversing them twice, gives the original hash") {
    Prop.forAll(Gen.nonEmptyListOf(Gen.posNum[Int]), Gen.posNum[Int])((hash, length) => {
      Day10.reverse(Day10.reverse(hash, length), length) == hash
    })
  }
}
