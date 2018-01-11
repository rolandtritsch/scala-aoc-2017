package aoc

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import sun.security.util.Length

class Day10Check extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  val hashes = for {
    n <- Gen.choose(0, 255)
    hash = (0 to n).toList
  } yield hash

  val hashesOfOne = for {
    n <- Gen.choose(0, 255)
  } yield List(n)

  property("for all hashes of one, the result should be the list itself (independent from the number of shifts)") {
    forAll(hashesOfOne, Gen.posNum[Int]) {(hash: List[Int], times: Int) => {
      Day10.shiftLeft(hash, times) should be (hash)
    }}
  }

  property("for all hashes, the result should be the hash itself (if we are shifting by the size of the hash)") {
    forAll(hashes) {(hash: List[Int]) => {
      Day10.shiftLeft(hash, hash.size) should be (hash)
    }}
  }

  property("for all hashes, shifting it left and then right again should give us the hash again") {
    forAll(hashes, Gen.posNum[Int]) {(hash: List[Int], times: Int) => {
      Day10.shiftLeft(Day10.shiftRight(hash, times), times) should be (hash)
    }}
  }

  property("for all hashes, reversing a hash of size one, gives the original hash") {
    forAll(hashesOfOne) {(hash: List[Int]) => {
      Day10.reverse(hash, 1) should be (hash)
    }}
  }

  property("for all hashes, reversing the hash should ... work :)") {
    forAll(hashes) {(hash: List[Int]) => {
      Day10.reverse(hash, hash.size) should be (hash.reverse)
    }}
  }

  property("for all hashes, reversing them twice, gives the original hash") {
    forAll(hashes, Gen.posNum[Int]) {(hash: List[Int], length: Int) => {
      Day10.reverse(Day10.reverse(hash, length), length) should be (hash)
    }}
  }
}