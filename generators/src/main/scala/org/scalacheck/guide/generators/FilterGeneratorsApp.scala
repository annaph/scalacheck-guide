package org.scalacheck.guide.generators

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{choose, listOf}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Prop}

object FilterGeneratorsApp extends App {

  // Gen.suchThat
  val oddIntGen: Gen[Int] = arbitrary[Int].suchThat(_ % 2 == 1)

  val oddIntProp: Prop = forAll(oddIntGen) { n: Int =>
    n % 2 == 1
  }

  // Gen.retryUntil
  val listOfOddIntGen: Gen[List[Int]] = listOf(
    arbitrary[Int] retryUntil (_ % 2 == 1)
  )

  val listOfOddIntProp: Prop = forAll(listOfOddIntGen) { xs: List[Int] =>
    xs.forall(_ % 2 == 1)
  }

  val primeProp1: Prop = forAll(choose(min = 38, max = 40)) { n: Int =>
    isPrime(n) ==> {
      !isPrime(2 * n)
    }
  }

  val primeProp2: Prop = forAll(choose(min = 38, max = 40) suchThat isPrime) { n: Int =>
    !isPrime(2 * n)
  }

  def isPrime(n: Int): Boolean =
    n > 1 && (2 to (n / 2)).forall(n % _ != 0)

  println("========================================================================")
  println("===> oddIntGen")
  println(oddIntGen.sample)
  println(oddIntGen.sample)
  println(oddIntGen.sample)

  println("========================================================================")
  println("===> listOfOddIntGen")
  println(listOfOddIntGen.sample)
  println(listOfOddIntGen.sample)
  println(listOfOddIntGen.sample)

  println("========================================================================")
  println("===> oddIntProp")
  oddIntProp.check()

  println("========================================================================")
  println("===> listOfOddIntProp")
  listOfOddIntProp.check()

  println("========================================================================")
  println("===> primeProp1")
  primeProp1.check()

  println("========================================================================")
  println("===> primeProp2")
  primeProp2.check()

  println("========================================================================")

}
