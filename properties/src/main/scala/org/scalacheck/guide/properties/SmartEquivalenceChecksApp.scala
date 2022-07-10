package org.scalacheck.guide.properties

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{AnyOperators, all, forAll}

object SmartEquivalenceChecksApp extends App {

  val interleaveProp = forAll(
    arbitrary[List[Int]] :| "xs",
    arbitrary[List[Int]] :| "ys"
  ) { (xs: List[Int], ys: List[Int]) =>
    val actual = interleave(xs, ys)
    actual.length ?= xs.length + ys.length
  }

  val interleaveCompleteProp = forAll(
    arbitrary[List[Int]] :| "xs",
    arbitrary[List[Int]] :| "ys"
  ) { (xs: List[Int], ys: List[Int]) =>
    val actual = interleave(xs, ys)

    val indices = 0 until math.min(xs.length, ys.length)

    lazy val atEvenIndices = indices
      .map(_ * 2)
      .map(actual(_))
      .toList

    lazy val atOddIndices = indices
      .map(i => (i * 2) + 1)
      .map(actual(_))
      .toList

    lazy val xsZip = atEvenIndices ++ actual.drop(2 * ys.length)
    lazy val ysZip = atOddIndices ++ actual.drop(2 * xs.length)

    s"actual: $actual" |: all(
      "length" |: (actual.length ?= xs.length + ys.length),
      "zip xs" |: (xsZip ?= xs),
      "zip ys" |: (ysZip ?= ys)
    )
  }

  println("========================================================================")
  println("===> interleave property")
  interleaveProp.check()

  println("========================================================================")
  println("===> interleave complete property")
  interleaveCompleteProp.check()

  println("========================================================================")

  // Method under test
  def interleave[T](xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
    case (Nil, Nil) =>
      Nil
    case (_, Nil) =>
      xs
    case (Nil, _) =>
      ys
    case (x :: xss, y :: yss) =>
      x :: y :: interleave(yss, xss) // intentional bug!
  }

}
