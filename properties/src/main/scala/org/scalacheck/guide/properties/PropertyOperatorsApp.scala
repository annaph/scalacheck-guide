package org.scalacheck.guide.properties

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, propBoolean}

object PropertyOperatorsApp extends App {

  val sortedProp = forAll(
    "xs" |: arbitrary[List[Int]]) { xs: List[Int] =>
    val actual = xs.sorted

    val isSorted = xs match {
      case Nil | _ :: Nil => true
      case _ =>
        actual
          .indices
          .tail
          .forall(i => actual(i) >= actual(i - 1))
    }

    lazy val correctSize = xs.length == actual.length

    lazy val containsAll = xs.forall(actual.contains)

    (s"sorted: ${actual mkString ", "}" |: isSorted) &&
      ("size" |: correctSize) &&
      ("all elements" |: containsAll)
  }

  println("========================================================================")
  println("===> sortedProp")
  sortedProp.check()

  println("========================================================================")

}
