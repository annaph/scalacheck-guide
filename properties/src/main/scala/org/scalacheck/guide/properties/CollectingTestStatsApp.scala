package org.scalacheck.guide.properties

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.choose
import org.scalacheck.Prop.{all, classify, collect, forAll}

object CollectingTestStatsApp extends App {

  val prop1 = forAll { n: Int =>
    classify(n % 2 == 0, ifTrue = "even", ifFalse = "odd") {
      classify(n < 0, ifTrue = "neg", ifFalse = "pos") {
        classify(math.abs(n) > 50, ifTrue = "large") {
          n + n == 2 * n
        }
      }
    }
  }

  val prop2 = forAll(
    arbitrary[List[Int]] :| "xs"
  ) { xs: List[Int] =>
    forAll(
      choose(min = 0, max = math.max(0, xs.length - 1)) :| "n"
    ) { n: Int =>
      forAll(
        choose(min = n, max = xs.length) :| "m"
      ) { m: Int =>
        val slice = xs.slice(n, m)

        val label = slice match {
          case Nil => "none"
          case _ :: Nil => "one"
          case _ if slice.length == xs.length => "whole"
          case _ => "part"
        }

        collect(label) {
          s"slice: '$slice'" |: all(
            xs containsSlice slice,
            xs containsSlice xs
          )
        }
      }
    }
  }

  println("========================================================================")
  println("===> prop1")
  prop1.check()

  println("========================================================================")
  println("===> prop2")
  prop2.check()

  println("========================================================================")

}
