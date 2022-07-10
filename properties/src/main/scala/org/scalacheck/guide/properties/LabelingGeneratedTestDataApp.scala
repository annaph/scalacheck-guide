package org.scalacheck.guide.properties

import org.scalacheck.Gen.{choose, oneOf}
import org.scalacheck.Prop.{forAll, propBoolean}

object LabelingGeneratedTestDataApp extends App {

  val prop1 = forAll { xs: List[Int] =>
    forAll { n: Int =>
      (n >= 0 && n < xs.length) ==>
        xs(n) > 0
    }
  }

  val prop2 = forAll(
    choose(min = 0, max = 100) :| "pos",
    choose(min = -100, max = 0) :| "neg"
  ) { (x: Int, y: Int) =>
    x * y < 0
  }

  val primes = Seq(2, 3, 5, 7)
  val prop3 = forAll("prime" |: oneOf(primes)) { n: Int =>
    n % 2 != 0
  }

  println("========================================================================")
  println("===> prop1")
  prop1.check()

  println("========================================================================")
  println("===> prop2")
  prop2.check()

  println("========================================================================")
  println("===> prop3")
  prop3.check()

  println("========================================================================")

}
