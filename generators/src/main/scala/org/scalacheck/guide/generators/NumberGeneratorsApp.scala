package org.scalacheck.guide.generators

import org.scalacheck.Gen.{choose, negNum}
import org.scalacheck.Prop.forAll

object NumberGeneratorsApp extends App {

  // Gen.choose
  val g = choose(min = -2, max = 5)
  val h = choose(min = 4.1, max = 4.2)

  println(s"g sample: ${g.sample}")
  println(s"h sample: ${h.sample}")

  val chooseProp = forAll(h) { n: Double =>
    n >= 4.1 && n <= 4.2
  }

  // Gen.negNum
  val absProp = forAll(negNum[Int]) { n: Int =>
    math.abs(n) == -n
  }

  println("========================================================================")
  println("===> chooseProp")
  chooseProp.check()

  println("========================================================================")
  println("===> absProp")
  absProp.check()

  println("========================================================================")

}
