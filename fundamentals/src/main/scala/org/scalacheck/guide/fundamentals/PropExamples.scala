package org.scalacheck.guide.fundamentals

import org.scalacheck.Prop

object PropExamples extends App {

  println("=======================================================================")

  val stringLengthProp: Prop = Prop.forAll { str: String =>
    val length = str.length
    (str + str).length == length + length
  }

  println("StringLength property ===>")
  stringLengthProp.check()

  println("=======================================================================")

  val divByZeroProp: Prop = Prop.throws(classOf[ArithmeticException]) {
    1 / 0
  }

  println("DivByZero property ===>")
  divByZeroProp.check()

  println("=======================================================================")

  val listIndexOutOfBoundsProp: Prop = Prop.forAll { xs: List[Int] =>
    Prop.throws(classOf[IndexOutOfBoundsException]) {
      xs(xs.length + 1)
    }
  }

  println("ListIndexOutOfBounds property ===>")
  listIndexOutOfBoundsProp.check()

  println("=======================================================================")

}
