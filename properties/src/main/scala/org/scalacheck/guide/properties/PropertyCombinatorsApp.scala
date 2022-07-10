package org.scalacheck.guide.properties

import org.scalacheck.Gen.choose
import org.scalacheck.Prop.{classify, collect, exists, forAll, throws}

object PropertyCombinatorsApp extends App {

  // Prop.throws
  val divByZeroProp = forAll { n: Int =>
    throws(classOf[ArithmeticException])(n / 0)
  }

  val listBoundsProp = forAll { (xs: List[String], i: Int) =>
    val inside = i >= 0 && i < xs.length

    classify(inside, ifTrue = "inside", ifFalse = "outside") {
      inside || throws(classOf[IndexOutOfBoundsException])(xs(i))
    }
  }

  val listBoundsProp2 = forAll { (xs: List[String], i: Int) =>
    val inside = i >= 0 && i < xs.length
    val label = if (inside) "inside" else "outside"

    collect(label) {
      inside || throws(classOf[IndexOutOfBoundsException])(xs(i))
    }
  }

  // Prop.exists
  val existsProp1 = exists { n: Int =>
    (n % 2 == 0) && (n % 3 == 0)
  }

  val existsProp2 = exists(choose(min = 0, max = 10)) { n: Int =>
    n == 3
  }

  println("========================================================================")
  println("===> divByZeroProp")
  divByZeroProp.check()

  println("========================================================================")
  println("===> listBoundsProp")
  listBoundsProp.check()

  println("========================================================================")
  println("===> listBoundsProp2")
  listBoundsProp2.check()

  println("========================================================================")
  println("===> existsProp1")
  existsProp1.check()

  println("========================================================================")
  println("===> existsProp2")
  existsProp2.check()

  println("========================================================================")

}
