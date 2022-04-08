package org.scalacheck.guide.fundamentals

import org.scalacheck.{Prop, Properties}

object MySpec extends Properties(name = "MySpec") {

  property("list tail") =
    Prop.forAll { (x: Int, xs: List[Int]) =>
      (x :: xs).tail == xs
    }

  property("list head") =
    Prop.forAll { xs: List[Int] =>
      if (xs.nonEmpty) xs.head == xs(0)
      else Prop.throws(classOf[NoSuchElementException]) {
        xs.head
      }
    }

}
