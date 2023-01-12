package org.scalacheck.guide.running

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

/**
 * Using ScalaCheck from the command line:
 *
 * $ scala -cp <path_to_scalacheck_2.13-1.15.4.jar>:. \
 * org.scalacheck.guide.running.ListSpec \
 * -minSuccessfulTests 5000 -verbosity 1 1
 */
object ListSpec extends Properties(name = "ListSpec") {

  property("prepend") = forAll { (xs: List[Int], x: Int) =>
    (x +: xs).head == x
  }

  property("append") = forAll { (xs: List[Int], x: Int) =>
    (xs :+ x).last == x
  }

}
