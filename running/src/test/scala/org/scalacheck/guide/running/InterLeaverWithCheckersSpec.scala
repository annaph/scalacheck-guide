package org.scalacheck.guide.running

import org.scalacheck.Prop.{AnyOperators, all, forAll}
import org.scalacheck.guide.running.InterLeaver.interleave
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.Checkers

class InterLeaverWithCheckersSpec extends AnyPropSpec with Checkers {

  property(testName = "the interleave method must interleave lists") {
    val prop = forAll { (l1: List[Int], l2: List[Int]) =>
      val actual = interleave(l1, l2)
      val indices = (0 until math.min(l1.length, l2.length)).toList

      s"actual: $actual" |: all(
        "length" |: l1.length + l2.length =? actual.length,
        "zip l1" |: l1 =? indices.map(i => actual(2 * i)) ++ actual.drop(2 * l2.length),
        "zip l2" |: l2 =? indices.map(i => actual(2 * i + 1)) ++ actual.drop(2 * l1.length)
      )
    }

    check(prop)
  }

}
