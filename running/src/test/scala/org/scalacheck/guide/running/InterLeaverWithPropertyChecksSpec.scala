package org.scalacheck.guide.running

import org.scalacheck.guide.running.InterLeaver.interleave
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class InterLeaverWithPropertyChecksSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {

  property(testName = "the interleave method must interleave lists") {
    forAll { (l1: List[Int], l2: List[Int]) =>
      val actual = interleave(l1, l2)
      val indices = (0 until math.min(l1.length, l2.length)).toList

      actual.length shouldBe (l1.length + l2.length)
      (indices.map(i => actual(2 * i)) ++ actual.drop(2 * l2.length)) shouldBe l1
      (indices.map(i => actual(2 * i + 1)) ++ actual.drop(2 * l1.length)) shouldBe l2
    }
  }

}
