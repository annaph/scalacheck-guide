package org.scalacheck.guide.designing.properties

import org.scalacheck.Gen.oneOf
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

import scala.math.{round, sqrt}

object RestrictingTestCasesApp extends App {

  val spec = new Properties(name = "RestrictingTestCases") {
    property("sqrt") = forAll { n: Int =>
      (n > 0) ==> {
        val m = sqrt(n)
        round(m * m) == n
      }
    }

    property("slice") = forAll { (xs: List[Int], n: Int, m: Int) =>
      (n >= 0 && m >= n && xs.length >= m) ==> {
        val slice = xs.slice(from = n, until = m)
        slice.length == (m - n) && xs.containsSlice(slice)
      }
    }

    property("slice 2") = forAll { xs: List[Int] =>
      val indices = if (xs.isEmpty) Seq(0) else xs.indices
      forAll(oneOf(indices), oneOf(indices)) { (n, m) =>
        (m >= n) ==> {
          val slice = xs.slice(from = n, until = m)
          slice.length == (m - n) && xs.containsSlice(slice)
        }
      }
    }
  }

  spec.check()

}
