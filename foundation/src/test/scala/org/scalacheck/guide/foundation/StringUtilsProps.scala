package org.scalacheck.guide.foundation

import org.scalacheck.Gen.{alphaStr, listOf, numChar}
import org.scalacheck.Prop.AnyOperators
import org.scalacheck.{Prop, Properties}

object StringUtilsProps extends Properties(name = "StringUtils") {

  property("truncate") =
    Prop.forAll { (s: String, n: Int) =>
      val actual = StringUtils.truncate(s, n)

      if (n < 0) actual.isEmpty else {
        lazy val actualWithoutDots = actual.take(actual.length - 3)
        lazy val actualDots = actual.drop(actual.length - 3)

        lazy val condition1 = s.length <= n && actual == s
        lazy val condition2 = s.length > n && s.indexOf(actualWithoutDots) == 0 && actualDots == "..."

        condition1 || condition2
      }
    }

  property("tokenize") =
    Prop.forAll(listOf(alphaStr), numChar) { (list, delim) =>
      val str = list mkString delim.toString

      val expected = list.filter(_.nonEmpty)
      val actual = StringUtils.tokenize(str, delim).toList

      actual ?= expected
    }

  property("contains") =
    Prop.forAll { (s1: String, s2: String, s3: String) =>
      StringUtils.contains(s1 + s2 + s3, s2)
    }

}
