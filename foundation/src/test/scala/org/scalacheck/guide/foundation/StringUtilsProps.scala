package org.scalacheck.guide.foundation

import org.scalacheck.Gen.{alphaStr, listOf, numChar}
import org.scalacheck.Prop.AnyOperators
import org.scalacheck.{Prop, Properties}

object StringUtilsProps extends Properties(name = "StringUtils") {

  property("truncate") =
    Prop.forAll { (str: String, n: Int) =>
      val actual = StringUtils.truncate(str, n)

      if (n < 0) actual.isEmpty
      else if (str.length <= n) actual == str
      else {
        lazy val condition1 = actual.length == n + 3
        lazy val condition2 = (0 until n).forall(i => actual(i) == str(i))
        lazy val condition3 = (1 to 3).forall(i => actual(actual.length - i) == '.')

        condition1 && condition2 && condition3
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
