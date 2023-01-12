package org.scalacheck.guide.running

import org.scalacheck.Prop.{all, forAll, propBoolean}
import org.scalacheck.{Arbitrary, Gen, Properties}

class MySpec extends Properties(name = "MySpec") {

  val myClassGen: Gen[MyClass] = Gen.resultOf {
    (s1: String, s2: String) => MyClass(s1, s2)
  }

  implicit val myClassArb: Arbitrary[MyClass] = Arbitrary(myClassGen)

  property("MyClass.toString") = forAll { myClass: MyClass =>
    val actual = myClass.toString

    s"actual: $actual" |: all(
      "contains s1" |: actual.contains(myClass.s1),
      "contains s2" |: actual.contains(myClass.s2)
    )
  }

}
