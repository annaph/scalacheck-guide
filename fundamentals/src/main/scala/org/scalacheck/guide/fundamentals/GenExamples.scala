package org.scalacheck.guide.fundamentals

import org.scalacheck.Gen.{negNum, posNum}
import org.scalacheck.Prop.propBoolean
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen, Prop}

object GenExamples extends App {

  println("=======================================================================")

  val myGen1 = Gen.choose(min = 1, max = 12)

  val number1 = myGen1(Gen.Parameters.default, Seed.random())
  println(s"Generated number: $number1")

  val number2 = myGen1.sample
  println(s"Generated number: $number2")

  println("=======================================================================")

  val myGen2 =
    for {
      n <- Gen.choose(min = 1, max = 12)
      m <- Gen.choose(min = n, max = 2 * n)
    } yield (n, m)

  val tuple1 = myGen2.sample
  println(s"Generated tuple: $tuple1")

  val tuple2 = myGen2.sample
  println(s"Generated tuple: $tuple2")

  println("=======================================================================")

  val colorGen = Gen.oneOf(Red, Green)

  val lineGen = colorGen.map(Line)
  val circleGen = colorGen.map(Circle)
  val boxGen =
    for {
      color <- colorGen
      shape <- shapeGen // boxGen and shapeGen are recursive generators!
    } yield Box(color, shape)

  val shapeGen: Gen[Shape] = Gen.oneOf(lineGen, circleGen, boxGen)

  val shape1 = shapeGen.sample
  println(s"Generated shape: $shape1")

  val shape2 = shapeGen.sample
  println(s"Generated shape: $shape2")

  println("=======================================================================")

  val evenIntGen = Gen
    .choose(min = -12, max = 12)
    .map(_ * 2)

  val divideProp = Prop.forAll(evenIntGen) { n: Int =>
    n % 2 == 0
  }

  println("Divide property ===>")
  divideProp.check()

  println("=======================================================================")

  val multiplyProp = Prop.forAll(posNum[Int], negNum[Int]) { (n: Int, m: Int) =>
    n * m < 0
  }

  println("Multiply property ===>")
  multiplyProp.check()

  println("=======================================================================")

  val prefixProp = Prop.forAll { str: String =>
    Prop.forAll(Gen.choose(min = 0, max = str.length)) { n: Int =>
      val prefix = str.substring(0, n)
      str startsWith prefix
    }
  }

  println("Prefix property ===>")
  prefixProp.check()

  println("=======================================================================")

  val stringWithNGen = for {
    str <- Arbitrary.arbitrary[String]
    n <- Gen.choose(min = 0, max = str.length)
  } yield (str, n)

  val prefixProp2 = Prop.forAll(stringWithNGen) {
    case (str, n) =>
      val prefix = str.substring(0, n)
      str startsWith prefix
  }

  println("Prefix property 2 ===>")
  prefixProp2.check()

  println("=======================================================================")

  val firstNameSeq = Seq("Anna", "Stacey", "Nicole")
  val lastNameSeq = Seq("Philips", "Poole", "Kidman")

  val personGen =
    for {
      firstName <- Gen.oneOf(firstNameSeq)
      lastName <- Gen.oneOf(lastNameSeq)
      age <- Gen.choose(min = 17, max = 37)
    } yield Person(firstName, lastName, age)

  implicit val personArb: Arbitrary[Person] = Arbitrary(personGen)

  val personProp = Prop.forAll { person: Person =>
    person.isTeenager == (person.age >= 13 && person.age <= 19)
  }

  println("Person property ===>")
  personProp.check()

  println("=======================================================================")

  val noPairsProp = Prop.forAll { xs: List[Byte] =>
    (xs.length >= 2) ==> Prop.forAll(Gen.choose(min = 0, max = xs.length - 2)) { i: Int =>
      xs(i) != xs(i + 1)
    }
  }

  println("No Pairs property ===>")
  noPairsProp.check()

  println("=======================================================================")

  trait Color

  trait Shape {
    def color: Color
  }

  case class Line(color: Color) extends Shape

  case class Circle(color: Color) extends Shape

  case class Box(color: Color, boxed: Shape) extends Shape

  case class Person(firstName: String, lastName: String, age: Int) {

    def isTeenager: Boolean = age >= 13 && age <= 19

  }

  case object Red extends Color

  case object Green extends Color

}
