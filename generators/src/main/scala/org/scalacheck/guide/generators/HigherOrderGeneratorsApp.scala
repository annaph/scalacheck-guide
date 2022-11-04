package org.scalacheck.guide.generators

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.{choose, const, containerOf, containerOfN, frequency, listOf, listOfN, nonEmptyContainerOf, nonEmptyListOf, oneOf, pick, sequence, sized, someOf}
import org.scalacheck.Prop.{all, collect, forAll, propBoolean}
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Buildable.buildableSeq

object HigherOrderGeneratorsApp extends App {

  // Gen.sequence
  val generators = Seq(
    choose(min = 1, max = 10),
    const(20),
    const(30)
  )

  val sequenceGen = sequence(generators)

  // Gen.frequency
  val evenNumberGen = for {
    n <- choose(min = 0, max = 100000)
    isPositive <- oneOf(t0 = true, t1 = false)
  } yield {
    val result = 2 * n + 2
    if (isPositive) result else -result
  }

  val oddNumberGen = for {
    n <- choose(min = 0, max = 100000)
    isPositive <- oneOf(t0 = true, t1 = false)
  } yield {
    val result = 2 * n + 1
    if (isPositive) result else -result
  }

  val numberGen = frequency(gs =
    1 -> evenNumberGen,
    2 -> oddNumberGen,
    4 -> const(0)
  )

  val frequencyProp = forAll(numberGen) { n: Int =>
    val label = n match {
      case 0 => "zero"
      case _ if n % 2 == 0 => "even"
      case _ => "odd"
    }

    collect(label)(prop = true)
  }

  // Gen.oneOf
  val notZeroGen = oneOf(
    choose(min = -10, max = -1),
    choose(min = 1, max = 10)
  )

  val vowelGen = oneOf(t0 = 'a', t1 = 'e', tn = 'i', 'o', 'u', 'y')

  // Gen.listOf
  val intListGen: Gen[List[Int]] = listOf(
    choose(min = 1, max = 12)
  )

  val nonEmptyListGen: Gen[List[String]] = nonEmptyListOf(
    oneOf(t0 = "foo", t1 = "bar")
  )

  val eightBytesGen: Gen[List[Byte]] = listOfN(
    n = 8,
    g = arbitrary[Byte]
  )

  val intsWithinBoundsProp = forAll(intListGen) { xs: List[Int] =>
    xs.forall(x => x >= 1 && x <= 12)
  }

  val correctStringProp = forAll(nonEmptyListGen) { xs: List[String] =>
    all(
      "non empty" |: xs.nonEmpty,
      "foo or bar" |: xs.forall(str => str == "foo" || str == "bar")
    )
  }

  val listLengthProp = forAll(eightBytesGen) { xs: List[Byte] =>
    xs.length == 8
  }

  // Gen.containerOf
  val intSetGen: Gen[Set[Int]] = containerOf[Set, Int](
    choose(min = 1, max = 12)
  )

  val nonEmptySetGen: Gen[Set[String]] = nonEmptyContainerOf[Set, String](
    oneOf(t0 = "foo", t1 = "bar")
  )

  val upToEightBytesSetGen: Gen[Set[Byte]] = containerOfN[Set, Byte](
    n = 8,
    g = arbitrary[Byte]
  )

  // Gen.someOf & Gen.pick
  val numbersGen: Gen[collection.Seq[Int]] = someOf(
    Seq(1, 2, 3, 4, 5, 6, 7)
  )

  val twoStringsGen: Gen[collection.Seq[String]] = pick(
    n = 2,
    Seq("red", "blue", "green", "pink")
  )

  val numberListsGen: Gen[collection.Seq[collection.Seq[Int]]] = someOf(
    numbersGen, numbersGen, numbersGen
  )

  // Gen.sized
  val smallIntsListGen: Gen[List[Int]] = smallListGen(
    elemGen = oneOf(xs = Seq(1, 2, 3, 4, 5, 6, 7))
  )

  def smallListGen[T](elemGen: Gen[T]): Gen[List[T]] = {
    def go(size: Int): Gen[List[T]] = size match {
      case 0 => elemGen.map(_ => List.empty[T])
      case 1 => elemGen.map(List(_))
      case n if n > 7 =>
        choose(min = 0, max = 7).flatMap(go)
      case _ =>
        for {
          tail <- go(size - 1)
          head <- elemGen
        } yield head :: tail
    }

    sized(go)
  }

  println("========================================================================")
  println("===> sequenceGen")
  println(sequenceGen.sample)
  println(sequenceGen.sample)
  println(sequenceGen.sample)

  println("========================================================================")
  println("===> frequencyProp")
  frequencyProp.check()
  frequencyProp.check(Parameters.default withMinSuccessfulTests 10000)
  frequencyProp.check(Parameters.default withMinSuccessfulTests 100000)

  println("========================================================================")
  println("===> notZeroGen")
  println(notZeroGen.sample)
  println(notZeroGen.sample)
  println(notZeroGen.sample)

  println("========================================================================")
  println("===> vowelGen")
  println(vowelGen.sample)
  println(vowelGen.sample)
  println(vowelGen.sample)

  println("========================================================================")
  println("===> intsWithinBoundsProp")
  intsWithinBoundsProp.check()

  println("========================================================================")
  println("===> correctStringProp")
  correctStringProp.check()

  println("========================================================================")
  println("===> listLengthProp")
  listLengthProp.check()

  println("========================================================================")
  println("===> intSetGen, nonEmptySetGen and upToEightBytesSetGen")
  println(intSetGen.sample)
  println(nonEmptySetGen.sample)
  println(upToEightBytesSetGen.sample)

  println("========================================================================")
  println("===> numbersGen, twoStringsGen and numberListsGen")
  println(numbersGen.sample)
  println(twoStringsGen.sample)
  println(numberListsGen.sample)

  println("========================================================================")
  println("===> smallIntsListGen")
  println(smallIntsListGen.sample)
  println(smallIntsListGen.sample)
  println(smallIntsListGen.sample)

  println("========================================================================")

}
