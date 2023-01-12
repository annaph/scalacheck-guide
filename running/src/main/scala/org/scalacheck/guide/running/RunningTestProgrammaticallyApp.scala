package org.scalacheck.guide.running

import org.scalacheck.Prop.{all, forAll}
import org.scalacheck.Test.Result
import org.scalacheck.{Prop, Properties, Test}

object RunningTestProgrammaticallyApp extends App {

  println("========================================================================")
  println("===> single property")

  private val singleProp: Prop = forAll { (xs: List[Int], n: Int) =>
    val len = xs.take(n).length

    if (n <= 0) len == 0
    else if (n >= xs.length) len == xs.length
    else len == n
  }

  private val singlePropTestResult: Result = Test
    .check(params = Test.Parameters.default, singleProp)

  Console.println(s"${Console.RESET}${Console.GREEN}${singlePropTestResult.prettyString}${Console.RESET}")

  println("========================================================================")
  println("===> property collection")

  private object SomeProps extends Properties(name = "SomeProps") {
    property("add") = forAll { (n: Int, m: Int) =>
      n + m == m + n
    }

    property("take") = singleProp
  }

  private val somePropsTestResults: Seq[(String, Result)] = Test
    .checkProperties(Test.Parameters.default, SomeProps)
    .toSeq

  somePropsTestResults.foreach {
    case (propName, result) =>
      Console.println(s"$propName | ${Console.RESET}${Console.GREEN}${result.prettyString}${Console.RESET}")
  }

  println("========================================================================")
  println("===> setting test parameters")

  private val concatProp: Prop = forAll { (s1: String, s2: String) =>
    val actual = s1 + s2

    all(
      actual.startsWith(s1),
      actual.endsWith(s2),
      actual.length == s1.length + s2.length
    )
  }

  private val concatPropTestResult: Result = Test.check(concatProp)(_
    .withMinSuccessfulTests(minSuccessfulTests = 400)
    .withWorkers(workers = 2)
  )

  Console.println(s"${Console.RESET}${Console.GREEN}$concatPropTestResult.prettyString${Console.RESET}")

  println("========================================================================")

  implicit class ResultOps(result: Result) {
    def prettyString: String =
      s"Result~> " +
        s"status: ${result.status}; " +
        s"succeeded: ${result.succeeded}; " +
        s"discarded: ${result.discarded}; " +
        s"frequencyMap: ${result.freqMap}; " +
        s"time: ${result.time}ms"
  }

}
