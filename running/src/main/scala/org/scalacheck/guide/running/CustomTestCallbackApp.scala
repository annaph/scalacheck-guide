package org.scalacheck.guide.running

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Test.{Result, TestCallback}
import org.scalacheck.{Prop, Test}

import scala.math.abs

object CustomTestCallbackApp extends App {

  private val myProp: Prop = forAll { n: Int =>
    (n > 0) ==> {
      abs(n) == n
    }
  }

  private val myTestCallback = new TestCallback {
    override def onPropEval(name: String, threadIdx: Int, succeeded: Int, discarded: Int): Unit =
      Console.printf("%s%s[%d] %s: s=%d d=%d%s\n",
        Console.RESET,
        Console.BLUE,
        threadIdx,
        name,
        succeeded,
        discarded,
        Console.RESET)
  }

  private val myPropTestResult: Result = Test.check(myProp)(_
    .withWorkers(workers = 3)
    .withTestCallback(myTestCallback)
  )

  Console.println(s"${Console.RESET}${Console.GREEN}Result~> $myPropTestResult${Console.RESET}")

}
