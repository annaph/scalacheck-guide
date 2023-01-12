package org.scalacheck.guide.running

import org.scalacheck.Prop.forAll
import org.scalacheck.Test
import org.scalacheck.Test.check

object NumberOfWorkerThreadsApp extends App {

  private val prop = forAll { xs: List[String] =>
    val str = if (xs.isEmpty) "" else xs.reduce(_ + _)
    xs.forall(x => str contains x)
  }

  private val oneWorkerParams = Test.Parameters
    .default
    .withMinSuccessfulTests(minSuccessfulTests = 10000)

  private val twoWorkersParams = Test.Parameters
    .default
    .withMinSuccessfulTests(minSuccessfulTests = 10000)
    .withWorkers(workers = 2)

  println("========================================================================")
  println("===> oneWorkerParams")
  private val result1 = check(oneWorkerParams, prop)
  println(s"Time of execution with 1 worker: ${result1.time}ms")

  println("========================================================================")
  println("===> twoWorkersParams")
  private val result2 = check(twoWorkersParams, prop)
  println(s"Time of execution with 2 workers: ${result2.time}ms")

  println("========================================================================")

}
