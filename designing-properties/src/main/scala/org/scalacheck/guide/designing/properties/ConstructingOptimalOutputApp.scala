package org.scalacheck.guide.designing.properties

import org.scalacheck.Gen.{alphaNumChar, choose, sized}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

import scala.annotation.tailrec
import scala.collection.mutable

object ConstructingOptimalOutputApp extends App {

  type Pair[T] = (Int, T)

  val pairGen: Gen[Pair[Char]] =
    for {
      n <- choose(min = 1, max = 20)
      ch <- alphaNumChar
    } yield (n, ch)

  val outputGen: Gen[List[Pair[Char]]] = {
    def listGen(size: Int): Gen[List[Pair[Char]]] =
      if (size == 0) pairGen.map(_ => List.empty)
      else if (size == 1) pairGen.map(List(_))
      else {
        for {
          (cnt, ch) :: tail <- listGen(size - 1)
          pair <- pairGen.retryUntil(_._2 != ch)
        } yield pair :: ((cnt, ch) :: tail)
      }

    sized(listGen)
  }

  val spec: Properties = new Properties(name = "ConstructingOptimalOutputProperties") {
    property("run-length encoder") = forAll(outputGen) { xs: List[Pair[Char]] =>
      val decoded = runLengthDecoder(xs)
      runLengthEncoder(decoded) == xs
    }

    def runLengthDecoder[T](xs: List[Pair[T]]): List[T] =
      xs.flatMap {
        case (cnt, t) =>
          List.fill(cnt)(t)
      }
  }

  spec.check()

  // Method to test
  def runLengthEncoder[T: Ordering](xs: List[T]): List[Pair[T]] = {
    case class Acc(result: mutable.ListBuffer[Pair[T]], t: T, cnt: Int)

    @tailrec
    def go(restXs: List[T], acc: Acc): Acc = restXs match {
      case Nil =>
        acc.copy(result = acc.result :+ (acc.cnt, acc.t))
      case t :: rest if implicitly[Ordering[T]].equiv(t, acc.t) =>
        go(rest, acc.copy(cnt = acc.cnt + 1))
      case t :: rest =>
        val newResult = acc.result :+ (acc.cnt, acc.t)
        go(rest, Acc(newResult, t, cnt = 1))
    }

    xs match {
      case Nil =>
        List.empty
      case head :: rest =>
        val initAcc = Acc(result = mutable.ListBuffer.empty, t = head, cnt = 1)
        val acc = go(rest, initAcc)
        acc.result.toList
    }
  }

}
