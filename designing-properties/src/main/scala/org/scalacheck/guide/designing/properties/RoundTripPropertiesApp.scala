package org.scalacheck.guide.designing.properties

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

import scala.annotation.tailrec

object RoundTripPropertiesApp extends App {

  val spec = new Properties(name = "RoundTripProperties") {
    property("reverse inverse") = forAll { xs: List[Int] =>
      xs.reverse.reverse == xs
    }

    property("decode encode") = forAll { n: Int =>
      (n >= 0) ==> {
        decodeBinary(encodeBinary(n)) == n
      }
    }
  }

  spec.check()

  def encodeBinary(n: Long): String = {
    @tailrec
    def go(x: Long, acc: List[String] = List.empty): List[String] = x match {
      case 0 =>
        "0" :: acc
      case 1 =>
        "1" :: acc
      case _ if x % 2 == 0 =>
        go(x / 2, "0" :: acc)
      case _ =>
        go(x / 2, "1" :: acc)
    }

    go(n) mkString ""
  }

  // Method to test
  def decodeBinary(str: String): Long = {
    @tailrec
    def go(i: Int, m: Long = 1, acc: Long = 0): Long = if (i < 0) acc else {
      str.charAt(i) match {
        case '0' =>
          go(i - 1, 2 * m, acc)
        case '1' =>
          go(i - 1, 2 * m, acc + m)
        case _ =>
          throw new Exception("Invalid character!")
      }
    }

    if (str.isEmpty) 0 else go(str.length - 1)
  }

}
