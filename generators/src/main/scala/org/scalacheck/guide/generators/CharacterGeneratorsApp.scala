package org.scalacheck.guide.generators

import org.scalacheck.Gen.{alphaChar, alphaLowerChar, alphaNumChar, alphaUpperChar, numChar}

object CharacterGeneratorsApp extends App {

  val stringGen = for {
    num <- numChar
    upper <- alphaUpperChar
    lower <- alphaLowerChar
    alpha <- alphaChar
    alphaNum <- alphaNumChar
  } yield List(num, upper, lower, alpha, alphaNum).mkString

  println(s"string sample 1: ${stringGen.sample}")
  println(s"string sample 2: ${stringGen.sample}")
  println(s"string sample 3: ${stringGen.sample}")

}
