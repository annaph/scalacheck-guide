package org.scalacheck.guide.generators

import org.scalacheck.Gen.{alphaNumStr, alphaStr, identifier, numStr}

object StringGeneratorsApp extends App {

  val stringsGen = for {
    alpha <- alphaStr
    num <- numStr
    alphaNum <- alphaNumStr
    id <- identifier
  } yield (alpha take 4, num take 4, alphaNum take 4, id take 4)

  println(s"strings sample 1: ${stringsGen.sample}")
  println(s"strings sample 2: ${stringsGen.sample}")
  println(s"strings sample 3: ${stringsGen.sample}")

}
