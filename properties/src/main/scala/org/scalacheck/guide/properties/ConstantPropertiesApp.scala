package org.scalacheck.guide.properties

import org.scalacheck.Prop.{exception, falsified, passed, proved, undecided}
import org.scalacheck.Properties

object ConstantPropertiesApp extends App {

  object ConstantProperties extends Properties(name = "Const") {
    property("p1") = undecided
    property("p2") = falsified
    property("p3") = proved
    property("p4") = passed
    property("p5") = exception(new Throwable("My fault"))
  }

  ConstantProperties.check()

}
