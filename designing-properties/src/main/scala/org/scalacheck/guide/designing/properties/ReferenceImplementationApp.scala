package org.scalacheck.guide.designing.properties

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{AnyOperators, forAll}
import org.scalacheck.{Gen, Properties}

import java.util
import scala.collection.immutable.IntMap
import scala.jdk.CollectionConverters._

object ReferenceImplementationApp extends App {

  type Maps = (java.util.HashMap[Int, Any], IntMap[Any])

  val mapsGen: Gen[Maps] = arbitrary[List[Int]].map { xs =>
    val entries = xs.map(_ -> new Object)

    val intMap = IntMap.newBuilder
      .addAll(entries)
      .result()

    val hashMap = new util.HashMap[Int, Any]()
    entries.foreach {
      case (key, value) =>
        hashMap.put(key, value)
    }

    (hashMap, intMap)
  }

  val intMapSpec = new Properties(name = "IntMap") {
    property("size") = forAll(mapsGen) {
      case (hashMap, intMap) =>
        intMap.size ?= hashMap.size
    }

    property("empty") = forAll(mapsGen) {
      case (hashMap, intMap) =>
        intMap.isEmpty ?= hashMap.isEmpty
    }

    property("add") = forAll(mapsGen, arbitrary[Int], arbitrary[String]) { (maps, key, value) =>
      val (hashMap, intMap) = maps
      hashMap.put(key, value)

      equalMaps(hashMap, intMap + (key -> value))
    }
  }

  intMapSpec.check()

  def equalMaps(hashMap: java.util.HashMap[Int, Any], intMap: IntMap[Any]): Boolean = {
    lazy val condition1 = hashMap.keySet.asScala.forall(intMap.contains)
    lazy val condition2 = intMap.keys.forall(hashMap.containsKey)
    lazy val condition3 = intMap.keys.forall(key => hashMap.get(key) == intMap(key))

    condition1 && condition2 && condition3
  }

}
