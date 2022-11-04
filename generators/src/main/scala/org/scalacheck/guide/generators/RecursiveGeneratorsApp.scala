package org.scalacheck.guide.generators

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.{choose, listOfN, lzy, oneOf, resize, sized}

object RecursiveGeneratorsApp extends App {

  val intTreeGen: Gen[Tree[Int]] = treeGen(arbitrary[Int])

  def treeGen[T](elemGen: Gen[T]): Gen[Tree[T]] = lzy {
    oneOf(leafGen(elemGen), nodeGen(elemGen))
  }

  def nodeGen[T](elemGen: Gen[T]): Gen[Node[T]] =
    sized { size =>
      for {
        n <- choose(min = 0, max = size)
        g = resize(size / (n + 1), treeGen(elemGen))
        children <- listOfN(n, g)
      } yield Node(children)
    }

  def leafGen[T](elemGen: Gen[T]): Gen[Leaf[T]] =
    elemGen.map(Leaf(_))

  println("========================================================================")
  println("===> intTreeGen")
  println(intTreeGen.sample)
  println(intTreeGen.sample)
  println(intTreeGen.sample)

  println("========================================================================")

  trait Tree[T] {
    def size: Long
  }

  case class Node[T](children: List[Tree[T]]) extends Tree[T] {
    override def size: Long = children.map(_.size).sum
  }

  case class Leaf[T](item: T) extends Tree[T] {
    override def size: Long = 1
  }

}
