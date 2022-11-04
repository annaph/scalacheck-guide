package org.scalacheck.guide.generators

import org.scalacheck.Gen.{choose, frequency, resize, sized}
import org.scalacheck.Prop.{AnyOperators, forAll}
import org.scalacheck.Shrink.shrink
import org.scalacheck.Test.Parameters
import org.scalacheck.{Gen, Shrink}

object TestCaseSimplificationApp extends App {

  println("========================================================================")
  // (((3 + 5) * 7) + 1)
  val expr1 = Add(3.toConst, 5.toConst)
  val expr2 = Multiply(expr1, 7.toConst)
  val expr3 = Add(expr2, 1.toConst)

  println("===> expr3")
  println(s"show: $expr3")
  println(s"eval: ${Expression.eval(expr3)}")

  println("========================================================================")
  val exprGen: Gen[Expression] = sized { size =>
    if (size == 0) constGen else {
      val newSize = size / 2
      frequency(gs =
        4 -> constGen,
        3 -> resize(newSize, addGen),
        3 -> resize(newSize, multiplyGen)
      )
    }
  }

  val constGen: Gen[Const] =
    choose(min = 0, max = 12).map(Const)

  val addGen: Gen[Add] =
    for {
      expr1 <- exprGen
      expr2 <- exprGen
    } yield Add(expr1, expr2)

  val multiplyGen: Gen[Multiply] =
    for {
      expr1 <- exprGen
      expr2 <- exprGen
    } yield Multiply(expr1, expr2)

  println("===> exprGen")
  println(exprGen.sample)
  println(exprGen.sample)
  println(exprGen.sample)

  println("========================================================================")
  // 1 * ((0 * 7) + (3 + 3)) ==> 0 + (2 * 3)
  val expr4 = Multiply(0.toConst, 7.toConst)
  val expr5 = Add(3.toConst, 3.toConst)
  val expr6 = Add(expr4, expr5)
  val expr7 = Multiply(1.toConst, expr6)

  println("===> expr7")
  println(s"show: $expr7")
  println(s"rewrite: ${Expression.rewrite(expr7)}")
  println(s"eval: ${Expression.eval(expr7)}")

  println("========================================================================")
  implicit val exprShrink: Shrink[Expression] = Shrink({
    case Const(n) =>
      shrink(n).map(Const)
    case Add(expr1, expr2) =>
      LazyList.concat(
        LazyList(expr1, expr2),
        shrink(expr1).map(Add(_, expr2)),
        shrink(expr2).map(Add(expr1, _))
      ).to(Stream)
    case Multiply(expr1, expr2) =>
      LazyList.concat(
        LazyList(expr1, expr2),
        shrink(expr1).map(Multiply(_, expr2)),
        shrink(expr2).map(Multiply(expr1, _))
      ).to(Stream)
  })

  println("===> shrink Const(12)")
  println(s"shrink: ${shrink[Expression](Const(12)).force.mkString(", ")}")

  println("========================================================================")
  val rewriteProp = forAll(exprGen) { expr =>
    val rewritten = Expression.rewrite(expr)
    (Expression.eval(rewritten) ?= Expression.eval(expr)) :| s"rewritten: $rewritten"
  }

  println("===> rewrite property")
  rewriteProp.check()
  rewriteProp.check(Parameters.default.withMinSuccessfulTests(minSuccessfulTests = 1000))

  println("========================================================================")

  trait Expression {
    override def toString: String = Expression.show(expr = this)
  }

  case class Const(n: Int) extends Expression

  case class Add(expr1: Expression, expr2: Expression) extends Expression

  case class Multiply(expr1: Expression, expr2: Expression) extends Expression

  object Expression {

    def eval(expr: Expression): Int = expr match {
      case Const(n) =>
        n
      case Add(expr1, expr2) =>
        eval(expr1) + eval(expr2)
      case Multiply(expr1, expr2) =>
        eval(expr1) * eval(expr2)
    }

    def rewrite(expr: Expression): Expression = expr match {
      case Add(expr1, expr2) if expr1 == expr2 =>
        Multiply(2.toConst, rewrite(expr1))
      case Multiply(Const(0), _) =>
        0.toConst
      // Intentional bug, should be: Multiply(Const(1), expr2)
      case Add(Const(1), expr2) =>
        rewrite(expr2)
      case const@Const(_) =>
        const
      case Add(expr1, expr2) =>
        Add(rewrite(expr1), rewrite(expr2))
      case Multiply(expr1, expr2) =>
        Multiply(rewrite(expr1), rewrite(expr2))
    }

    def show(expr: Expression): String = expr match {
      case Const(n) =>
        n.toString
      case Add(expr1, expr2) =>
        s"(${show(expr1)} + ${show(expr2)})"
      case Multiply(expr1, expr2) =>
        s"(${show(expr1)} * ${show(expr2)})"
    }

  }

  implicit class IntOps(n: Int) {
    def toConst: Const = Const(n)
  }

}
