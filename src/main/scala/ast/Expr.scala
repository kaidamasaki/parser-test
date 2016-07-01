package com.socrata.ice.importer
package ast

import scala.annotation.tailrec

import com.rojoma.json.v3.ast.{JString, JValue}

trait Expr {
  def apply(bindings: Map[String,String]): Option[String]
  def toJson(bindings: Map[String, String]): Option[JValue] = apply(bindings).map(JString)
}

object Expr extends Parser[Expr] {
  def apply(tokens: List[String]): Result[Expr] = {
    val expr = Util.chain(tokens)(Id, Literal, wrapped, Object)

    println(s"Expr: $expr / $tokens")
    parseFunc(expr, None)
  }

  def concat(tokens: List[String], left: Expr): Result[Func] =
    Expr(tokens).right.map { case (right, rest) => Func("+", List(left, right), None) -> rest }

  @tailrec
  def parseFunc(expr: Result[Expr], self: Option[Expr]): Result[Expr] = {
    println(s"parseFunc: $expr / $self")
    (expr, self) match {
      case (Right((next, Nil)), _) => Right((next, Nil))
      case (Right((left, "+" :: rest)), _) => parseFunc(concat(rest, left), self)
      case (Right((name, "(" :: rest)), _) => parseFunc(Func(rest, name, self), None)
      case (Right((newSelf, "." :: rest)), None) => parseFunc(Id(rest), Some(newSelf))
      case (Right(result), None) => Right(result)
      case (Left(err), _) => Left(err)
      case _ =>
        Util.unexpected
    }
  }

  def wrapped(tokens: List[String]): Result[Expr] = (tokens) match {
    case "("::tail => Expr(tail).right.flatMap {
      case (expr, ")"::rest) => Right(expr -> rest)
      case (expr, next::rest) => Left(s"""Expected ")" but got "$next"!""")
      case (expr, Nil) => Left(s"""Unexpected end of transform while looking for ")"!""")
    }
    case _ =>
      println("wrapped")
      Util.unexpected
  }
}
