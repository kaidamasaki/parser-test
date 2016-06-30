package com.socrata.ice.importer.ast

import com.rojoma.json.v3.ast.{JString, JValue}

trait Expr {
  def apply(bindings: Map[String,String]): Option[String]
  def toJson(bindings: Map[String, String]): Option[JValue] = apply(bindings).map(JString)
}

object Expr extends Parser[Expr] {
  def apply(tokens: List[String]): Result[Expr] = {
    val expr = Id(tokens).
      orElse(Literal(tokens)).
      orElse(wrapped(tokens)).
      orElse(Object(tokens))

    println(s"Expr: $expr")
    parseFunc(expr, None)
  }

  def concat(tokens: List[String], left: Expr): Result[Func] =
    Expr(tokens).map { case (right, rest) => Func("+", List(left, right), None) -> rest }

  def parseFunc(expr: Result[Expr], self: Option[Expr]): Result[Expr] = {
    println(s"parseFunc: $expr / $self")

    (expr, self) match {
      case (Some((next, Nil)), _) => Some(next, Nil)
      case (Some((left, "+" :: rest)), _) => parseFunc(concat(rest, left), self)
      case (Some((name, "(" :: rest)), _) => parseFunc(Func(rest, name, self), None)
      case (Some((newSelf, "." :: rest)), None) => parseFunc(Id(rest), Some(newSelf))
      case (Some(result), None) => Some(result)
      case _ => None
    }
  }


  def wrapped(tokens: List[String]): Result[Expr] = if (tokens.nonEmpty) {
    Expr(tokens.tail) match {
      case Some((expr, rest)) if (tokens.head == "(" && rest.head == ")") =>
        Some(expr -> rest.tail)
      case _ => None
    }
  } else {
    None
  }
}
