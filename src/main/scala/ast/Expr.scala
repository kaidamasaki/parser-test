package com.socrata.ice.importer
package ast

import scala.annotation.tailrec

import com.rojoma.json.v3.ast.{JString, JValue}

import token.Token

trait Expr {
  def apply(bindings: Map[String,String]): Either[Error, String]
  def toJson(bindings: Map[String, String]): Either[Error, JValue] = apply(bindings).right.map(JString)
  def idx: Int
  def endIdx: Int
  def src: String
}

object Expr extends Parser[Expr] {
  def apply(tokens: List[Token]): Result[Expr] = {
    val expr = Util.chain(tokens)(Id, Literal, wrapped, Object)

    parseFunc(expr, None)
  }

  def concat(tokens: List[Token], left: Expr, idx: Int): Result[Func] =
    Expr(tokens).right.map { case (right, rest) => Func("+", List(left, right), None, idx) -> rest }

  @tailrec
  def parseFunc(expr: Result[Expr], self: Option[Expr]): Result[Expr] = {
    (expr, self) match {
      case (Right((next, Nil)), _) => Right((next, Nil))
      case (Right((left, t::rest)), _) if t.body == "+" => parseFunc(concat(rest, left, t.idx), self)
      case (Right((name, Token("(")::rest)), _) => parseFunc(Func(rest, name, self), None)
      case (Right((newSelf, t::rest)), None) if t.body == "." => parseFunc(Id(rest), Some(newSelf))
      case (Right(result), None) => Right(result)
      case (Left(err), _) => Left(err)
      case (Right((expr, _)), _) => Util.unexpected(expr.idx)
    }
  }

  def wrapped(tokens: List[Token]): Result[Expr] = (tokens) match {
    case Token("(")::tail => Expr(tail).right.flatMap {
      case (expr, Token(")")::rest) => Right(expr -> rest)
      case (expr, next::rest) => Left(s"""Expected ")" but got "${next.body}"!""" -> next.idx)
      case (expr, Nil) => Left(s"""Unexpected end of transform while looking for ")"!""" -> expr.endIdx)
    }
    case _ => Util.unexpectedEnd
  }
}
