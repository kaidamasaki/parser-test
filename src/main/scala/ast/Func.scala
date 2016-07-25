package com.socrata.ice.importer
package ast

import scala.annotation.tailrec

import token.Token
import Util.SuccessEither

case class Func(name: String, args: List[Expr], self: Option[Expr], idx: Int) extends Expr {
  override def endIdx = (args.map(_.endIdx) :+ (idx + name.length)).reduceLeft(_ max _) + 1

  def parseArgs(bindings: Map[String, String]) = Util.sequence(args.map(_(bindings)))

  def apply(bindings: Map[String, String]) = (name, args, self) match {
    case ("+", _, None) => parseArgs(bindings).collect { case left::right::Nil => left + right }
    // TODO: Add method variants of trim and slice?
    case ("trim", _, None) => parseArgs(bindings).collect { case str::Nil => str.trim }
    case ("upper", _, None) => parseArgs(bindings).collect { case str::Nil => str.toUpperCase }
    case ("lower", _, None) => parseArgs(bindings).collect { case str::Nil => str.toLowerCase }
    case ("title", _, None) => parseArgs(bindings).collect { case str::Nil =>
      str.replace('_', ' ').split(' ').map(_.capitalize).mkString(" ")
    }
    case ("toStateCode", _, None) => parseArgs(bindings).collect { case str::Nil => Util.stateCodes(str) }
    case ("slice", expr::(beg: IntLiteral)::Nil, None) =>
      expr(bindings).map { str => str.slice(beg.value, str.length) }
    case ("slice", expr::(beg: IntLiteral)::(end: IntLiteral)::Nil, None) =>
      expr(bindings).map { str => str.slice(beg.value, end.value) }
    case ("replace", RegexLiteral(pattern, global, _)::(toExpr: Expr)::Nil, Some(fromExpr)) =>
      for {
        from <- fromExpr(bindings).right
        to <- toExpr(bindings).right
      } yield {
        val matcher = pattern.matcher(from)
        if (global) matcher.replaceAll(to) else matcher.replaceFirst(to)
      }
    case ("stringify", (obj:Object)::Nil, Some(Id("JSON"))) => obj(bindings)
    case _ => Left(s"Incorrect arguments: $src." -> idx)
  }

  // case class Func(name: String, args: List[Expr], self: Option[Expr]) extends Expr {
  override def toString = s"""Func(${self.map{_ + "."}.getOrElse("")}${name}(${args.mkString(",")}))"""

  private def selfSrc = self.map { s => s.src + "." }.getOrElse("")
  override def src = s"""${selfSrc}${name}(args.map(_.src).mkString(",")"""
}

object Func {
  def apply(tokens: List[Token], name: Expr, self: Option[Expr]): Result[Func] = (name, tokens) match {
    case (id: Id, Token(")")::tail) =>
      Right(Func(id.name, Nil, self, id.idx) -> tail)
    case (id: Id, head::tail) =>
      parseArguments(tokens).collect { case (args, rest) =>
        Func(id.name, args, self, id.idx) -> rest
      }
    case _ => Util.unexpectedEnd
  }

  @tailrec
  def parseArguments(tokens: List[Token], args: List[Expr] = Nil): Result[List[Expr]] = {
    Expr(tokens) match {
      case Right((expr, Token(",")::tail)) =>
        parseArguments(tail, expr::args)
      case Right((expr, Token(")")::tail)) =>
        Right((expr::args).reverse -> tail)
      case Right((expr, head::tail)) =>
        Left(s"""Invalid argument list: expected "," or ")" got "${head.body}"!""" -> head.idx)
      case Right((expr, Nil)) =>
        Left("""Unexpected end of transform while looking for matching ")"!""" -> expr.endIdx)
      case Left(err) => Left(err)
    }
  }
}
