package com.socrata.ice.importer
package ast

import scala.annotation.tailrec

case class Transform(expressions: List[Expr]) {
  def apply(row: List[String]): Option[List[String]] = {
    val bindings = row.zipWithIndex.map { case (cell, idx) => s"col${idx + 1}" -> cell }.toMap
    val parts = expressions.map { e => e(bindings) }

    Util.sequence(parts)
  }
}

object Transform {
  def apply(definition: String): Either[String, Transform] = definition.toList match {
    case '['::inner => for {
      tokens <- tokenizeInner(inner).right
      body <- parseBody(tokens).right
    } yield body match {
      case (transform, _) => transform
    }

    case head::_ => Left(s"""Expected "[" got "$head"!""")
    case Nil => Left("""Unexpected end of transform while looking for "["!""")
  }

  def parseBody(tokens: List[String], exprs: List[Expr] = Nil): Result[Transform] = {
    if (tokens == Nil) {
      Right(Transform(exprs.reverse) -> Nil)
    } else {
      Expr(tokens) match {
        case Right((expr, Nil)) =>
          parseBody(Nil, expr::exprs)
        case Right((expr, ","::rest)) =>
          parseBody(rest, expr::exprs)
        case Right((expr, _)) => Left("Missing comma!")
        case Left(err) => Left(err)
        case _ => Util.unexpected
      }
    }
  }

  private val alnum = Set.empty ++ ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
  private val escapes = Map('n' -> '\n',
                            't' -> '\t',
                            'b' -> '\b',
                            'f' -> '\f',
                            'r' -> '\r')

  @tailrec
  def tokenizeAlnum(input: List[Char],
                    token: List[Char] = Nil): (String, List[Char]) = input match {
    case head::tail if alnum(head) => tokenizeAlnum(tail, head::token)
    case rest => token.reverse.mkString -> rest
  }

  @tailrec
  def tokenizeString(input: List[Char],
                     delim: Char,
                     token: List[Char]): Either[String, (String, List[Char])] = input match {
    case `delim`::rest => Right((delim::token).reverse.mkString -> rest)
    case '\\'::`delim`::rest => tokenizeString(rest, delim, delim::token)
    case '\\'::c::rest if escapes.keySet(c) => tokenizeString(rest, delim, escapes(c)::token)
    case '\\'::c::rest => tokenizeString(rest, delim, c::token)
    case c::rest => tokenizeString(rest, delim, c::token)
    case Nil => Left(s"""Unexpected end of transform while looking for matching "$delim"!""")
  }

  @tailrec
  def tokenizeInner(input: List[Char],
                    tokens: List[String] = Nil): Either[String, List[String]] = input match {
    case ']'::Nil => Right(tokens.reverse)
    case ']'::_ => Left("""Unexpected "]"!""")
    case head::tail if alnum(head) => tokenizeAlnum(input) match {
      case (token, rest) => tokenizeInner(rest, token::tokens)
    }
    case (delim @ ('"'|'\''|'/'))::tail => tokenizeString(tail, delim, delim::Nil) match {
      case Right((token, rest)) => tokenizeInner(rest, token::tokens)
      case Left(err) => Left(err)
    }
    case head::tail => tokenizeInner(tail, head.toString::tokens)
    case Nil => Left("""Unexpected end of transform while looking for "]"!""")
  }
}
