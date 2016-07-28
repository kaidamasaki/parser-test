package com.socrata.ice.translate
package ast

import scala.annotation.tailrec

import token.{Symbol, Token}

import Transform._

case class Transform(expressions: List[Expr]) extends Transformation {
  def apply(row: List[String]): Either[String, List[String]] = {
    val bindings = row.zipWithIndex.map { case (cell, idx) => s"col${idx + 1}" -> cell }.toMap
    val parts = expressions.map { e => e(bindings) }

    extractResult(Util.sequence(parts))
  }

  override def toString = s"Transform(${expressions})"
}

object Transform {
  type Transformation = List[String] => Either[String, List[String]]

  def apply(definition: String): Either[String, Transformation] = symbolize(definition) match {
    case Symbol('[')::inner => for {
      tokens <- tokenizeInner(inner).right
      res <- extractResult(parseBody(tokens)).right
    } yield {
      val (transform, _) = res
      transform
    }

    case head::_ => Left(s"""Expected "[" got "$head"!""")
    case Nil => Left("""Unexpected end of transform while looking for "["!""")
  }

  def extractResult[T](result: Either[Error, T]): Either[String, T] = result match {
    case Left((msg, idx)) => Left(s"""$msg\n${" " * idx + "^"}""")
    case Right(res) => Right(res)
  }

  def symbolize(definition: String): List[Symbol] = definition.zipWithIndex.map(Symbol.fromTuple).toList

  def parseBody(tokens: List[Token], exprs: List[Expr] = Nil): Result[Transform] = {
    if (tokens == Nil) {
      Right(Transform(exprs.reverse) -> Nil)
    } else {
      Expr(tokens) match {
        case Right((expr, Nil)) => parseBody(Nil, expr::exprs)
        case Right((expr, Token(",")::rest)) => parseBody(rest, expr::exprs)
        case Right((expr, _)) => Left("Missing comma!", expr.endIdx)
        case Left(err) => Left(err)
        case _ => Util.unexpectedEnd
      }
    }
  }

  private val delims =  Set('"', '\'', '/')
  private val alnum = Set.empty ++ ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
  private val escapes = Map('n' -> '\n',
                            't' -> '\t',
                            'b' -> '\b',
                            'f' -> '\f',
                            'r' -> '\r')

  @tailrec
  def tokenizeAlnum(input: List[Symbol],
                    token: List[Symbol] = Nil): (Token, List[Symbol]) = input match {
    case head::tail if alnum(head.char) => tokenizeAlnum(tail, head::token)
    case rest => Token.fromSymbols(token.reverse) -> rest
  }

  @tailrec
  def tokenizeString(input: List[Symbol],
                     delim: Char,
                     token: List[Symbol]): Either[String, (Token, List[Symbol])] = input match {
    case head::rest if head.char == delim => Right(Token.fromSymbols((head::token).reverse) -> rest)
    case Symbol('\\')::(s @ Symbol(`delim`))::rest => tokenizeString(rest, delim, s::token)
    case Symbol('\\')::s::rest if escapes.keySet(s.char) => tokenizeString(rest, delim, s(escapes)::token)
    case Symbol('\\')::s::rest => tokenizeString(rest, delim, s::token)
    case s::rest => tokenizeString(rest, delim, s::token)
    case Nil => Left(s"""Unexpected end of transform while looking for matching "$delim"!""")
  }

  @tailrec
  def tokenizeInner(input: List[Symbol],
                    tokens: List[Token] = Nil): Either[String, List[Token]] = input match {
    case Symbol(']')::Nil => Right(tokens.reverse)
    case Symbol(']')::_ => Left("""Unexpected "]"!""")
    case (Symbol(' ')|Symbol('\t'))::rest => tokenizeInner(rest, tokens)
    case head::tail if alnum(head.char) => tokenizeAlnum(input) match {
      case (token, rest) => tokenizeInner(rest, token::tokens)
    }
    case delim::tail if delims(delim.char) => tokenizeString(tail, delim.char, delim::Nil) match {
      case Right((token, rest)) => tokenizeInner(rest, token::tokens)
      case Left(err) => Left(err)
    }
    case head::tail => tokenizeInner(tail, Token(head)::tokens)
    case Nil => Left("""Unexpected end of transform while looking for "]"!""")
  }
}
