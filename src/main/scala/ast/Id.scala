package com.socrata.ice.importer
package ast

import token.Token

case class Id(name: String)(val idx: Int) extends Expr {
  val endIdx = idx + name.length

  def apply(bindings: Map[String, String]) = Right(bindings.getOrElse(name, ""))
}

object Id extends Parser[Id] {
  private val underlyingRegex = """^[a-zA-Z][a-zA-Z0-9]*$""".r
  def regex(s: String): java.util.regex.Matcher = underlyingRegex.pattern.matcher(s)

  def apply(token: Token): Id = Id(token.body)(token.idx)

  def apply(tokens: List[Token]): Result[Id] = tokens match {
    case head::tok::tail if tok.body == ":" => Left("""Unexpected ":"!""" -> tok.endIdx)
    case head::tail if regex(head.body).matches => Right(Id(head) -> tail)
    case _ => Util.unexpectedEnd
  }
}
