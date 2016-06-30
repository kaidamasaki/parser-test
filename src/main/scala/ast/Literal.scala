package com.socrata.ice.importer.ast

import java.util.regex.Pattern

import com.rojoma.json.v3.ast.JNumber

class Literal(val body: String, val delim: Option[String]) extends Expr {
  def apply(bindings: Map[String, String]) = Some(body)

  override def toString = {
    val inner = delim.map { d => s"${d}${body}${d}" }.getOrElse(body)

    s"Literal(${inner})"
  }
}

case class IntLiteral(value: Int) extends Literal(value.toString, None) {
  override def toJson(bindings: Map[String, String]) = Some(JNumber(value))
}

case class RegexLiteral(regex: Pattern, global: Boolean, flags: String)
    extends Literal(s"/${regex}/${flags}", None)

object Literal extends Parser[Literal] {
  def apply(tokens: List[String]) = {
    if (tokens.isEmpty) {
      None
    } else {
      quote("\"", tokens).
        orElse(quote("'", tokens)).
        orElse(regex(tokens)).
        orElse(int(tokens)).
        orElse(objectKey(tokens))
    }
  }

  def unapply(literal: Literal): Option[(String, Option[String])] = Some((literal.body, literal.delim))

  def apply(body: String, delim: Option[String]) = new Literal(body, delim)

  private def objectKey(tokens: List[String]) = {
    if (Id.regex(tokens.head).matches && tokens.tail.head == ":") {
      Some(Literal(tokens.head, None) -> tokens.tail)
    } else {
      None
    }
  }

  private def quote(delim: String, tokens: List[String]) = {
    val body = tokens.tail.takeWhile(_ != delim)
    val rest = tokens.tail.dropWhile(_ != delim)

    if (rest.nonEmpty && tokens.head == delim && rest.head == delim) {
      Some(Literal(body.mkString, Some(rest.head)) -> rest.tail)
    } else {
      None
    }
  }

  private def regex(tokens: List[String]) = quote("/", tokens) match {
    case Some((Literal(pattern, _), head::tail)) if Id.regex(head).matches =>
      val flagSet: Set[Char] = head.toSet
      val flags = 1 |
        (if (flagSet('i')) Pattern.CASE_INSENSITIVE else 0) |
        (if (flagSet('m')) Pattern.MULTILINE else 0)
      val underlying = Pattern.compile(pattern, flags)
      Some(RegexLiteral(underlying, flagSet('g'), flagSet.mkString) -> tail)
    case Some((Literal(pattern, _), rest)) =>
      Some(RegexLiteral(Pattern.compile(pattern), false, "") -> rest)
    case _ => None
  }

  private def int(tokens: List[String]) = {
    try {
      Some(IntLiteral(tokens.head.toInt) -> tokens.tail)
    } catch {
      case _: NumberFormatException => None
    }
  }
}
