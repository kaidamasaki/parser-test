package com.socrata.ice.importer
package ast

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
  // Because we aren't a case class...
  def apply(body: String, delim: Option[String]) = new Literal(body, delim)
  def unapply(literal: Literal): Option[(String, Option[String])] = Some((literal.body, literal.delim))

  // Parse
  def apply(tokens: List[String]) = tokens match {
    case Nil => Left("Invalid literal!")
    case _ => Util.chain(tokens)(quote("\""), quote("'"), regex, int, objectKey)
  }

  private def objectKey(tokens: List[String]) = tokens match {
    case head::(tail @ (":"::_)) if Id.regex(head).matches => Right(Literal(head, None) -> tail)
    case head::(tail @ (":"::_)) => Left(s"""Invalid identifier "$head"!""")
    case head::tail => Left("""Missing ":"!""")
    case Nil => Left("Unexpected end of transform!")
  }

  private def quote(delim: String)(tokens: List[String]) = tokens match {
    case lit::rest if lit.startsWith(delim) && lit.endsWith(delim) =>
      val body = lit.substring(1, lit.length - 1)

      Right(Literal(body, Some(delim)) -> rest)
    // TODO: Consoldate the language around these.  Do we need to localize errors?  Did we previously?
    case _ => Left("""Unexpected end of transform!""")
  }

  private def regex(tokens: List[String]) = quote("/")(tokens) match {
    case Right((Literal(pattern, _), head::tail)) if Id.regex(head).matches =>
      val flagSet: Set[Char] = head.toSet
      val flags = 1 |
        (if (flagSet('i')) Pattern.CASE_INSENSITIVE else 0) |
        (if (flagSet('m')) Pattern.MULTILINE else 0)
      val underlying = Pattern.compile(pattern, flags)
      Right(RegexLiteral(underlying, flagSet('g'), flagSet.mkString) -> tail)
    case Right((Literal(pattern, _), rest)) =>
      Right(RegexLiteral(Pattern.compile(pattern), false, "") -> rest)
    case err @ Left(_)  => err
  }

  private def int(tokens: List[String]) = tokens match {
    case head::tail => try {
      Right(IntLiteral(head.toInt) -> tail)
    } catch {
      case _: NumberFormatException => Left(s"""Invalid number: "$head"!""")
    }
    case _ => Left("Unexpected end of file!")
  }
}
