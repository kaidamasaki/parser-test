package com.socrata.ice.importer
package ast

import java.util.regex.Pattern

import com.rojoma.json.v3.ast.JNumber

import token.Token

class Literal(val body: String, val delim: Option[String], val idx: Int) extends Expr {
  def apply(bindings: Map[String, String]) = Some(body)

  lazy val endIdx = idx + body.length + 2 * delim.getOrElse("").length

  override def toString = {
    val inner = delim.map { d => s"${d}${body}${d}" }.getOrElse(body)

    s"Literal(${inner})"
  }
}

case class IntLiteral(value: Int)(idx: Int) extends Literal(value.toString, None, idx) {
  override def toJson(bindings: Map[String, String]) = Some(JNumber(value))
}

case class RegexLiteral(regex: Pattern, global: Boolean, flags: String)(idx: Int)
    extends Literal(s"/${regex}/${flags}", None, idx)

object Literal extends Parser[Literal] {
  // Because we aren't a case class...
  def apply(body: String, delim: Option[String], idx: Int) = new Literal(body, delim, idx)
  def unapply(literal: Literal): Option[(String, Option[String])] = Some((literal.body, literal.delim))

  // Parse
  def apply(tokens: List[Token]) = tokens match {
    case Nil => Util.unexpectedEnd
    case _ => Util.chain(tokens)(quote("\""), quote("'"), regex, int, objectKey)
  }

  private def objectKey(tokens: List[Token]): Result[Literal] = tokens match {
    case head::(tail @ (Token(":")::_)) if Id.regex(head.body).matches =>
      Right(Literal(head.body, None, head.idx) -> tail)
    case head::(tail @ (Token(":")::_)) => Left(s"""Invalid identifier "${head.body}"!""" -> head.idx)
    case head::tail => Left("""Missing ":"!""" -> head.endIdx)
    case Nil => Util.unexpectedEnd
  }

  private def quote(delim: String)(tokens: List[Token]): Result[Literal] = tokens match {
    case lit::rest if lit.body.startsWith(delim) && lit.body.endsWith(delim) =>
      val body = lit.body.substring(1, lit.body.length - 1)

      Right(Literal(body, Some(delim), lit.idx) -> rest)
    // TODO: Consoldate the language around these.  Do we need to localize errors?  Did we previously?
    case _ => Util.unexpectedEnd
  }

  private def regex(tokens: List[Token]): Result[Literal] = quote("/")(tokens) match {
    case Right((Literal(pattern, _), head::tail)) if Id.regex(head.body).matches =>
      val flagSet: Set[Char] = head.body.toSet
      val flags = 1 |
        (if (flagSet('i')) Pattern.CASE_INSENSITIVE else 0) |
        (if (flagSet('m')) Pattern.MULTILINE else 0)
      val underlying = Pattern.compile(pattern, flags)
      Right(RegexLiteral(underlying, flagSet('g'), flagSet.mkString)(head.idx) -> tail)
    case Right(((l @ Literal(pattern, _)), rest)) =>
      Right(RegexLiteral(Pattern.compile(pattern), false, "")(l.idx) -> rest)
    case err @ Left(_)  => err
  }

  private def int(tokens: List[Token]): Result[Literal] = tokens match {
    case head::tail => try {
      Right(IntLiteral(head.body.toInt)(head.idx) -> tail)
    } catch {
      case _: NumberFormatException => Left(s"""Invalid number: "${head.body}"!""" -> head.idx)
    }
    case _ => Util.unexpectedEnd
  }
}
