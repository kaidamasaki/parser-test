package com.socrata.ice.importer
package ast

import scala.annotation.tailrec

import com.rojoma.json.v3.util.JsonUtil
import com.rojoma.json.v3.ast.{JObject, JValue}

import token.Token

case class Object(underlying: Map[Expr, Expr])(val idx: Int, val endIdx: Int) extends Expr {
  def apply(bindings: Map[String, String]) = toJson(bindings).map(JsonUtil.renderJson(_))

  override def toJson(bindings: Map[String, String]) = {
    val evaluated: Map[Option[String], Option[JValue]] =
      underlying.collect { case (keyExp, valExp) => keyExp(bindings) -> valExp.toJson(bindings) }

    if (evaluated.forall { case (k, v) => k.isDefined && v.isDefined }) {
      Some(JObject(evaluated.collect { case (Some(key), Some(value)) => key -> value }))
    } else {
      None
    }
  }
}

object Object extends Parser[Object] {
  def apply(tokens: List[Token]) = tokens match {
    case open::close::tail if open.body == "{" && close.body == "}" =>
      Right(Object(Map[Expr, Expr]())(open.idx, close.idx) -> tail)
    case open::tail if open.body == "{" => parseBody(tail, open.idx)
    case _ => Util.unexpectedEnd
  }

  @tailrec
  def parseBody(tokens: List[Token],
                idx: Int,
                lastKey: Option[Expr] = None,
                pairs: List[(Expr, Expr)] = Nil): Result[Object] = (Expr(tokens), lastKey) match {
    case (Right((key, Token(":")::tail)), None) =>
      parseBody(tail, idx, Some(key), pairs)
    case (Right((value, Token(",")::tail)), Some(key)) =>
      parseBody(tail, idx, None, (key -> value)::pairs)
    case (Right((value, close::tail)), Some(key)) if close.body == "}" =>
      Right(Object(((key -> value)::pairs).reverse.toMap)(idx, close.idx) -> tail)
    // Error handling!
    case (Right((key, Token(":")::tail)), Some(_)) =>
      Left(s"""Malformed object: expected value got "${key.src}:"!""" -> key.idx)
    case (Right((value, Token(",")::tail)), None) =>
      Left(s"""Malformed object: expected KEY : VALUE got "${value.src},"!""" -> value.idx)
    case (Right((value, Token("}")::tail)), None) =>
      Left(s"""Malformed object: expected KEY: VALUE got "${value.src}}"!""" -> value.idx)
    case (Right((expr, unexp::tail)), _) =>
      Left(s"""Malformed object: unexpected character "${unexp.body}"!""" -> unexp.idx)
    case (Right((expr, Nil)), _) =>
      Util.unexpectedEnd
    case (Left(err), _) => Left(err)
  }
}
