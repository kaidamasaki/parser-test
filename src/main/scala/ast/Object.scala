package com.socrata.ice.importer.ast

import scala.annotation.tailrec

import com.rojoma.json.v3.util.JsonUtil
import com.rojoma.json.v3.ast.{JObject, JValue}

case class Object(underlying: Map[Expr, Expr]) extends Expr {
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
  def apply(tokens: List[String]) = tokens match {
    case "{"::"}"::tail => Right(Object(Map[Expr, Expr]()) -> tail)
    case "{"::tail => parseBody(tail)
    case _ => Left("Malformed object!")
  }

  @tailrec
  def parseBody(tokens: List[String],
                lastKey: Option[Expr] = None,
                pairs: List[(Expr, Expr)] = Nil): Result[Object] = (Expr(tokens), lastKey) match {
    case (Right((key, ":"::tail)), None) => parseBody(tail, Some(key), pairs)
    case (Right((value, ","::tail)), Some(key)) => parseBody(tail, None, (key -> value)::pairs)
    case (Right((value, "}"::tail)), Some(key)) => Right(Object(((key -> value)::pairs).reverse.toMap) -> tail)
    // Error handling!
    case (Right((key, ":"::tail)), Some(_)) => Left(s"""Malformed object: expected value got "$key:"!""")
    case (Right((value, ","::tail)), None) => Left(s"""Malformed object: expected KEY : VALUE got "$value,"!""")
    case (Right((value, "}"::tail)), None) => Left(s"""Malformed object: expected KEY: VALUE got "$value}"!""")
    case (Right((expr, unexp::tail)), _) => Left(s"""Malformed object: unexpected character "$unexp"!""")
    case (Right((expr, Nil)), _) => Left(s"""Malformed object: unexpected end of transform!""")
    case (Left(err), _) => Left(err)
  }
}
