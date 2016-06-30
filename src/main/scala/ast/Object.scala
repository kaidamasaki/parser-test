package com.socrata.ice.importer.ast

import com.rojoma.json.v3.util.JsonUtil
import com.rojoma.json.v3.ast.{JObject, JValue}

case class Object(underlying: Map[Expr, Expr]) extends Expr {
  def apply(bindings: Map[String, String]) = toJson(bindings).map(JsonUtil.renderJson(_))

  override def toJson(bindings: Map[String, String]) = {
    val evaluated: Map[Option[String], Option[JValue]] =
      underlying.collect { case (keyExp, valExp) => keyExp(bindings) -> valExp.toJson(bindings) }

    if (evaluated.forall { case (k, v) => k.isDefined && v.isDefined }) {
      val obj = evaluated.collect { case (Some(key), Some(value)) => key -> value }

      Some(JObject(obj))
    } else {
      None
    }
  }
}

object Object extends Parser[Object] {
  def apply(tokens: List[String]) = tokens match {
    case "{"::"}"::tail => Some(Object(Map[Expr, Expr]()) -> tail)
    case "{"::tail => parseBody(tokens.tail)
    case _ => None
  }

  def parseBody(tokens: List[String],
                lastKey: Option[Expr] = None,
                pairs: List[(Expr, Expr)] = Nil): Result[Object] = Expr(tokens) match {
    case Some((key, ":"::tail)) =>
      parseBody(tail, Some(key), pairs)
    case Some((value, ","::tail)) => lastKey.flatMap { k =>
      parseBody(tail, None, (k -> value)::pairs)
    }
    case Some((value, "}"::tail)) =>
      val obj: Option[Map[Expr, Expr]] = lastKey.map { k => ((k -> value)::pairs).reverse.toMap }

      obj.map { o => Object(o) -> tail }
    case _ => None
  }
}
