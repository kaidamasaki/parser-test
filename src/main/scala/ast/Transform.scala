package com.socrata.ice.importer
package ast

case class Transform(expressions: List[Expr]) {
  def apply(row: List[String]): Option[List[String]] = {
    val bindings = row.zipWithIndex.map { case (cell, idx) => s"col${idx + 1}" -> cell }.toMap
    val parts = expressions.map { e => e(bindings) }

    Util.sequence(parts)
  }
}

object Transform {
  private val regex = """^\[(.*)\]$""".r

  def parseBody(tokens: List[String], exprs: List[Expr] = Nil): Option[List[Expr]] = {
    if (tokens == Nil) {
      Some(exprs.reverse)
    } else {
      Expr(tokens) match {
        case Some((expr, Nil)) =>
          parseBody(Nil, expr::exprs)
        case Some((expr, rest)) if rest.head == "," => // TODO: Handle whitespace?
          parseBody(rest.tail, expr::exprs)
        case _ => None
      }
    }
  }

  def apply(definition: String): Option[Transform] = definition match {
    case regex(inner) =>
      val tokens = inner.split("((?<=[^a-zA-Z0-9])|(?=[^a-zA-Z0-9]))").toList
      parseBody(tokens).map(Transform(_))
    case _ => None
  }
}
