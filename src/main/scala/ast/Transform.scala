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

  def parseBody(tokens: List[String], exprs: List[Expr] = Nil): Either[String, List[Expr]] = {
    if (tokens == Nil) {
      Right(exprs.reverse)
    } else {
      Expr(tokens) match {
        case Right((expr, Nil)) =>
          parseBody(Nil, expr::exprs)
        case Right((expr, ","::rest)) =>
          parseBody(rest, expr::exprs)
        case Right((expr, _)) => Left("Missing comma!")
        case Left(err) => Left(err)
        case _ => Left("Unexpected error!")
      }
    }
  }

  def apply(definition: String): Either[String, Transform] = definition match {
    case regex(inner) =>
      val tokens = inner.split("((?<=[^a-zA-Z0-9])|(?=[^a-zA-Z0-9]))").toList
      parseBody(tokens).right.map(Transform(_))
    case _ => Left("""Missing opening "[" or closing "]"!""")
  }
}
