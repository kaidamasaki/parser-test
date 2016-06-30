package com.socrata.ice.importer.ast

case class Id(name: String) extends Expr {
  def apply(bindings: Map[String, String]) = Some(bindings.getOrElse(name, ""))
}

object Id extends Parser[Id] {
  private val underlyingRegex = """^[a-zA-Z][a-zA-Z0-9]*$""".r
  def regex(s: String): java.util.regex.Matcher = underlyingRegex.pattern.matcher(s)

  def apply(tokens: List[String]) =
    if (tokens.nonEmpty && regex(tokens.head).matches && tokens.tail.head != ":") {
      Some(Id(tokens.head) -> tokens.tail)
    } else {
      None
    }
}
