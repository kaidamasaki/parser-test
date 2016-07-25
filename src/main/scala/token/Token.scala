package com.socrata.ice.importer
package token

case class Token(body: String)(val idx: Int) {
  override def toString = s"Token($body, $idx)"
  val endIdx = idx + body.length
}

object Token {
  def apply(symbol: Symbol): Token = Token(symbol.char.toString)(symbol.idx)

  def fromSymbols(symbols: List[Symbol]): Token = symbols match {
    case head::tail => Token(symbols.map(_.char).mkString)(head.idx)
    case Nil => throw new IllegalArgumentException("Could not token underlying from empty symbols!")
  }
}
