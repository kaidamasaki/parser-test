package com.socrata.ice.translate
package token

case class Symbol(char: Char)(val idx: Int) {
  def apply(transform: Char => Char) = Symbol(transform(char))(idx)

  override def toString = s"Symbol($char, $idx)"
}

object Symbol {
  def fromTuple(pair: (Char, Int)): Symbol = {
    val (char, idx) = pair
    Symbol(char)(idx)
  }
}
