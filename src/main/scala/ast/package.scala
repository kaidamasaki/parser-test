package com.socrata.ice.importer

import token.Token

package object ast {
  type Result[T] = Either[String, Tuple2[T, List[Token]]]
  type Parser[T] = (List[Token] => Result[T])
}
