package com.socrata.ice.importer

import token.Token

package object ast {
  type Error = (String, Int)
  type Result[T] = Either[Error, Tuple2[T, List[Token]]]
  type Parser[T] = (List[Token] => Result[T])
}
