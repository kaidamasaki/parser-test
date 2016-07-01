package com.socrata.ice.importer

package object ast {
  type Result[T] = Either[String, Tuple2[T, List[String]]]
  type Parser[T] = (List[String] => Result[T])
}
