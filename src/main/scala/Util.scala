package com.socrata.ice.importer

import scala.reflect.ClassTag

import ast.{Parser, Result}
import token.Token

object Util {
  private[importer] def unexpected[T](implicit tag: ClassTag[T]): Result[T] = {
    Left(s"Unexpected error whlie parsing ${tag.runtimeClass.getSimpleName}!")
  }

  private[importer] val stateCodes = Map(
    "alabama" -> "AL",
    "alaska" -> "AK",
    "arizona" -> "AZ",
    "arkansas" -> "AR",
    "california" -> "CA",
    "colorado" -> "CO",
    "connecticut" -> "CT",
    "delaware" -> "DE",
    "florida" -> "FL",
    "georgia" -> "GA",
    "hawaii" -> "HI",
    "idaho" -> "ID",
    "illinois" -> "IL",
    "indiana" -> "IN",
    "iowa" -> "IA",
    "kansas" -> "KS",
    "kentucky" -> "KY",
    "louisiana" -> "LA",
    "maine" -> "ME",
    "maryland" -> "MD",
    "massachusetts" -> "MA",
    "michigan" -> "MI",
    "minnesota" -> "MN",
    "mississippi" -> "MS",
    "missouri" -> "MO",
    "montana" -> "MT",
    "nebraska" -> "NE",
    "nevada" -> "NV",
    "new hampshire" -> "NH",
    "new jersey" -> "NJ",
    "new mexico" -> "NM",
    "new york" -> "NY",
    "north carolina" -> "NC",
    "north dakota" -> "ND",
    "ohio" -> "OH",
    "oklahoma" -> "OK",
    "oregon" -> "OR",
    "pennsylvania" -> "PA",
    "rhode island" -> "RI",
    "south carolina" -> "SC",
    "south dakota" -> "SD",
    "tennessee" -> "TN",
    "texas" -> "TX",
    "utah" -> "UT",
    "vermont" -> "VT",
    "virginia" -> "VA",
    "washington" -> "WA",
    "west virginia" -> "WV",
    "wisconsin" -> "WI",
    "wyoming" -> "WY"
  )

  private[importer] def sequence[T](list: List[Option[T]]) =
    if (list.forall(_.isDefined)) Some(list.collect { case Some(item) => item }) else None

  private[importer] def chain[T](in: List[Token])(parsers: Parser[T]*)(implicit tag: ClassTag[T]): Result[T] =
    parsers.toList match {
      case head::tail => head(in) match {
        case expr @ Right(_) => expr
        case Left(_) => chain(in)(tail: _*)
      }
      case Nil => unexpected[T]
    }
}
