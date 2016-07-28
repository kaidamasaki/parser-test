package com.socrata.ice.translate

import scala.reflect.ClassTag

import ast.{Parser, Result}
import token.Token

object Util {
  private val EndOfFile = -1

  private[translate] def unexpected[T](idx: Int)(implicit tag: ClassTag[T]): Result[T] = {
    Left(s"Unexpected error while parsing ${tag.runtimeClass.getSimpleName}!" -> idx)
  }

  private[translate] def unexpectedEnd[T](implicit tag: ClassTag[T]): Result[T] = {
    Left(s"Unexpected end of transform while parsing ${tag.runtimeClass.getSimpleName}!" -> EndOfFile)
  }

  private[translate] val stateCodes = Map(
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

  implicit class SuccessEither[F,S](underlying: Either[F,S]) {
    def map[T](f: S => T): Either[F,T] = underlying.right.map(f)
    def collect[T](f: PartialFunction[S,T]): Either[F,T] = underlying.right.map(i => f(i))
  }

  private[translate] def sequence[T](list: List[Either[ast.Error, T]]): Either[ast.Error, List[T]] = {
    list.find(_.isLeft) match {
      case Some(Left(err)) => Left(err)
      case None => Right(list.collect { case Right(item) => item })
      case _ => throw new IllegalStateException("Type should already have been verified with find(_.isLeft)!")
    }
  }

  private[translate] def chain[T] (in: List[Token], last: Option[ast.Error] = None)(parsers: Parser[T]*)
                             (implicit tag: ClassTag[T]): Result[T] =
    parsers.toList match {
      case head::tail => head(in) match {
        case expr @ Right(_) => expr
        case Left(err) => chain(in, Some(err))(tail: _*)
      }
      case Nil => Left(last.get)
    }
}
