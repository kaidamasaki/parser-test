package transform

package object ast {
  type Result[T] = Option[Tuple2[T, List[String]]]
  type Parser[T] = (List[String] => Result[T])

  trait AstNode

  case class Transform(expressions: List[Expr]) extends AstNode
  object Transform {
    private val regexp = """^\[(.*)\]$""".r

    // TODO: Transform to @tailrec
    def parseBody(tokens: List[String], exprs: List[Expr] = Nil): Option[List[Expr]] = {
      if (tokens == Nil) {
        Some(exprs.reverse)
      } else {
        Expr(tokens).flatMap {
          case (expr, Nil) => Some(exprs :+ expr)
          case (expr, rest) if rest.head == "," => // TODO: Handle whitespace?
            parseBody(rest.tail, exprs :+ expr)
          case _ => None
        }
      }
    }

    def apply(definition: String): Option[Transform] = definition match {
      case regexp(inner) =>
        val tokens = inner.split("((?<=[^a-zA-Z0-9])|(?=[^a-zA-Z0-9]))").toList
        parseBody(tokens).map(Transform(_))
      case _ => None
    }
  }

  trait Expr extends AstNode
  object Expr extends Parser[Expr] {
    def concat(tokens: List[String], left: Expr): Result[Func] =
      Expr(tokens).map { case (right, rest) => Func("+", List(left, right)) -> rest }

    def apply(tokens: List[String]): Result[Expr] = {
      val expr = Id(tokens).
        orElse(Literal(tokens)).
        orElse(Wrapped(tokens))

      expr match {
        case Some((next, head :: rest)) if (head == "+") =>
          concat(rest, next)
        case e @ Some(_) => e
        case _ => None
      }
    }
  }

  case class Id(name: String) extends Expr
  object Id extends Parser[Id] {
    private val regexp = """^[a-zA-Z][a-zA-Z0-9]+$""".r

    def apply(tokens: List[String]) = if (regexp.pattern.matcher(tokens.head).matches) {
      Some(Id(tokens.head) -> tokens.tail)
    } else {
      None
    }
  }

  object Wrapped extends Parser[Expr] {
    def apply(tokens: List[String]) = Expr(tokens.tail) match {
      case Some((expr, rest)) if (tokens.head == "(" && rest.head == ")") =>
        Some(expr -> rest.tail)
      case _ => None
    }
  }

  case class Func(name: String, args: List[Expr]) extends Expr

  case class Literal(body: String) extends Expr
  object Literal extends Parser[Literal] {
    private def quote(delim: String, tokens: List[String]) = {
      val body = tokens.tail.takeWhile(_ != delim)
      val rest = tokens.tail.dropWhile(_ != delim)

      if (!rest.isEmpty && tokens.head == delim && rest.head == delim) {
        Some(Literal(tokens.head + body.mkString + rest.head) -> rest.tail)
      } else {
        None
      }
    }

    def apply(tokens: List[String]) = {
      if (tokens.isEmpty) None else quote("\"", tokens).orElse(quote("'", tokens))
    }
  }
}
