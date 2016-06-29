package transform

package object ast {
  def sequence[T](list: List[Option[T]]) =
    if (list.forall(_.isDefined)) Some(list.collect { case Some(item) => item }) else None

  type Result[T] = Option[Tuple2[T, List[String]]]
  type Parser[T] = (List[String] => Result[T])

  trait AstNode {
    def apply(bindings: Map[String,String]): Option[String]
  }

  case class Transform(expressions: List[Expr]) {
    def apply(row: List[String]): Option[List[String]] = {
      val bindings = row.zipWithIndex.map { case (cell, idx) => s"col${idx + 1}" -> cell }.toMap
      val parts = expressions.map { e => e(bindings) }

      sequence(parts)
    }
  }

  object Transform {
    private val regexp = """^\[(.*)\]$""".r

    def parseBody(tokens: List[String], exprs: List[Expr] = Nil): Option[List[Expr]] = {
      if (tokens == Nil) {
        Some(exprs.reverse)
      } else {
        Expr(tokens) match {
          case Some((expr, Nil)) =>
            parseBody(Nil, expr::exprs)
          case Some((expr, rest)) if rest.head == "," => // TODO: Handle whitespace?
            parseBody(rest.tail, expr::exprs)
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

    def parseFunc(expr: Result[Expr]): Result[Expr] = expr match {
      case Some((next, Nil)) => Some(next, Nil)
      case Some((next, "+" :: rest)) =>
        parseFunc(concat(rest, next))
      case Some((next, "(" :: rest)) =>
        parseFunc(Func(rest, next))
      case e @ Some(_) => e
      case _ => None
    }

    def apply(tokens: List[String]): Result[Expr] = {
      val expr = Id(tokens).
        orElse(Literal(tokens)).
        orElse(Wrapped(tokens))

      parseFunc(expr)
    }
  }

  case class Id(name: String) extends Expr {
    def apply(bindings: Map[String, String]) = Some(bindings.getOrElse(name, ""))
  }

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

  case class Func(name: String, args: List[Expr]) extends Expr {
    def parseArgs(bindings: Map[String, String]) = sequence(args.map(_(bindings)))

    def apply(bindings: Map[String, String]) = {
      (name, args) match {
        case ("+", _) => parseArgs(bindings).collect { case left::right::Nil => left + right }
        case ("trim", _) => parseArgs(bindings).collect { case str::Nil => str.trim }
        case ("slice", str::beg::Nil) => (str(bindings), beg) match {
          case (Some(str), begExp: IntLiteral) =>
            Some(str.slice(begExp.value, str.length))
          case _ => None
        }
        case ("slice", str::beg::end::Nil) => (str(bindings), beg, end) match {
          case (Some(str), begExp: IntLiteral, endExp: IntLiteral) =>
            Some(str.slice(begExp.value, endExp.value))
          case _ => None
        }
        case _ =>
          None
      }
    }
  }

  object Func {
    def apply(tokens: List[String], name: Expr): Result[Func] = {
      (name, tokens) match {
        case (id: Id, ")"::tail) =>
          Some(Func(id.name, Nil) -> tail)
        case (id: Id, head::tail) =>
          parseArguments(tokens).map { case (args, rest) =>
            Func(id.name, args) -> rest
          }
        case _ => None
      }
    }

    def parseArguments(tokens: List[String], args: List[Expr] = Nil): Result[List[Expr]] = {
      Expr(tokens).flatMap {
        case (expr, ","::tail) =>
          parseArguments(tail, expr::args)
        case (expr, ")"::tail) =>
          Some((expr::args).reverse -> tail)
        case _ =>
          None
      }
    }
  }

  class Literal(body: String, delim: Option[String]) extends Expr {
    def apply(bindings: Map[String, String]) = Some(body)

    override def toString = delim.map { d => s"Literal(${d}${body}${d})" }.getOrElse(body)
  }

  case class IntLiteral(value: Int) extends Literal(value.toString, None)

  object Literal extends Parser[Literal] {
    def apply(tokens: List[String]) = {
      if (tokens.isEmpty) {
        None
      } else {
        quote("\"", tokens).orElse(quote("'", tokens)).orElse(int(tokens))
      }
    }

    def apply(body: String, delim: Option[String]) = new Literal(body, delim)

    private def quote(delim: String, tokens: List[String]) = {
      val body = tokens.tail.takeWhile(_ != delim)
      val rest = tokens.tail.dropWhile(_ != delim)

      if (!rest.isEmpty && tokens.head == delim && rest.head == delim) {
        Some(Literal(body.mkString, Some(rest.head)) -> rest.tail)
      } else {
        None
      }
    }

    private def int(tokens: List[String]) = {
      try {
        Some(IntLiteral(tokens.head.toInt) -> tokens.tail)
      } catch {
        case _: NumberFormatException => None
      }
    }
  }

  case class Object(underlying: Map[String, Expr]) extends Expr {
    def apply(bindings: Map[String, String]) = ???
  }

  object Object {
    def parseBody(tokens: List[String]) = ???

    def apply(tokens: List[String]) = ???
  }
}
