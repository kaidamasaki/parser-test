package transform

import java.util.regex.Pattern

import com.rojoma.json.v3.util.JsonUtil
import com.rojoma.json.v3.ast.{JNumber, JObject, JString, JValue}

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
    private val regex = """^\[(.*)\]$""".r

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
      case regex(inner) =>
        val tokens = inner.split("((?<=[^a-zA-Z0-9])|(?=[^a-zA-Z0-9]))").toList
        parseBody(tokens).map(Transform(_))
      case _ => None
    }
  }

  trait Expr extends AstNode {
    def toJson(bindings: Map[String, String]): Option[JValue] = apply(bindings).map(JString)
  }

  object Expr extends Parser[Expr] {
    def apply(tokens: List[String]): Result[Expr] = {
      val expr = Id(tokens).
        orElse(Literal(tokens)).
        orElse(Wrapped(tokens)).
        orElse(Object(tokens))

      println(s"Expr: $expr")
      parseFunc(expr, None)
    }

    def concat(tokens: List[String], left: Expr): Result[Func] =
      Expr(tokens).map { case (right, rest) => Func("+", List(left, right), None) -> rest }

    def parseFunc(expr: Result[Expr], self: Option[Expr]): Result[Expr] = {
      println(s"parseFunc: $expr / $self")

      (expr, self) match {
        case (Some((next, Nil)), _) => Some(next, Nil)
        case (Some((left, "+" :: rest)), _) => parseFunc(concat(rest, left), self)
        case (Some((name, "(" :: rest)), _) => parseFunc(Func(rest, name, self), None)
        case (Some((newSelf, "." :: rest)), None) => parseFunc(Id(rest), Some(newSelf))
        case (Some(result), None) => Some(result)
        case _ => None
      }
    }
  }

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

  object Wrapped extends Parser[Expr] {
    def apply(tokens: List[String]) = {
      if (tokens.isEmpty) {
        None
      } else {
        Expr(tokens.tail) match {
          case Some((expr, rest)) if (tokens.head == "(" && rest.head == ")") =>
            Some(expr -> rest.tail)
          case _ => None
        }
      }
    }
  }

  case class Func(name: String, args: List[Expr], self: Option[Expr]) extends Expr {
    def parseArgs(bindings: Map[String, String]) = sequence(args.map(_(bindings)))

    def apply(bindings: Map[String, String]) = (name, args, self) match {
      case ("+", _, None) => parseArgs(bindings).collect { case left::right::Nil => left + right }
      // TODO: Add method variants of trim and slice?
      case ("trim", _, None) => parseArgs(bindings).collect { case str::Nil => str.trim }
      case ("slice", expr::(beg: IntLiteral)::Nil, None) =>
        expr(bindings).map { str => str.slice(beg.value, str.length) }
      case ("slice", expr::(beg: IntLiteral)::(end: IntLiteral)::Nil, None) =>
        expr(bindings).map { str => str.slice(beg.value, end.value) }
      case ("replace", RegexLiteral(pattern, global, _)::(toExpr: Expr)::Nil, Some(fromExpr)) =>
        for {
          from <- fromExpr(bindings);
          to <- toExpr(bindings)
        } yield {
          val matcher = pattern.matcher(from)
          if (global) matcher.replaceAll(to) else matcher.replaceFirst(to)
        }
      case ("stringify", (obj:Object)::Nil, Some(Id("JSON"))) => obj(bindings)
      case _ => None
    }
  }

  object Func {
    def apply(tokens: List[String], name: Expr, self: Option[Expr]): Result[Func] = (name, tokens) match {
      case (id: Id, ")"::tail) =>
        Some(Func(id.name, Nil, self) -> tail)
      case (id: Id, head::tail) =>
        parseArguments(tokens).map { case (args, rest) =>
          Func(id.name, args, self) -> rest
        }
      case _ => None
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

  class Literal(val body: String, val delim: Option[String]) extends Expr {
    def apply(bindings: Map[String, String]) = Some(body)

    override def toString = {
      val inner = delim.map { d => s"${d}${body}${d}" }.getOrElse(body)

      s"Literal(${inner})"
    }
  }

  case class IntLiteral(value: Int) extends Literal(value.toString, None) {
    override def toJson(bindings: Map[String, String]) = Some(JNumber(value))
  }

  case class RegexLiteral(regex: Pattern, global: Boolean, flags: String)
      extends Literal(s"/${regex}/${flags}", None)

  object Literal extends Parser[Literal] {
    def apply(tokens: List[String]) = {
      if (tokens.isEmpty) {
        None
      } else {
        quote("\"", tokens).
          orElse(quote("'", tokens)).
          orElse(regex(tokens)).
          orElse(int(tokens)).
          orElse(objectKey(tokens))
      }
    }

    def unapply(literal: Literal): Option[(String, Option[String])] = Some((literal.body, literal.delim))

    def apply(body: String, delim: Option[String]) = new Literal(body, delim)

    private def objectKey(tokens: List[String]) = {
      if (Id.regex(tokens.head).matches && tokens.tail.head == ":") {
        Some(Literal(tokens.head, None) -> tokens.tail)
      } else {
        None
      }
    }

    private def quote(delim: String, tokens: List[String]) = {
      val body = tokens.tail.takeWhile(_ != delim)
      val rest = tokens.tail.dropWhile(_ != delim)

      if (rest.nonEmpty && tokens.head == delim && rest.head == delim) {
        Some(Literal(body.mkString, Some(rest.head)) -> rest.tail)
      } else {
        None
      }
    }

    private def regex(tokens: List[String]) = quote("/", tokens) match {
      case Some((Literal(pattern, _), head::tail)) if Id.regex(head).matches =>
        val flagSet: Set[Char] = head.toSet
        val flags = 1 |
          (if (flagSet('i')) Pattern.CASE_INSENSITIVE else 0) |
          (if (flagSet('m')) Pattern.MULTILINE else 0)
        val underlying = Pattern.compile(pattern, flags)
        Some(RegexLiteral(underlying, flagSet('g'), flagSet.mkString) -> tail)
      case Some((Literal(pattern, _), rest)) =>
        Some(RegexLiteral(Pattern.compile(pattern), false, "") -> rest)
      case _ => None
    }

    private def int(tokens: List[String]) = {
      try {
        Some(IntLiteral(tokens.head.toInt) -> tokens.tail)
      } catch {
        case _: NumberFormatException => None
      }
    }
  }

  case class Object(underlying: Map[Expr, Expr]) extends Expr {
    def apply(bindings: Map[String, String]) = toJson(bindings).map(JsonUtil.renderJson(_))

    override def toJson(bindings: Map[String, String]) = {
      val evaluated: Map[Option[String], Option[JValue]] =
        underlying.collect { case (keyExp, valExp) => keyExp(bindings) -> valExp.toJson(bindings) }

      if (evaluated.forall { case (k, v) => k.isDefined && v.isDefined }) {
        val obj = evaluated.collect { case (Some(key), Some(value)) => key -> value }

        Some(JObject(obj))
      } else {
        None
      }
    }
  }

  object Object extends Parser[Object] {
    def apply(tokens: List[String]) = tokens match {
      case "{"::"}"::tail => Some(Object(Map[Expr, Expr]()) -> tail)
      case "{"::tail => parseBody(tokens.tail)
      case _ => None
    }

    def parseBody(tokens: List[String],
                  lastKey: Option[Expr] = None,
                  pairs: List[(Expr, Expr)] = Nil): Result[Object] = Expr(tokens) match {
      case Some((key, ":"::tail)) =>
        parseBody(tail, Some(key), pairs)
      case Some((value, ","::tail)) => lastKey.flatMap { k =>
        parseBody(tail, None, (k -> value)::pairs)
      }
      case Some((value, "}"::tail)) =>
        val obj: Option[Map[Expr, Expr]] = lastKey.map { k => ((k -> value)::pairs).reverse.toMap }

        obj.map { o => Object(o) -> tail }
      case _ => None
    }
  }
}
