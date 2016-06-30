package com.socrata.ice.importer
package ast

case class Func(name: String, args: List[Expr], self: Option[Expr]) extends Expr {
  def parseArgs(bindings: Map[String, String]) = Util.sequence(args.map(_(bindings)))

  def apply(bindings: Map[String, String]) = (name, args, self) match {
    case ("+", _, None) => parseArgs(bindings).collect { case left::right::Nil => left + right }
    // TODO: Add method variants of trim and slice?
    case ("trim", _, None) => parseArgs(bindings).collect { case str::Nil => str.trim }
    case ("upper", _, None) => parseArgs(bindings).collect { case str::Nil => str.toUpperCase }
    case ("lower", _, None) => parseArgs(bindings).collect { case str::Nil => str.toLowerCase }
    case ("title", _, None) => parseArgs(bindings).collect { case str::Nil =>
      str.replace('_', ' ').split(' ').map(_.capitalize).mkString(" ")
    }
    case ("toStateCode", _, None) => parseArgs(bindings).collect { case str::Nil => Util.stateCodes(str) }
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
