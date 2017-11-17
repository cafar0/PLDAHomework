package parser.internal

case class FunctionCall(name: String, value: Map[String, Expr]) extends Expr with Statement
