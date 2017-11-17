package parser.internal

import scala.collection.immutable.HashMap

case class FunctionCall(name: String, values: HashMap[String, Expr]) extends Expr with Statement
