package parser.internal

case class IfStatement(val cond: Condition, val left: List[Statement], val right: List[Statement]) extends Statement
