package parser

import parser.internal._
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers


class LanguageParser extends JavaTokenParsers {
  override val skipWhitespace: Boolean = true

  def program: Parser[Program] = {
    for {
      _ <- literal("main :")
      block <- codeblock
    } yield new Program(block)
  }

  def codeblock: Parser[List[Statement]] = {
    for {
      block <- rep(statement)
    } yield block
  }

  def statement: Parser[Statement] =
    for {
      statement <- variableAssignment | ifStatement | outStatement
    } yield statement

  //Print Function
  def outStatement: Parser[PrintStatement] = {
    for {
      _ <- literal("println")
      e <- expr
    } yield  new PrintStatement(e)
  }


  //Variable assignment
  def variableAssignment: Parser[VariableDefinition] = {
    for {
      _ <- literal("let")
      name <- ident
      _ <- literal("=")
      value <- expr
    } yield new VariableDefinition(name, value)
  }

  //If statement
  def ifStatement: Parser[IfStatement] = {
    for {
      cond <- conditional
      trueCase <- codeblock
      _ <- literal(":")
      falseCase <- codeblock
    } yield  new IfStatement(cond, trueCase, falseCase)
  }

  def conditional: Parser[Condition] = {
    for {
      _ <- literal("if")
      _ <- literal("(")
      cond <- condition
      _ <- literal(")")
      _ <- literal("?")
    } yield cond
  }

  def condition: Parser[Condition] = {
    for {
      left <- expr
      symbol <- literal("<") | literal("<=") | literal(">") | literal(">=") | literal("==") | literal("!=")
      right <- expr
    }
      yield new Condition(symbol, left, right)
  }

  //Expression + Algebraic operations
  def expr: Parser[Expr] = {
    for {
      value <- expr1 | term
    } yield  value
  }

  def expr1: Parser[Expr] = {
    for {
      left <- term
      op <- literal("+") | literal("-")
      right <- term
    } yield new Operator(op, left, right)
  }


  def term: Parser[Expr] = {
    for {
      value <- multiplyDivideMod | factor
    } yield value
  }

  def multiplyDivideMod: Parser[Expr] = {
    for {
      left <- factor
      op <- literal("*") | literal("/") | literal("%")
      right <- factor
    } yield new Operator(op, left, right)
  }

  def factor: Parser[Expr] = {
    for {
      value <- expr2 | name | number
    } yield value
  }

  def expr2: Parser[Expr] = {
    for{
      _ <- literal("(")
      e <- expr
      _ <- literal(")")
    } yield e
  }

  def name: Parser[Expr] = {
    for {
      name <- ident
    } yield  new Identifier(name)
  }

  def number: Parser[Expr] = {
    for {
      value <- wholeNumber
    } yield new Number(value.toInt)
  }

}