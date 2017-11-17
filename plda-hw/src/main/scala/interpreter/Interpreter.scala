package interpreter

import parser._
import parser.internal._


class Interpreter(program: Program) {
  var currentScope = new Scope("global", null)

  def run(): Unit = {
    walk(program.statements)
  }


  def getVariable(ident: Identifier): Expr = {
    var s: Scope = currentScope

    while ((!s.name.equals("global")) && !s.variables.contains(ident.name)) s = s.parentScope

    if (s.variables.contains(ident.name)) s.variables(ident.name)
    else {
      sys.error("Error: Undefined variable " + ident.name +
        " at position [" +
        ident.pos.column + "] on line: " +
        ident.pos.line)
    }
  }

  def printStatemnet(value: Expr) = {
    println(calculateExpression(value))
  }

  def calculateExpression(expr: Expr): Int = {
    expr match {
      case Number(value) => value
      case Identifier(_) => {
        calculateExpression(getVariable(expr.asInstanceOf[Identifier]))
      }
      case Operator(op, left, right) => {
        op match {
          case "*" => calculateExpression(left) * calculateExpression(right)
          case "/" => calculateExpression(left) / calculateExpression(right)
          case "%" => calculateExpression(left) % calculateExpression(right)
          case "+" => calculateExpression(left) + calculateExpression(right)
          case "-" => calculateExpression(left) - calculateExpression(right)
        }
      }
    }
  }

  def evaluateVariableDefinition(name: String, value: Expr): Unit = {
    currentScope.variables(name) = value
  }

  def evaluateIfStatement(cond: Condition, left: List[Statement], right: List[Statement]): Unit = {
    if (isConditionTrue(cond))
      walk(left)
    else
      walk(right)
  }

  def isConditionTrue(cond: Condition): Boolean = {
    val a = calculateExpression(cond.left)
    val b = calculateExpression(cond.right)

    cond.op match {
      case "==" => (a == b)
      case "!=" => (a != b)
      case "<=" => (a <= b)
      case ">=" => (a >= b)
      case "<"  => (a < b)
      case ">"  => (a > b)
    }
  }

  def walk(tree: List[Statement]): Unit = {
    if (!tree.isEmpty) {
      tree.head match {
        case VariableDefinition(name, value) => evaluateVariableDefinition(name, value)
                                                walk(tree.tail)
        case IfStatement(condition, trueBranch, falseBranch) => evaluateIfStatement(condition, trueBranch, falseBranch)
                                                                walk(tree.tail)
        case PrintStatement(value) => printStatemnet(value)
                                      walk(tree.tail)
      }
    }
  }

}
