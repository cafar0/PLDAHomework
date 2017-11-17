package interpreter

import parser._
import parser.internal._

import scala.collection.immutable.HashMap


class Interpreter(program: Program) {
  type Scope = HashMap[String, Expr]
  var testScope = new HashMap[String, Expr]()
  val currentScope = new HashMap[String, Expr]()

  def run(): Unit = {
    walk(program.statements, currentScope)
  }


  def getVariable(ident: Identifier, scope: Scope): Expr = {

    if (scope.contains(ident.name)) scope(ident.name)
    else {
      sys.error("Error: Undefined variable " + ident.name +
        " at position [" +
        ident.pos.column + "] on line: " +
        ident.pos.line)
    }
  }

  def printStatemnet(value: Expr, scope: Scope): Unit = {
    println(calculateExpression(value,scope))
  }

  def calculateExpression(expr: Expr, scope: Scope): Int = {
    expr match {
      case Number(value) => value
      case Identifier(_) => {
        calculateExpression(getVariable(expr.asInstanceOf[Identifier], scope), scope)
      }
      case Operator(op, left, right) => {
        op match {
          case "*" => calculateExpression(left,scope) * calculateExpression(right,scope)
          case "/" => calculateExpression(left,scope) / calculateExpression(right,scope)
          case "%" => calculateExpression(left,scope) % calculateExpression(right,scope)
          case "+" => calculateExpression(left,scope) + calculateExpression(right,scope)
          case "-" => calculateExpression(left,scope) - calculateExpression(right,scope)
        }
      }
    }
  }

  def evaluateVariableDefinition(name: String, value: Expr, scope: Scope): Scope = {
    val scope1 =  scope + (name -> value)
    this.testScope = scope1
    scope1
//    currentScope.variables(name) = value
  }

  def evaluateIfStatement(cond: Condition, left: List[Statement], right: List[Statement], scope: Scope): Unit = {
    if (isConditionTrue(cond, scope))
      walk(left, scope)
    else
      walk(right, scope)
  }

  def isConditionTrue(cond: Condition, scope: Scope): Boolean = {
    val a = calculateExpression(cond.left, scope)
    val b = calculateExpression(cond.right, scope)

    cond.op match {
      case "==" => (a == b)
      case "!=" => (a != b)
      case "<=" => (a <= b)
      case ">=" => (a >= b)
      case "<"  => (a < b)
      case ">"  => (a > b)
    }
  }

  def walk(tree: List[Statement], scope: Scope): Unit = {
    if (!tree.isEmpty) {
      tree.head match {
        case VariableDefinition(name, value) => walk(tree.tail, evaluateVariableDefinition(name, value, scope))
        case IfStatement(condition, trueBranch, falseBranch) => evaluateIfStatement(condition, trueBranch, falseBranch, scope)
                                                                walk(tree.tail, scope)
        case PrintStatement(value) => printStatemnet(value, scope)
                                      walk(tree.tail, scope)
      }
    }
  }

}
