package interpreter

import parser._
import parser.internal._

import scala.collection.immutable.HashMap


class Interpreter(program: Program) {
//  val scope = new Scope(new HashMap[String, Expr], new HashMap[String, Expr])
  type Scope = HashMap[String, Expr]
  var testScope = new HashMap[String, Expr]()
  var testFuncScope: List[FunctionTerm] = program.functions

  val variablesScope = new HashMap[String, Expr]()
  val functionsScope: List[FunctionTerm] = program.functions

  def run(): Unit = {
    walk(program.statements, variablesScope)
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

  def addToScope(scope: Scope, v: HashMap[String, Expr]): Scope = {
    val a = scope.asInstanceOf[HashMap[String, Expr]]
    val b = a ++ v
    b.asInstanceOf[Scope]
  }

  def executeFunction(f: FunctionTerm, arguments: HashMap[String, Expr], scope: Scope): Option[Int] = {
    //TODO: this might overwrite existing variables -> functions args cannot be named as higher scope vars
    val funcScope = addToScope(scope, arguments)
    walk(f.statements, funcScope)
    if (f.returnedValue != null) {
      val retVal = calculateExpression(f.returnedValue, funcScope)
      return Option(retVal)
    }
    return Option.empty
  }

  def evaluateFunctionCall(name: String, values: HashMap[String, Expr], scope: Scope): Option[Int] = {
    val f = program.functions.filter( x => x.name == name)

    if(f.size < 1) sys.error("Error: undefined function " + name)
    else{
      val result = executeFunction(f(0), values, scope)
      return result
    }
    return Option.empty
  }

  def evaluateVariableDefinition(name: String, value: Expr, scope: Scope): Scope = {

    if (value.isInstanceOf[FunctionCall]) {
      val functionCall = value.asInstanceOf[FunctionCall]
      val result = evaluateFunctionCall(functionCall.name, functionCall.values, scope)

      if (!result.isEmpty) {
        val newScope =  scope + (name -> Number(result.get))
        this.testScope = newScope
        return newScope
      }
      scope
    }
    else {
      val newScope = scope + (name -> value)
      this.testScope = newScope
      return newScope
    }
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
        case FunctionCall(name, values) => evaluateFunctionCall(name, values, scope)
        case IfStatement(condition, trueBranch, falseBranch) => evaluateIfStatement(condition, trueBranch, falseBranch, scope)
                                                                walk(tree.tail, scope)
        case PrintStatement(value) => printStatemnet(value, scope)
                                      walk(tree.tail, scope)
      }
    }
  }

}
