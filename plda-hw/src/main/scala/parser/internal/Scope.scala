package parser.internal

import scala.collection.immutable.HashMap


class Scope(val variables: HashMap[String, Expr], functions: HashMap[String, Expr]) {

//  def this(v: HashMap[String, Expr], f: HashMap[String, Expr]) = {
//    this(v, f)
//  }
//
//  def addVariables(vars: HashMap[String, Expr]): Scope = {
//    val newVars = this.variables ++ vars
//    new this.type(newVars, this.functions)
//  }
//
//  def addFunctions(vars: HashMap[String, Expr]): Scope = {
//    val newFunc = this.functions ++ vars
//    new this.type(this.variables, newFunc)
//  }
}
