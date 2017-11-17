package parser.internal

import scala.collection.mutable

class Scope(val name: String, val parentScope: Scope) {
  var variables = new mutable.HashMap[String, Expr]()
}
