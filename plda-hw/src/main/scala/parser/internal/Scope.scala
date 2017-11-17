package parser.internal

import scala.collection.immutable.HashMap


class Scope() {
  val variables = new HashMap[String, Expr]()
  /*, val functions: HashMap[String, ???]*/
}
