package parser.internal

class FunctionTerm(val name: String, val arguments: Map[String, Int], val statements: List[Statement], val returnedValue: Expr) {

  def this(name: String, arguments: Map[String, Int], statements: List[Statement]) {
    this(name, arguments, statements, null)
  }

  def this(partialFunction: FunctionTerm, returnedValue: Expr) {
    this(partialFunction.name,
        partialFunction.arguments,
        partialFunction.statements,
        returnedValue)
  }

}
