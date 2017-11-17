package parser.internal

class Program(val functions: List[FunctionTerm], val statements: List[Statement]) {

  def this(statements: List[Statement]) = {
    this(null, statements)
  }
}
