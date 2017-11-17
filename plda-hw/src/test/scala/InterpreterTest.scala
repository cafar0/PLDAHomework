import interpreter.Interpreter
import org.scalatest.FlatSpec
import parser.LanguageParser
import parser.internal._

class InterpreterTest extends FlatSpec {

  def runInterpreter(input: String): Interpreter = {
    val parser = new LanguageParser
    val result = parser.parseAll(parser.program, input) match {
    case parser.Success(r, _) => {
    val interpreter = new Interpreter(r.asInstanceOf[Program])
    interpreter.run()
    return interpreter
  }
    case _ => fail()
  }
    fail()
  }

  "The interpreter" should "ignore single value" in {
    val input = """|main :
                   |let a = 5
                """.stripMargin
    val interpreter = runInterpreter(input)
    val a = interpreter.getVariable(Identifier("a"), interpreter.testScope)
    assert(a == Number(5))
    }

  it should "be able to compute the value of a variable" in {
    val input =
      """|main :
         |let a = 2 + 2
      """.stripMargin
    val interpreter = runInterpreter(input)
    val a = interpreter.getVariable(Identifier("a"), interpreter.testScope)
    assert(a == Operator("+", Number(2), Number(2)))
  }

  it should "be able to use the value of a variable" in {
    val input =
      """|main :
         |let a = 2 + 2
         |let b = a + 2
         |if (b == 6) ? let c = 1 : let c = 0
      """.stripMargin
    val interpreter = runInterpreter(input)
    val c = interpreter.getVariable(Identifier("c"), interpreter.testScope)
    assert(c == Number(1))
  }


  it should "evaluate succeeding if statements" in {
    val input =
      """|main :
         |if(2 < 3) ? let a = 3 : let b = 4
      """.stripMargin
    val interpreter = runInterpreter(input)
    val a = interpreter.getVariable(Identifier("a"), interpreter.testScope)
    assert(a == Number(3))
  }

  it should "evaluate failing if statements" in {
    val input =
      """|main :
         |if(2 > 3) ? let a = 3 : let b = 4
      """.stripMargin
    val interpreter = runInterpreter(input)
    val a = interpreter.getVariable(Identifier("b"), interpreter.testScope)
    assert(a == Number(4))
  }



}
