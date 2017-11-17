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
    case parser.Error(msg, n) => println("Error: " + msg)
    case parser.Failure(msg, n) => println("Error: " + msg)
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

  it should "allow me to do this. hopefully" in {
    val input =
      """|func max(a, b)
         |if (a < b) ? let r = a : let r = b
         |return a
         |
         |main :
         |let d = max( a=3, b=4 )
         |println d
      """.stripMargin
    val interpreter = runInterpreter(input)
    val d = interpreter.getVariable(Identifier("d"), interpreter.testScope)
    assert(d == Number(3))
  }

  it should "getting really ambitious. hopefully" in {
    val input =
      """|func max(a, b)
         |if (a < b) ? let r = a : let r = b
         |return a
         |
         |func min(y, u)
         |if(y < u) ? let o = y : let o = u
         |return y
         |
         |main :
         |let d = min (y=4, u=9)
         |let n = max (a=4, b=9)
         |println d + n
      """.stripMargin
    val interpreter = runInterpreter(input)
    val d = interpreter.getVariable(Identifier("d"), interpreter.testScope)
    assert(d == Number(5))
  }

  it should "pass this to be recursive" in {
    val input =
      """|func max(a)
         |println a
         |if(a < 5) ?  max(a = 6) : let b = 4
         |println a + 1
         |return a
         |
         |
         |func min(y, u)
         |if(y < u) ? max(a = 2) : let o = u
         |return y
         |
         |main :
         |let d = max(a=2)
      """.stripMargin
    val interpreter = runInterpreter(input)
    val d = interpreter.getVariable(Identifier("d"), interpreter.testScope)
    assert(d == Number(2))
  }

}
