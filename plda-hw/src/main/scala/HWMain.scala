import parser.LanguageParser

import scala.io.Source
import interpreter._
import parser.internal._

object HWMain {

  def main(args: Array[String]) {
//    val inputFile = Source.fromFile("scripts/program.small")
//    val inputSource = inputFile.mkString

    val parser = new LanguageParser
    parser.parseAll(parser.program,
      """|main :
         |if (3 > 5) ? let a = 6 : let a = 7 + 2
         |println a
      """.stripMargin) match {

//      """|main :
//         |let b = 5
//         |if (b < 10) ? 1 : 4
//         |println 23
//         |let a = 18
//      """.stripMargin) match {
      case parser.Success(r, n) => {
        val interpreter = new Interpreter(r.asInstanceOf[Program])

        try {
          interpreter.run()
          val a = interpreter.getVariable(Identifier("a"), interpreter.testScope)
          print("result : " + a)
        } catch {
          case e: RuntimeException => println(e.getMessage)
        }
      }
      case parser.Error(msg, n) => println("Error: " + msg)
      case parser.Failure(msg, n) => println("Error: " + msg)
      case _ =>
    }
  }
}
