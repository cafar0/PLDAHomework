package typeChecker

import signature._

object Main {
  def main(args: Array[String]): Unit = {


//      - compose


    val testAST = FUN("2", IF(BOOL(true), INT(3), INT(4)) )

//   fn a => fn b => a
    val testAST1 = FUN("a", FUN("b", VAR("a")))


//   fn f => fn g => fn x => f (g x)
    val testAST2 = FUN("f", FUN("g", FUN("x", APP(VAR("f"), APP(VAR("g"), VAR("x"))))))

//    fn pred => if pred 1 then 2 else 3
    val testAST3 = FUN(
      "pred",
      IF(
        APP(VAR("pred"), INT(1)),
        INT(2),
        INT(3)
      )
    )

//    let
//    val inc = fn a => a + 1
//    in
//    let
//    val dec = fn a => a - 1
//    in
//    dec (inc 42)
//    end
//    end
    val testAST4 = LET(
      "inc", FUN("a", APP(APP(VAR("+"), VAR("a")), INT(1))),
      LET(
        "dec", FUN("a", APP(APP(VAR("-"), VAR("a")), INT(1))),
        APP(VAR("dec"), APP(VAR("inc"), INT(42)))
      )
    )


    val testAST5 = FUN(
      "f1",
      IF(
        APP(VAR("f1"), INT(1)),
        INT(2),
        INT(3)
      )
    )

    println(Signature.forType(Infer.typeOf(testAST5)))
  }
}
