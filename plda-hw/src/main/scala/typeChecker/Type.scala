package typeChecker

import scala.util.Random

sealed trait Type

object Type {
  type Var = Int
  val random = Random

  case object INT extends Type
  case object BOOL extends Type
  case class FUN(paramTy: Type, returnTy: Type) extends Type
  case class VAR(tvar: Type.Var) extends  Type


  def newVar(): Type = {
    VAR(random.nextInt(Int.MaxValue))
  }
}

// Limitation -> functions will work with only one argument
// in order to simulate lambda calculus