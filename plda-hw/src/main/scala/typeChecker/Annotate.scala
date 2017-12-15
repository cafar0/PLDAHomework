package typeChecker

// Used for annotating each term with a type
object Annotate {
  def annotate(term: Term, tenv: TypeEnv): TypedTerm = {
    term match {
      case INT(value) => TypedTerm.INT(Type.newVar(), value)
      case BOOL(value) => TypedTerm.BOOL(Type.newVar(), value)
      case FUN(param, body) =>
        val paramTy = Type.newVar()
        val paramBinder = TypedTerm.Binder(paramTy, param)
        val extendedTenv = tenv.set(param, paramTy)
        TypedTerm.FUN(Type.newVar(), paramBinder, annotate(body, extendedTenv))
      case VAR(name) =>
        tenv.get(name) match {
          case None => throw new RuntimeException(s"unbound identifier: $name")
          case Some(ty) => TypedTerm.VAR(ty, name)
        }
      case APP(fn, arg) =>
      TypedTerm.APP(Type.newVar(), annotate(fn, tenv), annotate(arg, tenv))
      case IF(testCondition, trueBranch, falseBranch) =>
      TypedTerm.IF(
      Type.newVar(),
          annotate(testCondition, tenv),
          annotate(trueBranch, tenv),
          annotate(falseBranch, tenv)
        )
      case LET(binding, value, body) =>
        val bindingTy = Type.newVar()
        val extendedEnv = tenv.set(binding, bindingTy)
        TypedTerm.LET(
          Type.newVar(),
          TypedTerm.Binder(bindingTy, binding),
          annotate(value, tenv),
          annotate(body, extendedEnv)
        )
    }
  }
}
