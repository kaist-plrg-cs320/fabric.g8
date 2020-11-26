package cs320

trait Template {

  def run(str: String): String = {
    val expr = Typed.Expr(str)
    typeCheck(expr)
    val erased = Typed.erase(expr)
    Untyped.showValue(interp(erased))
  }

  def runWithStdLib(str: String): String = run(StdLib.code + str)

  def typeCheck(expr: Typed.Expr): Typed.Type

  def interp(expr: Untyped.Expr): Untyped.Value
}
