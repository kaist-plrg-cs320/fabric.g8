package cs320

object Implementation extends Template {

  def typeCheck(e: Typed.Expr): Typed.Type = T.typeCheck(e)

  def interp(e: Untyped.Expr): Untyped.Value = U.interp(e)

  object T {
    import Typed._

    def typeCheck(expr: Expr): Type = ???
  }

  object U {
    import Untyped._

    def interp(expr: Expr): Value = ???
  }
}
