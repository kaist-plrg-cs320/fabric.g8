package cs320

// untyped FABRIC
object Untyped {

  // expression
  sealed trait Expr

  // identifier
  case class Id(name: String) extends Expr
  // integer
  case class IntE(value: BigInt) extends Expr
  // boolean
  case class BooleanE(value: Boolean) extends Expr
  // unit
  case object UnitE extends Expr
  // addition
  case class Add(left: Expr, right: Expr) extends Expr
  // multiplication
  case class Mul(left: Expr, right: Expr) extends Expr
  // division
  case class Div(left: Expr, right: Expr) extends Expr
  // modulo
  case class Mod(left: Expr, right: Expr) extends Expr
  // equal-to
  case class Eq(left: Expr, right: Expr) extends Expr
  // less-than
  case class Lt(left: Expr, right: Expr) extends Expr
  // sequence
  case class Sequence(left: Expr, right: Expr) extends Expr
  // conditional
  case class If(cond: Expr, texpr: Expr, fexpr: Expr) extends Expr
  // local variable
  case class Val(name: String, expr: Expr, body: Expr) extends Expr
  // recursive defintions
  case class RecBinds(defs: List[RecDef], body: Expr) extends Expr
  // anonymous function
  case class Fun(params: List[String], body: Expr) extends Expr
  // assignment
  case class Assign(name: String, expr: Expr) extends Expr
  // function application
  case class App(fun: Expr, args: List[Expr]) extends Expr
  // pattern matching
  case class Match(expr: Expr, cases: List[Case]) extends Expr

  // recursive definition
  sealed trait RecDef

  // lazy local variable
  case class Lazy(name: String, expr: Expr) extends RecDef
  // recursive function
  case class RecFun(
    name: String, params: List[String], body: Expr
  ) extends RecDef
  // algebraic data type
  case class TypeDef(variants: List[Variant]) extends RecDef

  // variant definition
  case class Variant(name: String, empty: Boolean)

  // match case
  case class Case(variant: String, names: List[String], body: Expr)

  // value
  sealed trait Value

  // address
  type Addr = Int

  // environment
  type Env = Map[String, Addr]

  // unit value
  case object UnitV extends Value
  // integer value
  case class IntV(value: BigInt) extends Value
  // boolean value
  case class BooleanV(value: Boolean) extends Value
  // closure
  case class CloV(params: List[String], body: Untyped.Expr, env: Env) extends Value
  // delayed computation
  case class ExprV(expr: Untyped.Expr, env: Env) extends Value
  // constructor
  case class ConstructorV(name: String) extends Value
  // variant value
  case class VariantV(name: String, values: List[Value]) extends Value

  def showValue(value: Value): String = value match {
    case UnitV => "()"
    case IntV(n) => n.toString
    case BooleanV(b) => b.toString
    case CloV(_, _, _) => "<function>"
    case ExprV(_, _) => "<lazy>"
    case ConstructorV(vn) => s"<constructor:$vn>"
    case VariantV(vn, Nil) => vn
    case VariantV(vn, vs) => s"$vn(${vs.mkString(", ")})"
  }
}
