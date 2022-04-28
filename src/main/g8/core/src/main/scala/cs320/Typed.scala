package cs320

import scala.util.parsing.combinator._

case class ParsingError(msg: String) extends Exception

// typed FABRIC
object Typed {

  // expression
  sealed trait Expr

  // type application
  case class Id(name: String, targs: List[Type]) extends Expr
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
  case class Val(
    mut: Boolean, name: String, typ: Option[Type], expr: Expr, body: Expr
  ) extends Expr
  // recursive defintions
  case class RecBinds(defs: List[RecDef], body: Expr) extends Expr
  // anonymous function
  case class Fun(params: List[(String, Type)], body: Expr) extends Expr
  // assignment
  case class Assign(name: String, expr: Expr) extends Expr
  // function application
  case class App(fun: Expr, args: List[Expr]) extends Expr
  // pattern matching
  case class Match(expr: Expr, cases: List[Case]) extends Expr

  // recursive definition
  sealed trait RecDef

  // lazy local variable
  case class Lazy(name: String, typ: Type, expr: Expr) extends RecDef
  // recursive function
  case class RecFun(
    name: String,
    tparams: List[String],
    params: List[(String, Type)],
    rtype: Type,
    body: Expr
  ) extends RecDef
  // algebraic data type
  case class TypeDef(
    name: String, tparams: List[String], variants: List[Variant]
  ) extends RecDef

  // variant definition
  case class Variant(name: String, params: List[Type])

  // match case
  case class Case(variant: String, names: List[String], body: Expr)

  // type
  sealed trait Type

  // type application
  case class AppT(name: String, targs: List[Type]) extends Type
  // type variable
  case class VarT(name: String) extends Type
  // integer type
  case object IntT extends Type
  // boolean type
  case object BooleanT extends Type
  // unit type
  case object UnitT extends Type
  // function type
  case class ArrowT(ptypes: List[Type], rtype: Type) extends Type

  def showType(typ: Type): String = typ match {
    case IntT => "Int"
    case BooleanT => "Boolean"
    case UnitT => "Unit"
    case AppT(t, Nil) => t
    case AppT(t, as) => s"$t[${as.map(showType).mkString(", ")}]"
    case VarT(a) => s"\'$a"
    case ArrowT(p :: Nil, rt) => s"(${showType(p)} => ${showType(rt)})"
    case ArrowT(ps, rt) =>
      s"((${ps.map(showType).mkString(", ")}) => ${showType(rt)})"
  }

  // type erasure
  def erase(e: Expr): Untyped.Expr = e match {
    case Id(x, _) => Untyped.Id(x)
    case IntE(n) => Untyped.IntE(n)
    case BooleanE(b) => Untyped.BooleanE(b)
    case UnitE => Untyped.UnitE
    case Add(l, r) => Untyped.Add(erase(l), erase(r))
    case Mul(l, r) => Untyped.Mul(erase(l), erase(r))
    case Div(l, r) => Untyped.Div(erase(l), erase(r))
    case Mod(l, r) => Untyped.Mod(erase(l), erase(r))
    case Eq(l, r) => Untyped.Eq(erase(l), erase(r))
    case Lt(l, r) => Untyped.Lt(erase(l), erase(r))
    case Sequence(l, r) => Untyped.Sequence(erase(l), erase(r))
    case If(c, t, f) => Untyped.If(erase(c), erase(t), erase(f))
    case Val(_, x, _, e, b) => Untyped.Val(x, erase(e), erase(b))
    case RecBinds(ds, b) => Untyped.RecBinds(ds.map(erase), erase(b))
    case Fun(ps, b) => Untyped.Fun(ps.map(_._1), erase(b))
    case Assign(x, e) => Untyped.Assign(x, erase(e))
    case App(f, as) => Untyped.App(erase(f), as.map(erase))
    case Match(e, cs) => Untyped.Match(erase(e), cs.map(erase))
  }

  def erase(d: RecDef): Untyped.RecDef = d match {
    case Lazy(x, t, b) => Untyped.Lazy(x, erase(b))
    case RecFun(x, _, ps, _, b) => Untyped.RecFun(x, ps.map(_._1), erase(b))
    case TypeDef(_, _, vs) => Untyped.TypeDef(vs.map(erase))
  }

  def erase(v: Variant): Untyped.Variant = v match {
    case Variant(n, ps) => Untyped.Variant(n, ps.isEmpty)
  }

  def erase(c: Case): Untyped.Case = c match {
    case Case(v, ns, b) => Untyped.Case(v, ns, erase(b))
  }

  object Expr extends RegexParsers {

    private def error(msg: String): Nothing = throw ParsingError(msg)

    private def wrapR[T](e: => Parser[T]): Parser[T] = "(" ~> e <~ ")"
    private def wrapC[T](e: => Parser[T]): Parser[T] = "{" ~> e <~ "}"
    private def wrapS[T](e: => Parser[T]): Parser[T] = "[" ~> e <~ "]"
    private def wrapA[T](e: => Parser[T]): Parser[T] = "<" ~> e <~ ">"

    lazy val keywords = Set(
      "true", "false", "val", "var", "lazy", "def",
      "if", "else", "type", "case", "match"
    )
    lazy val tKeywords = Set("Int", "Boolean", "Unit")

    private lazy val n: Parser[BigInt] = "-?[0-9]+".r ^^ BigInt.apply
    private lazy val b: Parser[Boolean] = "true" ^^^ true | "false" ^^^ false
    private lazy val x: Parser[String] =
      "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!keywords(_))
    private lazy val tx: Parser[String] =
      "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!tKeywords(_))

    private lazy val e: Parser[Expr] =
      (wrapR(repsep(p, ",")) <~ "=>") ~ e ^^ {
        case ps ~ b =>
          val names = ps.map(_._1)
          if (dupCheck(names))
            error(s"Duplicated parameters: ${names.mkString(", ")}")
          Fun(ps, b)
      } | e1
    private lazy val e1: Parser[Expr] =
      rep1sep(e2, "||") ^^ (_.reduceLeft(Or))
    private lazy val e2: Parser[Expr] =
      rep1sep(e3, "&&") ^^ (_.reduceLeft(And))
    private lazy val e3: Parser[Expr] =
      e4 ~ rep(("==" | "!=" | "<=" | "<" | ">=" | ">") ~ e4) ^^ {
        case e ~ es => es.foldLeft(e){
          case (l, "==" ~ r) => Eq(l, r)
          case (l, "!=" ~ r) => Neq(l, r)
          case (l, "<"  ~ r) => Lt(l, r)
          case (l, "<=" ~ r) => Lte(l, r)
          case (l, ">"  ~ r) => Gt(l, r)
          case (l,   _  ~ r) => Gte(l, r)
        }
      }
    private lazy val e4: Parser[Expr] =
      e5 ~ rep(("+" | "-") ~ e5) ^^ { case e ~ es => es.foldLeft(e){
        case (l, "+" ~ r) => Add(l, r)
        case (l,  _  ~ r) => Sub(l, r)
      }}
    private lazy val e5: Parser[Expr] =
      e6 ~ rep(("*" | "/" | "%") ~ e6) ^^ { case e ~ es => es.foldLeft(e){
        case (l, "*" ~ r) => Mul(l, r)
        case (l, "/" ~ r) => Div(l, r)
        case (l,  _  ~ r) => Mod(l, r)
      }}
    private lazy val e6: Parser[Expr] =
      "-" ~> e6 ^^ Neg | "!" ~> e6 ^^ Not | e7
    private lazy val e7: Parser[Expr] =
      e8 ~ rep(
        wrapR(repsep(e, ",")) ^^ AppP |
        "match" ~> wrapC(rep1(c)) ^^ (cs => {
          val vars = cs.map(_.variant)
          if (dupCheck(vars))
            error(s"Duplicated variant names: ${vars.mkString(", ")}")
          MatchP(cs)
        })
      ) ^^ { case e ~ es => es.foldLeft(e){
        case (f, AppP(as)) => App(f, as)
        case (e, MatchP(cs)) => Match(e, cs)
      }}
    private lazy val e8: Parser[Expr] =
      x ~ ("=" ~> e) ^^ { case x ~ e => Assign(x, e) } |
      x ~ opt(wrapS(rep1sep(t, ","))) ^^ { case x ~ ts => Id(x, ts.getOrElse(Nil)) } |
      n ^^ IntE | b ^^ BooleanE |
      ("\"" ~> """([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt])*""".r <~ "\"") ~
        wrapA(e ~ ("," ~> e)) ^^ { case s ~ (c ~ n) => StringLit(s, c, n) } |
      ("if" ~> wrapR(e)) ~ e ~ opt("else" ~> e) ^^ {
        case c ~ t ~ Some(f) => If(c, t, f)
        case c ~ t ~ None => If(c, t, UnitE)
      } |
      m ~ x ~ opt(":" ~> t) ~ ("=" ~> e <~ ";") ~ e ^^ {
        case m ~ x ~ t ~ e ~ b => Val(m, x, t, e, b)
      } |
      rep1(d) ~ e ^^ {
        case ds ~ e =>
          val names = ds.flatMap{
            case Lazy(x, _, _) => List(x)
            case RecFun(x, _, _, _, _) => List(x)
            case TypeDef(_, _, vs) => vs.map(_.name)
          }
          if (dupCheck(names))
            error(s"Duplicated definitions: ${names.mkString(", ")}")
          val ts = ds.flatMap{
            case TypeDef(t, _, _) => Some(t)
            case _ => None
          }
          if (dupCheck(ts))
            error(s"Duplicated type names: ${ts.mkString(", ")}")
          RecBinds(ds, e)
      } |
      wrapR(opt(e)) ^^ (_.getOrElse(UnitE)) |
      wrapC(rep1sep(e, ";")) ^^ (_.reduceLeft(Sequence))

    private sealed trait E6P
    private case class AppP(as: List[Expr]) extends E6P
    private case class MatchP(cs: List[Case]) extends E6P

    private lazy val d: Parser[RecDef] =
      ("lazy" ~> "val" ~> x) ~
      (":" ~> t) ~
      ("=" ~> e <~ ";") ^^ {
        case n ~ t ~ b => Lazy(n, t, b)
      } |
      ("def" ~> x) ~
      opt(wrapS(rep1sep("\'" ~> tx, ","))) ~
      wrapR(repsep(p, ",")) ~
      (":" ~> t) ~
      ("=" ~> e <~ ";") ^^ {
        case n ~ tvsOpt ~ ps ~ rt ~ b =>
          val tvs = tvsOpt.getOrElse(Nil)
          if (dupCheck(tvs))
            error(s"Duplicated type parameters: ${tvs.mkString(", ")}")
          val names = ps.map(_._1)
          if (dupCheck(names))
            error(s"Duplicated parameters: ${names.mkString(", ")}")
          RecFun(n, tvs, ps, rt, b)
      } |
      ("type" ~> tx) ~
      opt(wrapS(rep1sep("\'" ~> tx, ","))) ~
      wrapC(rep1(v)) ^^ {
        case t ~ tvsOpt ~ vars =>
          val tvs = tvsOpt.getOrElse(Nil)
          if (dupCheck(tvs))
            error(s"Duplicated type parameters: ${tvs.mkString(", ")}")
          val names = vars.map(_.name)
          if (dupCheck(names))
            error(s"Duplicated variant names: ${names.mkString(", ")}")
          TypeDef(t, tvs, vars)
      }

    private lazy val v: Parser[Variant] =
      ("case" ~> x) ~ opt(wrapR(rep1sep(t, ","))) ^^ {
        case t ~ ts => Variant(t, ts.getOrElse(Nil))
      }

    private lazy val c: Parser[Case] =
      ("case" ~> x) ~ opt(wrapR(repsep(x, ","))) ~ ("=>" ~> e) ^^ {
        case vn ~ None ~ e => Case(vn, Nil, e)
        case vn ~ Some(xs) ~ e => Case(vn, xs, e)
      }

    private lazy val p: Parser[(String, Type)] =
      x ~ (":" ~> t) ^^ { case x ~ t => (x, t) }
    private lazy val m: Parser[Boolean] =
      "val" ^^^ false | "var" ^^^ true

    private lazy val t: Parser[Type] =
      (wrapR(repsep(t, ",")) ^^ WrapedP | t1 ^^ SimpleP) ~ ("=>" ~> t) ^^ {
        case WrapedP(ps) ~ rt => ArrowT(ps, rt)
        case SimpleP(pt) ~ rt => ArrowT(List(pt), rt)
      } | t1
    private lazy val t1: Parser[Type] =
      tx ~ opt(wrapS(rep1sep(t, ","))) ^^ { case x ~ ts => AppT(x, ts.getOrElse(Nil)) } |
      "Int" ^^^ IntT |
      "Boolean" ^^^ BooleanT |
      "Unit" ^^^ UnitT |
      "\'" ~> tx ^^ VarT |
      wrapR(t)

    private sealed trait TP
    private case class SimpleP(t: Type) extends TP
    private case class WrapedP(t: List[Type]) extends TP

    // desugaring
    private val T = BooleanE(true)
    private val F = BooleanE(false)
    private def Neg(e: Expr): Expr = Mul(e, IntE(-1))
    private def Not(e: Expr): Expr = If(e, F, T)
    private def Sub(l: Expr, r: Expr): Expr = Add(l, Neg(r))
    private def Neq(l: Expr, r: Expr): Expr = Not(Eq(l, r))
    private def Lte(l: Expr, r: Expr): Expr = {
      val lv, rv = fresh()
      Val(false, lv, None, l,
      Val(false, rv, None, r,
      Or(Eq(Id(lv, Nil), Id(rv, Nil)), Lt(Id(lv, Nil), Id(rv, Nil)))))
    }
    private def Gt(l: Expr, r: Expr): Expr = Not(Lte(l, r))
    private def Gte(l: Expr, r: Expr): Expr = Not(Lt(l, r))
    private def And(l: Expr, r: Expr): Expr = If(l, r, F)
    private def Or(l: Expr, r: Expr): Expr = If(l, T, r)
    private def StringLit(s: String, cons: Expr, nil: Expr): Expr = {
      import scala.collection.mutable.StringBuilder
      val v = fresh()
      val builder = new StringBuilder
      var esc = false
      for (c <- s)
        if (esc) {
          builder += (c match {
            case '\\' => '\\'
            case '\'' => '\''
            case '\"' => '\"'
            case 'b' => '\b'
            case 'f' => '\f'
            case 'n' => '\n'
            case 'r' => '\r'
            case 't' => '\t'
            case _ => error(s"invalid string literal: $s")
          })
          esc = false
        } else if (c == '\\')
          esc = true
        else
          builder += c
      Val(
        false, v, None, cons,
        builder.toString.foldRight(nil){
          case (h, t) => App(Id(v, Nil), List(IntE(h.toInt), t))
        }
      )
    }
    private var id = -1
    private def fresh(): String = {
      id += 1
      s"$$x$id"
    }

    private def dupCheck(ss: List[String]): Boolean =
      ss.distinct.length != ss.length

    def apply(str: String): Expr = parseAll(e, str).getOrElse(error(""))
  }
}
