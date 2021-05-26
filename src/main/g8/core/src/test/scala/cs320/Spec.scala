package cs320

import Macros._

class Spec extends SpecBase {

  val run = Implementation.run _
  val runWithStdLib = Implementation.runWithStdLib _
  val typeCheck = Implementation.typeCheck _
  def typeOf(s: String): String =
    try {
      Typed.showType(typeCheck(Typed.Expr(s)))
    } catch {
      case _: PLError => "!Error!"
      case _: Exception => "!Crash!"
    }
  def typeOf(s1: String, s2: String): (String, String) =
    (typeOf(s1), typeOf(s2))
  def typeOf(s1: String, s2: String, s3: String): (String, String, String) =
    (typeOf(s1), typeOf(s2), typeOf(s3))

  test(run("42"), "42")
  test(run("true"), "true")
  test(run("()"), "()")
  test(run("1 + 2"), "3")
  test(run("2 * 4"), "8")
  test(run("5 / 2"), "2")
  test(run("13 % 5"), "3")
  test(run("1 == 1"), "true")
  test(run("1 < 1"), "false")
  test(run("{1; 2}"), "2")
  test(run("if (true) 1 else 2"), "1")
  test(run("""
    val x = 1 + 2;
    val y: Int = x * 4 + 1;
    y / (x - 1)
  """), "6")
  test(run("""
    lazy val f: Int => Int = (x: Int) => if (x < 1) 0 else x + f(x - 1);
    f(10)
  """), "55")
  test(run("""
    def f(x: Int): Int = if (x < 1) 0 else x + f(x - 1);
    f(10)
  """), "55")
  test(run("(x: Int) => x + x"), "<function>")
  test(run("((x: Int, y: Int) => x + y)(1, 2)"), "3")
  test(run("""
    var x = 1;
    var y: Int = x * 4 + 8;
    { x = 3; y / (x - 1) }
  """), "6")
  test(run("""
    type Fruit {
      case Apple
      case Banana(Int)
    }
    (Apple match {
      case Apple => 1
      case Banana(x) => 0
    }) + (Banana(1) match {
      case Apple => 0
      case Banana(x) => x
    })
  """), "2")
  test(run("""
    def f['T, 'S](t: 'T, s: 'S): 'T = t;
    f[Int, Boolean](1, true)
  """), "1")
  test(run("""
    type Fruit['T] {
      case Apple
      case Banana('T)
    }
    (Apple[Boolean] match {
      case Apple => 1
      case Banana(x) => 0
    }) + (Banana[Int](1) match {
      case Apple => 0
      case Banana(x) => x
    })
  """), "2")

  test(runWithStdLib("""
    var score = 0;
    def check(b: Boolean): Unit =
      if (b) score = score + 1;

    {
    check(!intEquals(1, 2));
    check(intEquals(3, 3));
    check(intMax(3, 6) == 6);
    check(intMin(3, 6) == 3);
    check(!booleanEquals(true, false));
    check(booleanEquals(true, true));
    check(unitEquals((), ()));

    score
    }
  """), "7")
  test(runWithStdLib("""
    var score = 0;
    def check(b: Boolean): Unit =
      if (b) score = score + 1;

    val p1 = Pair[Int, Boolean](1, true);
    val p2 = Pair[Int, Boolean](1, false);
    val p3 = Pair[Int, Boolean](2, true);

    val eq = pairEquals[Int, Boolean](intEquals, booleanEquals);

    {
    check(pairFst[Int, Boolean](p1) == 1);
    check(pairSnd[Int, Boolean](p1));
    check(pairFst[Int, Boolean](p2) == 1);
    check(!pairSnd[Int, Boolean](p2));
    check(pairFst[Int, Boolean](p3) == 2);
    check(pairSnd[Int, Boolean](p3));
    check(eq(p1, p1));
    check(!eq(p1, p2));
    check(!eq(p1, p3));

    score
    }
  """), "9")
  test(runWithStdLib("""
    var score = 0;
    def check(b: Boolean): Unit =
      if (b) score = score + 1;

    val opt1 = Some[Int](1);
    val opt2 = optionMap[Int, Int](opt1, (x: Int) => x + x);
    val opt3 = optionFilter[Int](opt1, (x: Int) => x < 2);
    val opt4 = optionFilter[Int](opt2, (x: Int) => x < 2);
    val opt5 = optionFlatten[Int](Some[Option[Int]](opt1));
    val opt6 = optionFlatten[Int](Some[Option[Int]](opt4));
    val opt7 = optionFlatten[Int](None[Option[Int]]);

    def aux(i: Int): Option[Int] =
      if (i == 1) Some[Int](i) else None[Int];

    val opt8 = optionFlatMap[Int, Int](opt1, aux);
    val opt9 = optionFlatMap[Int, Int](opt2, aux);
    val opt10 = optionFlatMap[Int, Int](opt4, aux);
    val opt11 = optionFilterNot[Int](opt1, (x: Int) => x < 2);
    val opt12 = optionFilterNot[Int](opt2, (x: Int) => x < 2);

    val eq = optionEquals[Int](intEquals);
    val eql = listEquals[Int](intEquals);

    {
    check(eq(Some[Int](1), Some[Int](1)));
    check(!eq(Some[Int](1), Some[Int](2)));
    check(!eq(Some[Int](1), None[Int]));
    check(eq(None[Int], None[Int]));
    check(eq(opt1, Some[Int](1)));
    check(eq(opt2, Some[Int](2)));
    check(eq(opt3, Some[Int](1)));
    check(eq(opt4, None[Int]));
    check(eq(opt5, Some[Int](1)));
    check(eq(opt6, None[Int]));
    check(eq(opt7, None[Int]));
    check(eq(opt8, Some[Int](1)));
    check(eq(opt9, None[Int]));
    check(eq(opt10, None[Int]));
    check(eq(opt11, None[Int]));
    check(eq(opt12, Some[Int](2)));
    check(!optionIsEmpty[Int](opt1));
    check(optionIsEmpty[Int](opt4));
    check(optionNonEmpty[Int](opt1));
    check(!optionNonEmpty[Int](opt4));
    check(eql(optionToList[Int](opt1), List1[Int](1)));
    check(eql(optionToList[Int](opt4), List0[Int]()));
    check(optionGetOrElse[Int](opt1, 0) == 1);
    check(optionGetOrElse[Int](opt4, 0) == 0);
    optionForeach[Int](opt1, (i: Int) => check(i == 1));
    optionForeach[Int](opt4, (i: Int) => check(true));

    score
    }
  """), "25")
  test(runWithStdLib("""
    var score = 0;
    def check(b: Boolean): Unit =
      if (b) score = score + 1;

    val b = Box[Int](1);
    val i1 = boxGet[Int](b);
    val i2 = boxSet[Int](b, 2);
    val i3 = boxGet[Int](b);
    val i4 = boxSet[Int](b, 1);
    val i5 = boxGet[Int](b);

    {
    check(i1 == 1);
    check(i2 == 1);
    check(i3 == 2);
    check(i4 == 2);
    check(i5 == 1);

    score
    }
  """), "5")
  test(runWithStdLib("""
    var score = 0;
    def check(b: Boolean): Unit =
      if (b) score = score + 1;

    val l0 = List5[Int](1, 2, 3, 4, 5);
    val l1 = List3[Int](1, 2, 3);
    val l2 = List2[Int](4, 5);
    val zipped0 = listZip[Int, Int](l0, l0);
    val unzipped0 = listUnzip[Int, Int](zipped0);
    val l3 = pairFst[List[Int], List[Int]](unzipped0);
    val l4 = pairSnd[List[Int], List[Int]](unzipped0);
    val zipped1 = listZip[Int, Int](l0, l1);
    val unzipped1 = listUnzip[Int, Int](zipped1);
    val l5 = pairFst[List[Int], List[Int]](unzipped1);
    val l6 = pairSnd[List[Int], List[Int]](unzipped1);
    val zipped2 = listZipWithIndex[Int](l0);
    val unzipped2 = listUnzip[Int, Int](zipped2);
    val l7 = pairFst[List[Int], List[Int]](unzipped2);
    val l8 = pairSnd[List[Int], List[Int]](unzipped2);

    val eq = listEquals[Int](intEquals);
    val eqo = optionEquals[Int](intEquals);
    def odd(n: Int): Boolean = n % 2 != 0;
    def lt4(n: Int): Boolean = n < 4;

    {
    check(eq(l0, l0));
    check(!eq(l0, l1));
    check(!eq(l0, l2));
    check(!eq(l1, l2));
    check(!eq(l0, Nil[Int]));
    check(eq(Nil[Int], Nil[Int]));
    check(eq(listAppended[Int](listAppended[Int](l1, 4), 5), l0));
    check(eq(listConcat[Int](l1, l2), l0));
    check(listCount[Int](l0, odd) == 3);
    check(eq(listDrop[Int](l0, 3), l2));
    check(listExists[Int](l0, lt4));
    check(!listExists[Int](l2, lt4));
    check(eq(listFilter[Int](l0, lt4), l1));
    check(eq(listFilterNot[Int](l0, lt4), l2));
    check(eqo(listFind[Int](l0, lt4), Some[Int](1)));
    check(eqo(listFind[Int](l2, lt4), None[Int]));
    check(eq(listFlatMap[Int, Int](l1, (n: Int) => if (n == 1) l1 else if (n == 2) l2 else Nil[Int]), l0));
    check(eq(listFlatten[Int](List2[List[Int]](l1, l2)), l0));
    check(listFoldLeft[Int, Int](0, l0, (n: Int, m: Int) => n + m) == 15);
    check(listFoldRight[Int, Int](l0, 0, (n: Int, m: Int) => n + m) == 15);
    check(!listForall[Int](l0, lt4));
    check(listForall[Int](l1, lt4));
    listForeach[Int](l0, (n: Int) => check(odd(n)));
    check(eqo(listGet[Int](l0, 4), Some[Int](5)));
    check(eqo(listGet[Int](l0, 5), None[Int]));
    check(!listIsEmpty[Int](l0));
    check(listIsEmpty[Int](Nil[Int]));
    check(listLength[Int](l0) == 5);
    check(eq(listMap[Int, Int](l0, (n: Int) => n * n), List5[Int](1, 4, 9, 16, 25)));
    check(listNonEmpty[Int](l0));
    check(!listNonEmpty[Int](Nil[Int]));
    check(eq(listPrepended[Int](listPrepended[Int](listPrepended[Int](l2, 3), 2), 1), l0));
    check(eq(listReverse[Int](l0), List5[Int](5, 4, 3, 2, 1)));
    check(eq(listTake[Int](l0, 3), l1));
    check(eq(l0, l3));
    check(eq(l0, l4));
    check(eq(l1, l5));
    check(eq(l1, l6));
    check(eq(l0, l7));
    check(eq(l0, listMap[Int, Int](l8, (n: Int) => n + 1)));

    score
    }
  """), "42")
  test(runWithStdLib("""
    var score = 0;
    def check(b: Boolean): Unit =
      if (b) score = score + 1;

    val m0 = Map1[Int, Int](intEquals, 0, 0);
    val m1 = mapUpdated[Int, Int](m0, 1, 2);
    val m2 = mapUpdated[Int, Int](m1, 2, 4);
    val m3 = mapUpdated[Int, Int](m2, 3, 6);
    val m4 = mapRemoved[Int, Int](m3, 2);
    val m5 = mapUpdated[Int, Int](m2, 3, 8);

    val eqo = optionEquals[Int](intEquals);

    {
    check(eqo(mapGet[Int, Int](m0, 0), Some[Int](0)));
    check(eqo(mapGet[Int, Int](m0, 1), None[Int]));
    check(eqo(mapGet[Int, Int](m0, 2), None[Int]));
    check(eqo(mapGet[Int, Int](m0, 3), None[Int]));
    check(eqo(mapGet[Int, Int](m0, 4), None[Int]));

    check(eqo(mapGet[Int, Int](m1, 0), Some[Int](0)));
    check(eqo(mapGet[Int, Int](m1, 1), Some[Int](2)));
    check(eqo(mapGet[Int, Int](m1, 2), None[Int]));
    check(eqo(mapGet[Int, Int](m1, 3), None[Int]));
    check(eqo(mapGet[Int, Int](m1, 4), None[Int]));

    check(eqo(mapGet[Int, Int](m2, 0), Some[Int](0)));
    check(eqo(mapGet[Int, Int](m2, 1), Some[Int](2)));
    check(eqo(mapGet[Int, Int](m2, 2), Some[Int](4)));
    check(eqo(mapGet[Int, Int](m2, 3), None[Int]));
    check(eqo(mapGet[Int, Int](m2, 4), None[Int]));

    check(eqo(mapGet[Int, Int](m3, 0), Some[Int](0)));
    check(eqo(mapGet[Int, Int](m3, 1), Some[Int](2)));
    check(eqo(mapGet[Int, Int](m3, 2), Some[Int](4)));
    check(eqo(mapGet[Int, Int](m3, 3), Some[Int](6)));
    check(eqo(mapGet[Int, Int](m3, 4), None[Int]));

    check(eqo(mapGet[Int, Int](m4, 0), Some[Int](0)));
    check(eqo(mapGet[Int, Int](m4, 1), Some[Int](2)));
    check(eqo(mapGet[Int, Int](m4, 2), None[Int]));
    check(eqo(mapGet[Int, Int](m4, 3), Some[Int](6)));
    check(eqo(mapGet[Int, Int](m4, 4), None[Int]));

    check(eqo(mapGet[Int, Int](m4, 0), Some[Int](0)));
    check(eqo(mapGet[Int, Int](m4, 1), Some[Int](2)));
    check(eqo(mapGet[Int, Int](m4, 2), None[Int]));
    check(eqo(mapGet[Int, Int](m4, 3), Some[Int](6)));
    check(eqo(mapGet[Int, Int](m4, 4), None[Int]));

    score
    }
  """), "30")
  test(runWithStdLib("""
    var score = 0;
    def check(b: Boolean): Unit =
      if (b) score = score + 1;

    {
    check(stringEquals("abc \n"<STRP, EOS>, List5[Int](97, 98, 99, 32, 10)));
    check(stringEquals(substring("12abc \n"<STRP, EOS>, 2, 5), List3[Int](97, 98, 99)));
    check("abc \n"<(n: Int, m: Int) => n + m, 0> == 336);

    score
    }
  """), "3")
  test(runWithStdLib("""
    type Expr {
      case Num(Int)
      case Add(Expr, Expr)
      case Sub(Expr, Expr)
      case Id(Int)
      case Fun(Int, Expr)
      case App(Expr, Expr)
    }

    type Value {
      case NumV(Int)
      case CloV(Int, Expr, Map[Int, Value])
    }

    def interp(e: Expr, env: Map[Int, Value]): Option[Value] = e match {
      case Num(n) => Some[Value](NumV(n))
      case Add(l, r) => optionFlatMap[Value, Value](interp(l, env), (lv: Value) => lv match {
        case NumV(n) => optionFlatMap[Value, Value](interp(r, env),
          (rv: Value) => rv match {
            case NumV(m) => Some[Value](NumV(n + m))
            case CloV(x, e, fenv) => None[Value]
          }
        )
        case CloV(x, e, fenv) => None[Value]
      })
      case Sub(l, r) => optionFlatMap[Value, Value](interp(l, env), (lv: Value) => lv match {
        case NumV(n) => optionFlatMap[Value, Value](interp(r, env),
          (rv: Value) => rv match {
            case NumV(m) => Some[Value](NumV(n - m))
            case CloV(x, e, fenv) => None[Value]
          }
        )
        case CloV(x, e, fenv) => None[Value]
      })
      case Id(x) => mapGet[Int, Value](env, x)
      case Fun(x, e) => Some[Value](CloV(x, e, env))
      case App(f, a) => optionFlatMap[Value, Value](interp(f, env), (fv: Value) => fv match {
        case NumV(n) => None[Value]
        case CloV(x, e, fenv) => optionFlatMap[Value, Value](interp(a, env),
          (av: Value) => interp(e, mapUpdated[Int, Value](fenv, x, av))
        )
      })
    };

    lazy val digit: Parser[Expr] =
      parserMap[Int, Expr](
        () => parserCond((x: Int) => 48 <= x && x < 58),
        (x: Int) => Num(x - 48)
      );

    lazy val add: Parser[Expr] =
      parserMap[Pair[Int, Pair[Expr, Expr]], Expr](
        () => parserThen[Int, Pair[Expr, Expr]](
          () => parserConst(43),
          () => parserThen[Expr, Expr](() => e, () => e)
        ),
        (p: Pair[Int, Pair[Expr, Expr]]) =>
          pairSnd[Int, Pair[Expr, Expr]](p) match {
            case Pair(l, r) => Add(l, r)
          }
      );

    lazy val sub: Parser[Expr] =
      parserMap[Pair[Int, Pair[Expr, Expr]], Expr](
        () => parserThen[Int, Pair[Expr, Expr]](
          () => parserConst(45),
          () => parserThen[Expr, Expr](() => e, () => e)
        ),
        (p: Pair[Int, Pair[Expr, Expr]]) =>
          pairSnd[Int, Pair[Expr, Expr]](p) match {
            case Pair(l, r) => Sub(l, r)
          }
      );

    lazy val id: Parser[Expr] =
      parserMap[Int, Expr](
        () => parserCond((x: Int) => 97 <= x && x <= 122),
        (x: Int) => Id(x)
      );

    lazy val fun: Parser[Expr] =
      parserMap[Pair[Int, Pair[Int, Expr]], Expr](
        () => parserThen[Int, Pair[Int, Expr]](
          () => parserConst(47),
          () => parserThen[Int, Expr](
            () => parserCond((x: Int) => 97 <= x && x <= 122),
            () => e
          )
        ),
        (p: Pair[Int, Pair[Int, Expr]]) =>
          pairSnd[Int, Pair[Int, Expr]](p) match {
            case Pair(p, b) => Fun(p, b)
          }
      );

    lazy val app: Parser[Expr] =
      parserMap[Pair[Int, Pair[Expr, Expr]], Expr](
        () => parserThen[Int, Pair[Expr, Expr]](
          () => parserConst(64),
          () => parserThen[Expr, Expr](() => e, () => e)
        ),
        (p: Pair[Int, Pair[Expr, Expr]]) =>
          pairSnd[Int, Pair[Expr, Expr]](p) match {
            case Pair(l, r) => App(l, r)
          }
      );

    lazy val e: Parser[Expr] =
      parserOr[Expr](
        () => parserOr[Expr](
          () => parserOr[Expr](
            () => parserOr[Expr](
              () => parserOr[Expr](
                () => digit,
                () => add
              ),
              () => sub
            ),
            () => id
          ),
          () => fun
        ),
        () => app
      );

    parseAll[Expr](e, "@@/x/y+xy23"<STRP, EOS>) match {
      case None => -1
      case Some(e) => interp(e, Map0[Int, Value](intEquals)) match {
        case None => -2
        case Some(v) => v match {
          case NumV(n) => if (n < 0) -3 else n
          case CloV(x, e, env) => -4
        }
      }
    }
  """), "5")

	// Fall 2020

  test(run("5 % -3"), "2")
  test(run("if(42/6 > 42%7) 1*0 else 1/0"), "0")
  test(run("""
    var x:Int = 0;
    val y = (x = 1);
    y
  """), "()")
  test(run("""
    def f(x:Int):Int = if (x == 0) 0 else (if (x % 3 == 2) h(x - 1) else x + g(x - 1));
    def g(x:Int):Int = if (x == 0) 0 else (if (x % 3 != 1) f(x - 1) else x + h(x - 1));
    def h(x:Int):Int = if (x == 0) 0 else (if (x % 3 == 0) g(x - 1) else x + f(x - 1));
    f(9)
  """), "24")
  test(run("""
    var sum = 0;
    def append(d:Int):Unit = sum = sum * 10 + d;
    {append(1);(x:Int, y:Int) => if({append(6); x} == {append(7); y}) sum else 0}(
      {append(2); 1} + {append(3); 2},
      {append(4); 15} % {append(5); 4}
    )
  """), "1234567")
  test(run("""
    type Adder {
      case add0
      case add1(Int)
      case add2(Int, Int)
    }
    add2(4, 2) match {
      case add2(x, y) => x + y
      case add0 => 0
      case add1(x) => x
    }
  """), "6")
  test(run("lazy val x:Int = 3 / 0; 42"), "42")
  test(run("var x:Int = 0; lazy val y:Int = {x = x + 1; x}; y + y"), "2")
  test(run("var x:Boolean = false; lazy val y:Boolean = x; {x = !x; y}"), "true")
  test(run("""
    type fruit {case apple case orange}
    val x = orange match {case apple => 1 case orange => 2};
    type color {case red case orange}
    val y = orange match {case red => 4 case orange => 8};
    x + y
  """), "10")
  test(run("""
    type fruit {case apple case orange}
    val x = orange;
    type color {case red case orange}
    x match {
      case apple => 0
      case orange => 1
    }
  """), "1")
  test(run("""
    val y:Int = 0;
    lazy val x:Int = y;
    val y:Int = 1;
    x
  """), "0")
  test(run("""
    type t {case c(Int)}
    val x:Int = 0;
    c(1) match {case c(x) => x}
  """), "1")
  test(run("""
    var x = 0;
    def f(t:Int):Int = x;
    val y = f(0);
    {x = 1;
    val x = 2;
    var x = 3;
    {x = 4;
    y + f(0)}}
  """), "1")
  testExc(run("(3 + 4) % (3 - 3)"), "")

  test(run("""
    type x['x] {case x('x)}
    val x = x[Int](0) match {case x(x) => x};
    x
  """), "0")
  test(run("""
    type x['x] {case x('x)}
    def f['x](x:x['x]):'x = x match {case x(x)=>x};
    f[Int](x[Int](0))
  """), "0")
  test(run("""
    def app['a, 'b](f:'a=>'b, x:'a):'b = f(x);
    def app2['a, 'b](f:'b=>'a, x:'b):'a = app['b,'a](f,x);
    app2[Boolean, Int]((x:Int)=>x%2==0, 42)
  """), "true")

  test(typeOf("1 + 1", "true + 1"), ("Int", "!Error!"))
  test(typeOf("def f(): Int = 1 + f(); f() + 1"), "Int")
  test(typeOf("1 - 1", "1 - false"), ("Int", "!Error!"))
  test(typeOf("def f(): Int = 1 + f(); f() - 1"), "Int")
  test(typeOf("1 * 1", "365 * ()"), ("Int", "!Error!"))
  test(typeOf("def f(): Int = 1 + f(); f() * 1"), "Int")
  test(typeOf("1 / 1", "1 / true"), ("Int", "!Error!"))
  test(typeOf("def f(): Int = 1 + f(); f() / 1"), "Int")
  test(typeOf("1 % 1", "false % true"), ("Int", "!Error!"))
  test(typeOf("def f(): Int = 1 + f(); f() % 1"), "Int")
  test(typeOf("1 == 1", "false == 1"), ("Boolean", "!Error!"))
  test(typeOf("1 < 1", "() < ()"), ("Boolean", "!Error!"))
  test(typeOf("def f(): Int = 1 + f(); { f(); 1 }"), "Int")
  test(typeOf("if (false) false else true"), "Boolean")
  test(typeOf("val x = 1; val x = true; x"), "Boolean")
  test(typeOf("(x: Int) => x"), "(Int => Int)")
  test(typeOf("(x: Int) => x", "(x: 'T) => x"), ("(Int => Int)", "!Error!"))
  test(typeOf("(x: Int) => x", "(x: Int) => y"), ("(Int => Int)", "!Error!"))
  test(typeOf("((x: Int) => x)(1)"), "Int")
  test(typeOf("var x: Int = 1; { x = 10; 1 }"), "Int")
  test(typeOf("var x: Int = 1; { x = 10; 1 }", "var x: Int = 10; { x = true; x }"), ("Int", "!Error!"))

  test(typeOf("""
  val x = 1;
  type A {
    case Y(Boolean)
  }
  Y(true) match {
    case Y(x) => x
  }
  """), "Boolean")
  test(typeOf("1", "a + 1"), ("Int", "!Error!"))
  test(typeOf("type A['T] { case X } 1", """
  type A {
    case X
  }
  X
  """), ("Int", "!Error!"))
  test(typeOf("type A['T] { case X } 1", """
  type A['T] {
    case X
  }
  type B['S] {
    case Y('T)
  }
  1
  """), ("Int", "!Error!"))
  test(typeOf("def id['T](x: 'T): 'T = x; id[Boolean](true)", """
  def id['T](x: 'T): 'T = x;
  id[Boolean](1)
  """), ("Boolean", "!Error!"))
  test(typeOf("def id['T](y: 'T): 'T = y; id[Boolean](true)", """
  def id['T](x: 'T): 'T = x;
  def id2['S](y: 'S): 'S = id['S](y);
  id2[Int](1)
  """), ("Boolean", "Int"))
  test(typeOf("def id['T](z: 'T): 'T = z; id[Boolean](true)", """
  def f['T, 'S](t: 'T): 'T = t;
  def g['S](): Int = f['S, Int](1);
  g[Boolean]()
  """), ("Boolean", "!Error!"))
  test(typeOf(
  "type A['T] { case X } 1",
  "def id['T](x: 'T): 'T = x; id[Boolean](true)", """
  type A {
    case X(B['S])
  }
  type B['S] {
    case Y('S)
  }
  def x['S](i: 'S): A = X(Y['S](i));
  def y['S](): 'S = x[Boolean](false) match {
    case X(b) => b
  } match {
    case Y(k) => k
  };
  y[Int]()
  """), ("Int", "Boolean", "!Error!"))
  test(typeOf(
  "type A['T] { case X } 1",
  "def id['T](y: 'T): 'T = y; id[Boolean](true)", """
  type A['T] {
    case X('T)
  }
  def id['T](x: 'T): 'T = x;
  X[Int](1) match {
      case X(i) => id['T](i)
  }
  """), ("Int", "Boolean", "!Error!"))

  /* Write your own tests */
}
