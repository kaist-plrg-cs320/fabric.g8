package cs320

import Macros._

class Spec extends SpecBase {

  val run = Implementation.run _
  val runWithStdLib = Implementation.runWithStdLib _

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

  /* Write your own tests */
}
