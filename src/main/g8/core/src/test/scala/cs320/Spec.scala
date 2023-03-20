package cs320

import Macros._

class Spec extends SpecBase {

  def check(s: String): String =
    try {
      Typed.showType(Implementation.typeCheck(Typed.Expr(s)))
    } catch {
      case e: PLError => "Type error"
    }
  def check(s1: String, s2: String, ss: String*): List[String] =
    (s1 :: s2 :: ss.toList).map(check)
  def interp(s: String): String =
    Untyped.showValue(Implementation.interp(Typed.erase(Typed.Expr(s))))
  def lib(s: String): String = {
    val e = Typed.Expr(StdLib.code + s)
    val t = Typed.showType(Implementation.typeCheck(e))
    val v = Untyped.showValue(Implementation.interp(Typed.erase(e)))
    s"Type: $t, Value: $v"
  }

  test(
    check(
      "{ val x = 6; x }",
      "x",
      "{ val x = 6; x[Int] }"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "{ def x['x](): Int = 6; x[Int] }",
      "{ def x['x](): Int = 6; x }",
      "{ def x['x](): Int = 6; x[Int, Int] }",
      "{ def x['x](): Int = 6; x['x] }"
    ),
    List(
      "(() => Int)",
      "Type error",
      "Type error",
      "Type error"
    )
  )
  test(check("()"), "Unit")
  test(
    check(
      "(26 + 6)",
      "(false + 6)",
      "(26 + true)"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "(26 * 6)",
      "(false * 6)",
      "(26 * false)"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "(26 / 6)",
      "(true / 6)",
      "(26 / false)"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "(26 % 6)",
      "(false % 6)",
      "(26 % false)"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "(26 == 6)",
      "(true == 6)",
      "(26 == false)"
    ),
    List(
      "Boolean",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "(26 < 6)",
      "(false < 6)",
      "(26 < false)"
    ),
    List(
      "Boolean",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "{ 26; 6 }",
      "{ x; 6 }"
    ),
    List(
      "Int",
      "Type error"
    )
  )
  test(
    check(
      "(if (true) 26 else 6)",
      "(if (6) 26 else 6)",
      "(if (false) true else 6)"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "{ val x = 6; x }",
      "{ val x: 'x = 6; 26 }",
      "{ val x: Boolean = 6; 26 }"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "{ lazy val x: Int = 6; x }",
      "{ lazy val x: 'x = 6; 26 }",
      "{ lazy val x: Int = false; 26 }"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(check("{ lazy val x: Int = x; x }"), "Int")
  test(check("{ lazy val x: Int = f; lazy val f: Int = x; x }"), "Int")
  test(
    check(
      "{ def x(): Int = 6; x }",
      "{ def x(f: 'x): Int = 6; 26 }",
      "{ def x(): 'x = 6; 26 }",
      "{ def x(): Int = false; 26 }"
    ),
    List(
      "(() => Int)",
      "Type error",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "{ def x['x](f: 'x): 'x = { def x['f](f: 'f): 'f = f; x['x](f) }; x[Int] }",
      "{ def x['x](f: 'x): 'x = { def x['x](f: 'x): 'x = f; f }; 6 }"
    ),
    List(
      "(Int => Int)",
      "Type error"
    )
  )
  test(check("{ def x(): Int = x(); x }"), "(() => Int)")
  test(check("{ def x(): Int = f(); def f(): Int = x(); x }"), "(() => Int)")
  test(check("{ def x['x](d: 'x): 'x = f['x](d); def f['x](d: 'x): 'x = x['x](d); x[Int] }"), "(Int => Int)")
  test(check("{ def x(): Int = f; lazy val f: Int = x(); x }"), "(() => Int)")
  test(
    check(
      "{ type x { case x } { val f: x = x; 6 } }",
      "{ type x { case x('x) } 6 }",
      "{ type x { case x } x }"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "{ type x { case x } { type f { case f } 6 } }",
      "{ type x { case x } { type x { case f } 6 } }"
    ),
    List(
      "Int",
      "Type error"
    )
  )
  test(
    check(
      "{ def x['x](): Int = { type x['f] { case x } 6 }; 6 }",
      "{ def x['x](): Int = { type x['x] { case x } 6 }; 6 }"
    ),
    List(
      "Int",
      "Type error"
    )
  )
  test(check("{ type x { case x(Int) case f(Int) } { val f: x = x(26); 6 } }"), "Int")
  test(check("{ type x['x] { case x('x) } { val f: x[Int] = x[Int](26); 6 } }"), "Int")
  test(check("{ type x { case x(x) } 6 }"), "Int")
  test(check("{ type x { case x(f) } type f { case f(x) } 6 }"), "Int")
  test(check("{ type x['x] { case x(f['x]) } type f['x] { case f(x['x]) } 6 }"), "Int")
  test(check("{ lazy val f: x = x; def d(): x = x; type x { case x } 6 }"), "Int")
  test(
    check(
      "((x: Int) => x)",
      "((x: 'x) => 26)"
    ),
    List(
      "(Int => Int)",
      "Type error"
    )
  )
  test(
    check(
      "{ var x = 6; (x = 26) }",
      "(x = 26)",
      "{ def x['x](): Int = 6; (x = 26) }",
      "{ val x = 6; (x = 26) }",
      "{ var x = 6; (x = true) }"
    ),
    List(
      "Unit",
      "Type error",
      "Type error",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "((x: Int) => x)(6)",
      "26(6)",
      "((x: Int) => 26)()",
      "((x: Int) => 26)(6, 6)",
      "((x: Int) => 26)(true)"
    ),
    List(
      "Int",
      "Type error",
      "Type error",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "(() => 26)()",
      "(() => 26)(6)",
      "(() => 26)(6, 6)"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "((x: Int, f: Int) => x)(6, 6)",
      "((x: Int, f: Int) => 26)()",
      "((x: Int, f: Int) => 26)(6)"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "{ type x { case x } (x match { case x => 6 }) }",
      "{ type x { case x } (26 match { case x => 6 }) }",
      "{ type x { case x } (x match { case x(f) => 6 }) }"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "{ type x { case x case f } (x match { case f => 6 case x => 6 }) }",
      "{ type x { case x case f } (x match { case x => 6 }) }",
      "{ type x { case x case f } (x match { case f => 6 }) }",
      "{ type x { case x case f } (x match { case x => 6 case f => true }) }"
    ),
    List(
      "Int",
      "Type error",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "{ type x { case x(Int) } (x(6) match { case x(f) => f }) }",
      "{ type x { case x(Int) } (x(6) match { case x => 26 }) }",
      "{ type x { case x(Int) } (x(6) match { case x(f, d) => 26 }) }"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(check("{ type x['x] { case x('x) } (x[Int](6) match { case x(f) => f }) }"), "Int")
  test(
    check(
      "{ type x['x] { case x('x) } { val x = ((x: x[Int]) => 6); 6 } }",
      "{ type x['x] { case x('x) } { val x = ((x: x['x]) => 6); 6 } }",
      "{ type x['x] { case x('x) } { val x = ((x: f[Int]) => 6); 6 } }",
      "{ type x['x] { case x('x) } { val x = ((x: x) => 6); 6 } }",
      "{ type x['x] { case x('x) } { val x = ((x: x[Int, Int]) => 6); 6 } }"
    ),
    List(
      "Int",
      "Type error",
      "Type error",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "{ def x['x](f: 'x): Int = 6; 6 }",
      "{ def x['x](f: 'f): Int = 6; 6 }"
    ),
    List(
      "Int",
      "Type error"
    )
  )
  test(
    check(
      "{ def x['x](f: ('x => 'x)): Int = 6; 6 }",
      "{ def x['x](f: ('f => 'x)): Int = 6; 6 }",
      "{ def x['x](f: ('x => 'f)): Int = 6; 6 }"
    ),
    List(
      "Int",
      "Type error",
      "Type error"
    )
  )
  test(
    check(
      "{ def x['x, 'f](f: 'x): 'x = f; def f['f, 'd](d: 'd): 'd = x['d, 'd](d); 6 }",
      "{ def x['x, 'f](f: 'x): 'x = f; def f['f, 'd](d: 'd): 'd = x['f, 'd](d); 6 }"
    ),
    List(
      "Int",
      "Type error"
    )
  )
  test(interp("(((a: Int) => a)(26) + 6)"), "32")
  test(interp("(((u: Int) => u)(26) * 6)"), "156")
  test(interp("(((k: Int) => k)(26) / 6)"), "4")
  test(interp("(26 / (-6))"), "-4")
  test(interp("((-26) / 6)"), "-4")
  test(interp("((-26) / (-6))"), "4")
  testExc(interp("(26 / ((q: Int) => q)(0))"), "")
  test(interp("(((w: Int) => w)(26) % 6)"), "2")
  test(interp("(26 % (-6))"), "2")
  test(interp("((-26) % 6)"), "-2")
  test(interp("((-26) % (-6))"), "-2")
  testExc(interp("(26 % ((g: Int) => g)(0))"), "")
  test(interp("(((u: Int) => u)(6) == 6)"), "true")
  test(interp("(6 == 26)"), "false")
  test(interp("(((y: Int) => y)(6) < 6)"), "false")
  test(interp("(6 < 26)"), "true")
  test(interp("{ 26; ((e: Int) => e)(6) }"), "6")
  test(interp("(if (((s: Int) => s)(true)) 6 else (26 / 0))"), "6")
  test(interp("(if (((n: Int) => n)(false)) (6 / 0) else 26)"), "26")
  test(interp("{ val x = 6; { val f = (() => x); { val x = 26; f() } } }"), "6")
  test(interp("{ lazy val x: Int = (6 / 0); 26 }"), "26")
  test(interp("{ var x = 6; { lazy val f: Int = { (x = (x + 1)); x }; ((f + f) + x) } }"), "21")
  test(interp("{ def q(o: Int): Int = (if ((o < 3)) (o + 3) else (q((o + (-1))) + (o + 2))); q(12) }"), "100")
  test(interp("{ def b(w: Int): Int = (if ((w < 2)) (w + 1) else (x((w + (-2)), (w + 3)) + (w + 2))); def r(c: Int, m: Int): Int = (if ((c < 3)) (c + 4) else (b((c + (-2))) + (c + 1))); def x(o: Int, p: Int): Int = (if ((o < 3)) (p + 1) else (r((o + (-2)), (p + 5)) + (p + 5))); b(11) }"), "65")
  test(interp("{ var x = 6; (x = 26) }"), "()")
  test(interp("{ var x = 6; { (x = 26); x } }"), "26")
  test(interp("((x: Int, f: Int) => (x + f))(6, 26)"), "32")
  test(interp("{ type x { case x case f } (x match { case f => 6 case x => 26 }) }"), "26")
  test(interp("{ type x { case x(Int) case f(Int) } (x(6) match { case f(d) => 26 case x(d) => d }) }"), "6")
  test(lib("""
  var score = 0;
  def check(b: Boolean): Unit =
    if (b) score = score + 1;
  {
  check(!intEquals(6, 2));
  check(intEquals(6, 6));
  check(intMax(6, 26) == 26);
  check(intMin(6, 26) == 6);
  check(!booleanEquals(true, false));
  check(booleanEquals(true, true));
  check(unitEquals((), ()));
  score
  }
  """), "Type: Int, Value: 7")
  test(lib("""
  var score = 0;
  def check(b: Boolean): Unit =
    if (b) score = score + 1;
  val p1 = Pair[Int, Boolean](6, true);
  val p2 = Pair[Int, Boolean](6, false);
  val p3 = Pair[Int, Boolean](26, true);
  val eq = pairEquals[Int, Boolean](intEquals, booleanEquals);
  {
  check(pairFst[Int, Boolean](p1) == 6);
  check(pairSnd[Int, Boolean](p1));
  check(pairFst[Int, Boolean](p2) == 6);
  check(!pairSnd[Int, Boolean](p2));
  check(pairFst[Int, Boolean](p3) == 26);
  check(pairSnd[Int, Boolean](p3));
  check(eq(p1, p1));
  check(!eq(p1, p2));
  check(!eq(p1, p3));
  score
  }
  """), "Type: Int, Value: 9")
  test(lib("""
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
  check(eq(Some[Int](6), Some[Int](6)));
  check(!eq(Some[Int](6), Some[Int](26)));
  check(!eq(Some[Int](6), None[Int]));
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
  """), "Type: Int, Value: 25")
  test(lib("""
  var score = 0;
  def check(b: Boolean): Unit =
    if (b) score = score + 1;
  val b = Box[Int](6);
  val i1 = boxGet[Int](b);
  val i2 = boxSet[Int](b, 26);
  val i3 = boxGet[Int](b);
  val i4 = boxSet[Int](b, 6);
  val i5 = boxGet[Int](b);
  {
  check(i1 == 6);
  check(i2 == 6);
  check(i3 == 26);
  check(i4 == 26);
  check(i5 == 6);
  score
  }
  """), "Type: Int, Value: 5")
  test(lib("""
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
  """), "Type: Int, Value: 42")
  test(lib("""
  var score = 0;
  def check(b: Boolean): Unit =
    if (b) score = score + 1;
  val m0 = Map1[Int, Int](intEquals, 0, 0);
  val m1 = mapUpdated[Int, Int](m0, 1, 2);
  val m2 = mapUpdated[Int, Int](m1, 2, 4);
  val m3 = mapUpdated[Int, Int](m2, 3, 6);
  val m4 = mapRemoved[Int, Int](m3, 2);
  val m5 = mapUpdated[Int, Int](m2, 3, 26);
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
  check(eqo(mapGet[Int, Int](m5, 0), Some[Int](0)));
  check(eqo(mapGet[Int, Int](m5, 1), Some[Int](2)));
  check(eqo(mapGet[Int, Int](m5, 2), Some[Int](4)));
  check(eqo(mapGet[Int, Int](m5, 3), Some[Int](26)));
  check(eqo(mapGet[Int, Int](m5, 4), None[Int]));
  score
  }
  """), "Type: Int, Value: 30")
  test(lib("""
  var score = 0;
  def check(b: Boolean): Unit =
    if (b) score = score + 1;
  {
  check(stringEquals("abc \n"<STRP, EOS>, List5[Int](97, 98, 99, 32, 10)));
  check(stringEquals(substring("12abc \n"<STRP, EOS>, 2, 5), List3[Int](97, 98, 99)));
  check("bca \n"<(n: Int, m: Int) => n + m, 0> == 336);
  score
  }
  """), "Type: Int, Value: 3")
  test(lib("""
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
  parseAll[Expr](e, "@@/x/y+xy62"<STRP, EOS>) match {
    case None => -1
    case Some(e) => interp(e, Map0[Int, Value](intEquals)) match {
      case None => -2
      case Some(v) => v match {
        case NumV(n) => if (n < 0) -3 else n
        case CloV(x, e, env) => -4
      }
    }
  }
  """), "Type: Int, Value: 8")

  /* Write your own tests */
}
