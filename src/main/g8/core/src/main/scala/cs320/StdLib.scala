package cs320

object StdLib {

  val code: String =
"""def intEquals(l: Int, r: Int): Boolean = l == r;

def intMax(l: Int, r: Int): Int =
  if (l > r) l else r;

def intMin(l: Int, r: Int): Int =
  if (l > r) r else l;

def booleanEquals(l: Boolean, r: Boolean): Boolean =
  if (l) r else !r;

def unitEquals(l: Unit, r: Unit): Boolean = true;

type Pair['T, 'S] {
  case Pair('T, 'S)
}

def pairFst['T, 'S](p: Pair['T, 'S]): 'T = p match {
  case Pair(t, s) => t
};

def pairSnd['T, 'S](p: Pair['T, 'S]): 'S = p match {
  case Pair(t, s) => s
};

def pairEquals['T, 'S](f: ('T, 'T) => Boolean, g: ('S, 'S) => Boolean):
  (Pair['T, 'S], Pair['T, 'S]) => Boolean =
  (l: Pair['T, 'S], r: Pair['T, 'S]) =>
    f(pairFst['T, 'S](l), pairFst['T, 'S](r)) &&
      g(pairSnd['T, 'S](l), pairSnd['T, 'S](r));

type Option['T] {
  case None
  case Some('T)
}

def optionEquals['T](f: ('T, 'T) => Boolean): (Option['T], Option['T]) => Boolean =
  (l: Option['T], r: Option['T]) => l match {
    case None => r match {
      case None => true
      case Some(t2) => false
    }
    case Some(t1) => r match {
      case None => false
      case Some(t2) => f(t1, t2)
    }
  };

def optionFilter['T](o: Option['T], f: 'T => Boolean): Option['T] = o match {
  case None => None['T]
  case Some(t) => if (f(t)) o else None['T]
};

def optionFilterNot['T](o: Option['T], f: 'T => Boolean): Option['T] =
  optionFilter['T](o, (t: 'T) => !f(t));

def optionFlatMap['T, 'S](o: Option['T], f: 'T => Option['S]): Option['S] =
  optionFlatten['S](optionMap['T, Option['S]](o, f));

def optionFlatten['T](o: Option[Option['T]]): Option['T] = o match {
  case None => None['T]
  case Some(o) => o match {
    case None => None['T]
    case Some(o1) => o
  }
};

def optionForeach['T](o: Option['T], f: 'T => Unit): Unit = {
  optionMap['T, Unit](o, f);
  ()
};

def optionGetOrElse['T](o: Option['T], t: 'T): 'T = o match {
  case None => t
  case Some(t) => t
};

def optionIsEmpty['T](o: Option['T]): Boolean = o match {
  case None => true
  case Some(o) => false
};

def optionMap['T, 'S](o: Option['T], f: 'T => 'S): Option['S] = o match {
  case None => None['S]
  case Some(t) => Some['S](f(t))
};

def optionNonEmpty['T](o: Option['T]): Boolean =
  !optionIsEmpty['T](o);

def optionToList['T](o: Option['T]): List['T] = o match {
  case None => List0['T]()
  case Some(t) => List1['T](t)
};

type Box['T] {
  case Box_(Option['T] => 'T)
}

def Box['T](t: 'T): Box['T] =
  var x: 'T = t;
  def aux(o: Option['T]): 'T = o match {
    case None => x
    case Some(y) =>
      val old = x; {
      x = y;
      old
    }
  };
  Box_['T](aux);

def boxGet['T](b: Box['T]): 'T = b match {
  case Box_(f) => f(None['T])
};

def boxSet['T](b: Box['T], t: 'T): 'T = b match {
  case Box_(f) => f(Some['T](t))
};

type List['T] {
  case Nil
  case Cons('T, List['T])
}

def List0['T](): List['T] = Nil['T];
def List1['T](t1: 'T): List['T] = Cons['T](t1, List0['T]());
def List2['T](t1: 'T, t2: 'T): List['T] = Cons['T](t1, List1['T](t2));
def List3['T](t1: 'T, t2: 'T, t3: 'T): List['T] = Cons['T](t1, List2['T](t2, t3));
def List4['T](t1: 'T, t2: 'T, t3: 'T, t4: 'T): List['T] = Cons['T](t1, List3['T](t2, t3, t4));
def List5['T](t1: 'T, t2: 'T, t3: 'T, t4: 'T, t5: 'T): List['T] = Cons['T](t1, List4['T](t2, t3, t4, t5));

def listAppended['T](l: List['T], t: 'T): List['T] =
  listConcat['T](l, List1['T](t));

def listConcat['T](l: List['T], r: List['T]): List['T] =
  listFoldRight['T, List['T]](l, r, (h: 'T, t: List['T]) => Cons['T](h, t));

def listCount['T](l: List['T], f: 'T => Boolean): Int =
  listLength['T](listFilter['T](l, f));

def listDrop['T](l: List['T], i: Int): List['T] = l match {
  case Nil => Nil['T]
  case Cons(h, t) => if (i <= 0) l else listDrop['T](t, i - 1)
};

def listEquals['T](f: ('T, 'T) => Boolean): (List['T], List['T]) => Boolean =
  def aux(l: List['T], r: List['T]): Boolean = l match {
    case Nil => r match {
      case Nil => true
      case Cons(h2, t2) => false
    }
    case Cons(h1, t1) => r match {
      case Nil => false
      case Cons(h2, t2) => f(h1, h2) && aux(t1, t2)
    }
  };
  aux;

def listExists['T](l: List['T], f: 'T => Boolean): Boolean =
  listFoldLeft['T, Boolean](false, l, (b: Boolean, t: 'T) => if (b) b else f(t));

def listFilter['T](l: List['T], f: 'T => Boolean): List['T] = l match {
  case Nil => Nil['T]
  case Cons(h, t) =>
    val filtered = listFilter['T](t, f);
    if (f(h)) Cons['T](h, filtered) else filtered
};

def listFilterNot['T](l: List['T], f: 'T => Boolean): List['T] =
  listFilter['T](l, (t: 'T) => !f(t));

def listFind['T](l: List['T], f: 'T => Boolean): Option['T] = l match {
  case Nil => None['T]
  case Cons(h, t) =>
    if (f(h)) Some['T](h) else listFind['T](t, f)
};

def listFlatMap['T, 'S](l: List['T], f: 'T => List['S]): List['S] =
  listFlatten['S](listMap['T, List['S]](l, f));

def listFlatten['T](l: List[List['T]]): List['T] =
  listFoldLeft[List['T], List['T]](Nil['T], l, (l: List['T], r: List['T]) => listConcat['T](l, r));

def listFoldLeft['T, 'S](a: 'S, l: List['T], f: ('S, 'T) => 'S): 'S =
  def aux(i: 'S, r: List['T]): 'S = r match {
    case Nil => i
    case Cons(h, t) => aux(f(i, h), t)
  };
  aux(a, l);

def listFoldRight['T, 'S](l: List['T], a: 'S, f: ('T, 'S) => 'S): 'S = l match {
  case Nil => a
  case Cons(h, t) => f(h, listFoldRight['T, 'S](t, a, f))
};

def listForall['T](l: List['T], f: 'T => Boolean): Boolean =
  !listExists['T](l, (t: 'T) => !f(t));

def listForeach['T](l: List['T], f: 'T => Unit): Unit = {
  listMap['T, Unit](l, f);
  ()
};

def listGet['T](l: List['T], i: Int): Option['T] = l match {
  case Nil => None['T]
  case Cons(h, t) =>
    if (i > 0) listGet['T](t, i - 1)
    else if (i == 0) Some['T](h)
    else None['T]
};

def listIsEmpty['T](l: List['T]): Boolean = l match {
  case Nil => true
  case Cons(h, t) => false
};

def listLength['T](l: List['T]): Int =
  listFoldLeft['T, Int](0, l, (i: Int, t: 'T) => i + 1);

def listMap['T, 'S](l: List['T], f: 'T => 'S): List['S] = l match {
  case Nil => Nil['S]
  case Cons(h, t) => Cons['S](f(h), listMap['T, 'S](t, f))
};

def listNonEmpty['T](l: List['T]): Boolean =
  !listIsEmpty['T](l);

def listPrepended['T](l: List['T], t: 'T): List['T] =
  Cons['T](t, l);

def listReverse['T](l: List['T]): List['T] =
  listFoldLeft['T, List['T]](Nil['T], l, (t: List['T], h: 'T) => Cons['T](h, t));

def listTake['T](l: List['T], i: Int): List['T] = l match {
  case Nil => Nil['T]
  case Cons(h, t) => if (i <= 0) Nil['T] else Cons['T](h, listTake['T](t, i - 1))
};

def listUnzip['T, 'S](l: List[Pair['T, 'S]]): Pair[List['T], List['S]] = l match {
  case Nil => Pair[List['T], List['S]](Nil['T], Nil['S])
  case Cons(h, t) =>
    val h1 = pairFst['T, 'S](h);
    val h2 = pairSnd['T, 'S](h);
    val unziped = listUnzip['T, 'S](t);
    val t1 = pairFst[List['T], List['S]](unziped);
    val t2 = pairSnd[List['T], List['S]](unziped);
    val l1 = Cons['T](h1, t1);
    val l2 = Cons['S](h2, t2);
    Pair[List['T], List['S]](l1, l2)
};

def listZip['T, 'S](l: List['T], r: List['S]): List[Pair['T, 'S]] = l match {
  case Nil => Nil[Pair['T, 'S]]
  case Cons(h1, t1) => r match {
    case Nil => Nil[Pair['T, 'S]]
    case Cons(h2, t2) =>
      val h = Pair['T, 'S](h1, h2);
      val t = listZip['T, 'S](t1, t2);
      Cons[Pair['T, 'S]](h, t)
  }
};

def listZipWithIndex['T](l: List['T]): List[Pair['T, Int]] =
  val len = listLength['T](l);
  def aux(i: Int): List[Int] =
    if (i >= len) Nil[Int]
    else Cons[Int](i, aux(i + 1));
  val toLen = aux(0);
  listZip['T, Int](l, toLen);

type Map['T, 'S] {
  case Map(List[Pair['T, 'S]], ('T, 'T) => Boolean)
}

def Map0['T, 'S](f: ('T, 'T) => Boolean): Map['T, 'S] =
  Map['T, 'S](List0[Pair['T, 'S]](), f);
def Map1['T, 'S](f: ('T, 'T) => Boolean, t1: 'T, s1: 'S): Map['T, 'S] =
  Map['T, 'S](List1[Pair['T, 'S]](
    Pair['T, 'S](t1, s1)
  ), f);
def Map2['T, 'S](f: ('T, 'T) => Boolean, t1: 'T, s1: 'S, t2: 'T, s2: 'S): Map['T, 'S] =
  Map['T, 'S](List2[Pair['T, 'S]](
    Pair['T, 'S](t2, s2),
    Pair['T, 'S](t1, s1)
  ), f);
def Map3['T, 'S](f: ('T, 'T) => Boolean, t1: 'T, s1: 'S, t2: 'T, s2: 'S, t3: 'T, s3: 'S): Map['T, 'S] =
  Map['T, 'S](List3[Pair['T, 'S]](
    Pair['T, 'S](t3, s3),
    Pair['T, 'S](t2, s2),
    Pair['T, 'S](t1, s1)
  ), f);
def Map4['T, 'S](f: ('T, 'T) => Boolean, t1: 'T, s1: 'S, t2: 'T, s2: 'S, t3: 'T, s3: 'S, t4: 'T, s4: 'S): Map['T, 'S] =
  Map['T, 'S](List4[Pair['T, 'S]](
    Pair['T, 'S](t4, s4),
    Pair['T, 'S](t3, s3),
    Pair['T, 'S](t2, s2),
    Pair['T, 'S](t1, s1)
  ), f);
def Map5['T, 'S](f: ('T, 'T) => Boolean, t1: 'T, s1: 'S, t2: 'T, s2: 'S, t3: 'T, s3: 'S, t4: 'T, s4: 'S, t5: 'T, s5: 'S): Map['T, 'S] =
  Map['T, 'S](List5[Pair['T, 'S]](
    Pair['T, 'S](t5, s5),
    Pair['T, 'S](t4, s4),
    Pair['T, 'S](t3, s3),
    Pair['T, 'S](t2, s2),
    Pair['T, 'S](t1, s1)
  ), f);

def mapFold['T, 'S, 'R](r: 'R, m: Map['T, 'S], f: ('R, 'T, 'S) => 'R): 'R = m match {
  case Map(m, g) =>
    listFoldLeft[Pair['T, 'S], 'R](r, m,
      (r: 'R, p: Pair['T, 'S]) => f(r, pairFst['T, 'S](p), pairSnd['T, 'S](p))
    )
};

def mapGet['T, 'S](m: Map['T, 'S], t: 'T): Option['S] = m match {
  case Map(m, f) =>
    def aux(p: Pair['T, 'S]): Boolean = f(pairFst['T, 'S](p), t);
    val o = listFind[Pair['T, 'S]](m, aux);
    optionMap[Pair['T, 'S], 'S](o, pairSnd['T, 'S])
};

def mapRemoved['T, 'S](m: Map['T, 'S], t: 'T): Map['T, 'S] = m match {
  case Map(m, f) =>
    Map['T, 'S](listFilterNot[Pair['T, 'S]](
      m, (p: Pair['T, 'S]) => f(pairFst['T, 'S](p), t)
    ), f)
};

def mapToList['T, 'S](m: Map['T, 'S]): List[Pair['T, 'S]] = m match {
  case Map(m, f) => m
};

def mapUpdated['T, 'S](m: Map['T, 'S], t: 'T, s: 'S): Map['T, 'S] = mapRemoved['T, 'S](m, t) match {
  case Map(m, f) =>
    Map['T, 'S](listPrepended[Pair['T, 'S]](m, Pair['T, 'S](t, s)), f)
};

val STRP = Cons[Int];
val EOS = Nil[Int];

val stringEquals = listEquals[Int](intEquals);

def substring(s: List[Int], i: Int, j: Int): List[Int] =
  if (i >= j) Nil[Int]
  else listTake[Int](listDrop[Int](s, i), j - i);

type Parser['T] {
  case Parser(List[Int] => ParseResult['T])
}

type ParseResult['T] {
  case Success('T, List[Int])
  case Failure
}

def parse['T](parser: Parser['T], str: List[Int]): ParseResult['T] = parser match {
  case Parser(f) => f(str)
};

def parseAll['T](parser: Parser['T], str: List[Int]): Option['T] = parse['T](parser, str) match {
  case Success(t, r) => if (listIsEmpty[Int](r)) Some['T](t) else None['T]
  case Failure => None['T]
};

def parserConst(c: Int): Parser[Int] = Parser[Int]((str: List[Int]) => str match {
  case Nil => Failure[Int]
  case Cons(h, t) => if (h == c) Success[Int](h, t) else Failure[Int]
});

def parserCond(p: Int => Boolean): Parser[Int] = Parser[Int]((str: List[Int]) => str match {
  case Nil => Failure[Int]
  case Cons(h, t) => if (p(h)) Success[Int](h, t) else Failure[Int]
});

def parserThen['T, 'S](l: () => Parser['T], r: () => Parser['S]): Parser[Pair['T, 'S]] =
  Parser[Pair['T, 'S]]((str: List[Int]) =>
    parse['T](l(), str) match {
      case Success(t, str1) => parse['S](r(), str1) match {
        case Success(s, str2) => Success[Pair['T, 'S]](Pair['T, 'S](t, s), str2)
        case Failure => Failure[Pair['T, 'S]]
      }
      case Failure => Failure[Pair['T, 'S]]
    }
  );

def parserOr['T](l: () => Parser['T], r: () => Parser['T]): Parser['T] =
  Parser['T]((str: List[Int]) =>
    val res = parse['T](l(), str);
    res match {
      case Success(t, str1) => res
      case Failure => parse['T](r(), str)
    }
  );

def parserMap['T, 'S](l: () => Parser['T], f: 'T => 'S): Parser['S] =
  Parser['S]((str: List[Int]) =>
    parse['T](l(), str) match {
      case Success(t, str1) => Success['S](f(t), str1)
      case Failure => Failure['S]
    }
  );
"""

}
