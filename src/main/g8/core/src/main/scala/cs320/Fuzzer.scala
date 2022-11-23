package cs320

import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global
import scala.sys.process._

object Fuzzer {
  def nodeExists(): Boolean = {
    val out: java.io.OutputStream = _ => ()
    ("node -v" #> out).! == 0
  }

  def generateExpr(seed: Int): List[(String, String)] =
    s"node js/fuzzer.js $seed".lazyLines
      .toList
      .sliding(2, 2)
      .collect { case List(a, b) => a -> b }
      .toList

  def lift(v: => String, msg: String): (String, String) = {
    val f = Future(blocking(v))
    try {
      (Await.result(f, 1000.millis), "")
    } catch {
      case e: Throwable =>
        val res = e match {
          case _: PLError => msg
          case _: TimeoutException => "Timeout"
          case _ => "Unexpected error"
        }
        val cause = if (e.getCause != null) e.getCause else e
        val trace = cause.getStackTrace.toList
          .map("\t" + _.toString)
          .takeWhile(!_.contains("cs320.Fuzzer"))
        (res, (cause.getMessage :: trace).mkString("\n"))
    }
  }

  def interp(str: String): (String, String) = {
    val expr = Typed.Expr(str)
    val (typ, t1) = lift(Typed.showType(Implementation.typeCheck(expr)), "Type error")
    if (typ.contains("error") || typ == "Timeout")
      (typ, t1)
    else {
      val erased = Typed.erase(expr)
      val (value, t2) =
        lift(Untyped.showValue(Implementation.interp(erased)), "Runtime error")
      (s"Type: $typ, Value: $value", t2)
    }
  }

  def runOnce(seed: Int): Boolean = {
    generateExpr(seed).to(LazyList).map {
      case (expr, res1) =>
        val (res2, trace) = interp(expr)
        (expr, res1, res2, trace)
    }.find {
      case (_, res1, res2, _) => res1 != res2
    } match {
      case None => true
      case Some((expr, res1, res2, trace)) =>
        println()
        println("Incorrect behavior detected")
        println(s"Expr: $expr")
        println(s"Correct result: $res1")
        println(s"Your result: $res2")
        println(trace)
        false
    }
  }

  def run(): Unit = {
    if (!nodeExists()) println("Node.js not found")
    else {
      var i = 0
      while (runOnce(i / 10000)) {
        if (i > 0)
          print("\b" * i.toString.length)
        i += 10000
        print(i)
      }
    }
  }
}
