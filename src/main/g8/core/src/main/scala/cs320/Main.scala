package cs320

import org.jline.reader.{LineReaderBuilder, EndOfFileException, UserInterruptException}
import org.jline.terminal.TerminalBuilder

import scala.Console.{MAGENTA => M, CYAN => C, RESET}

object Main {

  val name = "FABRIC"

  def main(args: Array[String]): Unit = {
    val withLib = args match {
      case Array("--lib") => true
      case _ => false
    }
    val terminal = TerminalBuilder.builder.build()
    val reader = LineReaderBuilder.builder.terminal(terminal).build()
    def strs: LazyList[String] = (
      try {
        reader.readLine(s"\n$M$name>$RESET ")
      } catch {
        case _: EndOfFileException | _: UserInterruptException => ":q"
      }
    ) #:: strs

    println(s"Welcome to the $M$name$RESET REPL.")
    println(s"Type in :q, :quit, or the EOF character to terminate the REPL.")

    for (
      str <- strs.takeWhile(s => !eof(s)) if str.nonEmpty;
      expr <- lift {
        val expr =
          if (withLib)
            Typed.Expr(StdLib.code + str)
          else {
            val expr = Typed.Expr(str)
            println(s"  ${C}Parsed:$RESET $expr")
            expr
          }
        expr
      };
      _ <- lift {
        val typ = Implementation.typeCheck(expr)
        println(s"  ${C}Type:$RESET ${Typed.showType(typ)}")
      };
      erased <- lift {
        val erased = Typed.erase(expr)
        if (!withLib)
          println(s"  ${C}Erased:$RESET $erased")
        erased
      };
      _ <- lift {
        val value = Implementation.interp(erased)
        println(s"  ${C}Result:$RESET ${Untyped.showValue(value)}")
      }
    ) ()
  }

  def eof(str: String): Boolean = str == ":quit" || str == ":q"

  def lift[T](res: => T): Option[T] = try {
    Some(res)
  } catch {
    case e: Throwable =>
      e.printStackTrace()
      None
  }
}
