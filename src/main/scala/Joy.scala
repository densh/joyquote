package joy

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharSequenceReader

sealed trait Joy
object Joy {
  final case class Int(value: scala.Int) extends Joy
  final case class Bool(value: scala.Boolean) extends Joy
  final case class Name(value: String) extends Joy
  final case class Quoted(elems: List[Joy]) extends Joy
  final case class Program(elems: List[Joy]) extends Joy

  object parse extends RegexParsers {
    val lexical = new StdLexical
    lexical.delimiters ++= List("[", "]")
    lexical.reserved   ++= List("true", "false", "+", "*", "/", "=",
                                "!=", ">", ">=", "<", "<=", "..")

    val intregex  = """(([1-9][0-9]*)|0)""".r
    val nameregex = """[a-zA-Z$][a-zA-Z0-9]*""".r

    def int:    Parser[Joy.Int]     = intregex                ^^  { v => Joy.Int(v.toInt) }
    def name:   Parser[Joy.Name]    = nameregex               ^^  Joy.Name
    def tru:    Parser[Joy.Bool]    = "true"                  ^^^ Joy.Bool(true)
    def fls:    Parser[Joy.Bool]    = "false"                 ^^^ Joy.Bool(false)
    def quoted: Parser[Joy.Quoted]  = "[" ~> rep(joy) <~ "]"  ^^  Joy.Quoted
    def op:     Parser[Joy.Name]    = ("+"  | "*"  | "/"  |
                                       ">=" | ">"  | "<=" |
                                       "<"  | "="  | "!=" |
                                       ".."               )   ^^  Joy.Name
    def bool:   Parser[Joy.Bool]    = tru | fls
    def joy:    Parser[Joy]         = bool | op | name | int | quoted
    def progr:  Parser[Joy.Program] = rep(joy)                ^^  Joy.Program

    def apply(s: String) = phrase(progr)(new CharSequenceReader(s))
  }
}
