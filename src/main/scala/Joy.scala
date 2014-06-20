package joy

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharSequenceReader

sealed trait Joy {
  final override def toString = this match {
    case Joy.Int(value)     => value.toString
    case Joy.Bool(value)    => value.toString
    case Joy.Name(value)    => value
    case Joy.Quoted(elems)  => elems.mkString("[", " ", "]")
    case Joy.Program(elems) => elems.mkString("", " ", "")
  }
}
object Joy {
  final case class Int(value: scala.Int) extends Joy
  final case class Bool(value: scala.Boolean) extends Joy
  final case class Name(value: String) extends Joy
  final case class Quoted(elems: List[Joy]) extends Iterable[Joy] with Joy { def iterator = elems.iterator }
  final case class Program(elems: List[Joy]) extends Iterable[Joy] with Joy { def iterator = elems.iterator }

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

  trait Lift[T] { def apply(value: T): Joy }
  object Lift {
    def apply[T](f: T => Joy): Lift[T] = new Lift[T] { def apply(value: T) = f(value) }

    implicit def liftJoy[J <: Joy]: Lift[J] = Lift(identity)
    implicit def liftInt[T <: scala.Int]: Lift[T] = Lift { i => Joy.Int(i) }
    implicit def liftBool[T <: Boolean]: Lift[T] = Lift { b => Joy.Bool(b) }
    implicit def liftSym: Lift[Symbol] = Lift { sym => Joy.Name(sym.name) }
    implicit def liftList[T](implicit liftT: Lift[T]): Lift[List[T]] = Lift { l => Joy.Quoted(l.map(liftT(_))) }
  }

  trait Unlift[T] { def unapply(joy: Joy): Option[T] }
  object Unlift {
    def apply[T](pf: PartialFunction[Joy, T]): Unlift[T] = new Unlift[T] { def unapply(joy: Joy) = pf.lift(joy) }

    implicit val unliftJoy: Unlift[Joy] = Unlift { case j => j }
    implicit val unliftInt: Unlift[scala.Int] = Unlift { case Joy.Int(i) => i }
    implicit val unliftBool: Unlift[scala.Boolean] = Unlift { case Joy.Bool(b) => b }
    implicit val unliftSym: Unlift[Symbol] = Unlift { case Joy.Name(value) => Symbol(value) }
    implicit def unliftList[T](implicit unlift: Unlift[T]): Unlift[List[T]] = Unlift {
      case Joy.Quoted(elems) if elems.forall(unlift.unapply(_).nonEmpty) => elems.flatMap(unlift.unapply(_))
    }
  }
}
