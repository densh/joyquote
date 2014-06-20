import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

package object joy {
  implicit class JoyQuote(ctx: StringContext) {
    def j[T](args: T*): Joy = macro JoyQuoteImpl.apply
  }
}

package joy {
  private[joy] object Hole {
    val pat = java.util.regex.Pattern.compile("^\\$placeholder(\\d+)$")
    def apply(i: Int) = s"$$placeholder$i"
    def unapply(s: String): Option[Int] = {
      val m = pat.matcher(s)
      if (m.find()) Some(m.group(1).toInt) else None
    }
  }

  private[joy] class JoyQuoteImpl(val c: Context) {
    import c.universe._

    lazy val q"$_($_(..${parts: List[String]})).j[..$_](..$args)" = c.macroApplication

    def code() =
      parts.init.zipWithIndex.map { case (part, i) =>
        s"$part${Hole(i)}"
      }.mkString("", "", parts.last)

    def flatten(joy: Joy): Joy = joy match {
      case Joy.Program(j :: Nil) => j
      case _                     => joy
    }

    def arg(i: Int, dotted: Boolean = false): Tree =
      if (dotted) args(i)
      else {
        val arg = args(i)
        val tpe = arg.tpe
        if (tpe <:< typeOf[Joy]) arg
        else {
          val lift = c.inferImplicitValue(appliedType(typeOf[Joy.Lift[_]], tpe), silent = true)
          if (lift.nonEmpty) q"$lift($arg)"
          else c.abort(arg.pos, s"couldn't find implicit value of type Lift[$tpe]")
        }
      }

    implicit def liftJoys: Liftable[List[Joy]] = Liftable { joys =>
      def prepend(joys: List[Joy], t: Tree) =
        joys.foldRight(t) { case (j, acc) => q"$j :: $acc" }
      def append(t: Tree, joys: List[Joy]) =
        joys.foldLeft(t) { case (acc, j) => q"$acc :+ $j" }

      val (pre, middle) = joys.span(_ != Joy.Name(".."))
      middle match {
        case Nil =>
          prepend(pre, q"$Nil")
        case Joy.Name("..") :: Joy.Name(Hole(i)) :: rest =>
          append(prepend(pre, arg(i, dotted = true)), rest)
        case _ =>
          c.abort(c.enclosingPosition, "incorrect usage of ..")
      }
    }

    implicit def lift[J <: Joy]: Liftable[J] = Liftable {
      case Joy.Int(value)    => q"_root_.joy.Joy.Int($value)"
      case Joy.Bool(value)   => q"_root_.joy.Joy.Bool($value)"
      case Joy.Name(Hole(i)) => arg(i)
      case Joy.Name(value)   => q"_root_.joy.Joy.Name($value)"
      case Joy.Quoted(joys)  => q"_root_.joy.Joy.Quoted($joys)"
      case Joy.Program(joys) => q"_root_.joy.Joy.Program($joys)"
    }

    def apply(args: Tree*) = lift(flatten(Joy.parse(code()).get))
  }
}
