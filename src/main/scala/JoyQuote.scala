import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

package object joy {
  implicit class JoyQuote(ctx: StringContext) {
    def j(args: Joy*): Joy = macro JoyQuoteImpl.apply
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

    lazy val q"$_($_(..${parts: List[String]})).j(..$args)" = c.macroApplication

    def code() =
      parts.init.zipWithIndex.map { case (part, i) =>
        s"$part${Hole(i)}"
      }.mkString("", "", parts.last)

    def flatten(joy: Joy): Joy = joy match {
      case Joy.Program(j :: Nil) => j
      case _                     => joy
    }

    implicit def lift[J <: Joy]: Liftable[J] = Liftable {
      case Joy.Int(value)    => q"_root_.joy.Joy.Int($value)"
      case Joy.Bool(value)   => q"_root_.joy.Joy.Bool($value)"
      case Joy.Name(Hole(i)) => args(i)
      case Joy.Name(value)   => q"_root_.joy.Joy.Name($value)"
      case Joy.Quoted(joys)  => q"_root_.joy.Joy.Quoted($joys)"
      case Joy.Program(joys) => q"_root_.joy.Joy.Program($joys)"
    }

    def apply(args: Tree*) = lift(flatten(Joy.parse(code()).get))
  }
}
