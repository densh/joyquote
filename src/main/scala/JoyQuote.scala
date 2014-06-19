import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

package object joy {
  implicit class JoyQuote(ctx: StringContext) {
    def j(args: Joy*): Joy = macro JoyQuoteImpl.apply
  }
}

package joy {
  private[joy] class JoyQuoteImpl(val c: Context) {
    import c.universe._

    lazy val q"$_($_(..${parts: List[String]})).j(..$args)" = c.macroApplication

    def flatten(joy: Joy): Joy = joy match {
      case Joy.Program(j :: Nil) => j
      case _                     => joy
    }

    implicit def lift[J <: Joy]: Liftable[J] = Liftable {
      case Joy.Int(value)    => q"_root_.joy.Joy.Int($value)"
      case Joy.Bool(value)   => q"_root_.joy.Joy.Bool($value)"
      case Joy.Name(value)   => q"_root_.joy.Joy.Name($value)"
      case Joy.Quoted(joys)  => q"_root_.joy.Joy.Quoted($joys)"
      case Joy.Program(joys) => q"_root_.joy.Joy.Program($joys)"
    }

    def apply(args: Tree*) = lift(flatten(Joy.parse(parts.head).get))
  }
}
