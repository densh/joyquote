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

    lazy val IterableClass: TypeSymbol =
      typeOf[Iterable[_]].typeSymbol.asType

    lazy val IterableTParam: Type =
      IterableClass.typeParams(0).asType.toType

    def code() =
      parts.init.zipWithIndex.map { case (part, i) =>
        s"$part${Hole(i)}"
      }.mkString("", "", parts.last)

    def flatten(joy: Joy): Joy = joy match {
      case Joy.Program(j :: Nil) => j
      case _                     => joy
    }

    def iterableT(tpe: Type): Type =
      IterableTParam.asSeenFrom(tpe, IterableClass)

    def arg(i: Int, dotted: Boolean = false) =  {
      val arg = args(i)
      val tpe = if (!dotted) arg.tpe else iterableT(arg.tpe)
      val subst: Tree => Tree =
        if (tpe <:< typeOf[Joy]) identity
        else {
          val LiftT = appliedType(typeOf[Joy.Lift[_]], tpe)
          val lift = c.inferImplicitValue(LiftT, silent = true)
          if (lift.nonEmpty) t => q"$lift($t)"
          else c.abort(arg.pos, s"couldn't find implicit value of type Lift[$tpe]")
        }
      if (!dotted) subst(arg)
      else {
        val x = TermName(c.freshName())
        q"$arg.map { ($x: $tpe) => ${subst(q"$x")} }.toList"
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
