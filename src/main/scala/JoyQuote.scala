import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

package object joy {
  implicit class JoyQuote(ctx: StringContext) {
    object j {
      def apply[T](args: T*): Joy = macro JoyQuoteImpl.apply
      def unapply(scrutinee: Any): Any = macro JoyQuoteImpl.unapply
    }
  }
}

package joy {
  object ` :+ ` {
    def apply[T](elems: List[T], elem: T): List[T] = elems :+ elem
    def unapply[T](elems:List[T]): Option[(List[T], T)] = :+.unapply(elems)
  }

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

    lazy val q"$_($_(..${parts: List[String]})).j.$method[..$_](..$args)" =
      c.macroApplication

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

    type Lifted = (List[Tree], Tree)

    def arg(i: Int, dotted: Boolean = false): Lifted = method match {
      case TermName("apply") =>
        val arg = args(i)
        val tpe = if (!dotted) arg.tpe else iterableT(arg.tpe)
        val subst: Tree => Tree =
          if (tpe <:< typeOf[Joy]) identity
          else {
            val lift = c.inferImplicitValue(appliedType(typeOf[Joy.Lift[_]], tpe), silent = true)
            if (lift.nonEmpty) t => q"$lift($t)"
            else c.abort(arg.pos, s"couldn't find implicit value of type Lift[$tpe]")
          }
        if (!dotted) (Nil, subst(arg))
        else {
          val x = TermName(c.freshName())
          (Nil, q"$arg.map { ($x: $tpe) => ${subst(q"$x")} }.toList")
        }
      case TermName("unapply") =>
        val x = TermName(s"x$i")
        val subpattern = c.internal.subpatterns(args.head).get.apply(i)
        subpattern match {
          case pq"$_: $tpt" =>
            val typed = c.typecheck(tpt, c.TYPEmode)
            val tpe = if (!dotted) typed.tpe else iterableT(typed.tpe)
            val unlift = c.inferImplicitValue(appliedType(typeOf[Joy.Unlift[_]], tpe), silent = true)
            if (unlift.isEmpty)
              c.abort(c.enclosingPosition,
                s"couldn't find implicit value of type Unlift[$tpe]")
            else if (!dotted) (Nil, pq"$unlift($x @ _)")
            else {
              val name = TermName(c.freshName("unlift"))
              val pre = q"""
                val $name: _root_.joy.Joy.Unlift.Elementwise[$tpe] =
                  new _root_.joy.Joy.Unlift.Elementwise[$tpe]($unlift)
              """
              (pre :: Nil, pq"$name($x @ _)")
            }
          case _ => (Nil, pq"$x @ _")
        }
    }

    def lift(joys: List[Joy]): Lifted = {
      def prepend(joys: List[Joy], t: Lifted): Lifted =
        joys.foldRight(t) { case (j, (pre, acc)) =>
          val (jpre, jlifted) = lift(j)
          (jpre ++: pre, q"_root_.scala.collection.immutable.::($jlifted, $acc)")
        }
      def append(t: Lifted, joys: List[Joy]): Lifted =
        joys.foldLeft(t) { case ((pre, acc), j) =>
          val (jpre, jlifted) = lift(j)
          (jpre ++: pre, q"_root_.joy.` :+ `($acc, $jlifted)")
        }

      val (pre, middle) = joys.span(_ != Joy.Name(".."))
      middle match {
        case Nil =>
          prepend(pre, (Nil, q"$Nil"))
        case Joy.Name("..") :: Joy.Name(Hole(i)) :: rest =>
          append(prepend(pre, arg(i, dotted = true)), rest)
        case _ =>
          c.abort(c.enclosingPosition, "incorrect usage of ..")
      }
    }

    def lift(joy: Joy): Lifted = joy match {
      case Joy.Int(value)    => (Nil, q"_root_.joy.Joy.Int($value)")
      case Joy.Bool(value)   => (Nil, q"_root_.joy.Joy.Bool($value)")
      case Joy.Name(Hole(i)) => arg(i)
      case Joy.Name(value)   => (Nil, q"_root_.joy.Joy.Name($value)")
      case Joy.Quoted(joys)  =>
        val (pre, lifted) = lift(joys)
        (pre, q"_root_.joy.Joy.Quoted($lifted)")
      case Joy.Program(joys) =>
        val (pre, lifted) = lift(joys)
        (pre, q"_root_.joy.Joy.Program($lifted)")
    }

    def wrap(joy: Joy): Tree = method match {
      case TermName("apply") =>
        val (preamble, lifted) = lift(joy)
        q"..$preamble; $lifted"
      case TermName("unapply") =>
        val (thenp, elsep) =
          if (parts.length == 1) (q"true", q"false")
          else {
            val xs = parts.init.zipWithIndex.map { case (_, i) => val x = TermName(s"x$i"); q"$x" }
            (q"_root_.scala.Some((..$xs))", q"_root_.scala.None")
          }
        val (preamble, lifted) = lift(joy)
        q"""
          new {
            def unapply(input: Joy) = {
              ..$preamble
              input match {
                case $lifted => $thenp
                case _       => $elsep
              }
            }
          }.unapply(..$args)
        """
    }

    def expand = wrap(flatten(Joy.parse(code()).get))
    def apply(args: Tree*) = expand
    def unapply(scrutinee: Tree) = expand
  }
}
