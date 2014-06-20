package joy

import org.scalatest.FunSuite
import Joy.parse

package object eval {
  type Stack     = Joy.Quoted
  type Transform = Stack => Stack

  private val lookup: Map[String, Transform] = Map(
    // quotation-specific operators
    "i"      -> { case j"[..$init [..$ops]]"        => j"..$ops" eval j"[..$init]" },
    "concat" -> { case j"[..$init [..$a] [..$b]]"   => j"[..$init [..${a ++ b}]]"  },
    "first"  -> { case j"[..$init [$first ..$_]]"   => j"[..$init $first]"         },
    "rest"   -> { case j"[..$init [$_ ..$rest]]"    => j"[..$init [..$rest]]"      },
    "size"   -> { case j"[..$init [..$lst]]"        => j"[..$init ${lst.size}]"    },
    "dip"    -> { case j"[..$init $top [..$ops]]"   =>
      val interm = j"..$ops" eval j"[..$init]"
      j"[..$interm $top]"
    },
    "map"    -> { case j"[..$init [..$lst] [..$f]]" =>
      val mapped = lst.map(el => j"..$f" eval j"[$el]" match { case j"[$value]" => value })
      j"[..$init [..$mapped]]"
    },
    "filter" -> { case j"[..$init [..$lst] [..$pred]]" =>
      val filtered = lst.filter(el => j"..$pred" eval j"[$el]" match {
        case j"[true]"  => true
        case j"[false]" => false
      })
      j"[..$init [..$filtered]]"
    },

    // type-insensitive operators
    "dup"  -> { case j"[..$init $x]"    => j"[..$init $x $x]"     },
    "swap" -> { case j"[..$init $a $b]" => j"[..$init $b $a]"     },
    "="    -> { case j"[..$init $a $b]" => j"[..$init ${a == b}]" },
    "!="   -> { case j"[..$init $a $b]" => j"[..$init ${a != b}]" },

    // bool-specific operators
    "not" -> { case j"[..$init ${a: Boolean}]"               => j"[..$init ${!a}]"     },
    "and" -> { case j"[..$init ${a: Boolean} ${b: Boolean}]" => j"[..$init ${a && b}]" },
    "or"  -> { case j"[..$init ${a: Boolean} ${b: Boolean}]" => j"[..$init ${a || b}]" },

    // int-specific operators
    "+"   -> { case j"[..$init ${a: Int} ${b: Int}]" => j"[..$init ${a + b}]"  },
    "*"   -> { case j"[..$init ${a: Int} ${b: Int}]" => j"[..$init ${a * b}]"  },
    "/"   -> { case j"[..$init ${a: Int} ${b: Int}]" => j"[..$init ${a / b}]"  },
    "rem" -> { case j"[..$init ${a: Int} ${b: Int}]" => j"[..$init ${a % b}]"  },
    "<"   -> { case j"[..$init ${a: Int} ${b: Int}]" => j"[..$init ${a < b}]"  },
    "<="  -> { case j"[..$init ${a: Int} ${b: Int}]" => j"[..$init ${a <= b}]" },
    ">"   -> { case j"[..$init ${a: Int} ${b: Int}]" => j"[..$init ${a > b}]"  },
    ">="  -> { case j"[..$init ${a: Int} ${b: Int}]" => j"[..$init ${a >= b}]" },

    // control flow
    "ifte" -> { case j"[..$init [..$cond] [..$thenp] [..$elsep]]" =>
      j"..$cond" eval j"[..$init]" match {
        case j"[..$_ true]"  => j"..$thenp" eval j"[..$init]"
        case j"[..$_ false]" => j"..$elsep" eval j"[..$init]"
      }
    },
    "primrec" -> { case j"[..$init ${top: Int} [..$zero] [..$step]]" =>
       if (top == 0) j"..$zero" eval j"[..$init]"
       else j"$top ${top - 1} [..$zero] [..$step] primrec ..$step" eval j"[..$init]"
    }
  )

  implicit class Eval(joy: Joy) {
    def eval: Transform = joy match {
      case name: Joy.Name    => lookup(name.value)
      case prog: Joy.Program => stack => prog.elems.map(_.eval).reduce((a, b) => b compose a).apply(stack)
      case _                 => stack => j"[..$stack $joy]"
    }
    def evali = eval(j"[]")
  }
}

class EvalSuite extends FunSuite {
  import eval._

  test("1") {
    val j"[1]" = j"1" evali
  }

  test("1 dup") {
    val j"[1 1]" = j"1 dup" evali
  }

  test("[1 dup] i") {
    val j"[1 1]" = j"[1 dup] i" evali
  }

  test("1 2 3") {
    val onetwo = j"1 2"
    val three = j"3"
    val j"[1 2 3]" = j"$onetwo $three" evali
  }

  test("4 2 /") {
    val j"[2]" = j"4 2 /" evali
  }

  test("4 2 rem") {
    val j"[2]" = j"5 3 rem" evali
  }

  test("1 2 + 4 *") {
    val j"[12]"= j"1 2 + 4 *" evali
  }

  test("[1 2] [3 4] concat") {
    val j"[[1 2 3 4]]" = j"[1 2] [3 4] concat" evali
  }

  test("true false or") {
    val j"[true]" = j"true false or" evali
  }

  test("= !=") {
    val j"[true]"  = j"1 1 ="  evali
    val j"[false]" = j"1 1 !=" evali
  }

  test("< <=") {
    val j"[true]"  = j"1 2 <"  evali
    val j"[true]"  = j"1 2 <=" evali
    val j"[true]"  = j"1 1 <=" evali
    val j"[false]" = j"2 1 <=" evali
    val j"[true]"  = j"1 2 <"  evali
  }

  test("> >=") {
    val j"[false]" = j"1 2 >"  evali
    val j"[false]" = j"1 2 >=" evali
    val j"[true]"  = j"1 1 >=" evali
    val j"[true]"  = j"2 1 >=" evali
    val j"[false]" = j"1 2 >"  evali
  }

  test("[1 1 1 1] size") {
    val j"[4]" = j"[1 1 1 1] size" evali
  }

  test("[1 2 3] first") {
    val j"[1]" = j"[1 2 3] first" evali
  }

  test("[1 2 3] rest") {
    val j"[[2 3]]" = j"[1 2 3] rest" evali
  }

  test("5 [1] [*] primrec") {
    val j"[120]" = j"5 [1] [*] primrec" evali
  }

  test("[1 2 3] [dup *] map") {
    val j"[[1 4 9]]" = j"[1 2 3] [dup *] map" evali
  }

  test("[1 2 3 4] [2 rem 0 =] filter") {
    val j"[[2 4]]" = j"[1 2 3 4] [2 rem 0 =] filter" evali
  }

  test("[1000 >] [2 /] [3 *] ifte") {
    val j"[501]" = j"1002 [1000 >] [2 /] [3 *] ifte" evali
    val j"[30]" = j"10 [1000 >] [2 /] [3 *] ifte" evali
  }

  test("2 3 1 [+] dip") {
    val j"[5 1]" = j"2 3 1 [+] dip" evali
  }
}
