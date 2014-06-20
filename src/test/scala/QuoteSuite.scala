package joy

import org.scalatest.FunSuite
import Joy.parse

class QuoteExprSuite extends FunSuite {
  test("42") {
    val Joy.Int(42) = j"42"
  }

  test("x") {
    val Joy.Name("x") = j"x"
  }

  test("1 1 add") {
    val Joy.Program(List(Joy.Int(1), Joy.Int(2), Joy.Name("add"))) = j"1 2 add"
  }

  test("[1 2 3] [2 mod] filter") {
    val Joy.Program(List(Joy.Quoted(List(Joy.Int(1), Joy.Int(2), Joy.Int(3))),
                         Joy.Quoted(List(Joy.Int(2), Joy.Name("mod"))),
                         Joy.Name("filter"))) = j"[1 2 3] [2 mod] filter"
  }

  test("$two $two +") {
    val two = j"2"
    val Joy.Program(List(Joy.Int(2), Joy.Int(2), Joy.Name("+"))) = j"$two $two +"
  }

  test("${42}") {
    val Joy.Int(42) = j"${42}"
  }

  test("${'foo}") {
    val Joy.Name("foo") = j"${'foo}"
  }

  test("$xs") {
    val xs = List(j"x1", j"x2")
    val Joy.Quoted(List(Joy.Name("x1"), Joy.Name("x2"))) = j"$xs"
  }

  test("$ints") {
    val ints = List(1, 2)
    val Joy.Quoted(List(Joy.Int(1), Joy.Int(2))) = j"$ints"
  }

  test("..$ab c") {
    val ab = List(j"a", j"b")
    val Joy.Program(List(Joy.Name("a"), Joy.Name("b"), Joy.Name("c"))) = j"..$ab c"
  }

  test("c ..$ab") {
    val ab = List(j"a", j"b")
    val Joy.Program(List(Joy.Name("c"), Joy.Name("a"), Joy.Name("b"))) = j"c ..$ab"
  }

  test("..$ints +") {
    val ints = List(1, 2)
    val Joy.Program(List(Joy.Int(1), Joy.Int(2), Joy.Name("+"))) = j"..$ints +"
  }

  test("0 ..$ints") {
    val ints = List(1, 2)
    val Joy.Program(List(Joy.Int(0), Joy.Int(1), Joy.Int(2))) = j"0 ..$ints"
  }

  test("..$prog +") {
    val prog = j"1 2"
    val Joy.Program(List(Joy.Int(1), Joy.Int(2), Joy.Name("+"))) = j"..$prog +"
  }

  test("0 ..$prog") {
    val prog = j"1 2"
    val Joy.Program(List(Joy.Int(0), Joy.Int(1), Joy.Int(2))) = j"0 ..$prog"
  }
}

class QuotePatSuite extends FunSuite {
  test("42") {
    val j"42" = Joy.Int(42)
  }

  test("foo") {
    val j"foo" = Joy.Name("foo")
  }

  test("[1 2 +]") {
    val j"[1 2 +]" = Joy.Quoted(List(Joy.Int(1), Joy.Int(2), Joy.Name("+")))
  }

  test("1 2 +") {
    val j"1 2 +" = Joy.Program(List(Joy.Int(1), Joy.Int(2), Joy.Name("+")))
  }

  test("$a $b +") {
    val j"$a $b +" = j"1 2 +"
  }

  test("..$ab c") {
    val j"..$ab c" = j"a b c"
    val List(j"a", j"b") = ab
  }

  test("a ..$bc") {
    val j"a ..$bc" = j"a b c"
    val List(j"b", j"c") = bc
  }

  test("${a: Int} ${b: Int} +") {
    val j"${a: Int} ${b: Int} +" = j"1 2 +"
    assert(a == 1)
    assert(b == 2)
  }
}
