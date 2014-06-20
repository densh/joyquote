package joy

import org.scalatest.FunSuite
import Joy.parse

class QuoteSuite extends FunSuite {
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
}
