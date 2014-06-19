package joy

import org.scalatest.FunSuite
import Joy.parse

class QuoteSuite extends FunSuite {
  test("42") {
    val Joy.Program(List(Joy.Int(42))) = j"42"
  }

  test("x") {
    val Joy.Program(List(Joy.Name("x"))) = j"x"
  }

  test("1 1 add") {
    val Joy.Program(List(Joy.Int(1), Joy.Int(2), Joy.Name("add"))) = j"1 2 add"
  }

  test("[1 2 3] [2 mod] filter") {
    val Joy.Program(List(Joy.Quoted(List(Joy.Int(1), Joy.Int(2), Joy.Int(3))),
                         Joy.Quoted(List(Joy.Int(2), Joy.Name("mod"))),
                         Joy.Name("filter"))) = j"[1 2 3] [2 mod] filter"
  }
}
