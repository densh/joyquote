package joy

import org.scalatest.FunSuite
import Joy.parse

class PrintSuite extends FunSuite {
  test("42") {
    val "42" = parse("42").get.toString
  }

  test("x") {
    val "x" = parse("x").get.toString
  }

  test("1 1 add") {
    val "1 2 add" = parse("1 2 add").get.toString
  }

  test("[1 2 3] [2 mod] filter") {
    val "[1 2 3] [2 mod] filter" = parse("[1 2 3] [2 mod] filter").get.toString
  }
}
