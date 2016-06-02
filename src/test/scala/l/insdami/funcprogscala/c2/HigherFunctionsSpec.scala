package l.insdami.funcprogscala.c2

import org.scalatest.FlatSpec

class HigherFunctionsSpec extends FlatSpec {

  "first fibonacci number" should "be 0" in {
    assert(HigherFunctions.fib(0) == 0)
  }

  "second fibonacci number" should "be 1" in {
    assert(HigherFunctions.fib(1) == 1)
  }

  "third fibonacci number" should "be 1" in {
    assert(HigherFunctions.fib(2) == 1)
  }

  "sixth fibonacci number" should "be 5" in {
    assert(HigherFunctions.fib(5) == 5)
  }

}
