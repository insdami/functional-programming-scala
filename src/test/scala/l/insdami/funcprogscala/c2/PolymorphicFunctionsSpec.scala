package l.insdami.funcprogscala.c2

import org.scalatest.FlatSpec

class PolymorphicFunctionsSpec extends FlatSpec {

  "given an array (1,2,3,4,5) and an ordered function asc " should "retrieve that is ordered" in {
    val ordered = (n1:Int, n2:Int) => { n1 < n2 }
    assert(PolymorphicFunctions.isSorted(Array(1,2,3,4,5), ordered))
  }

  "given an array (5,4,3,2,1) and an ordered function desc" should " be sorted" in {
    val ordered = (n1:Int, n2:Int) => { n1 > n2 }
    assert(PolymorphicFunctions.isSorted(Array(5, 4, 3, 2, 1), ordered))
  }

  "given an array (5,3,1,2,4) and an ordered function " should "not be sorted" in {
    val ordered = (n1:Int, n2:Int) => { n1 > n2 }
    assert(!PolymorphicFunctions.isSorted(Array(5,3,1,2,4), ordered))
  }

  "given null and an ordered function " should "not be sorted" in {
    val ordered = (n1:Int, n2:Int) => { n1 < n2 }
    assert(!PolymorphicFunctions.isSorted(null, ordered))
  }

  "given an array (1) and an ordered function " should "be sorted" in {
    val ordered = (n1:Int, n2:Int) => { n1 < n2 }
    assert(PolymorphicFunctions.isSorted(Array(1), ordered))
  }

  "given partial function that just add 1 to a number" should "return a number + 1" in {
    val plus:(Int, Int) => Int = (n1, n2) => n1 + n2
    val addOne = PolymorphicFunctions.partial1(1, plus)
    assert(addOne(1) == 2)
  }

  "given partial function that does a number times 2" should "return that number ^ 2" in {
    val multiply:(Int, Int) => Int = (n1, n2) => n1 * n2
    val pow2 = PolymorphicFunctions.partial1(2, multiply)
    assert(pow2(2) == 4)
  }

  "a curried function" should "return the same result as the uncurried one" in {
    val multiply:(Int, Int) => Int = (n1, n2) => n1 * n2
    val curried = PolymorphicFunctions.curry(multiply)
    assert(curried(2)(2) == multiply(2,2))
  }

  "an uncurried function" should "return the same result as the curried one" in {
    val multiply = (n1:Int) => (n2:Int) => { n1 * n2}
    val uncurried = PolymorphicFunctions.uncurry(multiply)
    assert(uncurried(2, 2) == multiply(2)(2))
  }

  "a composed function" should "return the same result as the non composed one" in {
    val addOne: Int => Int = _ + 1
    val toString: Int => String = _.toString
    val composed = PolymorphicFunctions.compose(toString, addOne)
    assert(composed(5) == toString(addOne(5)))
  }

}
