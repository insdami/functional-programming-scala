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


}
