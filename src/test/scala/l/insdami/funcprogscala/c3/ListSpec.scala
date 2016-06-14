package l.insdami.funcprogscala.c3

import org.scalatest.FlatSpec

class ListSpec extends FlatSpec {

  "executing tail method on List(1,2,3,4,5)" should "retrieve List(2,3,4,5)" in {
    assert(List.tail(List(1,2,3,4,5)) == List(2,3,4,5))
  }

  "executing tail method on Nil" should "retrieve Nil" in {
    assert(List.tail(Nil) == Nil)
  }

  "executing setHead 0 on List(1,2,3,4,5)" should "retrieve List(0,1,2,3,4,5)" in {
    assert(List.setHead(List(1,2,3,4,5), 0) == List(0,1,2,3,4,5))
  }

  "executing setHead 0 on Nil" should "retrieve List(0)" in {
    assert(List.setHead(Nil, 0) == List(0))
  }

  "executing drop 0 on List(1,2,3,4,5)" should "retrieve the original list" in {
    assert(List.drop(List(1,2,3,4,5), 0) == List(1,2,3,4,5))
  }

  "executing drop 2 on List(1,2,3,4,5)" should "retrieve List(3,4,5)" in {
    assert(List.drop(List(1,2,3,4,5), 2) == List(3,4,5))
  }

  "executing drop 10 on List(1,2,3,4,5)" should "retrieve Nil" in {
    assert(List.drop(List(1,2,3,4,5), 10) == Nil)
  }

  "executing dropWhile element is less than 3 on List(1,2,3,4,5)" should "retrieve List(3,4,5)" in {
    assert(List.dropWhile(List(1,2,3,4,5), (e: Int) => e < 3) == List(3,4,5))
  }

  "executing dropWhile element is less than 10 on List(1,2,3,4,5)" should "retrieve Nil" in {
    assert(List.dropWhile(List(1,2,3,4,5), (e: Int) => e < 10) == Nil)
  }

  "executing dropWhile element is greater than 10 on List(1,2,3,4,5)" should "retrieve List(1,2,3,4,5)" in {
    assert(List.dropWhile(List(1,2,3,4,5), (e: Int) => e > 10) == List(1,2,3,4,5))
  }

  "executing init on List(1,2,3,4,5)" should "retrieve List(1,2,3,4)" in {
    assert(List.init(List(1,2,3,4,5)) == List(1,2,3,4))
  }

  "executing init on List(1)" should "retrieve Nil" in {
    assert(List.init(List(1)) == Nil)
  }

  "executing length on List(1)" should "retrieve a size of 1" in {
    assert(List.length(List(1)) == 1)
  }

  "executing length on Nil" should "retrieve a size of 0" in {
    assert(List.length(Nil) == 0)
  }

  "executing length on List(1,2,3)" should "retrieve a size of 3" in {
    assert(List.length(List(1,2,3)) == 3)
  }

  "executing foldLeft on List(1,2,3) transforming it into a string" should "retrieve 123" in {
    assert(List.foldLeft(List(1,2,3), "")((str, n) => str + n) == "123")
  }

  "executing foldLeft on Nil transforming it into a string" should "retrieve an empty string" in {
    assert(List.foldLeft(Nil, "")((str, n) => str + n) == "")
  }

  "executing map on List(1,2,3) transforming each number into a string" should "retrieve list of strings" in {
    assert(List.map(List(1,2,3))(_.toString) == List("1", "2", "3"))
  }

  "executing map on List(1,2,3) multiplying them by 2" should "retrieve List(2,4,6)" in {
    assert(List.map(List(1,2,3))(_ * 2) == List(2,4,6))
  }

  "executing map on Nil transforming into String" should "retrieve Nil" in {
    assert(List.map(Nil)(_.toString) == Nil)
  }

  "executing reverse on Nil" should "retrieve Nil" in {
    assert(List.reverse(Nil) == Nil)
  }

  "executing reverse on List(1,2,3)" should "retrieve List(3,2,1)" in {
    assert(List.reverse(List(1,2,3)) == List(3,2,1))
  }

  "executing reverse on List(1)" should "retrieve List(1)" in {
    assert(List.reverse(List(1)) == List(1))
  }

  "executing append on List(1) and List(2,3)" should "retrieve List(1,2,3)" in {
    assert(List.append2(List(1), List(2,3)) == List(1,2,3))
  }

  "executing append on Nil and List(2,3)" should "retrieve List(2,3)" in {
    assert(List.append2(Nil, List(2,3)) == List(2,3))
  }

  "executing append on List(1) and Nil" should "retrieve List(1)" in {
    assert(List.append2(List(1), Nil) == List(1))
  }

  "executing append on Nil and Nil" should "retrieve Nil" in {
    assert(List.append2(Nil, Nil) == Nil)
  }

  "executing filter on List(1,2,3,4,5,6) filtering even numbers" should "retrieve List(2,4,6)" in {
    assert(List.filter(List(1,2,3,4,5,6))(n => n % 2 == 0 ) == List(2,4,6))
  }

  "executing concat on List(List(1), List(2), List(3)) filtering even numbers" should "retrieve List(1,2,3)" in {
    assert(List.concat(List(List(1), List(2), List(3))) == List(1,2,3))
  }

  "executing flatMap(List(1,2,3))(i => List(i,i))" should "List(1,1,2,2,3,3)" in {
    assert(List.flatMap(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3))
  }

  "executing zipWith(List(1,2,3), List(4,5,6))(_ + _)" should "List(5,7,9)" in {
    assert(List.zipWith(List(1,2,3), List(4,5,6))(_ + _) == List(5,7,9))
  }

  "executing zipWith(List(1,2,3,5,6,7), List(4,5,6))(_ + _)" should "List(5,7,9)" in {
    assert(List.zipWith(List(1,2,3,5,6,7), List(4,5,6))(_ + _) == List(5,7,9))
  }

  "executing zipWith(List(1,2,3,5,6,7), Nil)(_ + _)" should "return Nil" in {
    assert(List.zipWith(List(1,2,3,5,6,7), Nil)(_ + _) == Nil)
  }

  "executing hasSubsequence(List(1,2,3,5,6,7), Nil)" should "return true" in {
    assert(List.hasSubsequence(List(1,2,3,5,6,7), Nil))
  }

  "executing hasSubsequence(List(1,2,3,5,6,7), List(3,2,1))" should "return false" in {
    assert(!List.hasSubsequence(List(1,2,3,5,6,7), List(3,2,1)))
  }

  "executing hasSubsequence(List(1,2,3,5,6,7), List(1,2,3))" should "return true" in {
    assert(List.hasSubsequence(List(1,2,3,5,6,7), List(1,2,3)))
  }

  "executing hasSubsequence(List(1,2,3,5,6,7), List(5,6,7))" should "return true" in {
    assert(List.hasSubsequence(List(1,2,3,5,6,7), List(5,6,7)))
  }

  "executing hasSubsequence(List(1,2,3,5,6,7), List(3,5))" should "return true" in {
    assert(List.hasSubsequence(List(1,2,3,5,6,7), List(3,5)))
  }

}
