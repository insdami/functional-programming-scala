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



}
