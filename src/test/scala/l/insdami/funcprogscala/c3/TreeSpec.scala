package l.insdami.funcprogscala.c3

import org.scalatest.FlatSpec

class TreeSpec extends FlatSpec {

  "size on a Tree with 3 branches and 4 leaf" should "retrieve a size of 7" in {
    val tree = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    assert(Tree.size(tree) == 7)
  }

  "maximum on a Tree which contains 5 as mas value" should "retrieve as maximum value 5" in {
    val tree = Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Leaf(5)))
    assert(Tree.maximum(tree) == 5)
  }

  "depth on Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Leaf(5)))" should "retrieve 3 as max depth" in {
    val tree = Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Leaf(5)))
    assert(Tree.depth(tree) == 3)
  }

  "depth on Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))" should "retrieve 2 as max depth" in {
    val tree = Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))
    assert(Tree.depth(tree) == 2)
  }

  "map Tree[Int] converting each element into string" should "retrieve Tree[String]" in {
    val intTree = Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Leaf(5)))
    val stringTree = Branch(Branch(Leaf("1"), Leaf("3")), Branch(Leaf("2"), Leaf("5")))
    assert(Tree.map(intTree)(_.toString) == stringTree)
  }

  "size on a Tree with 3 branches and 4 leaf using fold" should "retrieve a size of 7" in {
    val tree = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    assert(Tree.size2(tree) == 7)
  }

  "maximum on a Tree which contains 5 as mas value  using fold" should "retrieve as maximum value 5" in {
    val tree = Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Leaf(5)))
    assert(Tree.maximum2(tree) == 5)
  }

  "depth on Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Leaf(5)))  using fold" should "retrieve 3 as max depth" in {
    val tree = Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Leaf(5)))
    assert(Tree.depth2(tree) == 3)
  }

  "depth on Branch(Branch(Leaf(1), Leaf(3)), Leaf(2)) using fold" should "retrieve 2 as max depth" in {
    val tree = Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))
    assert(Tree.depth2(tree) == 2)
  }

  "map Tree[Int] converting each element into string using fold" should "retrieve Tree[String]" in {
    val intTree = Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Leaf(5)))
    val stringTree = Branch(Branch(Leaf("1"), Leaf("3")), Branch(Leaf("2"), Leaf("5")))
    assert(Tree.map2(intTree)(_.toString) == stringTree)
  }

}
