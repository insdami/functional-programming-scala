package l.insdami.funcprogscala.c3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)((a) => 1)(1 + _ + _)

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l: Tree[Int], r: Tree[Int]) => maximum(l) max maximum(r)
  }

  def maximum2(tree: Tree[Int]): Int = fold(tree)((a: Int) => a)(_ max _)

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) + depth(r))
  }

  def depth2[A](tree: Tree[A]): Int = fold(tree)((a) => 0)(1 + _ + _)

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)((a) => Leaf(f(a)).asInstanceOf[Tree[B]])(Branch(_, _))

  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g),fold(r)(f)(g))
  }
}
