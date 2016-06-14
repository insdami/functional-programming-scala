package l.insdami.funcprogscala.c3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(a1, a2)(Cons(_, _))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
   foldLeft(reverse(as), z)((b, a) => f(a, b))


  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(h, t) => t
    case _ => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, Cons(x, xs))
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if(n <= 0) l
    else drop(tail(l), n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    def go(source: List[A], init: List[A]): List[A] = source match {
      case Cons(_,Nil) => init
      case Cons(x,xs) => go(xs, append(init, Cons(x, Nil)))
    }
    go(l, Nil)
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((elem, size) => size + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    def go(source: List[A], mapped: List[B] = Nil): List[B] = source match {
      case Nil => mapped
      case Cons(x, xs) => go(xs, append(mapped, Cons(f(x), Nil)))
    }
    go(l)
  }

  def reverse[A](l: List[A]): List[A] = {
    def go(c: List[A], r: List[A] = Nil): List[A] = c match {
      case Nil => r
      case Cons(x, xs) => go(xs, setHead(r, x))
    }
    go(l)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight2(as, Nil: List[A])((e, filtered) =>
    if(f(e)) Cons(e, filtered) else filtered
  )

  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A])(append2)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] = {
    def go(c1: List[A], c2: List[B], zipped: List[C] = Nil): List[C] = (c1, c2) match {
      case (_, Nil) => zipped
      case (Nil, _) => zipped
      case (Cons(x1, xs1), Cons(x2,xs2)) => go(xs1, xs2, append2(zipped, Cons(f(x1, x2), Nil)))
    }
    go(l1, l2)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (Cons(x1, xs1), Cons(x2, xs2)) if x1 == x2 => hasSubsequence(xs1, xs2)
      case (Cons(x1, xs1), Cons(x2, xs2)) if x1 != x2 => hasSubsequence(dropWhile(sup, ((e: A) => e != x2)), xs2)
      case _ => false
    }
  }
}