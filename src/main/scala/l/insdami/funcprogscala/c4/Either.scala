package l.insdami.funcprogscala.c4

import scala.annotation.tailrec

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(e) => Left(e)
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(e) => Left(e)
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => this
    case Left(e) => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C] = (this, b) match {
    case (Right(a), Right(b)) => Right(f(a,b))
    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)
  }
  def map2_forcomp[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    @tailrec
    def go(l: List[Either[E, A]], acc: Either[E, List[A]] = Right(List.empty)): Either[E, List[A]] = l match {
      case Nil => acc
      case Right(v) :: xs => go(xs, acc.map(_ :+ v))
      case Left(e) :: _ => Left(e)
    }
    go(es)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    @tailrec
    def go(curr: List[A], acc: Either[E, List[B]] = Right(List.empty[B])): Either[E, List[B]] = curr match {
      case Nil => acc
      case x :: xs => f(x) match {
        case Right(v) => go(xs, acc.map(_ :+ v))
        case Left(e) => Left(e)
      }
    }
    go(as)
  }

  def traverseWithSequence[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sequence(as.map(f))
}
