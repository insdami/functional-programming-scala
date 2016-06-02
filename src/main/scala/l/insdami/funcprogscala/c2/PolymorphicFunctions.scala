package l.insdami.funcprogscala.c2

import scala.util.{Failure, Success, Try}

object PolymorphicFunctions {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    Try(as.length) match {
      case Failure(_) => false
      case Success(length) if length < 2 => true
      case Success(length) if length == 2 => ordered(as(0), as(1))
      case Success(_) if !ordered(as(0), as(1)) => false
      case Success(_) if ordered(as(0), as(1)) => isSorted(as.slice(1, as.size), ordered)
    }
  }


  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = f(a, _)

}
