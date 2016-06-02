package l.insdami.funcprogscala.c2

object HigherFunctions {

  def fib(n: Int): Int = {
    def go(pos: Int, pth: Int, lth: Int): Int = pos match {
      case 0 => pth
      case _ => go(pos - 1, lth, (pth + lth))
    }
    go(n, 0, 1)
  }

}
