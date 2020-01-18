package chapter2

import scala.annotation.tailrec

object One extends App {
    // Not tail recursive
  def fib(n: Int): Int = {
    def go(n: Int): Int = {
      if(n <= 1) 0
      else if(n == 2) 1
      else (go(n-1) + go(n-2))
    }

    go(n)
 }

  def fibWithTailRec(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, curr: Int): Int = {
      if (n <= 1) prev
      else go(n - 1, curr, curr + prev)
    }

    go(n, 0, 1)
  }
}





