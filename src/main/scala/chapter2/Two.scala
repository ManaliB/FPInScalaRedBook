package chapter2

import scala.annotation.tailrec

object Two extends App {

  def isSorted[A](as: Array[A])(ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int, acc:Boolean): Boolean = {
      if(!acc) false // Fail-fast
      else if(n == as.length -1) acc
      else loop(n+1, ordered(as(n), as(n+1)))
    }

    loop(0, true)
  }
}
