package chapter5

import scala.annotation.tailrec

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty
// 1
  def toList: List[A] = {
    @tailrec
    def go(uc: Option[(A, Stream[A])], acc: List[A]): List[A] = {
      if (uc.isEmpty) {
        acc ::: Nil
      } else {
        go(uc.get._2.uncons, acc ::: uc.get._1 :: Nil)
      }
    }
    go(this.uncons, Nil)
  }

  // 2
  // Fixme This gets results in reverse
  def take(n: Int): Stream[A] = {
    @tailrec
    def go(uc: Stream[A], accUc: Stream[A], counter: Int): Stream[A] = {
      if (uc.isEmpty || counter <= 0) {
        accUc
      } else {
        go(uc.uncons.get._2, Stream.cons(uc.uncons.get._1, accUc), counter - 1)
      }
    }
    go(this, Stream.empty, n)
  }

  // 3
  def takeWhile(p: A => Boolean): Stream[A] = {
    @tailrec
    def go(uc: Stream[A], accUc: Stream[A]): Stream[A] = {
      if (uc.isEmpty) {
        accUc
      } else if (p(uc.uncons.get._1)) {
        go(uc.uncons.get._2, Stream.cons(uc.uncons.get._1, accUc))
      }
      else {
        go(uc.uncons.get._2, accUc)
      }
    }

    go(this, Stream.empty)
  }

}
object Stream {
  def empty[A]: Stream[A] = new Stream[A] {
      def uncons = None
    }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =  new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}