package chapter5

import scala.annotation.tailrec

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

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