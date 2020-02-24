package chapter3

import scala.annotation.tailrec


sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List extends App { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // 3.1 (Answer = 3)
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  //3.2
  def tail[A](ds: List[A]): List[A] = {
    ds match {
      case Nil => throw new UnsupportedOperationException("tail of empty list")
      case Cons(_, x) => x
    }
  }

  //3.3
  def setHead[A](x: A, ds: List[A]) = {
    ds match {
      case Nil => throw new UnsupportedOperationException("cannot set head of empty list")
      case Cons(_, xs) => Cons(x, xs)
    }
  }

  //3.4
  def drop[A](ds: List[A], n: Int): List[A] = {
      ds match {
        case Nil => Nil
        case x if(n <= 0) => x
        case Cons(x, xs) => drop(xs, n-1)
    }
  }

  // 3.5
  def dropWhile[A](ds: List[A], fn: A => Boolean): List[A] = {
    ds match {
      case Cons(x, xs) if(fn(x)) => dropWhile(xs, fn)
      case _ => ds
    }
  }

  def init[A](ds: List[A]): List[A] = {
    ds match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) =>  Cons(x, init(xs))
    }
  } // dangerous5


}





