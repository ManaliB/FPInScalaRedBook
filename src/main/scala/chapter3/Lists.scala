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

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
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
      case x if (n <= 0) => x
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  // 3.5
  def dropWhile[A](ds: List[A], fn: A => Boolean): List[A] = {
    ds match {
      case Cons(x, xs) if (fn(x)) => dropWhile(xs, fn)
      case _ => ds
    }
  }

  // 3.6
  def init[A](ds: List[A]): List[A] = {
    ds match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  } // dangerous5

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightInTermsOfFoldLeft[A, B](ds: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(ds), z)((acc, ele) => f(ele, acc))
  }

  def foldRightInTermsOfFoldLeft2[A, B](ds: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(ds, (x: B) => x)((acc, ele) => some => acc(f(ele, some)))(z)
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // 3.9
  def length[A](ds: List[A]): Int = {
    foldRight(ds, 0)((_, b) => 1 + b)
  }

  //3.10
  @tailrec
  def foldLeft[A, B](ds: List[A], z: B)(f: (B, A) => B): B = {
    ds match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def foldLeftITFFoldRight[A, B](ds: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(ds, (b: B) => b)((ele, acc) => some => acc(f(some, ele)))(z)
  }

  // 3.11
  def sumWFL(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  // 3.11
  def productWFL(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  // 3.11
  def lengthWFL[A](ds: List[A]): Int = {
    foldLeft(ds, 0)((b, _) => 1 + b)
  }

  // 3.12
  def reverse[A](ds: List[A]): List[A] = {
    foldLeft[A, List[A]](ds, Nil)((revList, lastEle) => Cons(lastEle, revList))
  }

  // 3.14
  def appendWithFold[A](ele: A, ds: List[A]): List[A] = {
    foldRight(ds, Cons(ele, Nil))((ele, newList) => Cons(ele, newList))
  }

  def concat[A](ds: List[List[A]]): List[A] = {
    foldRight(ds, List[A]())(append)
  }

  // 3.16
  def transformToAdd1(ds: List[Int]): List[Int] = {
    foldRight(ds, Nil: List[Int])((ele, acc) => Cons(ele + 1, acc))
  }

  // 3.17
  def transformToString(ds: List[Double]): List[String] = {
    foldRight(ds, Nil: List[String])((ele, acc) => Cons(ele.toString, acc))
  }

  // 3.18
  def map[A, B](ds: List[A])(f: A => B): List[B] = {
    //foldRight(ds, Nil: List[B])((ele, acc) => Cons(f(ele), acc))
    foldRightInTermsOfFoldLeft(ds, Nil: List[B])((ele, acc) => Cons(f(ele), acc))
  }

  // 3.19
  def filter[A](ds: List[A])(f: A => Boolean): List[A] = {
    foldRightInTermsOfFoldLeft(ds, Nil: List[A])((ele, acc) => if (f(ele)) {
      Cons(ele, acc)
    } else acc)
  }

  // 3.19
  def removeOdd(ds: List[Int]) = {
    filter(ds)(a => a % 2 == 0)
  }

  // 3.20
  def flatMap[A, B](ds: List[A])(f: A => List[B]): List[B] = {
    foldLeft(ds, Nil: List[B])((acc, ele) => append(acc, f(ele))) // or swap the append lists and foldRight
  }

  // 3.21
  def filterWFlatMap[A](ds: List[A])(f: A => Boolean): List[A] = {
    flatMap(ds)(a => if(f(a)) List(a) else Nil)
  }

  // 3.21
  def removeOdd1(ds: List[Int]) = {
    filterWFlatMap(ds)(a => a % 2 == 0)
  }

  // 3.22
  def addListValues(ds: List[Int], ds1: List[Int]): List[Int] = {
    (ds, ds1) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x,xs), Cons(y,ys)) => Cons(x + y, addListValues(xs, ys))
    }
  }

  // 3.23
  def zipWith[A, B, C](ds: List[A], ds1: List[B])(f: (A, B) => C): List[C] = {
    (ds, ds1) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x,xs), Cons(y,ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }
  }
}





