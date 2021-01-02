package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // 3.27
  def depth(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => 1
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

 // 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // 3.29
  def fold[A, B](tree: Tree[A])(f: A => B)(f1: (B, B) => B): B = tree match {
    case Leaf(x) => f(x)
    case Branch(left, right) => f1(fold(left)(f)(f1), fold(right)(f)(f1))
  }

  def sizeWithFold[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)(_ + _ + 1)
  }

  def maximumWithFold(tree: Tree[Int]): Int = {
    fold(tree)((i: Int) => i)(_ max _)
  }

  def depthWithFold(tree: Tree[Int]): Int = {
    fold(tree)(_ => 1)((x, y) => (x max y) + 1)
  }

  def mapWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
  }
}
