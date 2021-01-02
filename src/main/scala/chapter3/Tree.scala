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
}
