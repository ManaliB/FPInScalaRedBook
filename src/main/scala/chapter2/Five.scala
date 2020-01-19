package chapter2

object Five extends App {

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
