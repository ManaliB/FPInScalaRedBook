package chapter4


case class Right[+A](value: A) extends Either[Nothing, A]
case class Left[+E](value: E) extends Either[E, Nothing]
// 4.6
trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(b) => Left(b)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(b) => Left(b)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => x
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(aa => b.map(bb => f(aa, bb)))

}


// 4.7
object Either{

  def sequence[E,A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match {
      case Nil => Right(Nil)
      case x :: xs => x.flatMap(xx => sequence(xs).map(xss => xx :: xss))
    }
  }

  def traverse[E, A, B](as:List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case x :: xs => f(x).map2(traverse(xs)(f))(_ :: _)
    }
  }
}
