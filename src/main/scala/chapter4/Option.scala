package chapter4

sealed trait Option[+A] {
  // 4.1
  def map[B](f: A => B): Option[B] = this match {
      case Some(x) => Some(f(x))
      case _ => None
    }

  // 4.1
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case _ => None
  }

  // 4.1
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case _ => default
  }

  // 4.1
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(x) => Some(x)
    case _ => ob
  }

  // 4.1
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => this
    case _ => None
  }

  // 4.1
  private def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }
}

object Option {
  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(ax => b.map(bx => f(ax,bx)))

  // 4.4
  def sequence[A](a: List[Option[A]]) : Option[List[A]] = a match {
    case Nil => None
    case x :: xs => x.flatMap(xx => sequence(xs).map(xss => xx :: xss))
  }

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =  a match {
    case Nil => Some(Nil)
    case x :: xs => f(x).flatMap(xx => traverse(xs)(f).map(xss => xx :: xss)) // or map2.
  }
// 4.5
  def sequenceITFTraverse[A](a: List[Option[A]]) : Option[List[A]] = traverse(a)(aa => aa)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
