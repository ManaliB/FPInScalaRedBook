package chapter3

import org.scalatest._

class OneSpec extends FlatSpec with Matchers {
  "drop" should "drop first 2 elements" in {
    List.drop(List(1,2,3,4,5), 2) shouldBe List(3,4,5)
  }

  "drop" should "drop first 2 elements with empty list" in {
    List.drop(List(), 2) shouldBe Nil
  }

  "drop" should "drop first 2 elements with list of size 1" in {
    List.drop(List(1), 2) shouldBe Nil
  }

  "drop" should "drop all elements" in {
    List.drop(List(1,2,3,4,5), 5) shouldBe Nil
  }

  // 3.8
  "foldRight" should "break" in {
    List.foldRight(List(1,2,0,4,5), Nil:List[Int])(Cons(_, _)) shouldBe List(1,2,0,4,5)
  }

  // 3.10
  "foldLeft" should "sum" in {
    List.foldLeft(List(1,2,3,4,5), 0)(_ + _) shouldBe 15
  }

  "foldLeft" should "sum 1" in {
    List.foldLeft(List[Int](), 0)(_ + _) shouldBe 0
  }

  "foldLeft" should "sum 0" in {
    List.foldLeft(List(5), 0)(_ + _) shouldBe 5
  }

  "length" should "return 5" in {
    List.length(List(1,2,0,4,5)) shouldBe 5
  }

  "length" should "return 0" in {
    List.length(List()) shouldBe 0
  }
}
