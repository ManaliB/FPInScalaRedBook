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
}
