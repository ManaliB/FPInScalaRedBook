package chapter2

import org.scalatest._

class OneSpec extends FlatSpec with Matchers {
  "fib" should "return 1st number as 0" in {
    One.fib(1) shouldEqual 0
  }
  "fib" should "return 2nd number as 1" in {
    One.fib(2) shouldEqual 1
  }
  "fib" should "return 6th number as 5" in {
    One.fib(6) shouldEqual 5
  }
  "fib" should "return 0" in {
    One.fib(0) shouldEqual 0
  }
  //  "fib" should "return 10000th number as 5" in {
  //    One.fib(10000) shouldEqual 5
  //  }
  "fibWithTailRec" should "return 1st number as 0" in {
    One.fibWithTailRec(1) shouldEqual 0
  }
  "fibWithTailRec" should "return 2nd number as 1" in {
    One.fibWithTailRec(2) shouldEqual 1
  }
  "fibWithTailRec" should "return 6th number as 5" in {
    One.fibWithTailRec(6) shouldEqual 5
  }
  "fibWithTailRec" should "return 10000th number as 890489442" in {
    One.fibWithTailRec(10000) shouldEqual 890489442
  }
  "fibWithTailRec" should "return 0" in {
    One.fibWithTailRec(0) shouldEqual 0
  }
}
