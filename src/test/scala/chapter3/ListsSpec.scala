package chapter3

import org.scalatest._

class ListsSpec extends FlatSpec with Matchers {

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


  // 3.13
  "foldRightInTermsOfFoldLeft" should "break" in {
    List.foldRightInTermsOfFoldLeft2(List(1,2,0,4,5), Nil:List[Int])(Cons(_, _)) shouldBe List(1,2,0,4,5)
  }

  // 3.10 and 3.11
  "foldLeft" should "sum" in {
    List.sumWFL(List(1,2,3,4,5)) shouldBe 15
    List.foldLeft(List(1,2,3,4,5), 0)(_ + _) shouldBe 15
    List.foldLeftITFFoldRight(List(1,2,3,4,5), 0)(_ + _) shouldBe 15
  }

  // 3.11
  "foldLeft" should "sum 0" in {
    List.sumWFL(List[Int]()) shouldBe 0
    List.foldLeft(List[Int](), 0)(_ + _) shouldBe 0
  }

  // 3.11
  "foldLeft" should "sum 1" in {
    List.sumWFL(List(5)) shouldBe 5
    List.foldLeft(List(5), 0)(_ + _) shouldBe 5
  }

  // 3.11
  "length" should "return 5" in {
    List.lengthWFL(List(1,2,0,4,5)) shouldBe 5
    List.length(List(1,2,0,4,5)) shouldBe 5
  }

  // 3.11
  "length" should "return 0" in {
    List.lengthWFL(List()) shouldBe 0
    List.length(List()) shouldBe 0
  }

  // 3.12
  "reverse" should "reverse a list" in {
    List.reverse(List(1,2,3,4,5)) shouldBe List(5,4,3,2,1)
  }
  // 3.12
  "reverse" should "an empty list" in {
    List.reverse(List()) shouldBe List()
  }
  // 3.12
  "reverse" should "reverse one-element list" in {
    List.reverse(List(1)) shouldBe List(1)
  }

  // 3.14
  "append" should "append to a list" in {
    List.appendWithFold(6, List(1,2,3,4,5)) shouldBe List(1,2,3,4,5,6)
  }
  // 3.14
  "append" should "append to an empty list" in {
    List.appendWithFold(1, List()) shouldBe List(1)
  }
  // 3.14
  "append" should "append to a one-element list" in {
    List.appendWithFold(2, List(1)) shouldBe List(1, 2)
  }

  // 3.15
  "concat" should "concate two lists" in {
    List.concat(List(List(1, 2, 3, 4), List(5, 6))) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  // 3.15
  "concat" should "concate a list to an empty list" in {
    List.concat(List(List(), List(5, 6))) shouldBe List(5, 6)
  }

  // 3.15
  "concat" should "concate an empty list to a list" in {
    List.concat(List(List(1, 2, 3, 4), List())) shouldBe List(1, 2, 3, 4)
  }

  // 3.16
  "transform" should "add one and not modify the list" in {
    List.transformToAdd1(List(1,2,3,4,5)) shouldBe List(2,3,4,5,6)
  }

  // 3.17
  "transform" should "convert Double to String" in {
    List.transformToString(List(1.0,2.0,3.0,4.0,5.0)) shouldBe List("1.0","2.0","3.0","4.0","5.0")
  }

  // 3.19
  "removeOdd" should "remove odd numbers" in {
    List.removeOdd(List(1,2,3,4,5)) shouldBe List(2,4)
  }

  // 3.20
  "flatMap" should "i => List(i, i)" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) shouldBe List(1,1,2,2,3,3)
  }

  // 3.21
  "removeOdd1" should "remove odd numbers" in {
    List.removeOdd1(List(1,2,3,4,5)) shouldBe List(2,4)
  }

  //3.22
  "addListValues" should "add" in {
    List.addListValues(List(1,2,3), List(4,5,6)) shouldBe List(5,7,9)
  }

  //3.24
  "hasSubsequence" should "return true" in {
    List.hasSubsequence(List(1,2,3), List(1,2)) shouldBe true
    List.hasSubsequence(List(1,2,3), List(2,3)) shouldBe true
    List.hasSubsequence(List(1,2,3,4), List(4)) shouldBe true
    List.hasSubsequence(Nil, Nil) shouldBe true
    List.hasSubsequence(List(1,2,3,4), Nil) shouldBe true
  }

  //3.24
  "hasSubsequence" should "return false" in {
    List.hasSubsequence(List(1,2,3), List(3,2,1,4)) shouldBe false
    List.hasSubsequence(List(1,2,3), List(1,2,3,4)) shouldBe false
    List.hasSubsequence(List(1,2,3), List(3,2,1)) shouldBe false
    List.hasSubsequence(List(1,2,3), List(5,6)) shouldBe false
    List.hasSubsequence(Nil, List(4)) shouldBe false
  }
}
