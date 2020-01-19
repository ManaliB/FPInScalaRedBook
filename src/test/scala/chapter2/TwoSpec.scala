package chapter2

import org.scalatest.{FlatSpec, Matchers}

class TwoSpec extends FlatSpec with Matchers {
  "isSorted" should "return true for only one element" in {
    Two.isSorted(Array(1), (a: Int, b: Int) => a > b) shouldEqual true
  }
  "isSorted" should "return false for descending" in {
    Two.isSorted(Array(1,2,3,4,5,6), (a: Int, b: Int) => a > b) shouldEqual false
  }
  "isSorted" should "return false for descending 2" in {
    Two.isSorted(Array(1,2,3,2,1), (a: Int, b: Int) => a > b) shouldEqual false
  }
  "isSorted" should "return true for descending" in {
    Two.isSorted(Array(6,5,4,3,2,1), (a: Int, b: Int) => a > b) shouldEqual true
  }
  "isSorted" should "return true for ascending" in {
    Two.isSorted(Array(1,2,3,4,5,6), (a: Int, b: Int) => a < b) shouldEqual true
  }
  "isSorted" should "return false for ascending" in {
    Two.isSorted(Array(6,5,4,3,2,1), (a: Int, b: Int) => a < b) shouldEqual false
  }
  "isSorted" should "return false for ==" in {
    Two.isSorted(Array(1,1,2,3,1), (a: Int, b: Int) => a == b) shouldEqual false
  }
  "isSorted" should "return true for ==" in {
    Two.isSorted(Array(1,1,1), (a: Int, b: Int) => a == b) shouldEqual true
  }
  "isSorted" should "return true for String eq" in {
    Two.isSorted(Array("Abc", "Abc", "Abc", "Abc"), (a: String, b: String) => a == b) shouldEqual true
  }
  "isSorted" should "return true for empty Strings eq" in {
    Two.isSorted(Array("", ""), (a: String, b: String) => a == b) shouldEqual true
  }
}
