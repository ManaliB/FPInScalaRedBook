package chapter3

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  "maximum" should "find maximum integer in an integer tree" in {
    Tree.maximum(Leaf(2)) shouldBe 2
    Tree.maximum(Branch(Leaf(4), Leaf(2))) shouldBe 4
    Tree.maximum(Branch(Branch(Leaf(5), Leaf(17)), Leaf(-66))) shouldBe 17
  }
}
