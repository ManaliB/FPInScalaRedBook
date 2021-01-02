package chapter3

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  "size" should "find the size of the tree" in {
    Tree.size(Leaf(2)) shouldBe 1
    Tree.size(Branch(Leaf(4), Leaf(2))) shouldBe 3
    Tree.size(Branch(Branch(Leaf(5), Leaf(17)), Leaf(-66))) shouldBe 5
  }

  "maximum" should "find maximum integer in an integer tree" in {
    Tree.maximum(Leaf(2)) shouldBe 2
    Tree.maximum(Branch(Leaf(4), Leaf(2))) shouldBe 4
    Tree.maximum(Branch(Branch(Leaf(5), Leaf(17)), Leaf(-66))) shouldBe 17
  }

}
