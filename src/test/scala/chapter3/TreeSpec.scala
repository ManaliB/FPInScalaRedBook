package chapter3

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

// 3.25
  "size" should "find the size of the tree" in {
    Tree.size(Leaf(2)) shouldBe 1
    Tree.size(Branch(Leaf(4), Leaf(2))) shouldBe 3
    Tree.size(Branch(Branch(Leaf(5), Leaf(17)), Leaf(-66))) shouldBe 5
  }

  // 3.26
  "maximum" should "find maximum integer in an integer tree" in {
    Tree.maximum(Leaf(2)) shouldBe 2
    Tree.maximum(Branch(Leaf(4), Leaf(2))) shouldBe 4
    Tree.maximum(Branch(Branch(Leaf(5), Leaf(17)), Leaf(-66))) shouldBe 17
  }

  // 3.27
  "depth" should "find the depth of a tree" in {
    Tree.depth(Leaf(2)) shouldBe 1
    Tree.depth(Branch(Leaf(4), Leaf(2))) shouldBe 2
    Tree.depth(Branch(Leaf(-66), Branch(Leaf(5), Leaf(17)))) shouldBe 3
    Tree.depth(Branch(Branch(Leaf(5), Leaf(17)), Leaf(-66))) shouldBe 3
    Tree.depth(Branch(Branch(Branch(Leaf(55), Leaf(90)), Leaf(17)), Leaf(-66))) shouldBe 4
  }

  // 3.28
  "map" should "multiply all elements in tree by 2" in {
    Tree.map(Leaf(2))(_ * 2) shouldBe Leaf(4)
    Tree.map(Branch(Leaf(4), Leaf(2)))(_ * 2) shouldBe Branch(Leaf(8), Leaf(4))
    Tree.map(Branch(Leaf(-66), Branch(Leaf(5), Leaf(17))))(_ * 2) shouldBe Branch(Leaf(-132), Branch(Leaf(10), Leaf(34)))
    Tree.map(Branch(Branch(Leaf(5), Leaf(17)), Leaf(-66)))(_ * 2) shouldBe Branch(Branch(Leaf(10), Leaf(34)), Leaf(-132))
    Tree.map(Branch(Branch(Branch(Leaf(55), Leaf(0)), Leaf(17)), Leaf(-66)))(_ * 2) shouldBe Branch(Branch(Branch(Leaf(110), Leaf(0)), Leaf(34)), Leaf(-132))
  }

  // 3.29
  "sizeWithFold" should "find the size of the tree" in {
    Tree.sizeWithFold(Leaf(2)) shouldBe 1
    Tree.sizeWithFold(Branch(Leaf(4), Leaf(2))) shouldBe 3
    Tree.sizeWithFold(Branch(Branch(Leaf(5), Leaf(17)), Leaf(-66))) shouldBe 5
  }

  "maximumWithFold" should "find maximum integer in an integer tree" in {
    Tree.maximumWithFold(Leaf(2)) shouldBe 2
    Tree.maximumWithFold(Branch(Leaf(4), Leaf(2))) shouldBe 4
    Tree.maximumWithFold(Branch(Branch(Leaf(5), Leaf(17)), Leaf(-66))) shouldBe 17
  }

  "depthWithFold" should "find the depth of a tree" in {
    Tree.depthWithFold(Leaf(2)) shouldBe 1
    Tree.depthWithFold(Branch(Leaf(4), Leaf(2))) shouldBe 2
    Tree.depthWithFold(Branch(Leaf(-66), Branch(Leaf(5), Leaf(17)))) shouldBe 3
    Tree.depthWithFold(Branch(Branch(Leaf(5), Leaf(17)), Leaf(-66))) shouldBe 3
    Tree.depthWithFold(Branch(Branch(Branch(Leaf(55), Leaf(90)), Leaf(17)), Leaf(-66))) shouldBe 4
  }

  "mapWithFold" should "multiply all elements in tree by 2" in {
    Tree.mapWithFold(Leaf(2))(_ * 2) shouldBe Leaf(4)
    Tree.mapWithFold(Branch(Leaf(4), Leaf(2)))(_ * 2) shouldBe Branch(Leaf(8), Leaf(4))
    Tree.mapWithFold(Branch(Leaf(-66), Branch(Leaf(5), Leaf(17))))(_ * 2) shouldBe Branch(Leaf(-132), Branch(Leaf(10), Leaf(34)))
    Tree.mapWithFold(Branch(Branch(Leaf(5), Leaf(17)), Leaf(-66)))(_ * 2) shouldBe Branch(Branch(Leaf(10), Leaf(34)), Leaf(-132))
    Tree.mapWithFold(Branch(Branch(Branch(Leaf(55), Leaf(0)), Leaf(17)), Leaf(-66)))(_ * 2) shouldBe Branch(Branch(Branch(Leaf(110), Leaf(0)), Leaf(34)), Leaf(-132))
  }
}
