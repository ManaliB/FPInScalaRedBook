package chapter5

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  "toList" should "initialize all lazy values of a Stream" in {
    Stream[String]("A", "B", "C").toList shouldEqual List[String]("A", "B","C")
  }
  "toList" should "should work for empty Stream" in {
    Stream[String]().toList shouldEqual Nil
  }

  "take" should "return 2 results" in {
    Stream("A", "B", "C").take(2).toList shouldEqual Stream("A", "B").toList.reverse
  }
  "take" should "return empty in case of empty Stream" in {
    Stream().take(2).toList shouldEqual Stream().toList
  }
  "take" should "return all values if the Stream size is smaller than take value" in {
    Stream("A").take(2).toList shouldEqual Stream("A").toList
  }
}