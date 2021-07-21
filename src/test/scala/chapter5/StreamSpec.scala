package chapter5

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  "toList" should "initialize all lazy values of a Stream" in {
    Stream[String]("A", "B", "C").toList shouldEqual List[String]("A", "B","C")
  }
  "toList" should "should work for empty Stream" in {
    Stream[String]().toList shouldEqual Nil
  }

}